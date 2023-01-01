###### Data Aggregation #######
library(tidyverse)
library(extrafont)
library(ggridges)
library(RColorBrewer)
library(viridis)
library(nflfastR)
library(ggimage)
#font_import()
windowsFonts("Roboto" = windowsFont("Roboto"))

defensive_influence <- data.frame()
all_blocking_responsibilities <- data.frame()
players <- read_csv("players.csv")
# week1_influence <- read_csv("week1_influence.csv")
# week1_influence %>%
#   group_by(playId, frameId, x, y) %>%
#   mutate(total_weight = sum(influence),
#          influence = influence / total_weight) %>%
#   group_by(nflId, playId, frameId) %>%
#   summarize(avg_influence = mean(influence)) %>%
#   ungroup() %>%
#   group_by(nflId, playId) %>%
#   summarize(net_influence = last(avg_influence) - first(avg_influence)) %>%
#   ungroup() %>%
#   group_by(nflId) %>%
#   summarize(avg_net_influence = mean(net_influence),
#             plays = n()) %>%
#   inner_join(players) %>%
#   filter(officialPosition %in% c("DE", "SS", "FS", "NT", "DT", "CB", "OLB", "MLB", "ILB", "LB", "DB")) %>%
#   arrange(desc(avg_net_influence)) %>%
#   filter(plays > 10) -> test

##### DEFENSIVE ANALYSIS ######

# Load in all weeks
for (wk in 1:8)
{
  week_blocking <- read_csv(paste0("week", wk, "_blocking_assignments.csv"))
  week_influence <- read_csv(paste0("new_week", wk, "_influence.csv"))
  week_track <- read_csv(paste0("week", wk, ".csv"))
  
  week_influence %>%
    inner_join(week_track %>% select(nflId, playId, gameId) %>% distinct()) %>%
    group_by(playId, frameId, x, y) %>%
    mutate(total_weight = sum(influence),
           influence = influence / total_weight) %>%
    group_by(nflId, team, playId, frameId, gameId) %>%
    summarize(avg_influence = mean(influence)) %>%
    ungroup() %>%
    group_by(nflId, team, playId, gameId) %>%
    summarize(net_influence = last(avg_influence) - first(avg_influence)) %>%
    ungroup() %>%
    mutate(week = wk) -> week_results
  
  blocking_responsibilities <- week_blocking %>%
    inner_join(week_track %>% select(nflId, playId, gameId, team) %>% distinct(), by = c('gameId', 'playId', 'lineman_nflId' = 'nflId')) %>%
    group_by(gameId, playId, lineman_nflId, defender_nflId) %>%
    summarize(blocking_frames = n(),
              team = last(team)) %>%
    ungroup() %>%
    group_by(gameId, playId, lineman_nflId, team) %>%
    mutate(responsibility_pct = blocking_frames / sum(blocking_frames)) %>%
    ungroup() %>%
    filter(!is.na(defender_nflId))
  
  defensive_influence <- defensive_influence %>%
    bind_rows(week_results)
  
  all_blocking_responsibilities <- all_blocking_responsibilities %>%
    bind_rows(blocking_responsibilities)
}

##### DEFENSIVE ANALYSIS ######

defensive_influence %>%
  group_by(nflId) %>%
  summarize(avg_net_influence = mean(net_influence),
            sum_net_influence = sum(net_influence),
            plays = n(),
            team = last(team)) %>%
  inner_join(players) %>%
  filter(officialPosition %in% c("DE", "SS", "FS", "NT", "DT", "CB", "OLB", "MLB", "ILB", "LB", "DB")) %>%
  arrange(desc(avg_net_influence)) %>%
  filter(plays > 100) %>%
  group_by(officialPosition) %>%
  mutate(index = scale(avg_net_influence),
         position_rank = 1:n()) -> defensive_players

defensive_players %>%
  select(displayName, officialPosition, avg_net_influence, position_rank) %>%
  filter(position_rank <= 10) %>%
  arrange(desc(officialPosition)) %>%
  print(n = 40)


##### OFFENSIVE ANALYSIS ########

offensive_influence <- all_blocking_responsibilities %>%
  inner_join(defensive_influence %>% select(-team), by = c("gameId" = "gameId", "playId" = "playId", "defender_nflId" = "nflId"))

offensive_players <- offensive_influence %>%
  mutate(weighted_offensive_net_influence = -net_influence * responsibility_pct) %>%
  group_by(gameId, playId, lineman_nflId, team) %>%
  summarize(offensive_net_influence = sum(weighted_offensive_net_influence)) %>%
  ungroup() %>%
  group_by(lineman_nflId) %>%
  summarize(avg_net_influence = mean(offensive_net_influence),
            sum_net_influence = sum(offensive_net_influence),
            plays = n(),
            games = n_distinct(gameId),
            team = last(team)) %>%
  ungroup() %>%
  arrange(desc(avg_net_influence)) %>%
  filter(plays > 200) %>%
  inner_join(players, by = c("lineman_nflId" = "nflId")) %>%
  group_by(officialPosition) %>%
  mutate(index = scale(avg_net_influence),
         position_rank = 1:n())

##### Sanity checks ##### (any player named a team by any major outlet)

# 2021 All-Pro teams 
# Name (Position Rank)
# OFFENSE #
# Trent Williams (30)
# Joel Bitonio (44)
# Jason Kelce (3)
# Zack Martin (5)
# Tristan Wirfs (24)
# Rashawn Slater (43)
# Quenton Nelson (DNQ)
# Corey Linsley (10)
# Wyatt Teller (43)
# Lane Johnson (DNQ)

# DEFENSE #
# TJ Watt (13)
# Myles Garrett (8)
# Aaron Donald (2)
# Cameron Heyward (18)
# Chris Jones (1)
# Micah Parsons (34)
# Shaquille Leonard (DNQ)
# De'Vondre Campbell (DNQ)
# Robert Quinn (1)
# Maxx Crosby (2)
# Jeffery Simmons (7)
# Demario Davis (DNQ)
# Roquan Smith (DNQ)
# Bobby Wagner (DNQ)

##### Visualizations for first four vs second four correlation

### DEFENSE ###
defensive_players_half_season <- defensive_influence %>%
  mutate(half = case_when(week <= 4 ~ 1,
                          week > 4 ~ 2)) %>%
  group_by(nflId) %>%
  summarize(avg_net_influence_first_four = mean(net_influence[half == 1]),
            avg_net_influence_second_four = mean(net_influence[half == 2]),
            sum_net_influence = sum(net_influence),
            plays = n(),
            team = last(team),
            games = n_distinct(gameId)) %>%
  inner_join(players) %>%
  filter(officialPosition %in% c("DE", "SS", "FS", "NT", "DT", "CB", "OLB", "MLB", "ILB", "LB", "DB")) %>%
  filter(plays > 100, games >= 6)

cor(defensive_players_half_season$avg_net_influence_first_four, defensive_players_half_season$avg_net_influence_second_four)^2

def_cor_plot <- defensive_players_half_season %>%
  ggplot(aes(x = avg_net_influence_first_four, y = avg_net_influence_second_four, color = officialPosition)) +
  geom_point(size = 2.5) +
  geom_point(size = 2.5, shape = 1, color = "black") +
  labs(title = "Ownership Gained for Defensive Linemen in First 4 Games vs Last 4 Games",
       subtitle = "Minimum 100 Total Snaps and 6 Games Played",
       x = "Ownership Gained in First 4 Games",
       y = "Ownership Gained in Last 4 Games",
       color = "Position") +
  theme_minimal() +
  annotate(geom = "text", label = "R^2: 0.89", x = -0.013, y = 0.015, size = 5, family = "Roboto") +
  scale_color_viridis(discrete = T) +
  theme(text = element_text(family = "Roboto"))

ggsave("def_cor_plot.jpeg", def_cor_plot, width = 8)

### OFFENSE ###
offensive_players_half_seasons <- offensive_influence %>%
  mutate(weighted_offensive_net_influence = -net_influence * responsibility_pct,
         half = case_when(week <= 4 ~ 1,
                          week > 4 ~ 2)) %>%
  group_by(gameId, playId, lineman_nflId, team, half) %>%
  summarize(offensive_net_influence = sum(weighted_offensive_net_influence),
            half = last(half)) %>%
  ungroup() %>%
  group_by(lineman_nflId) %>%
  summarize(avg_net_influence_first_four = mean(offensive_net_influence[half == 1]),
            avg_net_influence_second_four = mean(offensive_net_influence[half == 2]),
            sum_net_influence = sum(offensive_net_influence),
            plays = n(),
            games = n_distinct(gameId),
            team = last(team)) %>%
  ungroup() %>%
  filter(plays > 200, games >= 6) %>%
  inner_join(players, by = c("lineman_nflId" = "nflId"))

cor(offensive_players_half_seasons$avg_net_influence_first_four, offensive_players_half_seasons$avg_net_influence_second_four)^2

off_cor_plot <- offensive_players_half_seasons %>%
  ggplot(aes(x = avg_net_influence_first_four, y = avg_net_influence_second_four, color = officialPosition)) +
  geom_point(size = 2.5) +
  geom_point(size = 2.5, shape = 1, color = "black") +
  labs(title = "Ownership Gained for Offensive Linemen in First 4 Games vs Last 4 Games",
       subtitle = "Minimum 200 Total Snaps and 6 Games Played",
       x = "Ownership Gained in First 4 Games",
       y = "Ownership Gained in Last 4 Games",
       color = "Position") +
  theme_minimal() +
  annotate(geom = "text", label = "R^2: 0.78", x = -0.013, y = -0.003, size = 5, family = "Roboto") +
  scale_color_viridis(discrete = T) +
  theme(text = element_text(family = "Roboto"))

ggsave("off_cor_plot.jpeg", off_cor_plot, width = 8)

display.brewer.all()


### JOINED ###
all_linemen = rbind(offensive_players, defensive_players)

all_linemen$officialPosition <- factor(all_linemen$officialPosition, levels = c("OLB", "NT", "DT", "DE", "C", "G", "T"))

og_position_dist_plot <- ggplot(all_linemen, aes(x = avg_net_influence, y = officialPosition, fill = officialPosition)) +
  geom_density_ridges(
    aes(point_fill = officialPosition, color = officialPosition), 
    alpha = .2, point_alpha = 1, jittered_points = TRUE
  ) +
  theme_minimal() +
  labs(title = "Ownership Gained for Linemen in First 8 Games",
       subtitle = "Minimum 100 (Defensive) or 200 (Offensive) Total Snaps and 6 Games Played",
       x = "Average Ownership Gained",
       y = "Position") +
  theme(legend.position="none",
        text = element_text(family = "Roboto")) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23, 24, 15, 16, 17)) +
  scale_color_viridis(discrete = T) +
  scale_fill_viridis(discrete = T)
  
ggsave("og_position_dist_plot.jpeg", og_position_dist_plot, width = 8)


#### Individual Players ####
offensive_players <- offensive_players %>%
  inner_join(teams_colors_logos, by = c("team" = "team_abbr"))

defensive_players <- defensive_players %>%
  inner_join(teams_colors_logos, by = c("team" = "team_abbr"))


### Offensive Tackles ###
t_top_10 <- offensive_players %>%
  arrange(desc(avg_net_influence)) %>%
  filter(position_rank <= 10, officialPosition == "T")

t_top_10 %>%
  ggplot(aes(x = avg_net_influence, y = reorder(displayName, avg_net_influence))) +
  geom_col(fill = t_top_10$team_color) +
  geom_image(aes(image = t_top_10$team_logo_wikipedia), nudge_x = ifelse(t_top_10$avg_net_influence > 0, 0.0007, -0.0007),
             size = 0.08) +
  theme_minimal() +
  labs(x = "Ownership Gained Per Play") +
  theme(text = element_text(family = "Roboto"),
        axis.title.y = element_blank()) +
  xlim(c(-0.011, 0.004))

### Offensive guards ###
g_top_10 <- offensive_players %>%
  arrange(desc(avg_net_influence)) %>%
  filter(position_rank <= 10, officialPosition == "G")

g_top_10 %>%
  ggplot(aes(x = avg_net_influence, y = reorder(displayName, avg_net_influence))) +
  geom_col(fill = g_top_10$team_color) +
  geom_image(aes(image = g_top_10$team_logo_wikipedia), nudge_x = 0.0002,
             size = 0.08) +
  theme_minimal() +
  labs(x = "Ownership Gained Per Play") +
  theme(text = element_text(family = "Roboto"),
        axis.title.y = element_blank()) +
  xlim(c(0, 0.004))

#### Centers ###
c_top_10 <- offensive_players %>%
  arrange(desc(avg_net_influence)) %>%
  filter(position_rank <= 10, officialPosition == "C")

c_top_10 %>%
  ggplot(aes(x = avg_net_influence, y = reorder(displayName, avg_net_influence))) +
  geom_col(fill = c_top_10$team_color) +
  geom_image(aes(image = c_top_10$team_logo_wikipedia), nudge_x = 0.0002,
             size = 0.08) +
  theme_minimal() +
  labs(x = "Ownership Gained Per Play") +
  theme(text = element_text(family = "Roboto"),
        axis.title.y = element_blank()) +
  xlim(c(0, 0.004))


