###### Data Aggregation #######
library(tidyverse)
library(extrafont)
library(ggridges)
library(RColorBrewer)
library(viridis)
library(nflfastR)
library(ggimage)
library(gt)
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

### TOP 5 LEADERBOARD ###
Top_5_by_pos = all_linemen %>%
  group_by(officialPosition) %>% 
  filter(position_rank < 6)
Top_5_by_pos$nflId <- ifelse(is.na(Top_5_by_pos$nflId), Top_5_by_pos$lineman_nflId, Top_5_by_pos$nflId)
Top_5_by_pos = Top_5_by_pos %>% relocate(nflId)
Top_5_by_pos = Top_5_by_pos %>% select(-c(lineman_nflId))
Top_5_by_pos = Top_5_by_pos %>% relocate(position_rank)

#load_roster from nflreadr to get 2021 headshots
roster = nflreadr::load_rosters(2021)
names(roster)[7] <- "displayName"
newdf = merge(Top_5_by_pos, roster, by = 'displayName')
newdf = newdf[-c(9), ]

Top_5_by_pos = Top_5_by_pos %>% arrange(displayName)
Top_5_by_pos = bind_cols(Top_5_by_pos,newdf %>% 
            select(headshot_url))

#Cleaned cols and will rename in gt_them_pff
Top_5_clean = Top_5_by_pos %>% 
  select(officialPosition,position_rank,displayName,headshot_url,team,games,plays,
                      avg_net_influence, sum_net_influence)
Top_5_clean = Top_5_clean %>% arrange(officialPosition, position_rank)

Top5_OL = Top_5_clean %>% 
  filter((officialPosition == 'C')|(officialPosition == 'G')|(officialPosition == 'T'))
Top5_DL = Top_5_clean %>% 
  filter((officialPosition == 'DE')|(officialPosition == 'DT')|(officialPosition == 'NT')|
           (officialPosition == 'OLB'))

#gt theme from TheMockUp
gt_theme_pff <- function(data, ...) {
  data %>%
    # Add head shot w/ web_image
    text_transform(
      locations = cells_body(
        vars(headshot_url)
      ),
      fn = function(x) {
        web_image(
          url = x,
          height = 35
        )
      }
    ) %>%
    # Relabel columns
    cols_label(
      position_rank = 'Rank',
      headshot_url = " ",
      plays = "Snaps",
      avg_net_influence = 'AVG Ownership Gained',
      displayName = "Player"
    ) %>%
    # if missing, replace NA w/ ---
    fmt_missing(
      columns = everything(),
      missing_text = "---"
    ) %>%
    # Change font color and weight for numeric col
    tab_style(
      style = list(
        cell_text(color = "black", weight = "bold")
      ),
      locations = cells_body(
        columns = 5
      )
    ) %>%
    # Make column labels and spanners all caps
    opt_all_caps() %>%
    # add row striping
    opt_row_striping() %>%
    # change overall table styling for borders and striping
    tab_options(
      column_labels.background.color = "black",
      table_body.hlines.color = "transparent",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "transparent",
      row.striping.background_color = "#f9f9fb",
      data_row.padding = px(3)
    ) %>%
    cols_width(
      position_rank ~ px(70),
      displayName ~ px(150),
      headshot_url ~ px(60),
      avg_net_influence ~ px(150),
      everything() ~ px(60)
    ) %>% 
    # change color of border separating the text from the sourcenote
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "black", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        rows = nrow(data$`_data`)
      )
    ) %>%
    # change font to Lato throughout (note no need to have Lato locally!)
    opt_table_font(
      font = "Roboto"
      ) %>%
    cols_align("center") %>%
    opt_align_table_header("center") %>%
    tab_style(style = cell_text(whitespace = "nowrap",
                                align = "center"),
              locations = cells_row_groups()) %>%
    tab_style(style = cell_text(v_align = "middle"),
              locations = cells_body())
}

## OL TOP 5
ol_table <- Top5_OL %>% 
  select(officialPosition, position_rank, displayName, headshot_url, avg_net_influence, plays) %>%
  mutate(avg_net_influence = round(avg_net_influence, 4),
         officialPosition = case_when(officialPosition == "C" ~ "Center",
                                      officialPosition == "G" ~ "Guard",
                                      officialPosition == "T" ~ "Tackle")) %>%
  gt(groupname_col = 'officialPosition') %>%
  gt_theme_pff() %>%
  tab_header(title = "Top 5 Offensive Linemen in Ownership Gained by Position",
             subtitle = "First 8 Games of 2021 Season Only | Minimum of 200 Snaps")

## DL TOP 5
dl_table <- Top5_DL %>% 
  select(officialPosition, position_rank, displayName, headshot_url, avg_net_influence, plays) %>%
  mutate(avg_net_influence = round(avg_net_influence, 4),
         officialPosition = case_when(officialPosition == "OLB" ~ "Outside Linebacker",
                                      officialPosition == "NT" ~ "Nose Tackle",
                                      officialPosition == "DT" ~ "Defensive Tackle",
                                      officialPosition == "DE" ~ "Defensive End")) %>%
  gt(groupname_col = 'officialPosition') %>%
  gt_theme_pff() %>%
  tab_header(title = "Top 5 Defensive Linemen in Ownership Gained by Position",
             subtitle = "First 8 Games of 2021 Season Only | Minimum of 100 Snaps")

gtsave(ol_table, "ol_leaderboard_table.png")
