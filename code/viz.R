##### VISUALIZATIONS ######
source("field_control.R")
library(ggnewscale)
week1 <- read_csv("week1.csv")
pff <- read_csv("pffScoutingData.csv")
plays <- read_csv("plays.csv")
week1_blocking <- read_csv("../data/week1_blocking_assignments.csv")
library(extrafont)
library(ggridges)
library(RColorBrewer)
library(viridis)
library(nflfastR)
library(ggimage)
#font_import()
windowsFonts("Roboto" = windowsFont("Roboto"))

###### Get screenshot of example play for illustrating how the backtracking and forward checking works

play_end_events <- c("pass_forward", "autoevent_passforward", "run", "qb_sack", "qb_strip_sack", "out_of_bounds",
                     "fumble_offense_recovered", "handoff", "fumble", "first_contact")

week1 <- week1 %>%
  mutate(x = ifelse(playDirection == "left", 120 - x, x),
         y = ifelse(playDirection == "left", 160/3 - y, y),
         dir = case_when(playDirection == "left" & dir <= 180 ~ dir + 180,
                         playDirection == "left" & dir > 180 ~ dir - 180,
                         T ~ dir),
         o = case_when(playDirection == "left" & o <= 180 ~ o + 180,
                       playDirection == "left" & o > 180 ~ o - 180,
                       T ~ o),
         dir = dir / 180 * pi,
         o = o / 180 * pi) %>%
  left_join(pff) %>% # Now add the info from PFF and the play by play info
  left_join(plays) %>%
  group_by(gameId, playId) %>%
  mutate(rushing_qb = pff_role == "Pass Rush",
         snap_frame = max(frameId[event == "ball_snap"]),
         end_frame = first(frameId[event %in% play_end_events]),
         los = first(x[team == "football" & frameId == snap_frame]),
         end_box_up = max(y[pff_role == "Pass Block" & frameId == snap_frame], na.rm = T),
         end_box_down = min(y[pff_role == "Pass Block" & frameId == snap_frame], na.rm = T)) %>%
  ungroup() %>%
  filter(pff_role %in% c("Pass Rush", "Pass Block") | team == "football") %>% # Only keep the blockers, rushers, and the ball 
  filter(frameId >= snap_frame, frameId <= end_frame)

example_play <- week1 %>%
  filter(gameId == 2021090900, playId == 97, frameId == snap_frame)

example_play_plot <- example_play %>%
  filter(team != "football") %>%
  ggplot(aes(x = y, y = x, color = team, group = nflId)) +
  geom_point(size = 15) +
  geom_text(data = example_play %>% filter(team != "football"), aes(x = y, y = x, label = jerseyNumber), color = "white",
                        vjust = 0.36, size = 7.5) +
  coord_fixed() +
  scale_color_manual(values = c("#e31837", "#002244"), guide = "none") +
  xlim(c(18, 34)) +
  ylim(c(40, 45)) +
  theme(text = element_text(family = "Roboto")) +
  theme_void()

ggsave(plot = example_play_plot, filename = "example_play.jpeg", width = 8, units = "in")



# create sample play gif to illustrate how the field control works over the course of a play

example_play <- week1 %>%
  filter(gameId == 2021090900, playId == 97)

tictoc::tic()
example_play_influence <- find_play_influence(97, example_play)
tictoc::toc()

team_example_play_influence <- example_play_influence %>%
  group_by(x, y, frameId) %>%
  summarize(team_control = sum(influence[team == "TB"]) - sum(influence[team == "DAL"]), # use exponential function as defined in Fernandez and Bornn's paper
            team_control = exp(team_control) / (1 + exp(team_control)),
            ownership = round(team_control))

min_frame <- min(example_play$frameId)
max_frame <- max(example_play$frameId)

ggplot() +
  geom_raster(data = team_example_play_influence, aes(x = x, y = y, fill = team_control), alpha = 0.7, interpolate = T) +
  scale_fill_gradient(high = "#e31837", low = "#002244") +
  new_scale("fill") +
  geom_point(data = example_play, aes(x = x, y = y, color = team, group = nflId), alpha = 0.7,
             size = 6.5) +
  scale_color_manual(values = c("#002244", "#654321", "#e31837"), guide = "none") +
  geom_text(data = example_play, aes(x = x, y = y, label = jerseyNumber), color = "white",
            vjust = 0.36, size = 3.5) +
  transition_time(frameId) +
  coord_fixed() +
  ease_aes('linear') +
  theme_minimal() +
  theme(text = element_text(family = "Roboto"),
        axis.title = element_blank(),
        legend.position = "none") -> anim

animate(anim, fps = 10, nframes = max_frame - min_frame + 5, width = 1000, end_pause = 5)

anim_save("field_control_example.gif")

# create a sample play gif to illustrate how the blocking assignments work and how they change over a play

min_frame <- min(example_play$frameId)
max_frame <- max(example_play$frameId)

example_play_blocking <- week1_blocking %>%
  filter(frameId >= min_frame, frameId <= max_frame) %>%
  mutate(defender_nflId = replace_na(defender_nflId, 0)) %>% # if it's NA it causes an issue with the join where a player's connected to the football, has no impact on analysis, just the viz
  filter(gameId == 2021090900, playId == 97) %>%
  left_join(example_play %>% select(lineman_nflId = nflId, frameId, lineman_x = x, lineman_y = y)) %>%
  left_join(example_play %>% select(defender_nflId = nflId, frameId, defender_x = x, defender_y = y))

ggplot() +
  geom_segment(aes(x = lineman_x, y = lineman_y, xend = defender_x, yend = defender_y), data = example_play_blocking) +
  geom_point(data = example_play, aes(x = x, y = y, color = team, group = nflId), alpha = 0.7,
             size = 6.5) +
  scale_color_manual(values = c("#002244", "#654321", "#e31837"), guide = "none") +
  geom_text(data = example_play, aes(x = x, y = y, label = jerseyNumber), color = "white",
            vjust = 0.36, size = 3.5) +
  transition_time(frameId) +
  ease_aes('linear') +
  coord_fixed() +
  theme_minimal() +
  theme(text = element_text(family = "Roboto"),
        axis.title = element_blank()) -> anim

animate(anim, fps = 10, nframes = max_frame - min_frame + 5, width = 1000, end_pause = 5)

anim_save("blocking_assignment_example.gif")


# Combine the blocking responsibilities with the field control


min_frame <- min(example_play$frameId)
max_frame <- max(example_play$frameId)

ggplot() +
  geom_raster(data = team_example_play_influence, aes(x = x, y = y, fill = team_control), alpha = 0.7, interpolate = T) +
  scale_fill_gradient(high = "#e31837", low = "#002244",
                      limits = c(0, 1)) +
  new_scale("fill") +
  geom_segment(aes(x = lineman_x, y = lineman_y, xend = defender_x, yend = defender_y), data = example_play_blocking) +
  geom_point(data = example_play, aes(x = x, y = y, color = team, group = nflId), alpha = 0.7,
             size = 6.5) +
  scale_color_manual(values = c("#002244", "#654321", "#e31837"), guide = "none") +
  geom_text(data = example_play, aes(x = x, y = y, label = jerseyNumber), color = "white",
            vjust = 0.36, size = 3.5) +
  transition_time(frameId) +
  coord_fixed() +
  ease_aes('linear') +
  theme_minimal() +
  theme(text = element_text(family = "Roboto"),
        axis.title = element_blank(),
        legend.position = "none") -> anim

animate(anim, fps = 10, nframes = max_frame - min_frame + 5, width = 1000, end_pause = 5)

anim_save("field_control_with_blocking_example.gif")


##### Visualizations for first four vs second four correlation

source("data_aggregation.R")

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

gtsave(ol_table, "ol_leaderboard_table.png", expand = 75)
gtsave(dl_table, "dl_leaderboard_table.png", expand = 75)

