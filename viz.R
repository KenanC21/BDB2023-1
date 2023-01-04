##### VISUALIZATIONS ######
source("field_control.R")
library(ggnewscale)
week1 <- read_csv("week1.csv")
pff <- read_csv("pffScoutingData.csv")
plays <- read_csv("plays.csv")
week1_blocking <- read_csv("week1_blocking_assignments.csv")
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
  scale_fill_gradient(high = "#e31837", low = "#002244",
                      limits = c(0, 1)) +
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
        axis.title = element_blank()) -> anim

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
        axis.title = element_blank()) -> anim

animate(anim, fps = 10, nframes = max_frame - min_frame + 5, width = 1000, end_pause = 5)

anim_save("field_control_with_blocking_example.gif")

