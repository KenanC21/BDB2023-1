####### EDA ########
source("field_plot.R")
library(tidyverse)
library(gganimate)


###### ANIMATE A PLAY ########

pff <- read_csv("pffScoutingData.csv")
week7 <- read_csv("week7.csv")
plays <- read_csv("plays.csv")
players <- read_csv("players.csv")

# plays %>%
#   filter(possessionTeam == "GB",
#          defensiveTeam == "WAS") %>% view()


week7 <- week7 %>%
  mutate(x = ifelse(playDirection == "left", 120 - x, x),
         y = ifelse(playDirection == "left", 160/3 - y, y),
         dir = case_when(playDirection == "left" & dir <= 180 ~ dir + 180,
                         playDirection == "left" & dir > 180 ~ dir - 180,
                         T ~ dir),
         o = case_when(playDirection == "left" & o <= 180 ~ o + 180,
                       playDirection == "left" & o > 180 ~ o - 180,
                       T ~ o))

valid_game_ids <- unique(week7$gameId)

sample_play_info <- sample_n(plays %>% filter(gameId %in% valid_game_ids), 1) %>% select(gameId, playId)
sample_play_id <- sample_play_info %>% pull(playId)
sample_game_id <- sample_play_info %>% pull(gameId)

week7 <- week7 %>%
  left_join(pff) %>%
  left_join(plays) %>%
  group_by(gameId, playId) %>%
  mutate(rushing_qb = pff_role == "Pass Rush",
         snap_frame = max(frameId[event == "ball_snap"]),
         los = first(x[team == "football" & frameId == snap_frame]),
         end_box_up = max(y[pff_role == "Pass Block" & frameId == snap_frame], na.rm = T),
         end_box_down = min(y[pff_role == "Pass Block" & frameId == snap_frame], na.rm = T)) %>%
  ungroup()

# How far back do QBs go behind the line of scrimmage on a play?

qb <- week7 %>%
  filter(pff_positionLinedUp == "QB")

qb <- qb %>%
  mutate(x_dist_from_los = x - los)

ol_dl <- week7 %>%
  filter(pff_role %in% c("Pass Rush", "Pass Block"))

sample_play <- week7 %>%
  filter(gameId == sample_game_id, playId == sample_play_id)

football_field +
  geom_point(data = play, aes(x = x, y = y, fill = team, group = nflId, color = rushing_qb), alpha = 0.7,
             size = 6.5) +
  geom_text(data = play, aes(x = x, y = y, label = jerseyNumber), color = "white",
            vjust = 0.36, size = 3.5) +
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
  scale_colour_manual(values = c("#654321", "#c60c30"), guide = FALSE) +
  transition_time(frameId)  +
  ease_aes('linear') -> animation

play.length.ex <- length(unique(play$frameId))

animate(animation, fps = 10, nframes = play.length.ex + 5, width = 1000, end_pause = 5) -> anim

anim_save("animated_play.gif")


# IDEAS #

# expected threats and expected threats neutralized by blocking
# threat: hurry, hit, or sack
# based on speed relative to QB at end point of plays where there was a threat, but calculate them in real time

# How many pass rushers by play?

pff %>%
  group_by(gameId, playId) %>%
  summarize(pass_rushers = sum(pff_role == "Pass Rush")) %>%
  arrange(desc(pass_rushers)) %>%
  filter(gameId %in% game_ids)

week7 %>%
  pull(event) %>%
  unique()
