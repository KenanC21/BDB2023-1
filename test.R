### Try and speed up the processes ###

source('field_control.R')

week_track <- read_csv("week8.csv")
pff <- read_csv("pffScoutingData.csv")
plays <- read_csv("plays.csv")

game <- week_track %>%
  filter(gameId == 2021102800)

play_end_events <- c("pass_forward", "autoevent_passforward", "run", "qb_sack", "qb_strip_sack", "out_of_bounds",
                     "fumble_offense_recovered", "handoff", "fumble", "first_contact") # handoff is questionable. since we're concerned with pass protection, handoff could indicate a trick play
game <- game %>%
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
  filter(frameId == snap_frame | frameId == end_frame) %>%
  group_by(gameId, playId) %>%
  mutate(n_frames = length(unique(frameId))) %>%
  filter(n_frames == 2) %>%
  select(-n_frames)


tictoc::tic()
week1_influence <- find_play_influence(189, game) # CHANGE THIS AT THE END
tictoc::toc()

