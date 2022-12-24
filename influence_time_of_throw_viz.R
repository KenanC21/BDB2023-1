### OL and DL unit evaluations by team
library(gt)

players <- read_csv("players.csv")
plays <- read_csv("plays.csv")

all_plays_team_influence <- data.frame()

for (wk in 1:8)
{
  week_influence <- read_csv(paste0("week", wk, "_influence.csv"))
  week_track <- read_csv(paste0("week", wk, ".csv"))
  
  week_influence %>%
    inner_join(week_track %>% select(nflId, playId, gameId, team) %>% distinct()) %>%
    inner_join(plays %>% select(gameId, playId, possessionTeam, defensiveTeam, passResult) %>% distinct()) %>%
    group_by(gameId, playId) %>%
    mutate(last_frame_id = max(frameId)) %>%
    ungroup() %>%
    filter(frameId == last_frame_id) %>%
    group_by(gameId, playId, frameId, x, y) %>%
    summarize(off_influence = sum(influence[team == possessionTeam]),
              def_influence = sum(influence[team != possessionTeam]),
              control = exp(off_influence - def_influence) / (exp(off_influence - def_influence) + 1),
              outcome = last(passResult),
              offense = last(possessionTeam),
              defense = last(defensiveTeam)) %>%
    ungroup() %>%
    group_by(gameId, playId) %>%
    summarize(avg_offense_field_control = mean(control),
              outcome = last(outcome),
              offense = last(offense),
              defense = last(defense)) -> week_results
  
  all_plays_team_influence <- all_plays_team_influence %>%
    bind_rows(week_results)
}
#### Visualization for outcomes by binned offensive field control #####

all_plays_team_influence <- all_plays_team_influence %>%
  mutate(influence_binned = case_when(avg_offense_field_control < 0.2 ~ "0 - 0.2",
                                      avg_offense_field_control >= 0.2 & avg_offense_field_control < 0.4 ~ "0.2 - 0.4",
                                      avg_offense_field_control >= 0.4 & avg_offense_field_control < 0.6 ~ "0.4 - 0.6",
                                      avg_offense_field_control >= 0.6 & avg_offense_field_control < 0.8 ~ "0.6 - 0.8",
                                      avg_offense_field_control >= 0.8 ~ "0.8 - 1"),
         outcome_text = case_when(outcome == "C" ~ "Complete",
                                  outcome == "I" ~ "Incomplete",
                                  outcome == "IN" ~ "Interception",
                                  outcome == "R" ~ "Run",
                                  outcome == "S" ~ "Sack"))

all_plays_team_influence %>%
  group_by(influence_binned, outcome_text) %>%
  count() %>%
  ggplot(aes(x = influence_binned, y = n, fill = outcome_text)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(fill = "Play Outcome",
       x = "Offensive Field Control at End of Play",
       title = "Passing Play Outcomes by Offensive Field Control at End of Play",
       subtitle = "Data: 2023 NFL Big Data Bowl | Weeks 1-8 of 2021 NFL Season",
       caption = "Visualization by Kenan Clarke and Michael Egle") +
  theme(text = element_text(family = "Roboto"),
        axis.title.y = element_blank()) -> influence_outcomes_viz

ggsave(plot = influence_outcomes_viz, filename = "influence_outcomes.jpeg", width = 8, units = "in")


