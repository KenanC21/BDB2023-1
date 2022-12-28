###### Data Aggregation #######

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
  week_influence <- read_csv(paste0("week", wk, "_influence.csv"))
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
  filter(plays > 100) -> defensive_players

defensive_players <- defensive_players %>%
  group_by(officialPosition) %>%
  mutate(index = scale(avg_net_influence),
         position_rank = 1:n())

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
            team = last(team)) %>%
  ungroup() %>%
  arrange(desc(avg_net_influence)) %>%
  filter(plays > 150) %>%
  inner_join(players, by = c("lineman_nflId" = "nflId")) %>%
  group_by(officialPosition) %>%
  mutate(index = scale(avg_net_influence),
         position_rank = 1:n())

offensive_players %>%
  ggplot(aes(x = ))


