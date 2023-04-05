
calculate_sos <- function(raw_pbp) {
  
  # Calculating Priors
  
clean_data <- data %>%
  dtplyr::lazy_dt() %>%
  select(
    game = game_id,
    team = posteam,
    opponent = defteam,
    week,
    home_team,
    res = result,
    spread = spread_line
  ) %>%
  drop_na() %>%
  unique() %>%
  # Removing Ties
  filter(res != 0) %>%
  # Correcting spread and res
  mutate(res = ifelse(team == home_team, res, res * -1),
         spread = ifelse(team == home_team, spread * -1, spread),
         win = if_else(res > 0, 1, 0)) %>%
  select(-home_team) %>%
  # Calculating Games Played and PCT
  group_by(team) %>%
  arrange(week, .by_group = T) %>%
  mutate(games_played = row_number()) %>%
  mutate(total_wins = cumsum(win),
         pct = total_wins / games_played) %>% 
  ungroup() %>%
  # Cleaning up
  select(game, team, opponent, week, games_played, pct, spread) %>%
  # Lagging to get prior
  group_by(team) %>%
  arrange(week, .by_group = T) %>%
  mutate(pct = lag(pct)) %>%
  ungroup()

# Re joining to clean data

clean_data %>%
  left_join(clean_data %>% select(game, team = opponent, opp_pct = pct),
            by = c('game', 'team')) %>%
  # Dropping first week
  drop_na() %>%
  # Calculating cumulative
  group_by(team) %>%
  arrange(week, .by_group = T) %>%
  mutate(sos = cummean(opp_pct)) %>%
  ungroup() %>%
  # Final cleanup
  select(game, team, sos) %>%
  as_tibble()

}




















