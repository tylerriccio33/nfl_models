

get_schedule_data <- function(.seasons = seasons) {


stored_data <-  nflreadr::load_schedules(seasons = .seasons) %>%
    as_tibble() %>%
    # Fixing team name
    mutate(across(
      c(home_team, away_team),
      ~ case_when(.x == 'OAK' ~ 'LV',
                  .x == 'SD' ~ 'LAC',
                  .x == 'STL' ~ 'LA',
                  TRUE ~ .x)
    ))

# Contains lookup detailing number of coaching previous starts
coach_lookup <- stored_data %>%
  select(game_id, home_team, away_team, home_coach, away_coach) %>%
  pivot_longer(
    cols = c(home_team, away_team),
    names_to = "team_type",
    values_to = "team_name"
  ) %>%
  pivot_longer(
    cols = c(home_coach, away_coach),
    names_to = "coach_type",
    values_to = "coach_name"
  ) %>%
  filter(str_sub(team_type,0, 4) == str_sub(coach_type,0,4)) %>%
  select(game_id, posteam = team_name, coach = coach_name) %>%
  group_by(coach) %>%
  arrange(game_id, .by_group = T) %>%
  mutate(n_coach_starts = row_number()) %>%
  ungroup() %>%
  select(-coach)

# Contains spread, moneyline, home
normal_cols <- stored_data %>%
    select(
      game_id,
      home_team,
      away_team,
      spread_line,
      home_moneyline,
      away_moneyline,
      total_line
    ) %>%
    pivot_longer(
      cols = c(home_team, away_team),
      names_to = 'home',
      values_to = 'posteam'
    ) %>%
    mutate(
      # Is home
      home = home == 'home_team',
      # Fixing Moneyline
      moneyline = ifelse(home, home_moneyline, away_moneyline),
      # Fixing Spread
      spread_line = if_else(home, spread_line * -1, spread_line)
    ) %>%
  # Join to coach starts
  left_join(coach_lookup, by = c('game_id','posteam'))

}















