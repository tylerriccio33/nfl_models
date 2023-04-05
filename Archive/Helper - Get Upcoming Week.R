get_upcoming_week <- function(raw_schedule_data,
                              .week = this_week) {
  
  stored_data <- raw_schedule_data %>%
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
  
  return(coach_lookup)
  
  # Game Lookup
  game_lookup <- stored_data %>%
    filter(week == .week) %>%
    select(
      game_id,
      posteam = home_team,
      defteam = away_team,
      week,
      season
    ) %>%
    pivot_longer(cols = c(posteam, defteam), values_to = 'posteam') %>%
    select(-name) %>%
    group_by(game_id) %>%
    mutate(id = row_number()) %>%
    group_modify(~ {
      team_vector <- .x$posteam
      .x %>%
        mutate(defteam = if_else(id == 1, team_vector[2], team_vector[1]))
    }) %>%
    ungroup() %>%
    select(-id) %>%
    relocate(game_id, posteam, defteam)
  
  return(game_lookup)
  
}