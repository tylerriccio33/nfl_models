get_elo <- function(.seasons = seasons) {
  read_csv("https://projects.fivethirtyeight.com/nfl-api/nfl_elo.csv") %>%
    # Filtering Season
    filter(season %in% .seasons) %>%
    # Fixing team name
    dplyr::mutate(
      team1 = gsub("WSH", "WAS", team1),
      team2 = gsub("WSH", "WAS", team2),
      team1 = gsub("LAR", "LA", team1),
      team2 = gsub("LAR", "LA", team2)
    ) %>%
    mutate(across(
      c(team1, team2),
      ~ case_when(.x == 'OAK' ~ 'LV',
                  .x == 'SD' ~ 'LAC',
                  .x == 'STL' ~ 'LA',
                  TRUE ~ .x)
    )) %>%
    select(
      date,
      season,
      home_team = team1,
      away_team = team2,
      home_elo = elo1_pre,
      away_elo = elo2_pre,
      home_elo_wp = elo_prob1,
      away_elo_wp = elo_prob2,
      home_qb_elo = qb1_value_pre,
      away_qb_elo = qb2_value_pre,
      home_qb_elo_adj_prob = qbelo_prob1,
      away_qb_elo_adj_prob = qbelo_prob2
    ) %>%
    pivot_longer(
      cols = -c(date, season, home_team, away_team),
      names_to = 'metric',
      values_to = 'value_metric'
    ) %>%
    pivot_longer(
      cols = c(home_team, away_team),
      names_to = 'home',
      values_to = 'team'
    ) %>%
    mutate(
      # Fixing Home
      home = home == 'home_team',
      # Extracting Metric
      metric_type = str_extract(metric, '^\\w{4}'),
      metric_type = metric_type == 'home'
    ) %>%
    # Filtering Metrics
    filter(home == metric_type) %>%
    # Fixing Metric
    select(-c(metric_type, home)) %>%
    mutate(metric = str_replace_all(metric, '^\\w{5}', '')) %>%
    # Pivoting Wide
    pivot_wider(names_from = metric, values_from = value_metric) %>%
    # Fixing Date
    mutate(date = as.character(date)) %>%
    left_join(
      load_schedules(seasons = .seasons) %>%
        as_tibble() %>%
        # Fixing team name
        mutate(across(
          c(home_team, away_team),
          ~ case_when(.x == 'OAK' ~ 'LV',
                      .x == 'SD' ~ 'LAC',
                      .x == 'STL' ~ 'LA',
                      TRUE ~ .x)
        )) %>%
        select(game_id, home_team, away_team, date = gameday) %>%
        pivot_longer(cols = c(home_team, away_team),
                     values_to = 'team') %>%
        select(-name),
      by = c('date', 'team')
    ) %>%
    relocate(game_id) %>%
    select(-c(date, season)) %>%
    rename(posteam = team)
}
