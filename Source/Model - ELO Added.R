calculate_elo_added <- function(elo_data, .schedule_data) {

  elo_data %>%
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
    # Selecting metrics
    select(
      date,
      season,
      home_team = team1,
      away_team = team2,
      home_elo = elo1_pre,
      away_elo = elo2_pre,
      home_elo_post = elo1_post,
      away_elo_post = elo2_post,
      home_qb_elo = qb1_value_pre,
      away_qb_elo = qb2_value_pre,
      home_qb_elo_post = qb1_value_post	,
      away_qb_elo_post = qb2_value_post	,
      home_qb_adj_elo = qbelo1_pre,
      away_qb_adj_elo = qbelo2_pre,
      home_qb_adj_elo_post = qbelo1_post,
      away_qb_adj_elo_post = qbelo2_post,
      quality
    ) %>%
    pivot_longer(
      cols = -c(date, season, home_team, away_team, quality),
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
    # Getting Game ID
    left_join(
      load_schedules(seasons = 1999:2022) %>%
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
    relocate(game_id, posteam = team) %>%
    select(-c(date, season)) %>%
    # Creating Added Features
    transmutate(
      eloa = elo_post - elo ,
      qb_eloa = qb_elo_post - qb_elo,
      qb_adj_eloa = qb_adj_elo_post - qb_adj_elo
    ) %>%
    mutate(across(c(contains('eloa')), ~ log(quality + .01) * .x, .names = "quality_adj_{.col}")) %>%
    select(-quality)

}
