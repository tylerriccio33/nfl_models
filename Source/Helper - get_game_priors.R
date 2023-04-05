get_game_priors <- function(.seasons = seasons) {
  
  read_csv("https://projects.fivethirtyeight.com/nfl-api/nfl_elo.csv") %>%
    # Filtering Seasons
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
    ))  %>%
    select(date,
           season,
           home_team = team1,
           away_team = team2,
           neutral,
           playoff
    ) %>%
    pivot_longer(
      cols = c(home_team, away_team),
      names_to = 'home',
      values_to = 'team'
    ) %>%
    # Fixing Date
    mutate(date = as.character(date)) %>%
    # Joining to Schedule Data
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
        select(game_id,
               home_team,
               away_team,
               date = gameday,
               div_game) %>% 
        pivot_longer(cols = c(home_team, away_team),
                     values_to = 'team') %>%
        select(-name),
      by = c('date', 'team')
    ) %>%
    select(game_id,
           team,
           neutral,
           playoff,
           home,
           div_game)  %>%
    mutate(
      # Site
      site = case_when(neutral == 1 ~ 'neutral',
                       home == 'home_team' ~ 'home',
                       TRUE ~ 'away'),
      # Playoff
      playoff = if_else(is.na(playoff), F, T),
      # Division Game
      div_game = div_game == 1
    ) %>%
    select(-c(neutral, home)) %>%
    rename(posteam = team)
}
