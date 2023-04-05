remove_clinched_teams <- function(.data,
                                  .priors = priors_and_posterior,
                                  .this_week = this_week,
                                  .this_season = this_season) {
  stored_data <- .data
  function_data <- .data %>%
    select(id_game, id_posteam, id_season, id_week)
  
  # Wrangle Playoff Data
  playoff_data <- .priors %>%
    filter(id_playoff) %>%
    select(id_game, id_posteam) %>%
    # Join to get id_season and week
    left_join(select(.data, id_game, id_posteam, id_season, id_week),
              by = c('id_game', 'id_posteam'))  %>%
    group_by(id_season) %>%
    transmutate(last_week = min(id_week) - 1) %>%
    ungroup() %>%
    select(id_season, id_posteam, last_week)
  
  # Semi Join to Function Data
  
  this_week_games <- function_data %>%
    filter(id_season == .this_season,
           id_week == .this_week) %>%
    pull(id_game)
  
  rm_games <- function_data %>%
    semi_join(playoff_data, by = c('id_season', 'id_posteam', 'id_week' = 'last_week')) %>%
    pull(id_game) %>%
    unique() %>%
    # Remove this week's games from this list
    # If you're serving this week, just keep the games
    keep(~ !.x %in% this_week_games)
  
  # Filter out rm games
  
  stored_data %>%
    filter(!id_game %in% rm_games)
  
  
}
