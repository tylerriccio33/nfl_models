


generate_qb_metrics <- function(data, 
                                .this_week = this_week,
                                .this_season = this_season,
                                bypass_warnings = F,
                                return_extra_circumstances = F) {
  
  min_season <- min(data$season) - 2
  max_season <- max(data$season)
  
  # PBP Stats
  pbp_stats <- data %>%
    # PBP
    select(season,
           week,
           posteam,
           gsis = passer_player_id,
           nfl_cpoe = cpoe) %>% 
    drop_na() %>%
    group_by(season, week, gsis, posteam) %>%
    summarize(
      across(c(where(is.numeric)), ~ mean(.x)),
      n = n()
    ) %>%
    ungroup()
  # Player Stats
  player_stats <-
    load_player_stats(if_else(min_season > 2004, min_season, 2004):max_season) %>%
    as_tibble() %>%
    filter(position == 'QB') %>%
    # Selections
    select(
      gsis = player_id,
      player_name = player_display_name,
      season,
      week,
      sack_yards,
      pacr,
      dakota,
      rushing_epa
    )
  # NGS Stats
  ngs_stats <-
    load_nextgen_stats(
      seasons = if_else(min_season > 2016, min_season, 2016):max_season,
      stat_type = 'passing'
    ) %>%
    as_tibble() %>%
    relocate(season,
             week,
             posteam = team_abbr,
             gsis = player_gsis_id) %>%
    select(
      -c(
        posteam,
        season_type,
        player_display_name,
        player_position,
        player_first_name,
        player_last_name,
        player_short_name,
        player_jersey_number
      )
    )
  # ELO Data
  elo_stats <- read_csv("https://projects.fivethirtyeight.com/nfl-api/nfl_elo.csv") %>%
    # Filtering Season
    filter(season >= min_season) %>%
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
    # Pivoting
    select(
      date,
      qb1_team = team1,
      qb2_team = team2,
      qb1_elo = qb1_value_pre,
      qb2_elo = qb2_value_pre,
      qb1_name = qb1,
      qb2_name = qb2
    ) %>%
    pivot_longer(
      cols = ends_with("name"),
      values_to = 'name',
      names_to = "name_t",
      names_prefix = "qb"
    ) %>%
    pivot_longer(
      cols = ends_with("elo"),
      names_to = "elo_t",
      values_to = 'elo',
      names_prefix = "qb"
    ) %>%
    pivot_longer(
      cols = ends_with('team'),
      names_to = 'team_t',
      values_to = 'posteam',
      names_prefix = 'qb'
    ) %>%
    filter(str_sub(name_t, 1, 1) == str_sub(elo_t, 1, 1),
           str_sub(team_t, 1,1) == str_sub(name_t, 1,1)) %>%
    select(-ends_with('_t')) %>%
    rename(player_name = name,
           game_date = date) %>%
    # Game Date to character
    # Some have Mitch instead of Mitchell Trubisky
    mutate(
      game_date = as.character(game_date),
      player_name = str_replace_all(player_name, 'Mitch Trubisky', 'Mitchell Trubisky')
    )
  # Schedule Data
  # Need to include all season, week, posteam combinations plus next week
  schedule_data <-
    load_schedules(if_else(min_season > 2004, min_season, 2004):max_season) %>%
    as_tibble() %>%
    select(game_id, week, season, home_team, away_team, game_date = gameday) %>%
    pivot_longer(cols = c(home_team, away_team), values_to = 'posteam') %>%
    select(-name) %>%
    mutate(
      posteam =  case_when(
        posteam == 'OAK' ~ 'LV',
        posteam == 'SD' ~ 'LAC',
        posteam == 'STL' ~ 'LA',
        TRUE ~ posteam
      )
    ) %>%
    filter(game_date %in% elo_stats$game_date)
  # New week data
  new_week_data <- schedule_data %>%
    filter(season == .this_season,
           week == .this_week)
  
  elo_stats <- elo_stats %>%
    # Join Schedule Data
    left_join(schedule_data, by = c('game_date', 'posteam')) %>%
    # Join New Data
    powerjoin::power_left_join(new_week_data, 
                               by = c('game_date','posteam'),
                               conflict= powerjoin::coalesce_xy) %>%
    # Joining to player stats for gsis
    left_join(player_stats %>%
                          select(player_name, gsis) %>%
                unique(),
                               by = c('player_name'))
  
  # Join Data
  joined_data <- pbp_stats %>%
    # Joining to Player Stats
    powerjoin::power_left_join(
      player_stats,
      by = c('gsis', 'season', 'week'),
      conflict = powerjoin::coalesce_xy
    ) %>%
    # Joining to NGS Stats
    powerjoin::power_left_join(
      ngs_stats,
      by = c('gsis', 'season', 'week'),
      conflict = powerjoin::coalesce_xy
    ) %>%
    # Clean up
    rename(cpoe = completion_percentage_above_expectation) %>%
    # Coalesce cpoe
    mutate(cpoe = coalesce(cpoe, nfl_cpoe)) %>%
    # Final Selection of Statistics
    select(season,
           week,
           gsis,
           player_name,
           posteam,
           n,
           pacr,
           dakota,
           rushing_epa,
           cpoe) %>%
    # Finding game date from schedule data
    # Join to schedule for schedule date
    left_join(schedule_data, by = c('season', 'week', 'posteam')) %>%
    # Join to ELO
    powerjoin::power_full_join(elo_stats, 
                               by = c('game_date', 'player_name'),
                               conflict = powerjoin::coalesce_xy) %>%
    # Clean up
    relocate(gsis,
             player_name,
             posteam,
             game_id,
             game_date,
             season,
             week,
             n,
             elo) %>%
    # Filling in GSIS and player for missing next week
    group_by(posteam) %>%
    arrange(game_date, .by_group = T) %>%
    fill(gsis, player_name, .direction = 'down') %>%
    ungroup() %>%
    # Filtering out less than 15 throws unless they're in new week
    filter(n >= 15 | game_id %in% new_week_data$game_id) 
    # There is missing ELO where the qb didn't start the game
    # Since this isn't being rolled, this doesn't really matter
  
  if(!bypass_warnings) {
  # Check if any stats are missing
  missing_stats <- joined_data %>%
    filter(if_any(c(pacr, dakota, cpoe), ~ is.na(.x)) &
             !game_id %in% new_week_data$game_id)
  if(nrow(missing_stats) != 0) {
    warning('Here is a tibble of missing pacr, dakota or cpoe')
    return(missing_stats)
  }
  }
  
  rolling_average <- function(x, window_size) {
    n <- n()
    window_size <- min(window_size, n)
    cum_avg <- cumsum(na.omit(x)) / (1:n)
    roll_avg <- ifelse(1:n < window_size, 
                       cum_avg[1:n], 
                       zoo::rollmean(x, 
                                     window_size, 
                                     align="right", 
                                     fill=NA, 
                                     na.rm = TRUE)
    )
    return(roll_avg)
  }

  # Arrange
  arranged_data <- joined_data %>%
    group_by(gsis) %>%
    arrange(game_date, .by_group = T) %>%
    ungroup()
  # Lag and roll
  lagged_data <- joined_data %>%
    arrange(gsis) %>%
    group_by(gsis) %>%
    mutate(across(c(dakota, pacr, cpoe),
                  ~ rolling_average(.x * n, 15)),
           rushing_epa = rolling_average(rushing_epa, 15)) %>%
    ungroup() %>%
    # Counting starts
    group_by(gsis) %>%
    arrange(gsis, .by_group = T) %>%
    mutate(starts = row_number()) %>%
    # Final cleanup
    select(game_id, posteam, elo, starts,  pacr, dakota, rushing_epa, cpoe) %>%
    # Replace rushing EPA NAN with missing
    # These can be imputed in the final processing
    mutate(rushing_epa = if_else(is.nan(rushing_epa), NA_real_, rushing_epa))

  
  # Checking for extraordinary circumstances
  if(return_extra_circumstances) {
    
  extra_circumstances <-  lagged_data %>%
    group_by(game_id, posteam) %>%
    filter(n()!=1)
  warning("If it has elo, qb started and played much of the game")
  warning("If it doesn't have elo, qb started but didn't play much of the game")
  return(extra_circumstances)

  }
  
  warning('pacr, dakota and cpoe are no longer in normal units')
  return(lagged_data)
  
}






















