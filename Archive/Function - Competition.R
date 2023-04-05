
calculate_competition <- function(raw_pbp, game_collapse = T) {
  
  
  ## This function adds the potential WP
  
  add_potential_wp <- function(data = raw_pbp, points) {
    temp_data <- data %>%
      # Creating primary key
      mutate(temp_id = row_number())
    
    wp_prepped <- temp_data %>%
      select(
        home_team,
        posteam,
        score_differential,
        half_seconds_remaining,
        game_seconds_remaining,
        spread_line,
        down,
        ydstogo,
        yardline_100,
        posteam_timeouts_remaining,
        defteam_timeouts_remaining,
        temp_id
      ) %>%
      drop_na() %>%
      mutate(
        # I subtract 7 points from the play
        # This stimulates a touchdown occurring on the previous play
        score_differential = score_differential + points,
        receive_2h_ko = first(posteam) != posteam,
        receive_2h_ko = if_else(receive_2h_ko, 1, 0)
      ) %>%
      # Win probability is computed using this new score differential
      nflfastR::calculate_win_probability() %>%
      select(temp_id, "potential_wp_{points}" := wp)
    
    temp_data %>%
      left_join(wp_prepped,
                by = 'temp_id') %>%
      select(-temp_id)
    
  }
  
  appended <- raw_pbp %>%
    # Invoke the function and append the raw PBP data
    add_potential_wp(points = 7) %>%
    add_potential_wp(points = -7) %>%
    # Selecting PBP Data
    select(
      game_id,
      # ID
      posteam,
      # Offense,
      defteam,
      # Defense
      wp,
      # Win Percentage
      best_potential_wp = potential_wp_7,
      worst_potential_wp = `potential_wp_-7`,
    ) %>%
    # Dropping missing values
    drop_na() %>%
    # Getting distance between best and worst case scenario
    mutate(wp_distance = best_potential_wp - worst_potential_wp)
  ## Logic controlling game collapse
  
  if (game_collapse) {
    game_collapsed <- appended %>%
      # Group by game and team
      group_by(game_id, posteam) %>%
      mutate(
        roll_mean = zoo::rollapply(
          wp_distance,
          width = 40,
          FUN = mean,
          align = "center",
          partial = TRUE
        )
      )  %>%
      # Calculating distance away from .5 mark
      mutate(dist_from_center = abs(wp - .5)) %>%
      summarize(
        wp_dist = median(wp_distance),
        wp_dist_from_center = mean(dist_from_center),
        end_game_distance = last(roll_mean)
      ) %>%
      ungroup() %>%
      ## Cutting Competitions
      mutate(
        intensity = cut_number(
          wp_dist,
          n = 3,
          labels = c('low', 'average', 'high')
        ),
        end_game_intensity = cut_number(
          end_game_distance,
          n = 3,
          labels = c('low', 'average', 'high')
        ),
        competition = cut_number(
          wp_dist_from_center,
          n = 3,
          labels = c('high', 'average', 'low')
        )
      ) %>%
      rename(game = game_id)
    return(game_collapsed)
  }
  # Else return the appended
  else {
    return(appended)
  }
}















