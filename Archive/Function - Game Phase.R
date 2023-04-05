
add_game_phases <- function(raw_pbp, game_collapse = T) {
  
  add_notch_count <- function(data) {
    notch_vector <- vector(mode = 'numeric', length = nrow(data))
    wp_vector <- data$wp
    
    last_notch <- 1
    wp_at_last <- wp_vector[1]
    
    for (i in 1:nrow(data)) {
      since_last <- abs(wp_vector[i] - wp_at_last)
      
      if (since_last > .15) {
        notch_vector[i] <- 1
        last_notch <- i
        wp_at_last <- wp_vector[i]
      } else {
        notch_vector[i] <- 0
      }
    }
    
    # Initializing first notch retroactively
    notch_vector[1] <- 1
    
    data %>%
      add_column(notch_vector)
  }
  
  pivoted <- raw_pbp %>%
    select(game = game_id,
           posteam,
           defteam,
           wp) %>%
    drop_na() %>%
    group_by(game) %>%
    mutate(play = row_number()) %>%
    ungroup() %>%
    pivot_longer(c(posteam, defteam), names_to = 'type', values_to = 'team') %>%
    relocate(game, type, team, play) %>%
    mutate(wp = if_else(type == 'posteam', wp, 1 - wp)) %>%
    select(-type) %>%
    # Only keeping one team since there will be the same amount of phases
    group_by(game) %>%
    filter(cur_data()$team[[1]] == team) %>%
    ungroup() %>%
    select(-team)
  
  # return(pivoted)
  
  ## 1.
  # Flag notches where there is an absolute change of .20%
  # Each notch will denote a change in game phases
  
  ## 2.
  # Fit linear model to each game phase
  # Calculate derivative of fitted line
  # Bin derivatives into 3 categories, decreasing, increasing and even
  # These are grouped together and become the new game phases
  
  ## 3.
  # Calculating new slope and line of WP using new game phases
  # Lagging each category
  # If n() game_phase == 1, merge with prior
  
  ## 4.
  # Smoothing over again
  
  ## 1.
  
  data <- pivoted %>%
    group_by(game) %>%
    nest() %>%
    # Flag notches where there is an absolute change of .20%
    # Each notch will denote a change in game phases
    mutate(data = map(
      data,
      ~ .x %>%
        arrange(play) %>%
        add_notch_count() %>%
        mutate(game_phase = cumsum(notch_vector)) %>%
        select(-notch_vector)
    )) %>%
    unnest(data) %>%
    ungroup() %>%
    
    ## 2.
    
    group_by(game, game_phase) %>%
    arrange(play, .by_group = T) %>%
    nest() %>%
    # Fit linear model to each game phase
    mutate(data = map(data, ~ lm(wp ~ play, data = .x) %>%
                        augment(.x) %>%
                        select(
                          -c(.resid, .hat, .sigma, .cooksd, .std.resid)
                        ))) %>%
    unnest(data) %>%
    # Calculate derivative of fitted line
    # Bin derivatives into 3 categories, decreasing, increasing and even
    mutate(
      prime = ((last(.fitted) - first(.fitted)) / n()) * 100,
      prime_cat = case_when(prime > .2 ~ 'up',
                            abs(prime) < .2 ~ 'even',
                            prime < .2 ~ 'down')
    ) %>%
    ungroup() %>%
    # These are grouped together and become the new game phases
    mutate(
      game_phase = if_else(lag(prime_cat) != prime_cat, 1, 0),
      game_phase = if_else(row_number() == 1, 1, game_phase),
      game_phase = cumsum(game_phase)
    ) %>%
    select(-c(prime, prime_cat, .fitted)) %>%
    
    ## 3.
    
    group_by(game, game_phase) %>%
    # Calculating new slope and line of WP using new game phases
    mutate(
      slope = (last(wp) - first(wp)) / n(),
      line = (row_number() * slope) + (first(wp) - slope),
      prime = ((last(line) - first(line)) / n()) * 100,
      prime_cat = case_when(prime > .1 ~ 'up',
                            abs(prime) < .1 ~ 'even',
                            prime < .1 ~ 'down')
    ) %>%
    ungroup() %>%
    select(-line) %>%
    # Lagging each category
    group_by(game) %>%
    mutate(
      new_cat = if_else(lag(prime_cat) != prime_cat, 1, 0),
      new_cat = if_else(row_number() == 1, 1, new_cat),
      new_cat = cumsum(new_cat)
    ) %>%
    ungroup() %>%
    mutate(game_phase = new_cat, .keep = 'unused') %>%
    # If n() game_phase == 1, merge with prior
    group_by(game, game_phase) %>%
    mutate(game_phase_n = n()) %>%
    ungroup() %>%
    mutate(game_phase = if_else(game_phase_n == 1, lag(game_phase), game_phase)) %>%
    select(-c(game_phase_n, slope, prime_cat, prime)) %>%
    
    ## 4.
    
    # Smoothing over again
    group_by(game, game_phase) %>%
    mutate(
      slope = (last(wp) - first(wp)) / n(),
      phase_trend_line = (row_number() * slope) + (first(wp) - slope)
    ) %>%
    ungroup()
  
  if(game_collapse) {
    
    data %>%
      group_by(game) %>%
      summarize(game_phases = last(game_phase)) %>%
      ungroup()
    
  } else {
    return(data)
  }
  
}



  
  