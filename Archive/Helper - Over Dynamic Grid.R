
### Data needs to be...
# 1. Summarized (can be by pass but remember to split)
# 2. Reversed
# 3. Ungrouped

calculate_over_prior <-
  function(game_summarized_data,
           base_stat,
           opposite_stat,
           .fn) {
    
    # Description
    # How was the stat based on opponent's defensive stat cumulative average
    # For example, how well is offensive epa based on opponent's prior epa
    
    # Original Data
    
    original_data <- game_summarized_data
    
    # Stat as String
    
    name <- game_summarized_data %>%
      select({{base_stat}}) %>%
      colnames()
    
    opp_name <- game_summarized_data %>%
      select({{opposite_stat}}) %>%
      colnames()
    
    prior_opp_name <- glue("prior_opp_{name}") %>%
      sym()
    
    name_op <- glue("{name}_op") %>%
      sym()
    
    # Summarize
    
    my_data <- game_summarized_data %>%
      # Selecting
      select(game_id,
             season,
             week,
             posteam,
             defteam, 
             {{base_stat}},
             {{opposite_stat}}) %>%
      drop_na() %>%
      # Summarizing
      # Order here doesn't matter
      group_by(season, game_id, week, posteam, defteam) %>%
      summarize("{name}" := match.fun(.fn)({{base_stat}}),
                "{opp_name}" := match.fun(.fn)({{opposite_stat}})) %>%
      ungroup() %>%
      # Arranging
      # Order here matters
      group_by(posteam) %>%
      arrange(season, week, .by_group = T) %>%
      # Lagging
      mutate("prior_opp_{name}" := lag({{opposite_stat}})) %>%
      drop_na() %>% # Drops first week
      # Cumulative Mean
      mutate("prior_opp_{name}" := pracma::movavg(x = {{prior_opp_name}}, n = 15, type = 'e')) %>%
      ungroup() %>%
      # In Game Over Prior
      mutate("{name}_op" := {{base_stat}} - {{prior_opp_name}}) %>%
      # Removing Prior
      select(-c(starts_with('prior_'), {{name}}, {{opp_name}}))
    
    # Joining to get original data
    
    original_data %>%
      left_join(my_data,
                by = c('game_id', 'season', 'week', 'posteam', 'defteam'))
    
    
  }


calculate_over_prior_grid <-
  function(game_summarized_data,
           base_stat,
           opposite_stat,
           .fn) {
    
    # Description
    # How was the stat based on opponent's defensive stat cumulative average
    # For example, how well is offensive epa based on opponent's prior epa
    
    # Original Data
    
    original_data <- game_summarized_data
    
    # Stat as String
    
    name <- game_summarized_data %>%
      select({{base_stat}}) %>%
      colnames()
    
    opp_name <- game_summarized_data %>%
      select({{opposite_stat}}) %>%
      colnames()
    
    prior_opp_name <- glue("prior_opp_{name}") %>%
      sym()
    
    grid_opp_name <- prior_opp_name %>% 
      as.character() %>%
      str_replace(pattern = 'prior_','') %>%
      str_replace(pattern = 'opp_','') %>%
      sym()
    
    name_op <- glue("{name}_op") %>%
      sym()
    
    # Pracma Scores
    
    grid <- pracma_scores %>%
      get_Winner(name, by = rank) %>%
      filter(name == name) %>%
      mutate(name = str_replace_all(name, '_op$',''),
             my_n = as.numeric(my_n))
    
    # Summarize
    
    my_data <- game_summarized_data %>%
      # Selecting
      select(game_id,
             season,
             week,
             posteam,
             defteam, 
             {{base_stat}},
             {{opposite_stat}}) %>%
      drop_na() %>%
      # Summarizing
      # Order here doesn't matter
      group_by(season, game_id, week, posteam, defteam) %>%
      summarize("{name}" := match.fun(.fn)({{base_stat}}),
                "{opp_name}" := match.fun(.fn)({{opposite_stat}})) %>%
      ungroup() %>%
      # Arranging
      # Order here matters
      group_by(posteam) %>%
      arrange(season, week, .by_group = T) %>%
      # Lagging
      mutate("prior_opp_{name}" := lag({{opposite_stat}})) %>%
      drop_na() %>% # Drops first week
      # Pracma
      mutate("prior_opp_{name}" := pracma::movavg(x = {{grid_opp_name}}, 
                                                  n = grid$my_n, 
                                                  type = grid$my_type)) %>%
      ungroup() %>%
      # In Game Over Prior
      mutate("{name}_op" := {{base_stat}} - {{prior_opp_name}}) %>%
      # Removing Prior
      select(-c(starts_with('prior_'), {{name}}, {{opp_name}}))
    
    # Joining to get original data
    
    original_data %>%
      left_join(my_data,
                by = c('game_id', 'season', 'week', 'posteam', 'defteam'))
    
    
  }


