
### Data needs to be...
# 1. Summarized (can be by pass but remember to split)
# 2. Reversed
# 3. Ungrouped

calculate_over_prior_grid <-
  function(game_summarized_data,
           ...,
           grid) {
    
    straight_over_prior_grid <-
      function(game_summarized_data,
               base_stat,
               opposite_stat,
               grid) {
        
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
        
        # Filtering Grid
        
        .name <- name
        
        print(.name)
        
        temp_grid <- grid %>%
          filter(name == .name) %>%
          arrange(-abs(c)) %>%
          slice_head(n = 1)
        
        # Summarize
        
        metric_character <- enexpr(opposite_stat) %>%
          sym()
        lag_expr <- expr(lag(!!sym(metric_character)))
        
        my_data <- game_summarized_data %>%
          # Lazy DT
          dtplyr::lazy_dt() %>%
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
          summarize("{name}" := mean({{base_stat}}, na.rm = T),
                    "{opp_name}" := mean({{opposite_stat}}, na.rm = T)) %>%
          ungroup() %>%
          # Arranging
          # Order here matters
          group_by(posteam) %>%
          arrange(season, week, .by_group = T) %>%
          # Lagging
          mutate("prior_opp_{name}" := !!lag_expr) %>%
          drop_na() %>% # Drops first week
          # Cumulative Mean
          mutate("prior_opp_{name}" := pracma::movavg(x = {{prior_opp_name}},
                                                      n = as.numeric(!!temp_grid$window), 
                                                      type = !!temp_grid$type)) %>%
          ungroup() %>%
          # In Game Over Prior
          mutate("{name}_op" := {{base_stat}} - {{prior_opp_name}}) %>%
          # Removing Prior
          select(-c(starts_with('prior_'), {{name}}, {{opp_name}})) %>%
          as_tibble()
          
        
        # Joining to get original data
        original_data %>%
          left_join(my_data,
                    by = c('game_id', 'season', 'week', 'posteam', 'defteam'))
        
      }

    base_list <- game_summarized_data %>%
      select(...) %>%
      colnames()
    
    return_tibble <- game_summarized_data
    
    for(i in 1:length(base_list)) {
      
      temp_base <- base_list[i] %>%
        sym()
      
      # If base has allowed at end the opposite should be the offense (base)
      
      if(str_detect(temp_base, '_allowed$')){
        
        temp_opposite <- glue("{temp_base}") %>%
          str_replace_all(pattern = '_allowed$', replacement = '') %>%
          sym()
        
      } else {
        
        temp_opposite <- glue("{temp_base}_allowed") %>%
          as.character() %>%
          sym()
        
      }
      
      return_tibble <- return_tibble %>%
        straight_over_prior_grid(base_stat = !!temp_base,
                             opposite_stat = !!temp_opposite,
                             grid = grid)
      
    }

    return(return_tibble)
    
}












