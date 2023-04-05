over_expected <- function(data,
                          ...,
                          team_stat = F) {
  
  # Get selections
  
  selections <- select(data, ...) %>%
    colnames()
  
  results_vector <- vector(mode  = 'list', length = length(selections))
  
  for(i in 1:length(selections)) {
    
    if(!team_stat) {
      
      # Get Reverse
      
      if(str_detect(selections[i], 'allowed$')) {
        # If contains allowed, remove it
        temp_reverse <- str_replace_all(selections[i], '_allowed$', '') %>%
          sym()
      } else {
        # If it doesn't contain allowed, add it
        temp_reverse <- str_c(selections[i], '_allowed') %>%
          sym()
      }
      
    } else {
      
      temp_reverse <- selections[i] %>%
        sym()
      
    }
    
    base_stat <- selections[i] %>%
      sym()
    
    # Get Opponent Data
    
    opponent_data <- data %>%
      group_by(id_posteam) %>%
      arrange(id_season, id_week, .by_group = T) %>%
      # Lag and Roll
      mutate("{{temp_reverse}}" := lag(pracma::movavg({{temp_reverse}}, type = 's', n = 10
      ))) %>%
      ungroup() %>%
      # Renaming
      rename_with(~ glue("{.x}_opponent_prior"), .cols = c(all_of(temp_reverse))) %>%
      # Only selecting the merge stat
      select(id_game,
             id_posteam, 
             contains('opponent_prior'))
    
    # Merge in opponent data back in with weekly
    
    merge_data <- data %>%
      left_join(opponent_data, by = c('id_game','id_defteam' = 'id_posteam'))
    
    # Calculate League Mean
    
    temp_league_mean <- glue("{selections[i]}_league_mean") %>%
      as.character() %>%
      sym()
    
    league_data <- data %>%
      group_by(id_season, id_week) %>%
      summarize("{{temp_league_mean}}" := mean({{temp_reverse}})) %>%
      ungroup() %>%
      # Lag and Roll league data
      arrange(id_season, id_week) %>%
      mutate("{{temp_league_mean}}" := lag(pracma::movavg(
        {{temp_league_mean}}, n = 10, type = 's'
      )))
    
    # Merge League Data back with merge data
    
    if(!team_stat) {
      
      
      if(str_detect( selections[i], 'allowed$')) {
        # If base stat is allowed, opposing prior doesn't contain allowed
        opposing_prior <-
          glue("{str_replace_all(selections[i], '_allowed$', '')}_opponent_prior") %>%
          as.character() %>%
          sym()
      } else {
        # If base stat isn't allowed, opposing prior should contain allowed
        opposing_prior <- glue("{selections[i]}_allowed_opponent_prior") %>%
          as.character() %>%
          sym()
      }
      
    } else {
      
      opposing_prior <- glue("{selections[i]}_opponent_prior") %>%
        as.character() %>%
        sym()
      
    }
    
    # Build factor name
    
    temp_factor <- glue("{selections[i]}_factor") %>%
      as.character() %>%
      sym()
    
    merge_data <- merge_data %>%
      left_join(league_data, by = c('id_season','id_week')) %>%
      # Constructing Factor
      mutate("{{temp_factor}}" := if_else(!is.na({{temp_league_mean}}), {{temp_league_mean}} - {{opposing_prior}}, 0)) %>%
      # Doing Adjustment
      mutate({{base_stat}} := {{base_stat}} + {{temp_factor}}) %>%
      # Clean out
      select(id_game, id_posteam, {{base_stat}})
    
    results_vector[[i]] <- merge_data
    
  }
  
  results_vector <- results_vector %>%
    reduce(left_join, by = c('id_game', 'id_posteam'))
  
  data %>%
    select(-c(all_of(selections))) %>%
    left_join(results_vector, by  = c('id_game', 'id_posteam'))
  
}
