

calculate_like_opponent_differential <- function(pbp_data,
                                                 new_week_data = new_week,
                                                 results_table = res_table) {
  

  results_table <- results_table %>%
    # Results table doesn't have id_defteam, id_week, id_season
    left_join(
      select(pbp_data, id_game, id_posteam, id_defteam, id_season, id_week) %>%
        unique(),
      by = c('id_game', 'id_posteam')
    ) %>%
    bind_rows(select(new_week_data, id_game, id_posteam, id_defteam, id_season, id_week)) %>%
    # Clean up
    select(id_game, id_posteam, id_defteam, id_season, id_week, result  = starts_with('outcome_')) %>%
    # Arrange by team
    group_by(id_posteam) %>%
    arrange(id_season, id_week, .by_group = T) %>%
    ungroup()
  
  like_opponents_helper <- function(data) {
    
    data <- data %>%
      ungroup()
    
    # Target col for id_posteam or id_defteam
    
    id_posteam_vector <- vector(mode = 'list', length = nrow(data))
    results_tibble <- vector(mode = 'list', length = nrow(data))
    
    for(i in 1:length(id_posteam_vector)) {
      
      cur_index <- i
      min_index <- i - 15 # Ok if negative
      index_vector <- min_index:(cur_index-1) # don't include current game
      
      # Filter and find like opponents
      
      proper_games <- data %>%
        filter(index %in% index_vector)
      
      # Pull opponents out
      id_posteam_vector[[i]] <- proper_games %>%
        pull(id_defteam)
      
      # Tibble of opponent, id_game and result
      results_tibble[[i]] <- proper_games %>%
        select(id_game, id_defteam, result)
      
    }
    
    # Create opponent column and tibble column
    return_tibble <- bind_cols(as_tibble_col(id_posteam_vector, 
                                             column_name = 'opponents'))
    
    return_tibble <- return_tibble %>%
      bind_cols(results_tibble %>%
                  enframe(value = 'results_tibble') %>%
                  select(-name))
    
    rebinded <- data %>%
      bind_cols(return_tibble)
    
    return(rebinded)
    
  }
  
  like_results_table <- results_table %>%
    group_by(id_posteam) %>%
    mutate(index = row_number()) %>%
    # This takes long
    # I'm not sure how to make it faster
    group_modify( ~ {
      .x %>%
        like_opponents_helper()
    }) %>%
    ungroup() %>%
    select(id_game,
           id_season,
           id_week,
           id_posteam,
           id_defteam,
           pos_previous = opponents,
           pos_previous_results = results_tibble) %>%
    # Join to self
    inner_join(
      x = .,
      y = select(
        .,
        id_game,
        id_posteam = id_defteam,
        def_previous = pos_previous,
        def_previous_results = pos_previous_results
      ),
      by = c('id_game', 'id_posteam'),
    ) %>% 
    # Finding Intersection of Vectors
    mutate(opp_intersect = map2(pos_previous, def_previous, ~ intersect(.x, .y))) %>%
    select(-c(pos_previous, def_previous)) %>%
    # Filter down each intersection tibble and compute results
    # This also takes a while to run
    mutate(pos_like_res = map2_dbl(pos_previous_results, opp_intersect, ~ {
      results <- .x %>%
        filter(id_defteam %in% .y) %>%
        pull(result)
      return(sum(results))
    })) %>%
    # Clean up
    select(id_game, id_posteam, id_defteam, pos_like_res)
  
  return(like_results_table)

}
