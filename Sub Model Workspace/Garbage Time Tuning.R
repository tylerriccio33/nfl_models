
# Initialization

library(tidyverse)
library(dtplyr)
library(rlang)
library(glue)

options(scipen = 999)
options(dplyr.summarise.inform = FALSE)
  
calculate_adjusted_drive_points <- function(raw_pbp, collapse = 'game') {
  
  # Collapse must be in ('drive','game')
  
  drive_collapsed <- raw_pbp %>%
    select(game_id, season, week, drive_points = fixed_drive_result, posteam, fixed_drive, ep) %>%
    mutate(
      real_drive_points = case_when(
        drive_points == 'Touchdown' ~ 7,
        drive_points == 'Field goal' ~ 3,
        TRUE ~ 0
      )
      ,
      adj_drive_points = ifelse(
        drive_points %in% c('Field goal', 'Missed field goal'),
        ep,
        real_drive_points
      )
    ) %>%
    drop_na() %>%
    # Drive collapsing
    group_by(game_id, season, week, posteam, fixed_drive) %>%
    summarize(real_drive_points =  sum(last(real_drive_points)),
              adj_drive_points = sum(last(adj_drive_points))) %>%
    ungroup()
  
  if(collapse == 'drive') return(drive_collapsed)
  
  # If not drive collapsed return game collapsed
  
  drive_collapsed %>%
    group_by(game_id, season, week, posteam) %>%
    summarize(
      drive_points = sum(real_drive_points),
      adj_drive_points = sum(adj_drive_points)
    ) %>%
    ungroup() %>%
    # Reverse
    group_by(game_id) %>%
    group_modify( ~ {
      drive_points_vector <- .x$drive_points
      adj_drive_points_vector <- .x$adj_drive_points
      .x %>%
        mutate(
          opposing_drive_points = if_else(
            row_number() == 1,
            drive_points_vector[2],
            drive_points_vector[1]
          ),
          opposing_adj_drive_points = if_else(
            row_number() == 1,
            adj_drive_points_vector[2],
            adj_drive_points_vector[1]
          )
        )
    }) %>%
    ungroup() %>%
    # Getting Result
    mutate(drive_points_result = drive_points - opposing_drive_points,
           adj_drive_points_result = adj_drive_points - opposing_adj_drive_points)
  
}

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
      play_id,
      # Play ID
      # Not needed for game collapsing
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

# Calculating Competition
# Only doing this so I don't have to recalculate

competition_lookup <-  data %>%
  calculate_competition(game_collapse = F) %>%
  select(game_id, play_id, wp_distance)
  # Binning WP Distance
  # mutate(wp_distance = round(wp_distance / 0.05) * .05) 

competition_lookup %>%
  ggplot(aes(wp_distance)) +
  geom_density()

# Tune Summary Conditions

tune_conditions <- function(data, expression) {
  
  # Parse Expression
  parsed_expression <- enexpr(expression) %>%
    as.character() %>%
    purrr::discard(~ .x == '[')
  column <- parsed_expression[1] %>%
    sym()
  condition <- parsed_expression[2]
  
  # Initializing Grid
  
  grid <- expand_grid(wp_distance = c(seq(from = 0, to = .5, by = .05), seq(from = .5, to = 1, by = .25))
                      , 
                      low_limit = (c(0:10) / 100)) %>%
    mutate(high_limit = 1 - low_limit)
  
  # Looping through
  
  loop_data <- data %>%
    # Dropping Key NA
    drop_na(game_id, posteam, epa, wp, {{column}})
  
  results_vector <- vector(mode = 'numeric', length = nrow(grid))
  
  for(i in 1:nrow(grid)) {
    
    print(glue("Iteration: {i} / {nrow(grid)}"))
    
    .low_limit <- grid$low_limit[i]
    .high_limit <- grid$high_limit[i]
    .wp_distance <- grid$wp_distance[i]

    summarized_data <- loop_data %>%
      # Filtering
      filter(wp >= .low_limit,
             wp <= .high_limit,
             wp_distance >= .wp_distance)
    
    if(nrow(summarized_data) <= 100){
      results_vector[i] <- NA_real_ 
      next
    }
    
    summarized_data <- summarized_data %>%
        # Grouping
        group_by(game_id, posteam, season, week) %>% 
        summarize("{{column}}" := mean({{expression}})) %>%
        # Lagging and cummulating
        group_by(posteam, season) %>%
        arrange(posteam, season, week) %>%
        mutate({{column}} := lag({{column}})) %>%
        drop_na() %>%
        mutate({{column}} := cummean({{column}})) %>%
        # Filtering Week
        ungroup() %>%
        filter(week > 7) %>%
        select(-c(week, season)) %>%
        # Joining to Lookup
        left_join(corrected_res %>%
                    select(game_id, posteam, adj_drive_points), 
                  by = c('game_id', 'posteam')) %>%
      drop_na()
    
    if(nrow(summarized_data) <= 1000) {
      results_vector[i] <- NA_real_
      next
    }
      # Correlation
    results_vector[i] <- 
      cor(pull(summarized_data, {{column}}), summarized_data$adj_drive_points)
     
    # if(i == 2) break
     
  }
  
  # Binding to Grid
  
  final_results <- grid %>%
    bind_cols(as_tibble_col(results_vector)) %>%
    drop_na() %>%
    arrange(-abs(value))
  
  oob <- final_results %>%
    filter(wp_distance == 0 & low_limit == 0) %>%
    pull(value)
  
  print(glue("OOB: {oob}"))
  
  final_results <- final_results %>%
    slice_head(n = 1)
  
  best_result <- final_results %>%
    pull(value)
  
  print(glue("Best Result: {best_result}"))
  
  # Taking Best Result and Copying
  
  final_results <- final_results %>%
    slice_head(n = 1)
  
  
  build_expression <-
    function(parsed_expression = parsed_expression,
             low_limit,
             high_limit,
             distance,
             copy = T) {
      
      column <- parsed_expression[1] %>%
        sym()
      condition <- parsed_expression[2]
      
      low_limit_condition <- glue("wp >= {low_limit}")
      high_limit_condition <- glue("wp <= {high_limit}")
      distance_condition <- glue("wp_distance >= {distance}")
      
      
      final_condition <-
        glue(
          "[{condition} & {low_limit_condition} & {high_limit_condition} & {distance_condition}]"
        )
      
      final_expression <- glue("mean({column}{final_condition})")
      
      if(copy) {
        print(final_expression)
        final_expression %>%
          as.character() %>%
          writeClipboard()
      } else {
        return(final_expression %>% as.character())
      }
      
    }
  
  build_expression(parsed_expression = parsed_expression,
                   low_limit = pull(final_results, low_limit),
                   high_limit = pull(final_results, high_limit),
                   distance = pull(final_results, wp_distance),
                   copy = T
                   )

}

data %>%
  left_join(competition_lookup, by = c('game_id', 'play_id')) %>%
  # Create dummy condition
  mutate(dummy = 1) %>%
  tune_conditions(expression = epa[qb_dropback == 0])




















