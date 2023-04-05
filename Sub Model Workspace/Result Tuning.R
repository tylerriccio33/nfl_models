
# Initialization

library(tidyverse)
library(rlang)
library(glue)
library(tabulaR)
library(corrr)

options(scipen = 999)

## Home Field Tuning

corrected_res

home_adjustment <- corrected_res %>%
  left_join(schedule_data, by = c('game_id','posteam')) %>%
  # Getting Season
  mutate(season = str_sub(game_id, 0,4) %>% as.numeric()) %>%
  select(game_id, posteam, home, season, res) %>%
  drop_na() %>%
  filter(home) %>%
  group_by(season) %>%
  summarize(res_factor = mean(res)) %>%
  ungroup()

dput(home_adjustment)

  pivot_longer(cols = -c(season, home)) %>%
  pivot_wider(id_cols = c(season, name),
              names_from = home) %>%
  # Stealing res
  filter(name == 'res') %>%
  select(season, res_factor = home)
  # Calculating Advantage
  mutate(advantage = abs(home) - abs(away)) %>%
  group_by(season, name) %>%
  arrange(advantage, .by_group = T) %>%
  slice() %>%
  ungroup()
  
  calculate_home_points <- function(data) {
    
    dput_data <-
      structure(
        list(
          season = c(
            2004,
            2005,
            2006,
            2007,
            2008,
            2009,
            2010,
            2011,
            2012,
            2013,
            2014,
            2015,
            2016,
            2017,
            2018,
            2019,
            2020,
            2021,
            2022
          ),
          res_factor = c(
            2.98395857098976,
            3.87749637149583,
            1.30701758036823,
            2.12802110185853,
            2.44273348883422,
            2.77592713388232,
            1.77811472565195,
            3.11496258669986,
            1.67102011253796,
            3.09796553423507,
            2.57099199662796,
            2.44010030073046,
            3.44604100970819,
            2.49388775059794,
            1.97026538007463,
            0.575504726393332,
            0.244668698807412,
            2.24013964065273,
            2.01165951552879
          )
        ),
        class = c("tbl_df", "tbl", "data.frame"),
        row.names = c(NA,-19L)
      )
    
    data %>%
      left_join(schedule_data, by = c('game_id', 'posteam')) %>%
      mutate(season = str_sub(game_id, 0, 4) %>% as.numeric()) %>%
      left_join(dput_data, by = 'season') %>%
      mutate(home_points = if_else(home, res_factor * -1, res_factor)) %>%
      select(game_id, posteam, home, home_points)
    
  }
  
  t <- corrected_res %>%
    calculate_home_points()

## Fumble Data
## ----------------------------------------

fumble_epa <- data %>%
  filter(fumble == 1) %>%
  select(game_id, posteam, defteam, epa) %>%
  drop_na() %>%
  group_by(game_id, posteam, defteam) %>%
  summarize(fumble_epa = sum(epa)) %>%
  ungroup()


  adj_result_data <- priors_and_posterior %>%
    # Getting Seasons
    left_join(data %>%
                select(game_id, season) %>% unique(), by = 'game_id') %>%
    select(game_id, season, posteam, vegas_factor) %>%
    filter(season %in% c(2015:2022)) %>%
    left_join(corrected_res, by = c('game_id', 'posteam')) %>%
    # Getting Fumble Data
    left_join(fumble_epa, by = c('game_id', 'posteam')) %>%
    mutate(
      fumble_epa = if_else(is.na(fumble_epa), 0, fumble_epa),
      adj_drive_points = adj_drive_points - fumble_epa
    )
  
  adj_result_data
  
  # Write Fumble Function
  
  calculate_fumble_points <- function(data, tune = F) {
    fumble_epa <- data %>%
      filter(fumble == 1) %>%
      select(game_id, posteam, defteam, epa) %>%
      drop_na() %>%
      group_by(game_id, posteam, defteam) %>%
      summarize(fumble_epa = sum(epa)) %>%
      ungroup() %>%
      select(-defteam)
    
    # If you're tuning this function
    
    if (tune) {
      return(fumble_epa)
      
    } else {
      fumble_epa %>%
        mutate(fumble_epa = fumble_epa * .7)
      
    }
    
  }
  
  fumble_lookup <- data %>%
    calculate_fumble_points(tune = T)
  
  fumble_lookup
  
  adj_result_data <- corrected_res %>%
    # Getting Vegas Factor
    left_join(
      priors_and_posterior %>%
        select(game_id, posteam, vegas_factor),
      by = c('game_id', 'posteam')
    ) %>%
    left_join(fumble_lookup, by = c('game_id', 'posteam')) %>%
    # Fixing missing fumble epa
    mutate(fumble_epa = if_else(is.na(fumble_epa), 0, fumble_epa)) %>%
    # Dropping Existing Allowed
    select(-adj_drive_points_allowed)
  
  adj_result_data %>%
    mutate(adj_drive_points = res - (fumble_epa * 1)) %>%
    # Reversing
    group_by(game_id, posteam, defteam) %>%
    reverse_stat_dynamic(adj_drive_points) %>%
    ungroup() %>%
    # Calculating new result
    mutate(res = adj_drive_points - adj_drive_points_allowed) %>%
    rmse(res, vegas_factor) 

# Tune Fumble EPA
  
quick_grid_tune <- function(fumble_data = fumble_lookup) {
  
  grid <- seq(from = .01, to = 1, by = .02)
  results <- vector(mode = 'numeric', length = length(grid))
  
  for(i in 1:length(grid)) {
    
    print(glue("Iteration: {i} / {length(grid)}"))
    
    results[i] <- adj_result_data %>%
      mutate(adj_drive_points = adj_drive_points - (fumble_epa * grid[i])) %>%
      # Reversing
      group_by(game_id, posteam, defteam) %>%
      reverse_stat_dynamic(adj_drive_points) %>%
      ungroup() %>%
      # Calculating new result
      mutate(res = adj_drive_points - adj_drive_points_allowed) %>%
      rmse(res, vegas_factor) %>%
      pull(.estimate)
  }
  
  results <- bind_cols(grid_value = as_tibble_col(grid), 
                       result_value = as_tibble_col(results))  %>%
    rename(grid_value = 1,
           result_value = 2)
  
  
  return(results)
  
}

adj_result_data %>%
  quick_grid_tune() %>%
  ggplot(aes(grid_value, result_value)) +
  geom_point() +
  geom_smooth()

# Penalty EPA
## ----------------------------------------

raw_penalty_data <- calculate_penalty_epa(.seasons = 2015:2022) %>%
  as_tibble() %>%
  select(game_id,
         posteam,
         defteam,
         penalty_type,
         penalty_epa)

raw_penalty_data

  # Filter the types here
  group_by(game_id, posteam)
  summarize(penalty_epa = sum(penalty_epa)) %>%
  ungroup()

raw_penalty_data

adj_penalty_data <- priors_and_posterior %>%
  # Getting Seasons
  left_join(data %>%
              select(game_id, season) %>% unique(), by = 'game_id') %>%
  select(game_id, season, posteam, vegas_factor) %>%
  filter(season %in% c(2015:2022)) %>%
  left_join(corrected_res, by = c('game_id', 'posteam')) %>%
  # Getting Fumble Data
  left_join(raw_penalty_data, by = c('game_id', 'posteam')) %>%
  mutate(
    penalty_epa = if_else(is.na(penalty_epa), 0, penalty_epa),
    adj_drive_points = adj_drive_points - penalty_epa
  ) %>%
  # Dropping Existing Allowed
  select(-adj_drive_points_allowed)

tune_penalty_grid <- function(this_data = raw_penalty_data) {
  
  this_data <- this_data %>%
    as_tibble()
  
  results_lookup <- priors_and_posterior %>%
    # Getting Seasons
    left_join(data %>%
                select(game_id, season) %>% unique(), by = 'game_id') %>%
    select(game_id, season, posteam, vegas_factor) %>%
    filter(season %in% c(2015:2022)) %>%
    left_join(corrected_res, by = c('game_id', 'posteam')) %>%
    # Dropping Existing Allowed
    select(-c(adj_drive_points_allowed, res))
  
  # Grid
  
  types <- pull(this_data, penalty_type) %>%
    na.omit() %>%
    unique()
  
  # Adding the Zero here double checks the benchmark
  grid <- expand_grid(type = types,
                      factor = c(0, .5,  1))
  
  results_vector <- vector(mode = 'numeric', length = nrow(grid))
  
  # Loop
  
  type_vector <- pull(grid, type)
  factor_vector <- pull(grid, factor)
  
  for(i in 1:length(results_vector)) {
    
    print(glue("Iteration: {i} / {length(results_vector)}"))
    
    temp_type <- type_vector[i]
    print(temp_type)
    temp_factor <- factor_vector[i]
    
    temp_data <- this_data %>%
      filter(penalty_type == temp_type) %>%
      # Filter the types here
      group_by(game_id, posteam) %>%
      summarize(penalty_epa = sum(penalty_epa)) %>%
      ungroup()

    # Join and calculate rmse
    results_vector[i] <-results_lookup %>%
      # Getting Penalty Data
      left_join(temp_data, by = c('game_id', 'posteam')) %>%
      mutate(
        penalty_epa = if_else(is.na(penalty_epa), 0, penalty_epa),
        adj_drive_points = adj_drive_points - (penalty_epa * temp_factor)) %>%
      # Reversing
      group_by(game_id, posteam, defteam) %>%
      reverse_stat_dynamic(adj_drive_points) %>%
      ungroup() %>%
      # Calculating new result
      mutate(adj_res = adj_drive_points - adj_drive_points_allowed) %>%
      rmse(adj_res, vegas_factor) %>%
      pull(.estimate)
      
      
    # if(i > 20) break
      
  }
  
  grid %>%
    bind_cols(as_tibble_col(results_vector))
  
}

t <- tune_penalty_grid()

t

final_penalty_marks <- t %>%
  group_by(type) %>%
  slice_min(order_by = value, n = 1) %>%
  ungroup() %>%
  filter(value < 13.15159)

# Copy the final marks into the penalty function
dput(final_penalty_marks %>% select(-value))

# Final Penalty Function
calculate_penalty_points <- function(seasons) {
  
  
  # Read in the final tuned marks
  dput_final_marks <- structure(
    list(
      type = c(
        "Chop Block",
        "Defensive 12 On-field",
        "Defensive Delay of Game",
        "Defensive Offside",
        "Defensive Pass Interference",
        "Face Mask",
        "Illegal Bat",
        "Illegal Blindside Block",
        "Illegal Forward Pass",
        "Illegal Shift",
        "Intentional Grounding",
        "Offensive 12 On-field",
        "Personal Foul",
        "Unnecessary Roughness",
        "Unsportsmanlike Conduct"
      ),
      factor = c(1, 1, 1, 0.5, 0.5, 1, 1, 0.5, 1, 0.5, 1, 1, 1,
                 1, 0.5)
    ),
    row.names = c(NA,-15L),
    class = c("tbl_df", "tbl",
              "data.frame")
  )
  
  # Calculate Full Penalty EPA
  raw_penalty_data <- calculate_penalty_epa(.seasons = 2020:2022) %>%
    as_tibble() %>%
    select(game_id,
           posteam,
           defteam,
           penalty_type,
           penalty_epa) %>%
    # Filter the types here
    filter(penalty_type %in% final_penalty_marks$type) %>%
    # Mutate Factors here
    left_join(final_penalty_marks %>% select(-value),
              by = c('penalty_type' = 'type')) %>%
    mutate(penalty_epa = penalty_epa * factor) %>%
    select(-c(factor, penalty_type)) %>%
    group_by(game_id, posteam, defteam) %>%
    summarize(penalty_epa = sum(penalty_epa)) %>%
    ungroup()

  return(raw_penalty_data)
  
  
}

penalty_lookup <- calculate_penalty_points(seasons = 2020:2022)

penalty_lookup

## -------------------------------

# Final Function

adjust_scores <- function(data, corrected_res){
  
  calculate_penalty_points <- function(seasons) {
    
    
    # Read in the final tuned marks
    dput_final_marks <- structure(
      list(
        type = c(
          "Chop Block",
          "Defensive 12 On-field",
          "Defensive Delay of Game",
          "Defensive Offside",
          "Defensive Pass Interference",
          "Face Mask",
          "Illegal Bat",
          "Illegal Blindside Block",
          "Illegal Forward Pass",
          "Illegal Shift",
          "Intentional Grounding",
          "Offensive 12 On-field",
          "Personal Foul",
          "Unnecessary Roughness",
          "Unsportsmanlike Conduct"
        ),
        factor = c(1, 1, 1, 0.5, 0.5, 1, 1, 0.5, 1, 0.5, 1, 1, 1,
                   1, 0.5)
      ),
      row.names = c(NA,-15L),
      class = c("tbl_df", "tbl",
                "data.frame")
    )
    
    # Calculate Full Penalty EPA
    raw_penalty_data <- calculate_penalty_epa(.seasons = 2004:2022) %>%
      as_tibble() %>%
      select(game_id,
             posteam,
             defteam,
             penalty_type,
             penalty_epa) %>%
      # Filter the types here
      filter(penalty_type %in% final_penalty_marks$type) %>%
      # Mutate Factors here
      left_join(final_penalty_marks %>% select(-value),
                by = c('penalty_type' = 'type')) %>%
      mutate(penalty_epa = penalty_epa * factor) %>%
      select(-c(factor, penalty_type)) %>%
      group_by(game_id, posteam, defteam) %>%
      summarize(penalty_epa = sum(penalty_epa)) %>%
      ungroup()
    
    return(raw_penalty_data)
    
    
  }
  

  # Get Penalty Lookup
  penalty_lookup <- calculate_penalty_points(seasons = 2004:2022)

  # Get Fumble Lookup
  
  calculate_fumble_points <- function(data, tune = F) {
    fumble_epa <- data %>%
      filter(fumble == 1) %>%
      select(game_id, posteam, defteam, epa) %>%
      drop_na() %>%
      group_by(game_id, posteam, defteam) %>%
      summarize(fumble_epa = sum(epa)) %>%
      ungroup() %>%
      select(-defteam)
    
    # If you're tuning this function
    
    if (tune) {
      return(fumble_epa)
      
    } else {
      fumble_epa %>%
        mutate(fumble_epa = fumble_epa * .7)
      
    }
    
  }
  
  fumble_lookup <- data %>%
    calculate_fumble_points()
  
  # Get Home Points
  calculate_home_points <- function(data) {
    
    dput_data <-
      structure(
        list(
          season = c(
            2004,
            2005,
            2006,
            2007,
            2008,
            2009,
            2010,
            2011,
            2012,
            2013,
            2014,
            2015,
            2016,
            2017,
            2018,
            2019,
            2020,
            2021,
            2022
          ),
          res_factor = c(
            2.98395857098976,
            3.87749637149583,
            1.30701758036823,
            2.12802110185853,
            2.44273348883422,
            2.77592713388232,
            1.77811472565195,
            3.11496258669986,
            1.67102011253796,
            3.09796553423507,
            2.57099199662796,
            2.44010030073046,
            3.44604100970819,
            2.49388775059794,
            1.97026538007463,
            0.575504726393332,
            0.244668698807412,
            2.24013964065273,
            2.01165951552879
          )
        ),
        class = c("tbl_df", "tbl", "data.frame"),
        row.names = c(NA,-19L)
      )
    
    data %>%
      left_join(schedule_data, by = c('game_id', 'posteam')) %>%
      mutate(season = str_sub(game_id, 0, 4) %>% as.numeric()) %>%
      left_join(dput_data, by = 'season') %>%
      mutate(home_points = if_else(home, res_factor * -1, res_factor)) %>%
      select(game_id, posteam, home_points)
    
  }
  
  corrected_res %>%
    # Penalty Lookup
    left_join(penalty_lookup, by = c('game_id', 'posteam', 'defteam')) %>%
    # Fumble Lookup
    left_join(fumble_lookup, by = c('game_id', 'posteam'))%>%
    # Fixing missing epa
    mutate(across(c(fumble_epa, penalty_epa), ~ if_else(is.na(.x), 0 , .x))) %>%
    # Drop existing result and drive points allowed
    select(-c(adj_drive_points_allowed, res)) %>%
    # Calculate new adjusted drive points
    mutate(adj_drive_points = adj_drive_points - penalty_epa,
           adj_drive_points = adj_drive_points - fumble_epa) %>%
    # Remove fumble and penalty epa
    select(-c(penalty_epa, fumble_epa)) %>%
    # Reverse drive points
    group_by(game_id, posteam, defteam) %>%
    reverse_stat_dynamic(adj_drive_points) %>%
    ungroup() %>%
    # Calculate new result
    mutate(res = adj_drive_points - adj_drive_points_allowed) %>%
    # Add Home Points
    calculate_home_points() %>%
    mutate(vegas_res = res,
           res = res + home_points)
  
    
}










