
# Goal
# Create a function that modifies the metrics before game collapsing
# Use the same function to adjust the score as well

# Libraries

library(tidyverse)
library(glue)
library(rlang)
library(nflfastR)
library(corrr)
library(ggrepel)

# Data

points_lookup <- data %>%
  filter(week > 8 & week < 17) %>%
  # Getting drive points instead of result
  select(game_id,
         season,
         week,
         posteam,
         wp,
         drive = fixed_drive,
         drive_result = fixed_drive_result) %>%
  # Fixing NA
  drop_na() %>%
  # Removing end of half
  filter(drive_result != 'End of half') %>%
  # Adding points
  mutate(drive_points = case_when(
    drive_result == 'Field goal' ~ 3,
    drive_result == 'Touchdown' ~ 7,
    TRUE ~ 0
  )) %>%
  # Slicing
  group_by(game_id, posteam, drive) %>%
  slice(1) %>%
  ungroup() %>%
  # Filtering wp
  # Aggregating
  group_by(posteam, season) %>%
  summarize(points = mean(drive_points), .groups = 'drop')

# Main Function
# This should only need EPA because it's the best tried and true PBP stat
# In the future, weighting drive weights could be valuable
# For example, drive_yards_oe or drive_wpa could be weighted differently

# Reworking OOS RSQ

calculate_weights <-
  function(data,
           ...,
           lookup = points_lookup) {

    # Weights
    weights <- seq(from = 0, to = 2, by = .05)
    
    data <- data %>%
      filter(week <= 8) %>%
      drop_na(epa, posteam, down)
    
    # Conditions
    conditions <- enexprs(...) %>%
      unlist()
    condition_weights <-
      vector(mode = 'list', length = length(conditions))
    
    # Iterate through each condition
    for (i in seq_along(conditions)) {
      
      # Initialize loop variables
      last_rsq <- -Inf
      best_rsq <- -Inf
      best_weight <- NA
      
      # Iterate through weights and apply modification
      for (j in seq_along(weights)) {
        temp_data <- data %>%
          mutate(epa = if_else(!!conditions[[i]], epa * weights[j], epa)) %>%
          group_by(posteam, season) %>%
          summarize(epa = mean(epa), .groups = 'drop')
        
        # Join modified data to lookup table and calculate RSQ
        temp_data <- temp_data %>%
          left_join(lookup, by = c('posteam', 'season')) %>%
          drop_na()

        # Check if new RSQ is less than last RSQ
        current_rsq <- cor(temp_data$epa, temp_data$points)
        print(current_rsq)
        if (current_rsq < last_rsq) {
          break
        } else {
          # Update best RSQ and weight values
          last_rsq <- current_rsq
          if (current_rsq > best_rsq) {
            best_rsq <- current_rsq
            best_weight <- weights[j]
          }
        }
      }
      # Add best weight for current condition to list
      condition_weights[[i]] <-
        list(condition = conditions[[i]] %>% as_label(),
             weight = best_weight) %>%
        as_tibble()
      
      # Print Condition Iteration
      print(glue("{conditions[i]}: {round(i / length(conditions), digits = 2) * 100}%"))
    }
    
    condition_weights <- condition_weights %>%
      reduce(bind_rows)
    
    return(condition_weights)
    
}

down_weights <- data %>%
  filter(season >= 2010) %>%
  calculate_weights(down == 1 | down == 2)
gt_weights <- data %>%
  filter(season >= 2010) %>%
  calculate_weights(wp >= .95 | wp <= .05)
ns_fumble_weights <- data %>%
  filter(season >= 2010) %>%
  calculate_weights(fumble == 1 & sack == 0)
int_weights <- data %>%
  filter(season >= 2010) %>%
  calculate_weights(interception == 1 & down != 4)
gl_weights <- data %>%
  filter(season >= 2010) %>%
  calculate_weights(yardline_100 <= 20)
run_weights <- data %>%
  filter(season >= 2010) %>%
  calculate_weights(qb_dropback == 0)
big_play_weights <- data %>%
  filter(season >= 2010) %>%
  calculate_weights(yards_gained > 50)
home_weights <- data %>%
  calculate_weights(posteam == home_team)

# Write Grids
# Make sure it ends in _weights.csv

write(down_weights,
      gt_weights,
      ns_fumble_weights,
      int_weights,
      gl_weights,
      run_weights,
      big_play_weights,
      home_weights)

# Now give each play a play weight, a WP weight and a down weight

add_weight <- function(data, weight_grid, name) {
  
  # Initialize weights
  data <- data %>%
    mutate({{name}} := 1)
  
  for (i in 1:nrow(weight_grid)) {
    # get the current condition and weight
    condition <- weight_grid$condition[i] %>%
      parse(text = .)
    # return(condition)
    weight <- weight_grid$weight[i]
  
    # use the condition to filter rows in a
    name_symbol <- sym(name)
    data <- data %>%
      mutate({{name}} := if_else(eval(condition), weight, !!name_symbol))
  }
  
  return(data)
  
}

data %>%
  filter(season >= 2010) %>%
  drop_na(sack, fumble, interception, down, epa) %>%
  # Adding weights
  add_weight(weight_grid = gt_weights, name = 'gt_weight') %>%
  add_weight(weight_grid = ns_fumble_weights, name = 'ns_fumble_weight') %>%
  add_weight(weight_grid = gl_weights, name = 'gl_weight') %>%
  # Modifying EPA
  # Weights are added together then multiplied
  mutate(melded_weight = rowMeans(across(ends_with('_weight')))) %>%
  
  mutate(wepa = epa * melded_weight) %>%
  
  # new RSQ
  drop_na(wepa, epa, result) %>%
  group_by(season, week, game_id, posteam) %>%
  summarize(
    epa  = mean(epa),
    wepa = mean(wepa),
    .groups = 'drop'
  )  %>%
  # Join to get points
  left_join(points_lookup, by = c('game_id','posteam')) %>%
  group_by(posteam) %>%
  arrange(season, week, .by_group = T) %>%
  # Lagging
  mutate(across(c(epa, wepa), ~ lag(pracma::movavg(
    .x, n = 15, type = 'w'
  )))) %>%
  ungroup() %>%
  drop_na() %>%
  # RSQ
  summarize(epa = cor(epa, points),
            wepa = cor(wepa, points))
  select(epa, wepa, points) %>%
  pivot_longer(-c(points))  %>%
  ggplot(aes(points, value, color = name)) +
  geom_point(alpha = 1 / 4) +
  geom_smooth() +
  facet_wrap(~ name)

# --------------------------------- Archive

{
  # Weighting WP
  
  smooth_curve <- function(df) {
    # Fit a loess curve to the data
    fit <- loess(y ~ x, data = df)
    
    # Create a sequence of x values to use as input for the curve
    x_values <- seq(min(df$x), max(df$x), length.out = 100)
    
    # Use the fitted curve to predict y values for the x values
    y_values <- predict(fit, newdata = data.frame(x = x_values))
    
    # Create a tibble with the x and y values
    result <- tibble(
      x = x_values,
      y = y_values
    )
    
    return(result)
  }
  
  wp_grid <- smooth_curve(tibble(x = c(0, .5, 1),
                                 y = c(0, 2, 0))) %>%
    mutate(across(c(x, y), ~ round(.x, digits = 2)))
  
  wp_grid %>%
    ggplot(aes(x, y)) +
    geom_point()
  
  # write_csv(wp_grid, 'wp_grid.csv')
  
  data %>%
    mutate(wp = round(wp, digits = 2)) %>%
    left_join(wp_grid, by = c('wp' = 'x')) %>%
    mutate(wepa = epa * y) %>%
    # new RSQ
    drop_na(wepa, epa, result) %>%
    group_by(season, week, game_id, posteam) %>%
    summarize(
      epa  = mean(epa),
      wepa = mean(wepa),
      .groups = 'drop'
    )  %>%
    # Join to get points
    left_join(points_lookup, by = c('game_id','posteam')) %>%
    group_by(posteam) %>%
    arrange(season, week, .by_group = T) %>%
    # Lagging
    mutate(across(c(epa, wepa), ~ lag(pracma::movavg(
      .x, n = 15, type = 'w'
    )))) %>%
    ungroup() %>%
    drop_na() %>%
    select(epa, wepa, points) %>%
    # RSQ
    summarize(epa = cor(epa, points),
              wepa = cor(wepa, points))
  
# This uses the optional tune metric so RMSE or RSQ
calculate_weights <-
  function(data,
           ...,
           tune = 'RSQ') {
    
    # Look up
    lookup <- data %>%
      select(game_id, season, week, posteam, epa) %>%
      drop_na() %>%
      group_by(game_id, season, week, posteam) %>%
      summarize(epa = mean(epa), .groups = 'keep') %>%
      ungroup() %>%
      # Get new drive id
      group_by(posteam) %>%
      mutate(future_epa = mean_n_forward(x = epa, n = 5)) %>%
      ungroup() %>%
      select(game_id, posteam, future_epa)
    
    # Weights
    weights <- seq(from = 0, to = 2, by = .1)
    
    # Conditions
    conditions <- enexprs(...)
    condition_weights <-
      vector(mode = 'list', length = length(conditions))
    
    if (tune == 'RMSE') {
      # Iterate through each condition
      for (i in seq_along(conditions)) {
        # Initialize loop variables
        last_rmse <- Inf
        best_rmse <- Inf
        best_weight <- NA
        
        # Iterate through weights and apply modification
        for (j in seq_along(weights)) {
          temp_data <- data %>%
            mutate(epa = if_else(!!conditions[[i]], epa * weights[j], epa)) %>%
            group_by(game_id, posteam) %>%
            summarize(epa = mean(epa), .groups = 'drop')
          
          
          # Join modified data to lookup table and calculate RMSE
          temp_data <- temp_data %>%
            left_join(lookup, by = c('game_id', 'posteam')) %>%
            drop_na()
          
          # Check if new RMSE is greater than last RMSE
          current_rmse <-
            mean((temp_data$epa - temp_data$future_epa) ^ 2)
          if (current_rmse > last_rmse) {
            break
          } else {
            # Update best RMSE and weight values
            last_rmse <- current_rmse
            if (current_rmse < best_rmse) {
              best_rmse <- current_rmse
              best_weight <- weights[j]
            }
          }
        }
        # Add best weight for current condition to list
        condition_weights[[i]] <-
          list(condition = conditions[[i]] %>% as_label(),
               weight = best_weight) %>%
          as_tibble()
      }
    } else if (tune == 'RSQ') {
      # Iterate through each condition
      for (i in seq_along(conditions)) {
        # Initialize loop variables
        last_rsq <- -Inf
        best_rsq <- -Inf
        best_weight <- NA
        
        # Iterate through weights and apply modification
        for (j in seq_along(weights)) {
          temp_data <- data %>%
            mutate(epa = if_else(!!conditions[[i]], epa * weights[j], epa)) %>%
            group_by(game_id, posteam) %>%
            summarize(epa = mean(epa), .groups = 'drop')
          
          
          # Join modified data to lookup table and calculate RSQ
          temp_data <- temp_data %>%
            left_join(lookup, by = c('game_id', 'posteam')) %>%
            drop_na()
          
          # Check if new RSQ is less than last RSQ
          current_rsq <- cor(temp_data$epa, temp_data$future_epa)
          if (current_rsq < last_rsq) {
            break
          } else {
            # Update best RSQ and weight values
            last_rsq <- current_rsq
            if (current_rsq > best_rsq) {
              best_rsq <- current_rsq
              best_weight <- weights[j]
            }
          }
        }
        # Add best weight for current condition to list
        condition_weights[[i]] <-
          list(condition = conditions[[i]] %>% as_label(),
               weight = best_weight) %>%
          as_tibble()
      }
    }
    
    condition_weights <- condition_weights %>%
      reduce(bind_rows)
    
    return(condition_weights)
    
  }

mean_n_forward <- function(x, n) {
  # Create an empty vector to store the result
  result <- rep(NA, length(x))
  
  # Iterate through the elements of x
  for (i in 1:length(x)) {
    # If there are not enough elements after the current position, set the result to NA
    if (i + n > length(x)) {
      result[i] <- NA
    } else {
      # Calculate the mean of the next n elements and store it in the result vector
      result[i] <- mean(x[(i + 1):(i + n)])
    }
  }
  
  # Return the result vector
  return(result)
}

create_intervals_tibble <- function(start, end, interval, variable) {
  # create a sequence of numbers from start to end with the specified interval
  seq <- seq(start, end, by = interval)
  
  # create a list of expressions for each interval
  expressions <-
    map_chr(seq, ~ glue("{variable} >= {.x} & {variable} < {.x+ interval}")) %>%
    parse_exprs() %>%
    unlist()
  # expressions <- map_chr(seq, ~paste0("x >= ", .x, " & x < ", .x + interval))
  return(expressions)
  
}

# This creates an expression vector off of the grid of weights
get_expression <- function(df) {
  expressions <- character(nrow(df))
  for (i in 1:nrow(df)) {
    expressions[i] <- paste(names(df), "==", df[i, ], collapse = " & ")
  }
  return(expressions)
}

# This uses the forward looking RSQ
calculate_weights <-
  function(data,
           ...) {
    
    # Calculate OOS RSQ
    lookup <- data %>%
      select(game_id, season, week, posteam, home_team, result) %>%
      drop_na() %>%
      group_by(game_id, season, week, posteam) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(result = if_else(posteam == home_team, result, result * -1)) %>%
      # Get new drive id
      group_by(posteam) %>%
      arrange(season, week, .by_group = T) %>%
      mutate(future_result = mean_n_forward(x = result, n = 15)) %>%
      ungroup() %>%
      select(game_id, posteam, future_result)
    
    # Weights
    weights <- seq(from = 0, to = 3, by = .1)
    
    # Conditions
    conditions <- enexprs(...) %>%
      unlist()
    condition_weights <-
      vector(mode = 'list', length = length(conditions))
    
    # Iterate through each condition
    for (i in seq_along(conditions)) {
      
      # Print Condition Iteration
      print(glue("{conditions[i]}: {round(i / length(conditions), digits = 2) * 100}%"))
      
      # Initialize loop variables
      last_rsq <- -Inf
      best_rsq <- -Inf
      best_weight <- NA
      
      # Iterate through weights and apply modification
      for (j in seq_along(weights)) {
        temp_data <- data %>%
          mutate(epa = if_else(!!conditions[[i]], epa * weights[j], epa)) %>%
          group_by(game_id, posteam, season, week) %>%
          summarize(epa = mean(epa), .groups = 'drop') %>%
          # Arrange and average
          group_by(posteam) %>%
          arrange(season, week, .by_group = T) %>%
          mutate(epa = lag(pracma::movavg(
            x = epa, n = 15, type = 's'
          ))) %>%
          ungroup()
        
        # Join modified data to lookup table and calculate RSQ
        temp_data <- temp_data %>%
          left_join(lookup, by = c('game_id', 'posteam')) %>%
          drop_na()
        
        # Check if new RSQ is less than last RSQ
        current_rsq <- cor(temp_data$epa, temp_data$future_result)
        if (current_rsq < last_rsq) {
          break
        } else {
          # Update best RSQ and weight values
          last_rsq <- current_rsq
          if (current_rsq > best_rsq) {
            best_rsq <- current_rsq
            best_weight <- weights[j]
          }
        }
      }
      # Add best weight for current condition to list
      condition_weights[[i]] <-
        list(condition = conditions[[i]] %>% as_label(),
             weight = best_weight) %>%
        as_tibble()
    }
    
    condition_weights <- condition_weights %>%
      reduce(bind_rows)
    
    return(condition_weights)
    
  }
}