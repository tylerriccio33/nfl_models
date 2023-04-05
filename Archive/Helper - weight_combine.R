
weight_combine <- function(data, ..., name, mode = 'derive') {
  
  stored_data <- data
  
  # Reducing data carried around
  data <- data %>%
    select(season, week, posteam, game_id, res, ...)
  
  # Use First 8 to Predict Last 8
  
  lookup <- data %>%
    # Week greater than 8 and less than 17
    filter(week > 8 & week < 17) %>%
    # Summarize
    group_by(season, posteam) %>%
    summarize(res = mean(res),.groups = 'drop')
  
  # Create a grid of weights
  feature_cols <- enexprs(...) %>%
    as.character()
  weight_grid <-
    crossing(!!!set_names(map(feature_cols, ~ seq(0, 1, by = 0.3)), feature_cols)) %>%
    # Compute Sums
    mutate(row_sums = rowSums(across(c(...)))) %>%
    # Removing invalid combinations
    filter(row_sums >= 1,
           if_all(everything())) %>%
    select(-row_sums)
  
  # Apply weights
  rmse_values <- vector(mode = 'numeric', length = nrow(weight_grid))

  for(i in 1:nrow(weight_grid)) {
    
    rmse_values[i] <- data %>%
      filter(week >= 1 & week <= 8) %>%
      # Apply weights
      mutate(
        across(c(...), ~ scales::rescale(.x)),
        across(c(...), ~ .x * weight_grid[i, as.character(cur_column())][[1]]),
             composite = rowSums(across(c(...)))) %>% 
      # Summarize by season
      reframe(composite = mean(composite), .by = c(season, posteam)) %>%
      # Join to lookup
      left_join(lookup, by = c('season','posteam')) %>%
      yardstick::rmse(res, composite) %>%
      pull(.estimate)
    
    # Print progress
    svMisc::progress(i)
      
  }
  
  weight_grid <-  weight_grid %>%
    bind_cols(as_tibble_col(rmse_values)) %>%
    arrange(value)
  
  return(weight_grid)
  
}

offense_game_collapsed %>%
  weight_combine(yards_gained_oe,
                 drive_points_oe,
                 drive_wpa, 
                 name = 'drive_efficacy')

combine_features <- function(data, 
                             outcome, 
                             feature_selector, 
                             weights_init, 
                             method) {
  
  # Select the feature variables using the feature_selector argument
  features <- data %>% 
    select(all_of(feature_selector))
  
  # number of features
  n <- ncol(features)
  
  # function to calculate composite feature
  composite <- function(weights) {
    return(features %*% weights)
  }
  
  # function to calculate RMSE of composite feature and outcome
  rmse <- function(weights) {
    y_pred <- composite(weights)
    sqrt(mean((outcome - y_pred)^2))
  }
  
  # constraints on the weights
  constraints <- list(type = "ineq",
                      fun = function(weights) 1 - sum(weights))
  
  # optimize weights using the specified method
  result <- optimx::optimx(weights_init, 
                           rmse, 
                           method = method,
                           constraints = constraints, 
                           control = list(fnscale = -1))
  
  # return the best set of weights and the composite feature
  list(weights = result$par, composite = composite(result$par))
}


combine_features(
  offense_game_collapsed,
  outcome = "res",
  feature_selector = c("yards_gained_oe", "drive_points_oe"),
  rep(1 / 3, 3),
  "nlm"
)















































