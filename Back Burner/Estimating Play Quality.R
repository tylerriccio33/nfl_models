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

data <- nflreadr::load_pbp(2010:2022) %>%
  drop_na(posteam, result, epa, down) %>%
  filter(
    play_type %in% c('run', 'pass'),
    special == 0,
    # Hail Mary Criteria
    !(half_seconds_remaining <= 10 &
        pass_attempt == 1 &
        pass_length == "deep")
  )

res_lookup <- data %>%
  filter(week >= 9 & week <= 16) %>%
  select(game_id, week, season, posteam, home_team, result) %>%
  mutate(result = ifelse(home_team == posteam, result, result * -1)) %>%
  unique() %>%
  drop_na() %>%
  group_by(posteam, season) %>%
  summarize(points = sum(result), .groups = 'drop')

# Main Function

calculate_weights <- function(data, ..., lookup = res_lookup) {
    
    # Weights
    weights <- seq(from = 0, to = 2, by = .05)
    
    data <- data %>%
      filter(week <= 8)
    
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

# Resample

resample_fn <- function(data, ..., v = 10) {
  
  results_vector <- vector(mode = 'list', length = v)
  for(i in 1:v) {
    # Have to resample the lookup
    temp_lookup <- res_lookup %>%
      slice_sample(prop = .75)
    
    results_vector[[i]] <- data %>%
      # Anti join on temp lookup
      semi_join(temp_lookup, by = c('season','posteam')) %>%
      calculate_weights(..., lookup = temp_lookup)
    
    print(glue("Fold Complete: {i} / {v}"))
    
  }
  
  results_vector %>%
    reduce(bind_rows) %>%
    group_by(condition) %>%
    mutate(v = row_number(),
           v = as.factor(v)) %>%
    ungroup()

}

condition_grid <- expand_grid(
  down = c(1:4),
  qb_dropback = c(0, 1),
  splash_play = c(0,1),
  incomplete_pass = c(0, 1),
  sack = c(0, 1),
  interception = c(0, 1),
  fumble = c(0, 1)
) %>%
  # Mutate dependencies
  mutate(across(
    # If qb_dropback == 0, incomplete pass, sack, interception all 0
    c(incomplete_pass, sack, interception),
    ~ if_else(qb_dropback == 0, 0, .x)),
    # If sack, interception or fumble, splash play is 0
    splash_play = if_else(incomplete_pass == 1 |sack == 1 | interception == 1 | fumble == 1, 0, splash_play),
    # If interception or incomplete, fumble is 0
    fumble = if_else(incomplete_pass ==1 | interception == 1, 0, fumble),
    # If interception or incomplete, sack is 0
    sack = if_else(incomplete_pass == 1 | interception ==1, 0, sack)
  ) %>%
  unique()

write_conditions <- function(condition_grid) {
  
  ncol <- ncol(condition_grid)
  cols <- colnames(condition_grid)
  results <- vector(mode = 'character', length = nrow(condition_grid))
  row_results <- vector(mode = 'numeric', length = length(cols))
  for(i in seq_along(results)) {
    row <- condition_grid[i,]
      for(j in seq_along(cols)) {
        row_results[j] <- glue("{cols[j]} == {row[, cols[j]][[1]]}") %>%
          as.character()
      }
    results[i] <- glue("{paste(row_results, collapse = ' & ')},")
  }
  return(results)
}

condition_grid %>%
  write_conditions() %>%
  .[1:5] %>%
  writeClipboard()

dt <- data %>%
  resample_fn(
    fumble == 1 & sack == 0,
    # Interceptions where down != 4 and it isn't 3rd down bomb
    interception == 1 & (down != 4 | !(down == 3 & pass_length == 'deep')),
    # Garbage Time
    (wp >= .95 | wp <= .05) & qtr == 4,
    yards_gained >= 30,
    posteam == home_team,
    down == 1,
    down == 2,
    down == 3,
    down == 4,
    v = 100
  )

dt

dt %>%
  ggplot(aes(v, weight, fill = condition)) +
  geom_col() +
  ylim(0, 2) +
  facet_wrap(~ condition) +
  ggthemes::theme_clean() +
  labs(title = 'EPA Weights Across V-Folds by Condition')

dt %>%
  group_by(condition) %>%
  summarize(mean = mean(weight),
            theta = sd(weight),
            folds = n()) %>%
  mutate(across(c(mean, theta), ~ round(.x, digits = 3))) %>%
  gt::gt() %>%
  gt::tab_header(title = 'Summary Stats Across Conditions') %>%
  gtExtras::gt_theme_538()

results <- data %>%
  drop_na(posteam, epa, result, down, interception) %>%
  mutate(
    # Downs
    down_weight = case_when(down == 1 ~ 1.6,
                            down == 2 ~ .73,
                            down == 3 ~ .78,
                            TRUE ~ 1),
    # GT
    gt_weight = case_when((wp >= .95 | wp <= .05) & qtr == 4 ~ .74,
                          TRUE ~ 1),
    # Fumble
    fumble_weight = if_else(fumble == 1 & sack == 0, .22, 1),
    # Interception
    int_weight = if_else(interception == 1 &
                           (down != 4 | !(
                             down == 3 & pass_length == 'deep'
                           )), 1.5, 1),
    # Splash Play Weight
    splash_weight = if_else(yards_gained >= 30, .81, 1),
    # Melding Weight
    # Checks for normal play then assigns down weight
    # Then checks in order of estimated importance
    melded_weight = case_when(gt_weight == 1 & fumble_weight == 1 & int_weight == 1 & splash_weight == 1 ~ down_weight,
                              gt_weight != 1 ~ gt_weight,
                              fumble_weight != 1 ~ fumble_weight,
                              int_weight != 1 ~ int_weight,
                              splash_weight != 1 ~ splash_weight),
    # WEPA
    wepa = epa * melded_weight
  ) %>%
  # Filter Weeks
  filter(week <= 8) %>%
  group_by(posteam, season) %>%
  summarize(epa = mean(epa),
            wepa = mean(wepa),
            .groups = 'drop') %>%
  # Join to get future points
  left_join(res_lookup, by = c('season','posteam'))

# Plot
results %>%
  pivot_longer(c(epa, wepa)) %>%
  ggplot(aes(points, value, color = name)) +
  geom_point() +
  geom_smooth() +
  facet_wrap( ~ name) +
  labs(title = 'Linear Points Model Across Metrics') +
  ggthemes::theme_fivethirtyeight()

# Summary Stats
results %>%
  select(-c(posteam, season)) %>%
  pivot_longer(c(epa, wepa)) %>%
  rename(metric = name) %>%
  group_by(metric) %>%
  summarize(rsq = cor(points, value),
            rmse = yardstick::rmse_vec(points, value)) %>%
  mutate(across(c(rsq, rmse), ~ round(.x, digits = 3))) %>%
  arrange(-rsq) %>%
  mutate(rank = row_number()) %>%
  relocate(metric, rank) %>%
  gt::gt() %>%
  gt::tab_header(title = 'Evaluations by Metric') %>%
  gtExtras::gt_theme_538()

  
# Write Grids

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


