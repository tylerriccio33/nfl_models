
# Libraries
library(tidyverse)
library(tidymodels)
library(nflreadr)
library(rlang)
library(glue)
library(gt)
library(gtExtras)

# Data
pbp_data <- load_pbp(seasons = 2021:2022) %>%
  as_tibble() %>%
  # Filtering
  filter(play_type %in% c('run', 'pass'),
         special == 0,
         fumble_lost == 0) %>%
  drop_na(success, down, epa) %>%
  # Break up seasons and eras
  # Filtering GT, downs and goaline
  filter(!(wp > .9 | wp < .1 & qtr == 4)) %>%
  select(posteam_id = posteam,
         defteam_id = defteam,
         game_id,
         season_id = season,
         play_id,
         # Outcome
         success, 
         # Player IDs
         rusher_player_id,
         receiver_player_id,
         passer_player_id,
         qb_dropback,
         run_location,
         xyac_success)

participation_data <- load_participation(seasons = 2021:2022) %>%
  as_tibble() %>%
  select(game_id = nflverse_game_id,
         play_id,
         offense_players) %>%
  filter(offense_players != '') %>%
  drop_na() %>%
  # This is remarkably quick
  separate_rows(offense_players, sep = ';') %>%
  rename(gsis_id = offense_players)

player_data <- load_players() %>%
  as_tibble() %>%
  select(gsis_id,
         position,
         name = display_name) %>%
  drop_na()

model_data <- participation_data %>%
  left_join(pbp_data, by = c('game_id', 'play_id')) %>%
  filter(season_id == 2022) %>%
  drop_na(posteam_id) %>%
  select(-play_id) %>%
  # Load Normal GSIS
  left_join(player_data,
            by = 'gsis_id') %>%
  # Load Rusher ID
  left_join(select(player_data, gsis_id, rush_pos = position),
            by = c('rusher_player_id'='gsis_id')) %>%
  # Load Passer ID
  left_join(select(player_data, gsis_id, pass_pos = position),
            by = c('passer_player_id'='gsis_id')) %>%
  # Load Receiver ID
  left_join(select(player_data, gsis_id, rec_pos = position),
            by = c('receiver_player_id'='gsis_id')) %>%
  relocate(ends_with('_id'))

# Deriving player responsibility

responsibility_table <- model_data %>%
  # Filter Positions
  filter(position %in% c('QB','RB','WR'))
  



# Code
oos_baseline_helper <- function(data) {
  # List team, season, era, player combinations
  all_pairs <- data %>%
    group_by(posteam_id, season_id, gsis_id, position) %>%
    # First level play cutoff filter
    filter(n() >= 60) %>%
    # Summarize pairs to list
    summarize(pairs = list(cur_group()), .groups = 'drop') %>%
    select(pairs)
  
  # Calculate defense adjustment LUT
  adjustment_lut <- data %>%
    group_by(defteam_id, season_id) %>%
    summarize(def_success = mean(success), .groups = 'drop') %>%
    # Join to seasonal baseline
    left_join(
      data %>%
        group_by(season_id) %>%
        summarize(season_avg = mean(success), .groups = 'drop'),
      by = c('season_id')
    ) %>%
    # Calculating adjustment
    mutate(adjustment = season_avg - def_success) %>%
    select(defteam_id, season_id, adjustment)
  
  # Adjusting Data
  adjusted_data <- data %>%
    left_join(adjustment_lut, by = c('defteam_id', 'season_id')) %>%
    # Adjusting success
    mutate(success = success + adjustment)
  
  # For each pair, calculate the OOS average
  oos_avg <- vector(mode = 'numeric', length = nrow(all_pairs))
  for (i in seq_along(oos_avg)) {
    cur_pair <- pull(all_pairs[i,])[[1]]
    cur_posteam <- cur_pair$posteam_id
    cur_season <- cur_pair$season_id
    cur_gsis <- cur_pair$gsis_id
    cur_position <- cur_pair$position
    # Need to check if play is related to player
    filter_expression <- NULL
    
    # Assigning Responsibilities
    if (cur_position == 'RB') {
      filter_expression <-
        expr(# Plays where RB catches ball
          (qb_dropback == 1 & receiver_player_id == cur_gsis) |
            # Plays where RB rushes
            rusher_player_id	== cur_gsis)
    }
    
    cur_data <- adjusted_data %>%
      filter(posteam_id == cur_posteam,
             season_id == cur_season,
             !!filter_expression)
    
    # Filter play cutoff at second level
    if (nrow(cur_data) <= 100)
      next
    
    oos_data <- cur_data %>%
      filter(gsis_id != cur_gsis)
    # Filter OOS Data
    if (nrow(oos_data) <= 100) {
      # If no realistic OOS set compare to baseline
      # Include all data
      oos_data <- adjusted_data %>%
        # Remove current posteam
        filter(posteam_id != cur_posteam,!!filter_expression)
    }
    
    cur_data <- cur_data %>%
      filter(gsis_id == cur_gsis)
    # Filter play cutoff at second level
    if (nrow(cur_data) <= 100)
      next
    
    # Comparison of means via T Test
    
    diff <- t.test(cur_data$success, oos_data$success) %>%
      broom::tidy()
    
    # Append metrics in line
    all_pairs[i, ][[1]][[1]]$p_val <- diff$p.value
    all_pairs[i, ][[1]][[1]]$t_stat <- diff$statistic
    
    # Print Progress
    print(glue("{i} / {length(oos_avg)}"))
    
  }
  
  reduced_pairs <- all_pairs %>%
    pull(pairs) %>%
    reduce(bind_rows) %>%
    # Calculate and arrange SAR
    mutate(t_stat = round(t_stat, digits = 4),
           p_val = round(p_val, digits = 4)) %>%
    arrange(-t_stat)
  
  return(reduced_pairs)
  
}

model_data %>%
  filter(position = 'RB') %>%
  oos_baseline_helper()

t <- model_data %>%
  filter(posteam_id == 'NYJ') %>%
  # Filter Responsibilities
  filter((qb_dropback == 0 &
            rush_pos == 'RB') |
           (qb_dropback == 1 & receiver_player_id == gsis_id)) %>%
  # Selecting
  select(success, xyac_success, name) %>%
  # Impute XYAC
  mutate(xyac_success = if_else(
    is.na(xyac_success),
    mean(xyac_success, na.rm = T),
    xyac_success
  ))

linear_reg() %>%
  set_engine('lm') %>%
  fit(success ~ xyac_success + name - 1, data = t) %>%
  tidy()

t <- model_data %>%
  group_by(name) %>%
  filter(n() > 100) %>%
  ungroup() %>%
  mutate(position = case_when(position %in% c('C', 'G', 'T') ~ 'OL',
                              TRUE ~ position)) %>%
  filter(position %in% c('OL', 'QB', 'RB', 'WR', 'TE')) %>%
  select(posteam_id, success, name, position) 
  

res <- t %>%
  group_by(posteam_id, position) %>%
  nest() %>%
  mutate(model = map(data, ~ {
    linear_reg() %>%
      set_engine('lm') %>%
      fit(success ~ name - 1, data = .x) %>%
      tidy() %>%
      mutate(term = str_replace_all(term, 'name', ''))
  }))

res %>%
  select(-data) %>%
  unnest(model)



# Mahomes GSIS
'00-0033873'
# Zach Wilson GSIS
'00-0037013'






res <- model_data %>%
  filter(position == 'RB') %>%
  oos_baseline_helper()

# Visuals
res %>%
  mutate(rank = row_number()) %>%
  select(-c(season_id)) %>%
  left_join(
    load_players() %>%
      as_tibble() %>%
      select(gsis_id,
             headshot),
    by = 'gsis_id'
  ) %>%
  filter(name == 'Breece Hall')
  slice_head(n = 100) %>%
  # Visualize
  select(-gsis_id) %>%
  gt() %>%
  gt_img_rows(headshot) %>%
  gt_theme_538()





