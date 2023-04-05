
calculate_expected_wp <- function(raw_pbp) {
  
  ## Calculating Game Control
  
  control_lookup <- raw_pbp %>%
    calculate_control() %>%
    left_join(raw_pbp %>%
                select(season,
                       week,
                       game = game_id) %>%
                unique(), by = 'game') %>%
    relocate(season, week, game, team = posteam)
  
  control_summarized <- control_lookup %>%
    # Lagging
    group_by(team) %>%
    arrange(season, week, .by_group = T) %>%
    mutate(median_wp = lag(median_wp)) %>%
    drop_na() %>%
    mutate(
      staight_mean = cummean(median_wp),
      pracma_6 = pracma::movavg(median_wp, n = 6, type = 'e'),
      pracma_12 = pracma::movavg(median_wp, n = 12, type = 'e')
    ) %>%
    ungroup() %>%
    select(game, team, staight_mean, pracma_6, pracma_12)
  
  ## Calculating Game Luck
  
  calculate_luck <- function(raw_pbp) {
    
    ## Getting Look-up table for 2h-ko
    
    kickoff_lookup <- raw_pbp %>%
      select(game = game_id,
             posteam) %>%
      drop_na() %>%
      group_by(game) %>%
      mutate(
        receive_2h_ko = first(posteam) != posteam,
        receive_2h_ko = if_else(receive_2h_ko, 1, 0)
      ) %>%
      ungroup() %>%
      unique()
    
    ## Cleaning up PBP and extracting information from description
    
    clean <- raw_pbp %>%
      # Selecting all with fumble
      select(
        play_id,
        game = game_id,
        posteam,
        defteam,
        desc,
        wp,
        wpa,
        yards = yards_gained,
        td_return = return_touchdown,
        fumble,
        lost = fumble_lost,
        oob = fumble_out_of_bounds,
        special,
        aborted_play
      ) %>%
      # Filtering fumble plays
      filter(fumble == 1) %>%
      mutate(yards_prior = str_match(desc, 'for\\s-?\\w\\w?')[, 1],
             yards_prior = ifelse(str_detect(yards_prior, 'no'),
                                  0,
                                  str_match(yards_prior, '-?\\d+')[, 1]),
             # Handling aborted plays
             yards_prior = ifelse(aborted_play == 1, 0, yards_prior),
             # Handling Punt yards
             yards_prior = ifelse(special == 1,
                                  str_match(desc, '.*punts (\\d+) yards')[, 2],
                                  yards_prior),
             # As Numeric
             yards_prior = yards_prior %>% as.numeric(),
             # Possession change
             end_pos = str_match(desc, 'by\\s[A-Z]+')[,1],
             # OOB
             end_pos = if_else(oob == 1, posteam, end_pos),
             # Remaining will be considered retained
             end_pos = if_else(is.na(end_pos), posteam, end_pos),
             # Cleaning up end pos
             end_pos = str_match(end_pos, '[A-Z]+')[,1],
             # New Lost
             lost = end_pos != posteam,
      ) %>%
      # Selecting only a few
      select(
        play_id,
        game,
        # desc,
        lost,
        initial_wp = wp,
        real_wpa = wpa,
        yards_prior,
        end_pos,
        td_return,
        special
      )
    
    ## Cleaning original data to use as baseline
    ## Joined with cleaned fumble data for the extra fields
    ## I could probably keep within the same pipeline but I broke it up
    
    baseline <- raw_pbp %>%
      # Selecting WP Cols
      select(
        play_id, 
        game = game_id,
        home_team,
        posteam,
        defteam,
        score_differential,
        half_seconds_remaining,
        game_seconds_remaining,
        spread_line,
        down,
        ydstogo,
        yardline_100,
        posteam_timeouts_remaining,
        defteam_timeouts_remaining,
        desc,
      ) %>%
      # Dropping NA
      drop_na() %>%
      # Getting 2h_ko
      mutate(
        receive_2h_ko = first(posteam) != posteam,
        receive_2h_ko = if_else(receive_2h_ko, 1, 0)
      ) %>%
      # Joining to cleaned fumble plays
      rename_with( ~ glue("real_{.x}"), c(down, ydstogo, yardline_100)) %>%
      left_join(clean, by = c('play_id', 'game')) %>%
      filter(!is.na(initial_wp))
    
    ## Getting all special plays
    
    specials <- baseline %>%
      filter(special == 1) %>%
      
      # Total Yards
      mutate(
        c_yardline_100 = 100 - (real_yardline_100 - yards_prior),
        # Yards to go
        c_ydstogo = if_else(c_yardline_100 < 10, c_yardline_100, 10),
        # Down
        c_down = 1,
        # Posteam
        c_posteam = defteam,
        # Score
        c_score_differential = score_differential * -1,
        # Timeouts
        c_posteam_timeouts_remaining = defteam_timeouts_remaining,
        c_defteam_timeouts_remaining = posteam_timeouts_remaining,
        # Spread
        c_spread_line = if_else(c_posteam == home_team, spread_line, spread_line * -1)
      ) %>%
      # Prepping
      select(
        play_id,
        game,
        old_posteam = posteam,
        home_team,
        posteam = c_posteam,
        score_differential = c_score_differential,
        half_seconds_remaining,
        game_seconds_remaining,
        spread_line = c_spread_line,
        down = c_down,
        ydstogo = c_ydstogo,
        yardline_100 = c_yardline_100,
        posteam_timeouts_remaining = c_posteam_timeouts_remaining,
        defteam_timeouts_remaining = c_defteam_timeouts_remaining
      ) %>%
      # Getting Kickoff
      left_join(kickoff_lookup, by = c('game', 'posteam')) %>%
      # Fixing Time
      mutate(across(c(contains('seconds')), ~ .x - 20)) %>%
      # Calculating new WP
      nflfastR::calculate_win_probability() %>%
      # Selecting a few
      select(play_id,
             game,
             old_posteam,
             posteam,
             wp) %>%
      # Joining to get initial wp
      left_join(clean %>% select(play_id, game, initial_wp, real_wpa),
                by = c('play_id', 'game')) %>%
      # Reversing wp
      mutate(wp = 1 - wp) %>%
      # New WPA
      mutate(wpa = wp - initial_wp,
             wp_diff = wpa - real_wpa)
    
    
    fair_game <- baseline %>%
      
      filter(special != 1) %>%
      
      ## Computing Counter-Factual
      
      mutate(
        # Total Yards
        c_yardline_100 = real_yardline_100 - yards_prior,
        # Yards To Go
        c_ydstogo = real_ydstogo - yards_prior,
        # Down
        c_down = case_when(c_ydstogo < 0 ~ 1,
                           TRUE ~ real_down + 1),
        # Yards to go again
        c_ydstogo = if_else(c_ydstogo < 0, 10, c_ydstogo),
        # Possession change
        c_posteam = case_when(c_down > 4 ~ defteam,
                              TRUE ~ posteam),
        switched_pos = c_posteam != posteam,
        c_down = if_else(switched_pos, 1, c_down),
        c_yardline_100 = if_else(switched_pos, 100 - c_yardline_100, c_yardline_100),
        # Score Diff
        c_score_differential = if_else(td_return == 1, score_differential + 7, score_differential),
        c_score_differential = if_else(switched_pos,
                                       c_score_differential * -1,
                                       score_differential),
        # Fix Spread
        spread_line = if_else(home_team == posteam, spread_line, spread_line * -1),
        c_spread_line = if_else(switched_pos, spread_line * -1, spread_line),
        # Fixing Timeouts
        c_posteam_timeouts_remaining = if_else(switched_pos,
                                               defteam_timeouts_remaining,
                                               posteam_timeouts_remaining),
        c_defteam_timeouts_remaining = if_else(switched_pos,
                                               posteam_timeouts_remaining,
                                               defteam_timeouts_remaining)
      ) %>%
      
      ## Calculating New WP
      
      # Prepping
      select(play_id,
             game,
             switched_pos,
             home_team,
             posteam = c_posteam,
             score_differential = c_score_differential,
             half_seconds_remaining,
             game_seconds_remaining,
             spread_line = c_spread_line,
             down = c_down,
             ydstogo = c_ydstogo,
             yardline_100 = c_yardline_100,
             posteam_timeouts_remaining = c_posteam_timeouts_remaining,
             defteam_timeouts_remaining = c_defteam_timeouts_remaining) %>%
      # Getting Kickoff
      left_join(kickoff_lookup, by = c('game','posteam')) %>%
      # Fixing Time
      mutate(across(c(contains('seconds')), ~ .x - 20)) %>%
      ## Calculating new WP
      nflfastR::calculate_win_probability() %>%
      
      ## Joining to get Old WP
      
      # Selecting a few
      select(play_id,
             game,
             posteam,
             switched_pos,
             wp) %>%
      left_join(clean %>% select(play_id, game, initial_wp, real_wpa),
                by = c('play_id', 'game')) %>%
      # Reversing wp
      mutate(wp = if_else(switched_pos,1 - wp, wp)) %>%
      # New WPA
      mutate(wpa = wp - initial_wp,
             wp_diff = wpa - real_wpa)
    
    fair_game %>%
      bind_rows(specials) %>%
      mutate(posteam = if_else(is.na(old_posteam), posteam, old_posteam)) %>%
      select(play_id,
             game,
             posteam,
             wp_diff) %>%
      # Game Collapse
      group_by(game, posteam) %>%
      summarize(luck_lost = sum(wp_diff)) %>%
      ungroup() %>%
      powerjoin::power_left_join(
        raw_pbp %>% select(game = game_id, posteam, defteam) %>% drop_na() %>% unique(),
        by =  c('game', 'posteam'),
        conflict = powerjoin::coalesce_xy
      )  %>%
      relocate(game, posteam, defteam, luck_lost) %>%
      
      ## Making sure both teams are there
      # This was absurdly complicated for no reason
      # If a game contained no fumbles it won't populate here
      
      group_by(game) %>%
      group_modify(~ {
        if (nrow(.x) != 2) {
          .x %>% add_row()
        } else {
          .x
        }
      }) %>%
      ungroup() %>%
      powerjoin::power_left_join(
        raw_pbp %>% select(game = game_id, posteam, defteam) %>% drop_na() %>% unique(),
        by = 'game',
        conflict = powerjoin::coalesce_xy
      ) %>% unique() %>%
      mutate(combination = glue("{posteam}_{defteam}")) %>%
      group_by(game, combination) %>%
      mutate(score = n()) %>%
      arrange(combination, score, .by_group = T) %>%
      filter(score != 2 | !is.na(luck_lost)) %>% 
      ungroup() %>%
      select(-c(score, combination)) %>%
      relocate(game, posteam, defteam, luck_lost) %>%
      
      ## Calculating luck gained 
      
      group_by(game) %>%
      mutate(luck_gained = case_when(row_number() == 1 ~ lead(luck_lost),
                                     TRUE ~ lag(luck_lost)),
             luck_gained = luck_gained * -1
      ) %>%
      ungroup() %>%
      mutate(across(c(luck_lost, luck_gained), ~ replace_na(.x, 0)),
             luck_percentage_added = luck_lost + luck_gained) %>%
      select(game, posteam, luck_percentage_added) %>%
      
      ## Negating to improve interpretation
      
      mutate(luck_percentage_added = luck_percentage_added * -1)
    
  }
  
  luck_lookup <- raw_pbp %>%
    calculate_luck() %>%
    rename(team = posteam)
  
  combined_lookup <- control_lookup %>%
    left_join(luck_lookup, by = c('game', 'team')) %>%
    mutate(luck_percentage_added = if_else(
      is.na(luck_percentage_added),
      median_wp,
      luck_percentage_added
    )) %>%
    select(season,
           week,
           game,
           team,
           median_wp,
           luck_percentage_added) %>%
    # Getting fair WP
    mutate(fair_wp = median_wp - luck_percentage_added) %>%
    select(-luck_percentage_added) %>%
    # Lagging
    group_by(team, season) %>%
    arrange(season, week, .by_group = T) %>%
    mutate(across(c(median_wp, fair_wp), ~ lag(.x))) %>%
    drop_na() %>%
    mutate(
      across(c(median_wp, fair_wp), ~ cummean(.x), .names = "straight_{.col}"),
      across(
        c(median_wp, fair_wp),
        ~ pracma::movavg(.x, n = 6, type = 'e'),
        .names = "{.col}_pracma_6"
      ),
      across(
        c(median_wp, fair_wp),
        ~ pracma::movavg(.x, n = 12, type = 'e'),
        .names = "{.col}_pracma_12"
      )
    ) %>%
    ungroup() %>%
    # Prepping
    select(game, team, contains('straight'), contains('pracma'))
  
  
  processed <- raw_pbp %>%
    select(season, week, game = game_id, posteam, home_team, result) %>%
    drop_na() %>%
    mutate(res = ifelse(home_team == posteam, result, result * -1)) %>%
    drop_na() %>%
    group_by(season, week, game, posteam) %>%
    summarize(res = first(res)) %>%
    ungroup() %>%
    rename(team = posteam) %>%
    group_by(team, season) %>%
    arrange(season, week , .by_group = T) %>%
    mutate(
      win = if_else(res > 0, 1, 0),
      cumulative_wins = cumsum(win),
      games_played = row_number()) %>%
    ungroup() %>% 
    mutate(win_pct = cumulative_wins / games_played) %>%
    select(game, team, win_pct) %>%
    left_join(combined_lookup, by = c('game', 'team'))
  
  return(processed)
  
  ## Predicting
  
  model <- read_rds('Sub Models/Model Objects/win_pct_model.rds')

  id_table <- processed %>%
    select(game, team)
  
  prediction_table <- model %>%
    predict(new_data = processed) %>%
    bind_cols(id_table)
  
  return(prediction_table)
  
}

nflreadr::load_pbp(seasons = 2022) %>%
  calculate_expected_wp() %>% 
  view()

library(glue)
library(tidyverse)
library(tidymodels)



















