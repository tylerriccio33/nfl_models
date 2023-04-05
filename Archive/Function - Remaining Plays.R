
add_expected_plays <- function(raw_pbp) {
  
  clean <- raw_pbp %>%
    
    # Calculate id for reverse lookup after predictions
    
    mutate(my_id = row_number()) %>%
    
    mutate(
      qtr = case_when(
        qtr == 1 ~ ms('45,0'),
        qtr == 2 ~ ms('30,0'),
        qtr == 3 ~ ms('15,0'),
        TRUE ~ ms('0,0')
      ),
      time = ms(time) + qtr,
      time = seconds(time) %>% base::as.numeric(),
      receive_2h_ko = dplyr::if_else(.data$qtr <= 2 &
                                       .data$posteam == dplyr::first(stats::na.omit(.data$defteam)),
                                     1,
                                     0)
    ) %>% 
    ungroup() %>%
    ## Selecting Down
    select(
      # IDs
      game_id,
      # Timeouts
      posteam_timeouts_remaining,
      defteam_timeouts_remaining,
      # Time
      time,
      half_seconds_remaining,
      quarter_seconds_remaining,
      # 2HKO
      receive_2h_ko,
      # Probabilities
      xpass,
      td_prob,
      opp_td_prob,
      fg_prob,
      opp_fg_prob,
      no_score_prob,
      wp
    ) %>% 
    # drop_na() %>%
    ## Calculating Play Number and Remaining
    group_by(game_id) %>%
    mutate(play = row_number(),
           remaining_plays = max(play) - play) %>%
    ungroup() %>%
    # Selecting out ID and relocating Res
    select(-c(game_id, play)) %>%
    relocate(res = remaining_plays)
  
  # Model
  
  remaining_plays_model <- readRDS(file = "model_remaining_plays.rds")
 
  predict(object = remaining_plays_model, new_data = clean) %>%
    bind_cols(raw_pbp) %>%
    relocate(x_remaining_plays = .pred, .after = last_col())
  
}












