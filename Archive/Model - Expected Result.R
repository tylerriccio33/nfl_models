
calculate_expected_results <- function(raw_pbp, model) {
  
  pivoted <- raw_pbp %>%
    dtplyr::lazy_dt() %>%
    select(game = game_id,
           result,
           home_team,
           posteam,
           defteam,
           wp) %>%
    drop_na() %>%
    mutate(play = row_number()) %>%
    pivot_longer(c(posteam,defteam), names_to = 'type', values_to = 'team') %>%
    mutate(wp = if_else(type == 'posteam', wp, 1 - wp),
           result = ifelse(home_team == team, result, result * -1),
           result = if_else(result > 0, 1, 0)) %>%
    group_by(game, team) %>%
    summarize(median_wp = median(wp),
              res = first(result)) %>%
    ungroup() %>%
    as_tibble()
  
pivoted %>%
  # Using augment to keep residual as variable
  predict(model, ., type = 'response') %>%
  as_tibble() %>%
  bind_cols(pivoted) %>%
  rename(win_odds = value) %>%
  mutate(unexpected_res = case_when(win_odds >= .5 & res == 0 ~ 1,
                                    win_odds <= .5 & res == 1 ~ 1,
                                    TRUE ~ 0),
         residual_win_odds = abs(res - win_odds)) %>%
  # Clean up
  select(game, posteam = team, unexpected_res, win_odds, residual_win_odds)
}
