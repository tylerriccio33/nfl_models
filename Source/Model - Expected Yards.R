get_expected_yards <- function(raw_pbp, model) {
  raw_pbp %>%
    dtplyr::lazy_dt() %>%
    filter(
      # Two point attempts
      two_point_attempt == 0,
      drive_end_transition != 'END_GAME',
      drive_end_transition != 'END_HALF',
      drive_end_transition != 'FUMBLE',
      drive_end_transition != 'INTERCEPTION',
      rush == 1 | qb_dropback == 1,
    ) %>%
    select(
      game = game_id,
      season,
      week,
      posteam,
      drive,
      game_seconds_remaining,
      yards = yards_gained,
      yards_left = yardline_100,
      drive_end_transition
    ) %>%
    drop_na() %>%
    group_by(season, week, game, posteam, drive) %>%
    summarize(
      time = first(game_seconds_remaining),
      initial_yards_left = first(yards_left),
      yards_gained = sum(yards),
      drive_end_transition = first(drive_end_transition)
    ) %>%
    ungroup() %>%
    # Fixing drives where penalty sets them back
    # These are a minority but it improves model utility
    mutate(
      yards_gained = if_else(
        drive_end_transition == 'TOUCHDOWN',
        initial_yards_left,
        yards_gained
      )
    ) %>%
    as_tibble() %>%
    # Predict
    # Augment for residual
    broom::augment(x = model, new_data = .) %>%
    # Selecting out
    select(
      season,
      week,
      game,
      posteam,
      drive,
      drive_yards = yards_gained,
      x_drive_yards = .pred,
      yards_gained_oe = .resid)
}






