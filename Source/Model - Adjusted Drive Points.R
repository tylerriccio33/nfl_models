calculate_adjusted_drive_points <- function(raw_pbp) {
  raw_pbp %>%
    dtplyr::lazy_dt() %>%
    # Filters
    filter(
      # Two point attempts
      two_point_attempt == 0,
      rush == 1 | qb_dropback == 1,
      !drive_end_transition %in% c('END_GAME',
                                        'END_HALF',
                                        'FUMBLE')) %>%
    # Clean up
    select(game_id, drive_points = fixed_drive_result, posteam, drive, ep) %>%
    # Real and adjusted drive points
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
    # Dropping NA
    drop_na() %>%
    # Drive collapsing
    group_by(game_id, posteam, drive) %>%
    summarize(real_drive_points =  sum(last(real_drive_points)),
              adj_drive_points = sum(last(adj_drive_points))) %>%
    ungroup() %>%
    as_tibble()

}
