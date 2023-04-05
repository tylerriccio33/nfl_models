calculate_drive_points_oe <- function(data = data){
  drive_points_oe <- data %>%
    dtplyr::lazy_dt() %>%
    mutate(drive_end_transition = str_to_lower(drive_end_transition)) %>%
    # Filtering
    filter(
      # Two point attempts
      two_point_attempt == 0,
      !drive_end_transition %in% c(
        'end of game',
        'fumble',
        'end_game',
        'end_half',
        'end of half',
        'unknown',
        'fumble_safety',
        'fumble, safety'
      ),!is.na(drive_end_transition),
      rush == 1 | qb_dropback == 1,
    ) %>%
    select(game = game_id,
           drive,
           drive_end_transition,
           ep) %>%
    group_by(game, drive) %>%
    summarize(drive_end_transition = first(drive_end_transition),
              ep = first(ep)) %>%
    ungroup() %>%
    as_tibble() %>%
    # Joining to adjusted drive points
    left_join(
      data %>%
        calculate_adjusted_drive_points() %>%
        rename(game = game_id),
      by = c('game', 'drive')
    ) %>%
    # Drive Points over expected
    mutate(drive_points_oe = adj_drive_points - ep) %>%
    select(id_game = game, drive, adj_drive_points, drive_points_oe)
  
  return(drive_points_oe)
  
}
