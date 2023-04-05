
calculate_adjusted_drive_points <- function(raw_pbp, collapse = 'game') {
  
  # Collapse must be in ('drive','game')
  
  drive_collapsed <- raw_pbp %>%
    select(game_id, season, week, drive_points = fixed_drive_result, posteam, fixed_drive, ep) %>%
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
    drop_na() %>%
    # Drive collapsing
    group_by(game_id, season, week, posteam, fixed_drive) %>%
    summarize(real_drive_points =  sum(last(real_drive_points)),
              adj_drive_points = sum(last(adj_drive_points))) %>%
    ungroup()
  
  if(collapse == 'drive') return(drive_collapsed)
  
  # If not drive collapsed return game collapsed
  
  drive_collapsed %>%
    group_by(game_id, season, week, posteam) %>%
    summarize(
      drive_points = sum(real_drive_points),
      adj_drive_points = sum(adj_drive_points)
    ) %>%
    ungroup() %>%
    # Reverse
    group_by(game_id) %>%
    group_modify( ~ {
      drive_points_vector <- .x$drive_points
      adj_drive_points_vector <- .x$adj_drive_points
      .x %>%
        mutate(
          opposing_drive_points = if_else(
            row_number() == 1,
            drive_points_vector[2],
            drive_points_vector[1]
          ),
          opposing_adj_drive_points = if_else(
            row_number() == 1,
            adj_drive_points_vector[2],
            adj_drive_points_vector[1]
          )
        )
    }) %>%
    ungroup() %>%
    # Getting Result
    mutate(drive_points_result = drive_points - opposing_drive_points,
           adj_drive_points_result = adj_drive_points - opposing_adj_drive_points)
  
}
