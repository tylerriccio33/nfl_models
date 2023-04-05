

adjust_scores <- function(data, corrected_res){
  
  calculate_penalty_points <- function(seasons) {

    # Read in the final tuned marks
    dput_final_marks <- structure(
      list(
        type = c(
          "Chop Block",
          "Defensive 12 On-field",
          "Defensive Delay of Game",
          "Defensive Offside",
          "Defensive Pass Interference",
          "Face Mask",
          "Illegal Bat",
          "Illegal Blindside Block",
          "Illegal Forward Pass",
          "Illegal Shift",
          "Intentional Grounding",
          "Offensive 12 On-field",
          "Personal Foul",
          "Unnecessary Roughness",
          "Unsportsmanlike Conduct"
        ),
        factor = c(1, 1, 1, 0.5, 0.5, 1, 1, 0.5, 1, 0.5, 1, 1, 1,
                   1, 0.5)
      ),
      row.names = c(NA,-15L),
      class = c("tbl_df", "tbl",
                "data.frame")
    )
    
    # Calculate Full Penalty EPA
    raw_penalty_data <- calculate_penalty_epa(.seasons = seasons) %>%
      as_tibble() %>%
      select(game_id,
             posteam,
             defteam,
             penalty_type,
             penalty_epa) %>%
      # Filter the types here
      filter(penalty_type %in% dput_final_marks$type) %>%
      # Mutate Factors here
      left_join(dput_final_marks,
                by = c('penalty_type' = 'type')) %>%
      mutate(penalty_epa = penalty_epa * factor) %>%
      select(-c(factor, penalty_type)) %>%
      group_by(game_id, posteam, defteam) %>%
      summarize(penalty_epa = sum(penalty_epa)) %>%
      ungroup()
    
    return(raw_penalty_data)
    
    
  }
  
  seasons <- unique(data$season)
  
  # Get Penalty Lookup
  penalty_lookup <- calculate_penalty_points(seasons = seasons)

  
  corrected_res %>%
    # Penalty Lookup
    left_join(penalty_lookup, by = c('game_id', 'posteam', 'defteam')) %>%
    # Fixing missing epa
    mutate(across(c(penalty_epa), ~ if_else(is.na(.x), 0 , .x))) %>%
    # Drop existing result and drive points allowed
    select(-c(adj_drive_points_allowed, res)) %>%
    # Calculate new adjusted drive points
    mutate(adj_drive_points = adj_drive_points - penalty_epa) %>%
    # Remove fumble and penalty epa
    select(-c(penalty_epa)) %>%
    # Reverse drive points
    group_by(game_id, posteam, defteam) %>%
    reverse_stat_dynamic(adj_drive_points) %>%
    ungroup() %>%
    # Calculate new result
    mutate(res = adj_drive_points - adj_drive_points_allowed)
  
  
}
