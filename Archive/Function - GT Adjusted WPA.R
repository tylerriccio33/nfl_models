


calculate_gt_adjusted_stats <- function(raw_pbp, .filter = .15) {
  
  # Everything here is hard coded
  
  
  data %>%
    # Base Filters
    filter(qtr != 5,
           rush == 1 | pass == 1) %>%
    # GT Filters
    filter(wp < (1 - .filter) & wp > .filter) %>%
    # Dropping NA
    drop_na(game_id, posteam, epa, wpa) %>%
    # Grouping
    ## Order Doesn't matter
    group_by(game_id, posteam) %>%
    # Summarize
    summarize(
      gt_pass_epa = mean(epa[qb_dropback == 1]),
      gt_pass_wpa = mean(wpa[qb_dropback == 1]),
      gt_rush_epa = mean(epa[qb_dropback != 1]),
      gt_rush_wpa = mean(wpa[qb_dropback != 1]),
      gt_epa = mean(epa),
      gt_wpa = mean(wpa)
    ) %>%
    ungroup()
              
  
  
}

