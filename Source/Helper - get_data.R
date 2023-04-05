get_data <- function(seasons) {
  load_pbp(season = seasons) %>%
    as_tibble() %>%
    # Fixing team name
    mutate(across(
      c(posteam, defteam, home_team, away_team),
      ~ case_when(.x == 'OAK' ~ 'LV',
                  .x == 'SD' ~ 'LAC',
                  .x == 'STL' ~ 'LA',
                  TRUE ~ .x)
    )) %>%
    # Filters
    filter(qtr < 5,
           two_point_attempt != 1) %>%
    # Hail Mary Criteria
    filter(
      !(
        half_seconds_remaining <= 10 &
          pass_attempt == 1 &
          pass_length == "deep"
      ),
      # Last Play Nonsense Criteria
      !(
        half_seconds_remaining <= 10 &
          (lateral_reception == 1 | lateral_rush == 1)
      )
    )
}
