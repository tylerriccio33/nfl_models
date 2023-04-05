
calculate_pythagorean_optimals <- function(offense_collapsed) {
  
  optimal_exp <- function(data, x) {
    
    theta <-
      data %>%
      drop_na({{x}}) %>%
        summarize(mean = mean({{x}}),
                  sd = sd({{x}})) %>%
        transmute(theta = sd / mean) %>%
        pull()
    
    
    2 / (theta * sqrt(pi))
    
  }
  
  # Calculate Pythagorean expected adjustment
  
  pythag_metric <- function(x, y, star) {
    
    (x ^ star) / ((x ^ star) + (y ^ star))
    
  }
  
  # Rename and Transforming Metrics
  data <- offense_collapsed %>%
    # Fixing some metrics that can't have negatives
    mutate(# Forcing negative drive points to 0
      across(
        c(points, points_allowed),
        ~ if_else(.x < 0, 0, .x)
      ),
      across(
        c(
          wepa,
          wepa_allowed
        ),
        ~ rescale(.x)
      ))
  
  # Optimal Stars
  p_star <- data %>%
    optimal_exp(points)
  e_star <- data %>%
    optimal_exp(wepa)
  
  data %>%
    mutate(
      team_pythag_points = ((points) ^ p_star)
      / ((points ^ p_star) + (points_allowed) ^ p_star
      ),
      team_pythag_epa = pythag_metric(x = wepa,
                                 y = wepa_allowed,
                                 star = e_star)
    )
    
  
}



