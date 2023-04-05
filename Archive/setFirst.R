setFirst <- function(data, ...) {
  data %>%
    mutate(across(c(...), ~ if_else(case == 1, .x, naType(.x))))
  
}