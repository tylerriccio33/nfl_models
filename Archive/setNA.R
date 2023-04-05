setNA <- function(data, reference, ...) {
  data %>%
    mutate(across(c(...), ~ if_else({
      {
        reference
      }
    } == 0, NA_real_, .x)))
}