setLast <- function(data, ...) {
  data %>%
    mutate(across(c(...), ~ if_else(drive_LastPlay, .x, naType(.x))))
}