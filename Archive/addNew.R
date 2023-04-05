addNew <- function(data, ref, ...) {
  defuse <- rlang::enquo(ref)
  defuse <- quo_name(defuse)
  
  data %>%
    mutate(across(c(...), ~ if_else({
      {
        ref
      }
    } == 1, .x, NA_real_), .names = "{defuse}_{.col}"))
  
}