add_weight <- function(data, weight_grid, name) {
  
  # Initialize weights
  data <- data %>%
    mutate({{name}} := 1)
  
  for (i in 1:nrow(weight_grid)) {
    # get the current condition and weight
    condition <- weight_grid$condition[i] %>%
      parse(text = .)
    # return(condition)
    weight <- weight_grid$weight[i]
    
    # use the condition to filter rows in a
    name_symbol <- sym(name)
    data <- data %>%
      mutate({{name}} := if_else(eval(condition), weight, !!name_symbol))
  }
  
  return(data)
  
}
