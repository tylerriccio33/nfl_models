
# Only need column of spread and moneyline
# Model first appends missing moneyline data
# Then it predicts on this data to get linear spread

add_vegas_odds <- function(data, linear_mod, moneyline_mod){

  
  # Calculating X Moneyline
  moneyline_data <- moneyline_mod %>%
    predict(data) %>%
    as_tibble_col(column_name = 'x_moneyline')
  
  dat1 <- data %>%
    # Binding to moneyline
    bind_cols(moneyline_data) %>%
    # Coalescing to fill in NA moneylines
    mutate(moneyline = coalesce(moneyline, x_moneyline))
  
  # Calculating Linear Spread
  # Uses the filled in moneyline data
  linear_spread_data <- linear_mod %>%
    predict(dat1) %>%
    as_tibble_col(column_name = 'linear_spread')
  
  dat1 %>%
    bind_cols(linear_spread_data)
    
}
