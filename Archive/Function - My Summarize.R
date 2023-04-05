
### Data must be...
# 1. Grouped by game_id, week[optional], posteam, defteam

my_summarize <- function(data, ..., .fn = mean) {
  
  # Setting Mean to na.rm = T
  
  mean <- function(x, ..., na.rm = TRUE) {
    base::mean(x, ..., na.rm = na.rm)
  }
  
  expr_list <- exprs(...)
  
  char_list <- expr_list %>%
    as.character()
  
  data_tibble <- data %>%
    select(group_vars(data)) %>%
    ungroup() %>%
    unique()
  
  for(i in 1:length(char_list)) {
    
    temp_var <- expr_list[[i]]
    
    temp_data <- data %>%
      summarize("{char_list[[i]]}" := match.fun(.fn)({{temp_var}})) %>%
      ungroup() %>%
      select({{temp_var}})
    
    data_tibble <- data_tibble %>%
      bind_cols(temp_data) 
    
  }
  
  # return(group_vars(data))
  
  return(data_tibble %>%
           group_by(across(group_vars(data)))
  )
  
}

