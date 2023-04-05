interact_team_stats <- function(data, ...) {
  
  selections <- c(...)
  
  for (i in 1:length(selections)) {
    
    name_one <- glue("{selections[i]}_1") %>%
      as.character() %>%
      sym()
    name_two <- glue("{selections[i]}_2") %>%
      as.character() %>%
      sym()
    
    data <- data %>%
      transmutate("{selections[i]}_offset" := 
                    {{name_one}} - {{name_two}})
    
  }
  
  return(data)
  
}





