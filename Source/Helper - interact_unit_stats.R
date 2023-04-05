interact_metrics <- function(data, ..., mode) {
  
  if(!mode %in% c('team','unit')) stop('Mode must be in `team` or `unit`')
  
  # Extract selections
  selections <- select(data, ...) %>%
    colnames()
  
  # Unit Interactions
  # Using addition [Base + Opponent = Interaction]
  if(mode == 'unit') {
    
    for(i in 1:length(selections)) {
      selection_string <- selections[i]
      base_sym <- sym(selection_string)
      opponent_sym <- sym(glue("{selection_string}_opponent"))
      # Mutate
      data <- data %>%
        transmutate("{selection_string}" := {{base_sym}} + {{opponent_sym}})
    }
    
  }
  
  # Team Interactions
  # Using subtraction [Base - Opponent = Interaction]
  if(mode == 'team') {
    
    for(i in 1:length(selections)) {
      selection_string <- selections[i]
      base_sym <- sym(selection_string)
      opponent_sym <- sym(glue("{selection_string}_opponent"))
      # Mutate
      data <- data %>%
        transmutate("{selection_string}" := {{base_sym}} - {{opponent_sym}})
    }
    
    
  }

  return(data)
  
}
