weight_combine <- function(data, ..., name, outcome = res) {

  pls_object <- data %>%
    select(res = {{outcome}}, ...) %>%
    recipe(res ~., data = .) %>%
    # Omit NA
    step_naomit(all_numeric_predictors()) %>%
    # Partial Least Squares to Single Component
    step_pls(all_numeric_predictors(),
             outcome = 'res',
             num_comp = 1) %>%
    # Prep
    prep()
    
  baked_data <- pls_object %>%
    bake(new_data = data) %>%
    select(-res) %>%
    bind_cols(data %>% select(-c(...))) %>%
    relocate({{name}} := PLS1, .after = last_col())
  
  return(baked_data)
  
}































