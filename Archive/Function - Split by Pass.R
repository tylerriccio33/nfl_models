
### Data must be...
# 1. Grouped by game_id, week, posteam, defteam, pass
# 2. Summarized by those groups using my_summarize

split_by_pass <- function(game_summarized_data) {
  
  game_summarized_data %>%
    ungroup() %>%
    # Pivoting Pass
    # Don't group by pass
    group_by(game_id, week, posteam, defteam) %>%
    group_modify(~ {
      
      pass_data <- .x %>%
        filter(pass == 1) %>%
        select(-pass) %>%
        rename_with( ~ glue("pass_{.x}"), .cols = everything())
      
      .x %>%
        filter(pass == 0) %>%
        select(-pass) %>%
        rename_with(~ glue("rush_{.x}"), .cols = everything()) %>%
        bind_cols(pass_data)
      
    })
  
  
}




