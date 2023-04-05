
calculate_above_average <- function(raw_pbp, stat, .fn){
  
  # Stat as Enquo
  # .fn as Enquo
  
  # Wrangle Game Team Data
  
  game_team_data <- raw_pbp %>%
    select(game = game_id, week, posteam, {{stat}}) %>%
    # Dropping NA
    drop_na() %>%
    # Summarize
    group_by(game, week, posteam) %>%
    summarize({{stat}} := match.fun(.fn)({{stat}})) %>%
    ungroup()
  
  # Wrangle Weekly Data
  
  weekly_data <- data %>%
    select(game = game_id, week, posteam, {{stat}}) %>%
    drop_na() %>%
    # Summarize W/Week Included
    group_by(game, week, posteam) %>%
    summarize({{stat}} := match.fun(.fn)({{stat}})) %>%
    ungroup() %>%
    # Summarize by Week
    group_by(week) %>%
    summarize("weekly_{{stat}}" := mean({{stat}})) %>%
    ungroup()
  
  # Joining to weekly
  
  joined <- game_team_data %>%
    left_join(weekly_data, by = 'week')
  
  # Calculating Over Average

    name <- raw_pbp %>%
      select({{stat}}) %>%
      colnames()
    
    name <- glue::glue("weekly_{name}") %>%
      sym()
    
  joined %>%
    mutate("{{stat}}_oa" :=  {{stat}} - {{name}}) %>%
    select(-{{name}}, -week)

}

multiple_above_average_wrapper <- function(raw_pbp, ..., wrapper_fn = mean) {
  
  # Stat List as Expressions
  
  stat_list <- exprs(...)
  
  # Stat Strings as Characters
  
  stat_strings <- stat_list %>%
    as.character()
  
  # Loop
  
  my_tibble <- tibble()
  
  for (i in 1:length(stat_strings)) {
    temp_tibble <- raw_pbp %>%
      calculate_above_average(stat = !!stat_list[[i]], .fn = wrapper_fn)
    
    if (i == 1) {
      my_tibble <- temp_tibble
      
    } else {
      my_tibble <- my_tibble %>%
        # I'm joining instead of just binding
        # This should catch cases where vector lengths aren't equal
        # I'm not sure why they wouldn't be but I drop NA so ?
        left_join(temp_tibble, by = c('game', 'posteam'))
      
      
    }
    
  }
  
  return(my_tibble)
  
}

