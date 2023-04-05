
add_st_epa <- function(raw_pbp){
  
  calculate_kickoff_epa <- function(raw_pbp) {
    
    raw_pbp %>%
      lazy_dt() %>%
      filter(special_teams_play == 1,
             play_type == 'kickoff',
             fumble != 1,
             penalty != 1) %>%
      select(game_id, posteam, defteam, epa, desc) %>%
      mutate(onside = str_detect(desc, 'onside')) %>%
      filter(!onside) %>%
      group_by(game_id, posteam, defteam) %>%
      summarize(kickoff_epa = mean(epa)) %>%
      ungroup()
    
    
  }
  
  calculate_punt_epa <- function(raw_pbp) {
    
    raw_pbp %>%
      lazy_dt() %>%
      filter(special_teams_play == 1,
             play_type == 'punt',
             fumble != 1,
             penalty != 1) %>%
      select(game_id, posteam, defteam, epa, desc) %>%
      mutate(blocked = str_detect(desc,'BLOCKED')) %>%
      filter(!blocked) %>%
      group_by(game_id, posteam, defteam) %>%
      summarize(punt_epa = mean(epa)) %>%
      ungroup()
    
    
  }
  
  kickoff_dt <- raw_pbp %>%
    calculate_kickoff_epa()
  
  punt_dt <- raw_pbp %>% 
    calculate_punt_epa()
  
  kickoff_dt %>%
    left_join(punt_dt, by = c('game_id','posteam', 'defteam')) %>%
    as_tibble()
  
}
