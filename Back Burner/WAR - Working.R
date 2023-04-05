
# Libraries
library(tidyverse)
library(rlang)
library(glue)
library(nflreadr)
library(gt)
library(gtExtras)
library(nflWAR)

# Data

new_data <- nflreadr::load_pbp(2021) %>%
  as_tibble() %>%
  # Setting Filters
  filter(week <= 17,
         play == 1,
         two_point_attempt == 0,
         extra_point_attempt == 0,
         aborted_play == 0,
         !str_detect(desc,'No Play'))

old_data <- get_pbp_data(2017)


t <- new_data %>%
  format_new_war_data() %>%
  mutate(Date = lubridate::as_date(Date)) %>%
  # Most of these are playoff games but some aren't
  anti_join(old_data, by = c('Date','play_id')) %>%
  relocate(desc) %>%
  arrange(Date)

t %>%
  tabulaR::check_na() %>%
  view()


if(F) {
  
old_data <-get_pbp_data(2017)

old_data %>%
  colnames() %>%
  as_tibble_col() %>%
  view()

old_data %>%
  colnames() %>%
  writeClipboard()
}

# Renaming and calculating new data

reworked <- new_data %>%
  select(
    Date = game_date,
    GameID = game_id,
    play_id,
    Drive = drive,
    qtr,
    down,
    time,
    TimeUnder = half_seconds_remaining,
    TimeSecs = game_seconds_remaining,
    yrdline100 = yardline_100,
    ydstogo,
    ydsnet,
    GoalToGo = goal_to_go,
    FirstDown = first_down,
    posteam,
    DefensiveTeam = defteam,
    desc,
    Yards.Gained = yards_gained,
    sp,
    Touchdown = touchdown,
    ExPointResult = extra_point_result,
    TwoPointConv = two_point_attempt,
    DefTwoPoint = defensive_two_point_attempt,
    safety,
    PuntResult = punt_blocked, # This might be in the wrong format
    PlayType = play_type,
    Passer = passer_player_name,
    Passer_ID = passer_player_id,
    PassAttempt = pass_attempt,
    PassOutcome = complete_pass, # This might be in the wrong format
    PassLength = pass_length,
    AirYards = air_yards,
    YardsAfterCatch = yards_after_catch,
    QBHit = qb_hit,
    PassLocation = pass_location,
    InterceptionThrown = interception,
    Interceptor = interception_player_id,
    Rusher = rusher_player_name,
    Rusher_ID = rusher_player_id	,
    RushAttempt = rush_attempt,
    RunLocation = run_location,
    RunGap = run_gap,
    Receiver = receiver_player_name,
    Receiver_ID = receiver_player_id,
    Reception = complete_pass, # Using discretion on this since no field is mapped
    BlockingPlayer = blocked_player_name,
    FieldGoalResult = field_goal_result,
    FieldGoalDistance = kick_distance,
    Fumble = fumble,
    RecFumbTeam = fumble_recovery_1_team,
    RecFumbPlayer = fumble_recovery_1_player_id	,
    Sack = sack,
    Challenge.Replay = replay_or_challenge,
    ChalReplayResult = replay_or_challenge_result	,
    PenalizedTeam = penalty_team,
    PenaltyType = penalty_type,
    PenalizedPlayer = penalty_player_name	,
    Penalty.Yards = penalty_yards,
    PosTeamScore = posteam_score,
    DefTeamScore = defteam_score,
    ScoreDiff = score_differential,
    HomeTeam = home_team,
    AwayTeam = away_team,
    Timeout_Indicator = timeout,
    Timeout_Team = timeout_team,
    HomeTimeouts_Remaining_Post = home_timeouts_remaining,
    AwayTimeouts_Remaining_Post = away_timeouts_remaining,
    No_Score_Prob = no_score_prob,
    Opp_Field_Goal_Prob = opp_fg_prob,
    Opp_Safety_Prob = opp_safety_prob,
    Opp_Touchdown_Prob = opp_td_prob,
    Field_Goal_Prob = fg_prob,
    Safety_Prob = safety_prob,
    Touchdown_Prob = td_prob,
    ExPoint_Prob = extra_point_prob,
    TwoPoint_Prob = two_point_conversion_prob,
    ExpPts = ep,
    EPA = epa,
    airEPA = air_epa,
    yacEPA = yac_epa,
    Home_WP_pre = home_wp,
    Away_WP_pre = away_wp,
    Home_WP_post = home_wp_post,
    Away_WP_post = away_wp_post,
    Win_Prob = wp,
    WPA = wpa,
    airWPA = air_wpa,
    yacWPA = yac_wpa,
    Season = season
  ) %>%
  # Adding PlayAttempted
  mutate(PlayAttempted = 1) %>%
  # Fixing PlayType
  mutate(PlayType = str_to_title(PlayType))
  # Filtering
  drop_na(yrdline100) # Looks like these were dropped

## Need to figure these out
# PlayTimeDiff
# SideofField
# yrdln
# Onsidekick
# ReturnResult
# Returner
# Tackler1
# Tackler2
# Accepted.Penalty
# AbsScoreDiff
# posteam_timeouts_pre
# HomeTimeouts_Remaining_Pre
# AwayTimeouts_Remaining_Pre

# Replacement Functions
league_replacement_functions <-
  list(
    "find_replacement_QB" = create_percentage_replacement_fn("Perc_Total_Plays", .1),
    "find_replacement_RB_rec" = create_league_replacement_fn(3, "RB", "Targets"),
    "find_replacement_WR_rec" = create_league_replacement_fn(4, "WR", "Targets"),
    "find_replacement_TE_rec" = create_league_replacement_fn(2, "TE", "Targets"),
    "find_replacement_RB_rush" = create_league_replacement_fn(3, "RB",
                                                              "Rush_Attempts"),
    "find_replacement_WR_TE_rush" = create_league_replacement_fn(1, "WR",
                                                                 "Rush_Attempts",
                                                                 combine_wrte = 1))
# Formulas

# Create the win probability based model formulas:
wp_model_formula_list <-
  list(
    "air_formula" = as.formula(
      airWPA_Result ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + QBHit +
        Receiver_Position + PassLocation + Rush_EPA_Att +
        (1 |
           Passer_ID_Name) + (1 | Receiver_ID_Name) + (1 | DefensiveTeam)
    ),
    "yac_formula" = as.formula(
      yacWPA_Result ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + QBHit +
        AirYards * Receiver_Position + PassLocation + Rush_EPA_Att +
        (1 |
           Passer_ID_Name) + (1 | Receiver_ID_Name) + (1 | DefensiveTeam)
    ),
    "qb_rush_formula" = as.formula(
      WPA ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + Pass_EPA_Att +
        (1 |
           Rusher_ID_Name) + (1 | DefensiveTeam)
    ),
    "main_rush_formula" = as.formula(
      WPA ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind +
        Rusher_Position + Pass_EPA_Att +
        (1 |
           Team_Side_Gap) + (1 |
                               Rusher_ID_Name) + (1 | DefensiveTeam)
    )
  )


new_add_positions <- function(pbp_df, years) {
  # Check to see if any of the plays are in 2016,
  # if so then change JAC to JAX for both sides of the ball:
  if (any(pbp_df$Season == 2016)) {
    pbp_df$posteam[which(pbp_df$posteam == "JAC" &
                           pbp_df$Season == 2016)] <- "JAX"
    pbp_df$DefensiveTeam[which(pbp_df$DefensiveTeam == "JAC" &
                                 pbp_df$Season == 2016)] <- "JAX"
  }
  
  # Define a function to find the common name for a player:
  find_player_name <- function(player_names) {
    if (length(player_names) == 0) {
      result <- "None"
    } else{
      table_name <- table(player_names)
      result <- names(table_name)[which.max(table_name)]
    }
    return(result)
  }
  
  # Create a table of passer names:
  passer_names <- pbp_df %>% dplyr::group_by(Passer_ID) %>%
    dplyr::summarise(Passer_Name = find_player_name(Passer[which(!is.na(Passer))])) %>%
    dplyr::ungroup()
  
  # Receiver names:
  receiver_names <- pbp_df %>% dplyr::group_by(Receiver_ID) %>%
    dplyr::summarise(Receiver_Name = find_player_name(Receiver[which(!is.na(Receiver))])) %>%
    dplyr::ungroup()
  
  # Rusher names:
  rusher_names <- pbp_df %>% dplyr::group_by(Rusher_ID) %>%
    dplyr::summarise(Rusher_Name = find_player_name(Rusher[which(!is.na(Rusher))])) %>%
    dplyr::ungroup()
  
  # Left join these columns:
  pbp_df <-
    pbp_df %>% dplyr::left_join(passer_names, by = "Passer_ID") %>%
    dplyr::left_join(receiver_names, by = "Receiver_ID") %>%
    dplyr::left_join(rusher_names, by = "Rusher_ID")
  
  # Create Passer_ID_Name and Receiver_ID_Name columns joining the two together:
  pbp_df <-
    pbp_df %>% dplyr::mutate(
      Passer_ID_Name = paste(Passer_Name, Passer_ID, sep = "-"),
      Receiver_ID_Name = paste(Receiver_Name, Receiver_ID, sep =
                                 "-"),
      Rusher_ID_Name = paste(Rusher_Name, Rusher_ID, sep =
                               "-")
    )
  
  # Include sacks in rushes and populate the Rusher_Name and Rusher_ID_Name
  # with the Passer fields for sacks:
  pbp_df$Rusher_ID <- ifelse(pbp_df$PlayType == "Sack",
                             pbp_df$Passer_ID,
                             pbp_df$Rusher_ID)
  pbp_df$Rusher_Name <- ifelse(pbp_df$PlayType == "Sack",
                               pbp_df$Passer_Name,
                               pbp_df$Rusher_Name)
  pbp_df$Rusher <- ifelse(pbp_df$PlayType == "Sack",
                          pbp_df$Passer,
                          pbp_df$Rusher)
  pbp_df$Rusher_ID_Name <- ifelse(pbp_df$PlayType == "Sack",
                                  pbp_df$Passer_ID_Name,
                                  pbp_df$Rusher_ID_Name)
  
  # Create a data frame with the rosters for the given years
  # (which are already filtered down to only the offense skill positions),
  # selecting only the position and ID columns:
  player_positions <- load_rosters(seasons = years) %>%
    select(GSIS_ID = gsis_id, Pos = position) %>%
    unique() %>%
    drop_na()
  
  # return(player_positions)
  
  # Make three versions of the rosters for each type of player:
  passer_pos <-
    player_positions %>% dplyr::rename(Passer_Position = Pos)
  receiver_pos <-
    player_positions %>% dplyr::rename(Receiver_Position = Pos)
  rusher_pos <-
    player_positions %>% dplyr::rename(Rusher_Position = Pos)
  
  # Left join the position columns based on the respective ID columns:
  pbp_df <- pbp_df %>%
    dplyr::left_join(passer_pos,
                     by = c("Passer_ID" = "GSIS_ID")) %>%
    dplyr::left_join(receiver_pos,
                     by = c("Receiver_ID" = "GSIS_ID")) %>%
    dplyr::left_join(rusher_pos,
                     by = c("Rusher_ID" = "GSIS_ID"))
  
  
  # Drop the unnecessary columns and return:
  pbp_df %>%
    dplyr::select(-Passer_Name,-Receiver_Name,-Rusher_Name) %>%
    distinct() %>%
    return
}

new_prepare_model_data <- function(pbp_df) {
  # Create datasets that are only passing plays and rushing plays
  # with EPA calculations:
  pass_pbp_df <- pbp_df %>% dplyr::filter(
    PlayType == "Pass",!is.na(airEPA_Result),!is.na(airWPA_Result),!is.na(yacEPA_Result),!is.na(yacWPA_Result),!is.na(PassLocation),!is.na(Receiver_Position),!is.na(Passer_Position),!is.na(Passer_ID_Name),!is.na(Receiver_ID_Name),
    Receiver_ID != "None",
    Passer_ID != "None",
    Passer_Position == "QB",
    Receiver_Position != "QB"
  )
  
  rush_pbp_df <-
    pbp_df %>% dplyr::filter(
      PlayType %in% c("Run", "Sack"),!is.na(EPA),!is.na(WPA),!is.na(Team_Side_Gap),!is.na(Rusher_Position),!is.na(Rusher_ID_Name),
      Rusher_ID != "None"
    )
  
  
  # For each of these datasets, group by the posteam to calculate both
  # passing and rushing EPA and WPA per attempt variables:
  
  team_passing <- pass_pbp_df %>%
    dplyr::group_by(posteam) %>%
    dplyr::summarise(
      Pass_EPA = sum(EPA, na.rm = TRUE),
      Pass_WPA = sum(WPA, na.rm = TRUE),
      Pass_Attempts = n(),
      Pass_EPA_Att = Pass_EPA / Pass_Attempts,
      Pass_WPA_Att = Pass_WPA / Pass_Attempts
    )
  
  team_rushing <- rush_pbp_df %>%
    dplyr::group_by(posteam) %>%
    dplyr::summarise(
      Rush_EPA = sum(EPA, na.rm = TRUE),
      Rush_WPA = sum(WPA, na.rm = TRUE),
      Rush_Attempts = n(),
      Rush_EPA_Att = Rush_EPA / Rush_Attempts,
      Rush_WPA_Att = Rush_WPA / Rush_Attempts
    )
  
  # Left join these to the pbp_dfs:
  
  pass_pbp_df <- pass_pbp_df %>%
    dplyr::left_join(team_passing, by = "posteam") %>%
    dplyr::left_join(team_rushing, by = "posteam")
  
  rush_pbp_df <- rush_pbp_df %>%
    dplyr::left_join(team_passing, by = "posteam") %>%
    dplyr::left_join(team_rushing, by = "posteam")
  
  return(list("pass_model_df" = pass_pbp_df,
              "rush_model_df" = rush_pbp_df))
}


# Comparing Old and New

new_code <- reworked %>%
  # No filters
  new_add_positions(2021) %>%
  add_model_variables()
  new_prepare_model_data() 

old_code <- old_data %>%
  add_positions(2017) %>%
  add_model_variables()
  prepare_model_data() 



  old_code %>% dplyr::filter(PlayType == "Pass",
                         !is.na(airEPA_Result),
                         !is.na(airWPA_Result),
                         !is.na(yacEPA_Result),
                         !is.na(yacWPA_Result),
                         !is.na(PassLocation),
                         !is.na(Receiver_Position),
                         !is.na(Passer_Position),
                         !is.na(Passer_ID_Name),
                         !is.na(Receiver_ID_Name),
                         Receiver_ID != "None",
                         Passer_ID != "None",
                         Passer_Position == "QB",
                         Receiver_Position != "QB")


# Finding plays in the new that's not in the old

new_code$pass_model_df %>%
  mutate(Date = lubridate::as_date(Date)) %>%
  # Most of these are playoff games but some aren't
  anti_join(old_code$pass_model_df, by = c('Date','play_id')) %>%
  relocate(desc) %>%
  arrange(Date) %>%
  view()
  

  ep_model_formula_list <- list("air_formula" = as.formula(airEPA_Result ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + QBHit + 
                                                             Receiver_Position + PassLocation + Rush_EPA_Att +
                                                             (1|Passer_ID_Name) + (1|Receiver_ID_Name) + (1|DefensiveTeam)),
                                "yac_formula" = as.formula(yacEPA_Result ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + QBHit + 
                                                             AirYards*Receiver_Position + PassLocation + Rush_EPA_Att +
                                                             (1|Passer_ID_Name) + (1|Receiver_ID_Name) + (1|DefensiveTeam)),
                                "qb_rush_formula" = as.formula(EPA ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + Pass_EPA_Att +
                                                                 (1|Rusher_ID_Name) + (1|DefensiveTeam)),
                                "main_rush_formula" = as.formula(EPA ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + 
                                                                   Rusher_Position + Pass_EPA_Att +
                                                                   (1|Team_Side_Gap) + (1|Rusher_ID_Name) + (1|DefensiveTeam)))

# Running Code

t <- reworked %>%
  new_add_positions(2021) %>%
  add_model_variables() %>%
  new_prepare_model_data() %>%
  add_position_tables() %>%
  join_position_statistics() %>%
  find_positional_replacement_level(league_replacement_functions) %>%
  estimate_player_value_added(ep_model_formula_list) %>%
  calculate_above_replacement() %>%
  convert_prob_to_wins()
  
  
  t$RB_table
  
  mod <- lme4::lmer(
    EPA ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind +
      Rusher_Position + Pass_EPA_Att +
      (1 |
         Team_Side_Gap) + (1 | Rusher_ID_Name) + (1 | DefensiveTeam),
  data = t$rush_model_df
  ) %>% 
    lme4::ranef()
  
  mod %>%
    as_tibble() %>%
    view()
  
  
  t$rush_model_df %>%
    select(contains('WPA'))
  
  

comparison <- reworked %>%
  add_positions(2017) %>%
  add_model_variables() %>%
  prepare_model_data() %>%
  add_position_tables() %>%
  join_position_statistics() %>%
  find_positional_replacement_level(league_replacement_functions) %>%
  estimate_player_value_added(wp_model_formula_list) %>%
  calculate_above_replacement()
  convert_prob_to_wins()


res <-   t %>%
  enframe() %>%
  filter(str_detect(name, 'table$')) %>%
  mutate(value = map(value, ~ {
    .x %>%
      select(Player_ID_Name, contains('war'))
  })) %>%
  unnest(value) %>%
  mutate(across(where(is.numeric), ~ round(.x, digits = 5))) %>%
  arrange(-total_WAR)

compare_res <-   comparison %>%
  enframe() %>%
  filter(str_detect(name, 'table$')) %>%
  mutate(value = map(value, ~ {
    .x %>%
      select(Player_ID_Name, contains('war'))
  })) %>%
  unnest(value) %>%
  mutate(across(where(is.numeric), ~ round(.x, digits = 5))) %>%
  arrange(-total_WAR)

res %>%
  select(-name) %>%
  gt() %>%
  gt_theme_538()


    
    
    
    
    


