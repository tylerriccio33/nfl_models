
library(nflWAR)
library(tidyverse)

league_replacement_functions <-
  list("find_replacement_WR_rec" = create_league_replacement_fn(4, "WR", "Targets"))

# Create the expected points based modula formulas:
ep_model_formula_list <-
  list(
    "air_formula" = as.formula(
      airEPA_Result ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + QBHit +
        Receiver_Position + PassLocation + Rush_EPA_Att +
        (1 |
           Passer_ID_Name) + (1 | Receiver_ID_Name) + (1 | DefensiveTeam)
    ),
    "yac_formula" = as.formula(
      yacEPA_Result ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + QBHit +
        AirYards * Receiver_Position + PassLocation + Rush_EPA_Att +
        (1 |
           Passer_ID_Name) + (1 | Receiver_ID_Name) + (1 | DefensiveTeam)
    ))


old_data <- get_pbp_data(2018)

# First WPA based WAR:
walk(c(2022:2022), function(x) {
  season_results <- x %>% 
    get_pbp_data() %>%
    add_positions(x) %>%
    add_model_variables() %>%
    prepare_model_data() %>%
    add_position_tables() %>%
    join_position_statistics() %>%
    find_positional_replacement_level(league_replacement_functions) %>%
    estimate_player_value_added(wp_model_formula_list) %>%
    calculate_above_replacement() %>%
    convert_prob_to_wins()
  
  saveRDS(season_results, file = paste("wpa_model_results_", as.character(x), ".rds", sep = ""))
})



# Create simulation results for each year with the appropriate
# pipeline that relies on the already found replacement level
# players, doing so for the WPA based model (and other typical
# statistics):

walk(c(2017:2017), function(x) {
  # Load the stored season results (modify for your file path)
  season_results <- readRDS(paste("wpa_model_results_", as.character(x), ".rds", sep = ""))
  
  # Create the pipeline expression to get the results in a simulation by resampling
  # at the drive level:
  generate_war_results <- . %>%
    resample_season(drive_level = 1) %>%
    prepare_model_data() %>%
    add_position_tables() %>%
    add_replacement_level_sim(season_results) %>%
    join_position_statistics() %>%
    estimate_player_value_added(wp_model_formula_list, return_models = 0) %>%
    calculate_above_replacement() %>%
    convert_prob_to_wins()
  
  # Simulate the results:
  sim_results <- x %>%
    get_pbp_data() %>%
    add_positions(x) %>%
    add_model_variables() %>%
    simulate_season_statistics(2, generate_war_results) %>%
    combine_simulations()
  
  # Save 
  saveRDS(sim_results, file = paste("wpa_model_play_sim_results_", as.character(x), ".rds", sep = ""))
  print(paste("Finished simulation for year ", as.character(x), sep = ""))
})

res <- read_rds('wpa_model_play_sim_results_2017.rds')

res %>%
  enframe() %>%
  group_by(name) %>%
  unnest(cols = value) %>%
  select(Player_ID_Name, contains('WAR')) %>%
  unique() %>%
  mutate(across(c(contains('WAR')), ~ round(.x, digits = 5))) %>% view()










