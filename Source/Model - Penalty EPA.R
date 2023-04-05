
calculate_penalty_epa <- function(.seasons = seasons) {
  
  
  data <- load_pbp(seasons = .seasons) %>%
    as_tibble() %>%
    dplyr::select(
      game_id,
      play_id,
      season,
      week,
      home_team,
      away_team,
      roof,
      half_seconds_remaining,
      yardline_100,
      ydstogo,
      posteam,
      defteam,
      desc,
      play_type,
      qb_dropback,
      qb_kneel,
      qb_spike,
      rush_attempt,
      pass_attempt,
      ep,
      epa,
      contains("penalty"),
      yards_gained,
      down,
      posteam_timeouts_remaining,
      defteam_timeouts_remaining,
      -c(penalty_player_id, penalty_player_name, first_down_penalty)
    ) %>%
    # change penalty yards to negative yardage if it is on the offense
    dplyr::mutate(penalty_yards = ifelse(penalty_team == posteam, -penalty_yards, penalty_yards))
  
  # find all penalties
  all_penalties <- data %>% 
    dplyr::filter(!is.na(epa), !is.na(ep), !is.na(posteam), penalty == 1, !is.na(down),
                  !play_type %in% c("punt", "field_goal", "kickoff", "extra_point")) %>% 
    # trying to fix NAs in penalty_type
    dplyr::mutate(penalty_type = dplyr::case_when(
      is.na(penalty_type) & stringr::str_detect(desc, "False Start") ~ "False Start",
      is.na(penalty_type) & stringr::str_detect(desc, "Delay of Game") ~ "Delay of Game",
      is.na(penalty_type) & stringr::str_detect(desc, "Face Mask|Personal Foul|Horse Collar") ~ "Personal Foul",
      is.na(penalty_type) & stringr::str_detect(desc, "Defensive Holding|Defensive Pass Interference|Illegal Contact") ~ "Defensive Pass Interference",
      is.na(penalty_type) & stringr::str_detect(desc, "Offensive Holding") ~ "Offensive Holding",
      is.na(penalty_type) & stringr::str_detect(desc, "Roughing the Passer") ~ "Roughing the Passer",
      is.na(penalty_type) & stringr::str_detect(desc, "Defensive 12 On-field|Defensive Too Many Men on Field") ~ "Defensive Too Many Men on Field",
      is.na(penalty_type) & stringr::str_detect(desc, "Offensive 12 On-field|Offensive Too Many Men on Field") ~ "Offensive Too Many Men on Field",
      is.na(penalty_type) & stringr::str_detect(desc, "Encroachment|Defensive Offside|Neutral Zone Infraction") ~ "Defensive Offside",
      is.na(penalty_type) & stringr::str_detect(desc, "Illegal Formation|Illegal Shift|Illegal Motion") ~ "Illegal Shift",
      is.na(penalty_type) & stringr::str_detect(desc, "Illegal Use of Hands") ~ "Illegal Use of Hands",
      is.na(penalty_type) & stringr::str_detect(desc, "Unnecessary Roughness|Lowering the Head to Initiate Contact") ~ "Unnecessary Roughness",
      is.na(penalty_type) & stringr::str_detect(desc, "Unsportsmanlike Conduct|Taunting") ~ "Unsportsmanlike Conduct",
      is.na(penalty_type) & stringr::str_detect(desc, "Chop Block") ~ "Chop Block",
      is.na(penalty_type) & !stringr::str_detect(desc, "INTERCEPTED") & stringr::str_detect(desc, "Illegal Blindside Block") ~ "Illegal Blindside Block",
      is.na(penalty_type) & stringr::str_detect(desc, "Tripping") ~ "Tripping",
      TRUE ~ penalty_type)) %>% 
    # binning penalties together
    dplyr::mutate(penalty_type = dplyr::case_when(
      penalty_type %in% c("Encroachment", "Defensive Offside", "Neutral Zone Infraction") ~ "Defensive Offside",
      penalty_type %in% c("Illegal Shift", "Illegal Motion", "Illegal Formation", "Illegal Procedure") ~ "Illegal Shift",
      penalty_type %in% c("Illegal Contact", "Defensive Holding", "Defensive Pass Interference") ~ "Defensive Pass Interference",
      penalty_type %in% c("Horse Collar Tackle", "Horse Collar", "Personal Foul", "Clipping") ~ "Personal Foul",
      penalty_type %in% c("Unsportsmanlike Conduct", "Taunting") ~ "Unsportsmanlike Conduct",
      penalty_type %in% c("Offensive Offside", "False Start") ~ "False Start",
      penalty_type %in% c("Low Block", "Illegal Peelback", "Illegal Crackback") ~ "Illegal Crackback",
      penalty_type %in% c("Unnecessary Roughness", "Lowering the Head to Initiate Contact") ~ "Unnecessary Roughness",
      TRUE ~ penalty_type))
  
  # Penalty EPA methodology
  # initialize pre-snap penalty types
  pre_snap_penalties <- c("Delay of Game", "False Start")
  # calculate expected points if the play had no penalty, and compare to actual expected points
  # to find `penalty_epa`
  all_penalties_epa <- all_penalties %>% 
    dplyr::select(-ep, -epa) %>% 
    dplyr::bind_cols(all_penalties %>% 
                       dplyr::rename_with(.cols = c(half_seconds_remaining, down, ydstogo, posteam_timeouts_remaining,
                                                    defteam_timeouts_remaining, yardline_100, ep, epa),
                                          ~ paste0("orig_", .)) %>% 
                       dplyr::select(dplyr::starts_with("orig_"))) %>% 
    dplyr::mutate(yards_gained_pre = as.integer(stringr::str_remove(stringr::str_extract(desc, " for \\d+| for -\\d+"), " for ")),
                  # extract yards gained if there was no penalty
                  yards_gained_pre = ifelse(is.na(yards_gained_pre), 0, yards_gained_pre),
                  # if actual yards gained does not equal if there was no penalty (like offensive holding 
                  # down the field spot foul), change yards gained to what it would have been without the foul
                  yards_gained = ifelse(yards_gained != yards_gained_pre & !stringr::str_detect(desc, "REVERSED"), yards_gained_pre, yards_gained),
                  # turnovers
                  turnover = ifelse(orig_down == 4 & yards_gained < orig_ydstogo, 1, 0),
                  turnover = ifelse(stringr::str_detect(desc, "INTERCEPTED") & !stringr::str_detect(desc, "REVERSED"), 1, 0),
                  fumble = ifelse(stringr::str_detect(desc, "FUMBLES"), 1, 0),
                  rec_team = str_sub(stringr::str_extract(desc, "RECOVERED by [[:upper:]]{2,3}|recovered by [[:upper:]]{2,3}"), start = 14, end = 17),
                  rec_team = ifelse(stringr::str_detect(desc, "and recovers"), posteam, rec_team),
                  rec_team = ifelse(stringr::str_detect(desc, "out of bounds"), posteam, rec_team),
                  turnover = ifelse(turnover == 1 | fumble == 1 & rec_team == defteam, 1, 0),
                  yardline_100 = ifelse(turnover == 1, orig_yardline_100, orig_yardline_100 - yards_gained),
                  down = ifelse(yards_gained >= orig_ydstogo, 1, orig_down + 1),
                  ydstogo = ifelse(yards_gained >= orig_ydstogo, 10, orig_ydstogo - yards_gained),
                  # possession change if 4th down failed
                  down = ifelse(turnover == 1, as.integer(1), as.integer(down)),
                  ydstogo = ifelse(turnover == 1, as.integer(10), as.integer(ydstogo)),
                  # flip yardline_100 and timeouts for turnovers
                  yardline_100 = ifelse(turnover == 1, as.integer(100 - yardline_100), as.integer(yardline_100)),
                  # posteam = ifelse(turnover == 1, defteam, posteam),
                  posteam_timeouts_remaining = ifelse(turnover == 1,
                                                      orig_defteam_timeouts_remaining,
                                                      orig_posteam_timeouts_remaining),
                  defteam_timeouts_remaining = ifelse(turnover == 1,
                                                      orig_posteam_timeouts_remaining,
                                                      orig_defteam_timeouts_remaining),
                  # ydstogo can't be bigger than yardline
                  ydstogo = ifelse(ydstogo >= yardline_100, as.integer(yardline_100), as.integer(ydstogo)),
                  half_seconds_remaining = orig_half_seconds_remaining - 5.065401) %>% 
    nflfastR::calculate_expected_points() %>% 
    dplyr::mutate(epa = ifelse(turnover == 1, -ep - orig_ep, ep - orig_ep),
                  penalty_epa = orig_epa - epa) %>% 
    dplyr::select(play_id, game_id, season, week, home_team, away_team, posteam, defteam, roof, desc, play_type,
                  yards_gained, dplyr::starts_with("orig_"), dplyr::contains("penalty")) %>% 
    dplyr::rename_with(.cols = dplyr::starts_with("orig_"), ~ stringr::str_remove(., "orig_")) %>% 
    # pre-snap penalties are equal to original epa
    dplyr::mutate(penalty_epa = ifelse(penalty_type %in% pre_snap_penalties, epa, penalty_epa),
                  penalty_epa = ifelse(stringr::str_detect(desc, "field goal|TWO-POINT CONVERSION ATTEMPT|punts"), 0, penalty_epa),
                  penalty_epa = ifelse(is.na(penalty_epa), 0, penalty_epa),
                  no_penalty_epa = epa - penalty_epa)
  
  # Return it here for the adjusted penalty scores
  return(all_penalties_epa)
  
  rm(all_penalties)
  
  all_pen <- all_penalties_epa %>% 
    dplyr::distinct(penalty_type) %>% 
    dplyr::filter(!is.na(penalty_type)) %>% 
    dplyr::pull(penalty_type)
  # join penalty epa
  with_penalties <- data %>% 
    dplyr::select(-penalty_type) %>% 
    dplyr::left_join(all_penalties_epa %>% 
                       dplyr::select(play_id, game_id, season, week, 
                                     posteam, defteam, desc, penalty_type, penalty_epa, no_penalty_epa),
                     by = c("play_id", "game_id", "season", "week", "posteam", "defteam", "desc")) %>% 
    # make NAs 0 (no penalty on play)
    dplyr::mutate(penalty_epa = ifelse(is.na(penalty_epa), 0, penalty_epa),
                  no_penalty_epa = epa - penalty_epa,
                  # change penalties to qb dropback plays when a pass or sack happens
                  qb_dropback = ifelse(stringr::str_detect(desc, " pass | sacked ") & penalty_type %in% all_pen, 1, qb_dropback),
                  # change certain penalty types to qb dropbacks always
                  qb_dropback = ifelse(penalty_type %in% c("Offensive Pass Interference", "Roughing the Passer", "Ineligible Downfield Pass", "Illegal Touch Pass", "Illegal Forward Pass", "Intentional Grounding"), 1, qb_dropback))
  # remove unnecessary data
  rm(data)
  
  # function to summarize pbp data into epa data by team, week
  summarise_epa_data <- function(pbp_off, pbp_def, metric_off = epa, metric_def = epa) {
    # Offense EPA
    epa_data <- pbp_off %>%
      dplyr::filter(!is.na(epa), !is.na(ep), !is.na(posteam),
                    play_type == "pass" | play_type == "run" | penalty == 1, qb_kneel != 1) %>%
      dplyr::group_by(game_id, season, week, posteam, home_team) %>%
      dplyr::summarise(off_epa = mean({{ metric_off }}),
                       off_pass_epa = mean({{ metric_off }}[qb_dropback == 1]),
                       off_rush_epa = mean({{ metric_off }}[qb_dropback == 0]),
                       .groups = "drop") %>%
      # Defense EPA
      dplyr::left_join(pbp_def %>%
                         filter(!is.na(epa), !is.na(ep), !is.na(posteam), 
                                play_type == "pass" | play_type == "run" | penalty == 1, qb_kneel != 1) %>%
                         dplyr::group_by(game_id, season, week, defteam, away_team) %>%
                         dplyr::summarise(def_epa = mean({{ metric_def }}),
                                          def_pass_epa = mean({{ metric_def }}[qb_dropback == 1]),
                                          def_rush_epa = mean({{ metric_def }}[qb_dropback == 0]),
                                          .groups = "drop"),
                       by = c("game_id", "posteam" = "defteam", "season", "week")) %>%
      dplyr::mutate(opponent = ifelse(posteam == home_team, away_team, home_team)) %>%
      dplyr::select(
        game_id,
        season,
        week,
        home_team,
        away_team,
        posteam,
        opponent,
        off_epa,
        off_pass_epa,
        off_rush_epa,
        def_epa,
        def_pass_epa,
        def_rush_epa
      ) %>% 
      # Not sure why, but there is one instance where the posteam = ""
      dplyr::filter(posteam != "")
  }
  
  # baseline data (penalties included and un-altered)
  epa_data <- summarise_epa_data(pbp_off = with_penalties, pbp_def = with_penalties)

  # completely 'penalty-free' world
  penalty_free <-
    summarise_epa_data(
      pbp_off = with_penalties,
      pbp_def = with_penalties,
      metric_off = no_penalty_epa,
      metric_def = no_penalty_epa
    )
  
  
  # # penalties that will be accounted for on offense
  off_penalties <-
    c(
      "False Start",
      "Offensive Holding",
      "Defensive Offside",
      "Defensive Pass Interference",
      "Defensive Too Many Men on Field"
    )
  # # penalties that will be accounted for on defense
  def_penalties <-
    c("Defensive Pass Interference",
      "Defensive Offside",
      "Unnecessary Roughness")

  final <- summarise_epa_data(
    pbp_off = with_penalties %>%
      dplyr::mutate(
        no_penalty_epa = dplyr::case_when(
          penalty == 0 ~ epa,
          penalty_type %in% off_penalties ~ epa,
          TRUE ~ 0.5 * no_penalty_epa
        )
      ),
    pbp_def = with_penalties %>%
      dplyr::mutate(
        no_penalty_epa = dplyr::case_when(
          penalty == 0 ~ epa,
          penalty_type %in% def_penalties ~ epa,
          TRUE ~ 0.5 * no_penalty_epa
        )
      ),
    metric_off = no_penalty_epa,
    metric_def = no_penalty_epa
  ) %>%
    select(game_id, posteam, contains('epa'), -contains('def')) %>%
    # Rename defensive stats to allowed
    rename_with( ~ str_replace(.x, 'off_', '')) %>%
    rename_with(~ glue("adjusted_{.x}"), .cols = contains('epa'))
  
  return(final)
  
}












