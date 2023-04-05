

library(nflreadr)
library(tidyverse)
library(tidymodels)
library(lubridate)

data <- load_schedules() %>%
  select(
    gameday,
    game_id,
    season,
    posteam = home_team,
    defteam = away_team,
    roof,
    surface,
    temp,
    wind,
    stadium_id,
    stadium
  ) %>%
  as_tibble() %>%
  filter(roof == 'outdoors') %>%
  drop_na(temp)

model_data <- data %>%
  select(game_id, season, gameday, stadium_id, temp)%>%
  # Game day as YMD date
  mutate(gameday = ymd(gameday)) %>%
  # Current Season Stadium Numbers
  group_by(stadium_id, season) %>%
  arrange(gameday, .by_group = T) %>%
  mutate(
    last_temp = lag(temp),
    time_from_last_temp = time_length(gameday - lag(gameday)),
    temp_differential = lag(temp, n =2) - last_temp) %>%
  replace_na(list(temp_differential = 0)) %>%
  ungroup()

rec_data <- model_data %>%
  drop_na() %>%
  recipe(temp ~ ., data = .) %>%
  update_role(game_id, stadium_id, season, new_role = 'id') %>%
  step_date(gameday, features = c('month','doy','week')) %>%
  prep() %>%
  juice() %>%
  select(-c(game_id, stadium_id, season))

rand_forest(mtry = 4) %>%
  set_mode('regression') %>%
  fit(temp ~ ., rec_data) %>%
  predict(new_data = rec_data) %>%
  bind_cols(rec_data) %>%
  mutate(resid = temp - .pred) %>%
  ggplot(aes(temp, .pred)) +
  geom_point(alpha = 1/4) +
  geom_smooth() +
  facet_wrap( ~ gameday_month)

workflow() %>%
  add_recipe(rec) %>%
  add_model(linear_reg() %>%
              set_engine('glm') %>%
              set_mode('regression')) %>%
  fit(temp ~ ., data = model_data)

















