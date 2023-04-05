

reverse_feature <- function(data, ...) {
  
  if(length(group_vars(data)) != 0) stop('Ungroup data before calling.')
  
  reversed <- data %>%
    select(id_game, id_posteam, ...) %>%
    # Rename id_posteam to id_defteam
    rename(id_defteam = id_posteam) %>%
    # Rename `...` to allowed
    rename_with(~ glue("{.x}_allowed"), .cols = -starts_with('id_'))
  
  # Join back to original
  
  re_joined <- data %>%
    left_join(reversed, by = c('id_game','id_defteam'))
  
  return(re_joined)
  
}

