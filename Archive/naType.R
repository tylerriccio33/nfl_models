naType <- function(x) {
  if (typeof(x) == 'character') {
    return(NA_character_)
  } else {
    return(NA_real_)
  }
}