#' Function to get abbreviations of country names
#' @param countries string with the names of the countries
#' @export
get_abbrevs <- function(countries){
  abbrevs <- sapply(seq_along(countries), function(i) substr(countries[i], 1, 3))
  
  if("Slovakia" %in% countries & "Slovenia" %in% countries){
    abbrevs[countries == "Slovakia"] <- "Sva"; abbrevs[countries == "Slovenia"] <- "Sve"
  }
  if("Czech_Republic" %in% countries){
    abbrevs[countries == "Czech_Republic"] <- "CR"
  }
  if("United_Kingdom" %in% countries){
    abbrevs[countries == "United_Kingdom"] <- "UK"
  }
  
  return(abbrevs)
}

