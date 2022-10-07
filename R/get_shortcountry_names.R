#' Function to obtain the ISO names of EU countries
#' @param longname one single string with the name of one country
#' @export
#' 
get_shortcountry_names <- function(longname){
  
  if (longname == "Austria") shortname <- "AT"
  if (longname == "Albania") shortname <- "AL"
  if (longname == "Belgium") shortname <- "BE"
  if (longname == "Bulgaria") shortname <- "BG"
  if (longname == "Switzerland") shortname <- "CH"
  if (longname == "Cyprus") shortname <- "CY"
  if (longname == "Czech_Republic") shortname <- "CZ"
  if (longname == "Germany") shortname <- "DE"
  if (longname == "Denmark") shortname <- "DK"
  if (longname == "Estonia") shortname <- "EE"
  if (longname == "Greece") shortname <- "EL"
  if (longname == "Spain") shortname <- "ES"
  if (longname == "Finland") shortname <- "FI"
  if (longname == "France") shortname <- "FR"
  if (longname == "Croatia") shortname <- "HR"
  if (longname == "Hungary") shortname <- "HU"
  if (longname == "Ireland") shortname <- "IE"
  if (longname == "Iceland") shortname <- "IS"
  if (longname == "Italy") shortname <- "IT"
  if (longname == "Liechtenstein") shortname <- "LI"
  if (longname == "Latvia") shortname <- "LV"
  if (longname == "Lithuania") shortname <- "LT"
  if (longname == "Luxembourg") shortname <- "LU"
  if (longname == "Montenegro") shortname <- "ME"
  if (longname == "Macedonia") shortname <- "MK"
  if (longname == "Malta") shortname <- "MT"
  if (longname == "Netherlands") shortname <- "NL"
  if (longname == "Norway") shortname <- "NO"
  if (longname == "Poland") shortname <- "PL"
  if (longname == "Portugal") shortname <- "PT"
  if (longname == "Romania") shortname <- "RO"
  if (longname == "Serbia") shortname <- "RS"
  if (longname == "Slovenia") shortname <- "SI"
  if (longname == "Sweden") shortname <- "SE"
  if (longname == "Slovakia") shortname <- "SK"
  if (longname == "Turkey") shortname <- "TR"
  if (longname == "United_Kingdom") shortname <- "UK"
  
  return(shortname)
}