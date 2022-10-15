#' @title Calculation of main features of droughts within the context of renewable energy power systems.
#' 
#' @description
#' Given a time series of the Standardized energy index obtained with \code{funSDEI}, 
#' the function \code{def_energy_drought} returns drought characteristics, including:
#  Occurrence ('occ' = 0, 1)
#  Intensity ('ins' = 0, 1, 2, 3, i.e. mild, moderate, severe, extreme)
#  Magnitude ('mag')
#  Duration ('dur')

#' @param dd dataframe that contains
#' @param mvar variable used to characterise the drought (e.g., PWS or the name of the variable used containing the index values)
#' @param threshval threshold values to use when defining droughts
#' @param higher is TRUE if the value is above the threshold, and FALSE otherwise
#' @param lag to account for the change of sign when examining the occurrence of the drought
#' @references
#' Sam Allen and Noelia Otero. 2022. Standardised indices to monitor energy droughts.
#' @export
#'


def_energy_drought <- function(dd, mvar, threshval, higher = T, lag = T){

  # occurrence and intensity
  if(higher){
    if(length(threshval) == 1){
      dd$ins <- as.numeric(dd[[mvar]] >= threshval)
    }else{
      dd$ins <- sapply(1:nrow(dd), function(i){sum(dd[[mvar]][i] >= threshval)})
    }
  }else{
    if(length(threshval) == 1){
      dd$ins <- as.numeric(dd[[mvar]] <= threshval)
    }else{
      dd$ins <- sapply(1:nrow(dd), function(i){sum(dd[[mvar]][i] <= threshval)})
    }
  }
  
  dd$occ <- as.numeric(dd$ins >= 1)
  
  if(lag){
    for(i in 2:nrow(dd)){
      if(dd$occ[i] == 0 & dd$occ[i-1] == 1 & sign(dd[[mvar]][i]) == sign(dd[[mvar]][i-1])){
        dd$occ[i] <- 1
      } 
    }
  }
  
  # duration and magnitude
  mag <- abs(dd[[mvar]])*(dd$occ == 1)
  dd['dur'] <- c(dd$occ[1], numeric(nrow(dd) - 1))
  dd['mag'] <- c(mag[1], numeric(nrow(dd) - 1))
  for(i in 2:nrow(dd)){
    if(dd$occ[i]){
      dd$dur[i] <- dd$dur[i-1] + 1
      dd$dur[i-1] <- 0
      
      dd$mag[i] <- dd$mag[i-1] + mag[i]
      dd$mag[i-1] <- 0
    }
  }
  
  dd <- dd[,c("date", mvar, "ins", "occ", "dur", "mag")]
  
  return(dd)
}
