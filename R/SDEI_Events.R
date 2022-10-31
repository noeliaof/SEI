#' Function to calculate drought events based on the SEI
#' @param values index value
#' @param type ?
#' @export
SDEI_Events <- function(values, type, thresholds = c(1, 1.5, 2)) {
  values <- as.numeric(values)
  sei.extremely.high   <- sum(values >= thresholds[3], na.rm=T)
  sei.severely.high    <- sum(values < thresholds[3] & values >= thresholds[2], na.rm=T)
  sei.moderately.high  <- sum(values < thresholds[2] & values >= thresholds[1], na.rm=T)
  sei.near.normal      <- sum(abs(values) < thresholds[1], na.rm=T)
  sei.moderately.low   <- sum(values <= -thresholds[1] & values > -thresholds[2], na.rm=T)
  sei.severely.low     <- sum(values <= -thresholds[2] & values > -thresholds[3], na.rm=T)
  sei.extremely.low    <- sum(values <= -thresholds[3], na.rm=T)
  r <- c(sei.extremely.high, sei.severely.high, sei.moderately.high, sei.near.normal, sei.moderately.low, sei.severely.low, sei.extremely.low)
  names(r) = c("Extremely high", "Severely high", "Moderately high", "Near normal", "Moderately low", "Severely low", "Extremely low")
  if (sti.near.normal == 0) {
    return (NA)
  } else {
    return (r)
  }
}
