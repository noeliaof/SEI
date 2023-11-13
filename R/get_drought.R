#' @title Get drought characteristics
#'
#' @description
#' Extract characteristics of droughts from a time series of values. Drought characteristics
#' include the occurrence, intensity, magnitude, and duration of the drought.
#'
#' @param x vector or xts object.
#' @param thresholds numeric vector containing thresholds to use when defining droughts.
#' @param exceed logical; TRUE if a drought is defined when \code{x} is above the thresholds, FALSE otherwise.
#' @param lag logical; TRUE if the drought should end when the value changes sign.
#'
#' @details
#' A drought is assumed to be defined as an instance when the vector \code{x} exceeds
#' (if \code{exceed = TRUE}) or falls below (if \code{exceed = FALSE}) the specified
#' thresholds in \code{thresholds}.
#'
#' \code{thresholds} can be a single value, or a vector of values. In the latter case,
#' each threshold is assumed to be a different level or intensity of the drought.
#' For example, if \code{thresholds = c(1, 1.5, 2)}, then a level 1 drought occurs
#' whenever \code{x} exceeds 1 but is lower than 1.5, a level 2 drought occurs
#' whenever \code{x} exceeds 1.5 but is lower than 2, and a level 3 drought occurs
#' whenever \code{x} exceeds 2.
#'
#' By default, \code{thresholds = c(1.28, 1.64, 1.96)}, which correspond to the
#' 90th, 95th, and 97.5th percentiles of the standard normal distribution.
#'
#' In meteorology, droughts are typically defined in terms of
#' standardised indices, such as the standardised precipitation index (SPI).
#' It is sometimes the case that a drought event ends not when the variable of
#' interest no longer exceeds (or falls below) the relevant thresholds, but rather
#' when the index changes sign. This can help to account for fluctuations around
#' the threshold values, classing it as one long drought rather than several shorter
#' droughts. This definition can be used by specifying \code{lag = TRUE}.
#'
#' \code{get_drought()} currently does not use the time series information in
#' the xts input, thereby assuming that the time series is complete, without missing
#' time periods. If \code{x} is a vector, rather than an xts object, then this
#' is also implicitly assumed.
#'
#' The output is a dataframe containing the vector \code{x}, a logical vector
#' specifying whether each value of \code{x} corresponds to a drought event,
#' and the magnitude of the drought. The magnitude of the drought is only shown
#' on the last day of the drought. This makes it easier to compute statistics about
#' the drought magnitude, such as the average drought magnitude.
#' If \code{thresholds} is a vector, the intensity or level of the drought is also returned.
#'
#' @return
#' A data frame containing the original values \code{x} and the corresponding drought characteristics.
#'
#' @references
#' Allen, S. and N. Otero (2023):
#' `Standardised indices to monitor energy droughts',
#' \emph{Renewable Energy}
#' \doi{10.1016/j.renene.2023.119206}
#'
#' McKee, T. B., Doesken, N. J., & Kleist, J. (1993):
#' `The relationship of drought frequency and duration to time scales',
#' \emph{In Proceedings of the 8th Conference on Applied Climatology} 17, 179-183.
#'
#' @author Sam Allen, Noelia Otero
#'
#' @examples
#' data(data_supply)
#' # consider hourly German energy supply data in 2019
#' supply_de <- subset(data_supply, country == "Germany", select = c("date", "PWS"))
#' supply_de <- xts::xts(supply_de$PWS, order.by = supply_de$date)
#' supply_de_std <- std_index(supply_de, timescale = "hours")
#'
#' get_drought(supply_de_std, thresholds = c(-1, -1.5, -2), exceed = FALSE)
#'
#' @name get_drought
NULL

#' @rdname get_drought
#' @export
get_drought <- function(x, thresholds = c(1.28, 1.64, 1.96), exceed = TRUE, lag = FALSE){

  if (xts::is.xts(x)) {
    x <- unname(x)
    df <- zoo::fortify.zoo(x)
  } else {
    df <- data.frame(x = x)
  }

  # intensity
  if (exceed) {
    if (length(thresholds) == 1) {
      df$occ <- as.numeric(df$x >= thresholds)
    } else {
      df$ins <- sapply(df$x, function(z) sum(z >= thresholds))
      df$occ <- as.numeric(df$ins >= 1)
    }
  }else{
    if (length(thresholds) == 1) {
      df$occ <- as.numeric(df$x <= thresholds)
    }else{
      df$ins <- sapply(df$x, function(z) sum(z <= thresholds))
      df$occ <- as.numeric(df$ins >= 1)
    }
  }

  # occurrence
  if (lag) {
    for (i in 2:nrow(df)){
      if (df$occ[i] == 0 & df$occ[i-1] == 1 & sign(df$x[i]) == sign(df$x[i - 1])) {
        df$occ[i] <- 1
      }
    }
  }

  # duration and magnitude
  mag <- abs(df$x)*(df$occ == 1)
  df['dur'] <- c(df$occ[1], numeric(nrow(df) - 1))
  df['mag'] <- c(mag[1], numeric(nrow(df) - 1))
  for (i in 2:nrow(df)) {
    if (df$occ[i]) {
      df$dur[i] <- df$dur[i - 1] + 1
      df$dur[i - 1] <- 0

      df$mag[i] <- df$mag[i - 1] + mag[i]
      df$mag[i - 1] <- 0
    }
  }

  return(df)
}

