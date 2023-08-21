#' @title Get drought characteristics
#'
#' @description
#' Extract characteristics of droughts from a time series of standardised index values, including:
#  Occurrence ('occ' = 0, 1)
#  Intensity ('ins')
#  Magnitude ('mag')
#  Duration ('dur')
#'
#' @param x xts object to be plotted.
#' @param thresholds threshold values to use when defining droughts
#' @param higher is TRUE if the value is above the threshold, and FALSE otherwise
#' @param lag to account for the change of sign when examining the occurrence of the drought
#'
#' @details
#' Details of the get_drought function will be added here.
#'
#' @return
#' An xts object containing the drought characteristics corresponding to the input xts.
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
#' options(xts_check_TZ = FALSE)
#' supply_de_std <- std_index(supply_de, timescale = "hours")
#'
#' get_drought(supply_de_std, thresholds = c(-1, -1.5, -2), higher = FALSE)
#'
#' @name get_drought
NULL

#' @rdname get_drought
#' @export
get_drought <- function(x, thresholds = c(1.28, 1.64, 1.96), higher = TRUE, lag = FALSE){

  df <- zoo::fortify.zoo(x)

  # intensity
  if (higher) {
    if (length(thresholds) == 1) {
      df$ins <- as.numeric(df$x >= thresholds)
    } else {
      df$ins <- sapply(df$x, function(z) sum(z >= thresholds))
    }
  }else{
    if (length(thresholds) == 1) {
      df$ins <- as.numeric(df$x <= thresholds)
    }else{
      df$ins <- sapply(df$x, function(z) sum(z <= thresholds))
    }
  }

  # occurrence
  df$occ <- as.numeric(df$ins >= 1)

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

