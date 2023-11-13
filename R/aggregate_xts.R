#' @title Aggregate values in xts objects
#'
#' @description Inputs an xts time series and outputs an xts time series whose
#' values have been aggregated over a moving window of a user-specified length.
#'
#' @param x xts object to be aggregated.
#' @param len length of the aggregation period.
#' @param scale timescale of the aggregation period, default is 'days'.
#' @param fun function to apply to the aggregated data, default is 'sum'.
#' @param timescale timescale of \code{x}, default is 'days'.
#' @param na_thres threshold for the percentage of NA values allowed in the
#'  aggregation period, default = 10.
#'
#' @details
#' This has been adapted from code available at
#' \url{https://github.com/WillemMaetens/standaRdized}.
#'
#' \code{len} is a single numeric value specifying over how many time units the
#' data \code{x} is to be aggregated. By default, \code{len} is assumed to correspond
#' to a number of days, but this can also be specified manually using the argument
#' \code{scale}. \code{scale} must be one of: "days", "weeks", "months", "quarters", and "years".
#'
#' \code{fun} determines the function used to aggregate the time series. By default,
#' \code{fun = "sum"}, meaning the aggregation results in accumulations over the
#' aggregation period. Alternative functions can also be used. For example, specifying
#' \code{fun = "mean"} would return the mean over the aggregation period.
#'
#' \code{timescale} is the timescale of the input data \code{x}. By default, this
#' is assumed to be "days".
#'
#' Since the time series \code{x} aggregates data over the aggregation period, problems
#' may arise when \code{x} contains missing values. For example, if interest is
#' on daily accumulations, but 50% of the values in the aggregation period are missing,
#' the accumulation over this aggregation period will not be accurate.
#' This can be controlled using the argument \code{na_thres}.
#' \code{na_thres} specifies the percentage of NA values in the aggregation period
#' before a NA value is returned. i.e. the proportion of values that are allowed to be missing.
#' The default is \code{na_thres = 10}.
#'
#' @return
#' An xts time series with aggregated values.
#'
#' @author Sam Allen, Noelia Otero
#'
#' @examples
#' \donttest{
#'
#' data(data_supply, package = "SEI")
#' # consider hourly German energy production data in 2019
#' supply_de <- subset(data_supply, country == "Germany", select = c("date", "PWS"))
#' supply_de <- xts::xts(supply_de$PWS, order.by = supply_de$date)
#'
#' # daily accumulations
#' supply_de_daily <- aggregate_xts(supply_de, len = 1, timescale = "hours")
#'
#' # weekly means
#' supply_de_weekly <- aggregate_xts(supply_de, len = 1, scale = "weeks", fun = "mean", "hours")
#'
#' plot(supply_de, main = "Hourly energy production in Germany")
#' plot(supply_de_daily, main = "Daily energy production in Germany")
#' plot(supply_de_weekly, main = "Weekly energy production in Germany")
#'
#' }
#'
#' @name aggregate_xts
NULL

#' @rdname aggregate_xts
#' @export
aggregate_xts <- function(x,
                          len,
                          scale = c("days", "hours", "weeks", "quarters", "years"),
                          fun = 'sum',
                          timescale = c("days", "hours", "weeks", "quarters", "years"),
                          na_thres = 10) {
  scale <- match.arg(scale)
  timescale <- match.arg(timescale)
  x_agg <- sapply(zoo::index(x), aggregate_xts_1, x, len, scale, fun, timescale, na_thres)
  x_agg <- xts::xts(x_agg, order.by = zoo::index(x))
  xts::xtsAttributes(x_agg) <- xts::xtsAttributes(x)
  xts::xtsAttributes(x_agg)$agg_length <- as.difftime(len, units = scale)
  return(x_agg)
}


aggregate_xts_1 <- function(date,
                            data,
                            len,
                            scale,
                            fun = 'sum',
                            timescale,
                            na_thres = 10) {
  from <- date - as.difftime(len, units = scale) + as.difftime(1, units = timescale)
  to <- date
  data <- data[paste(from, to, sep = '/')]
  dates <- seq(from = from, to = to, by = as.difftime(1, units = timescale))
  x <- c(zoo::coredata(merge(xts::xts(order.by = dates), data, join = 'left')))

  if (length(x) != 0) {
    pct.na <- (length(which(is.na(x))) / length(x)) * 100
    if (pct.na <= na_thres) {
      x <- x[!is.na(x)]
      x <- do.call(what = fun, args = list(x = x)) # apply aggregation function
      return(x)
    } else {
      return(as.numeric(NA))
    }
  } else {
    return(as.numeric(NA))
  }
}

