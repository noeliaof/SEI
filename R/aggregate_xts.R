#' @title Aggregate values in xts objects
#'
#' @description Inputs an xts time series and outputs an xts time series whose
#' values have been aggregated over a moving window of a user-specified length.
#'
#'
#' @param x xts object to be aggregated.
#' @param agg_period length of the aggregation period.
#' @param agg_scale timescale of \code{agg_period};
#'  one of `'mins'`, `'hours'`, `'days'`, `'weeks'`, `'months'`, `'years'`.
#' @param agg_fun string specifying the function used to aggregate the data over the
#'  aggregation period, default is `'sum'`.
#' @param timescale timescale of the data; `'mins'`, `'hours'`, `'days'`, `'weeks'`, `'months'`, `'years'`.
#' @param na_thres threshold for the percentage of NA values allowed in the
#'  aggregation period; default is 10.
#'
#'
#' @details
#' This has been adapted from code available at
#' \url{https://github.com/WillemMaetens/standaRdized}.
#'
#' \code{agg_period} is a single numeric value specifying over how many time units the
#' data \code{x} is to be aggregated. By default, \code{agg_period} is assumed to correspond
#' to a number of days, but this can also be specified manually using the argument
#' \code{agg_scale}.
#'
#' \code{agg_fun} determines the function used to aggregate the time series. By default,
#' \code{agg_fun = "sum"}, meaning the aggregation results in accumulations over the
#' aggregation period. Alternative functions can also be used. For example, specifying
#' \code{agg_fun = "mean"} would return the mean over the aggregation period.
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
#'
#' @return
#' An xts time series with aggregated values.
#'
#'
#' @author Sam Allen, Noelia Otero
#'
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
#' supply_de_daily <- aggregate_xts(supply_de, agg_period = 1, timescale = "hours")
#'
#' # weekly means
#' supply_de_weekly <- aggregate_xts(supply_de, agg_period = 1, agg_scale = "weeks",
#'                                   agg_fun = "mean", timescale = "hours")
#'
#' plot(supply_de, main = "Hourly energy production in Germany")
#' plot(supply_de_daily, main = "Daily accumulated energy production in Germany")
#' plot(supply_de_weekly, main = "Weekly averaged energy production in Germany")
#'
#' }
#'
#' @name aggregate_xts
NULL

#' @rdname aggregate_xts
#' @export
aggregate_xts <- function(x,
                          agg_period,
                          agg_scale = c('days', 'mins', 'hours', 'weeks', 'months', 'years'),
                          agg_fun = 'sum',
                          timescale = c('days', 'mins', 'hours', 'weeks', 'months', 'years'),
                          na_thres = 10) {
  agg_scale <- match.arg(agg_scale)
  timescale <- match.arg(timescale)
  x_agg <- sapply(zoo::index(x), aggregate_xts_1, x, agg_period, agg_scale, agg_fun, timescale, na_thres)
  x_agg <- xts::xts(x_agg, order.by = zoo::index(x))
  xts::xtsAttributes(x_agg) <- xts::xtsAttributes(x)
  xts::xtsAttributes(x_agg)$agg_length <- paste(agg_period, agg_scale)
  return(x_agg)
}


aggregate_xts_1 <- function(date,
                            data,
                            len,
                            scale,
                            fun = 'sum',
                            timescale,
                            na_thres = 10) {

  by <- paste0("-", len, " ", scale)
  from <- seq(date, length = 2, by = by)[2]
  from <- seq(from, length = 2, by = paste("+1", timescale))[2]
  to <- date
  data <- data[paste(format(from, "%Y-%m-%d %H:%M:%S"), format(to, "%Y-%m-%d %H:%M:%S"), sep = '/')]
  dates <- seq(from = from, to = to, by = paste("1", timescale))
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

