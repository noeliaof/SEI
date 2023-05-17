#' @title Aggregate values in xts objects
#'
#' @description Inputs an xts time series and outputs an xts time series whose
#' values have been aggregated over a moving window of a user-specified length.
#'
#' @param x xts object to be aggregated.
#' @param agg_length length of the aggregation period.
#' @param agg_scale timescale of the aggregation period, default is 'days'.
#' @param agg_fun function to apply to the aggregated data, default is 'sum'.
#' @param na_thres threshold for the percentage of NA values allowed in the
#'  aggregation period, default = 10.
#'
#' @details
#' This has been adapted from code available at
#' \url{https://github.com/WillemMaetens/standaRdized}.
#'
#' @return
#' An xts time series with aggregated values.
#'
#' @author Sam Allen, Noelia Otero
#'
#' @examples
#' Examples of the aggregate_xts function will be added here
#'
#' @name aggregate_xts
NULL

#' @rdname aggregate_xts
#' @export
aggregate_xts <- function(x,
                          agg_length,
                          agg_scale = c('days', 'hours', 'weeks'),
                          agg_fun = 'sum',
                          na_thres = 10) {
  agg_scale = match.arg(agg_scale)
  x_agg <- sapply(index(x), aggregate_xts_1, x, agg_length, agg_scale, agg_fun, na_thres)
  x_agg <- xts(x_agg, order.by = index(x))
  xtsAttributes(x_agg) <- xtsAttributes(x)
  xtsAttributes(x_agg)$agg_length <- agg_length
  return(x_agg)
}


aggregate_xts_1 <- function(date,
                            data,
                            agg_length,
                            agg_scale = 'days',
                            agg_fun = 'sum',
                            na_thres = 10) {
  from <- date - as.difftime(agg_length - 1, units = agg_scale)
  to <- date
  data <- data[paste(from, to, sep = '/')]
  dates <- seq(from = from, to = to, by = as.difftime(1, units = agg_scale))
  x <- c(coredata(merge(xts(order.by = dates), data, join = 'left')))

  if (length(x) != 0) {
    pct.na <- (length(which(is.na(x))) / length(x)) * 100
    if (pct.na <= na_thres) {
      if(!all(is.na(x))){
        x <- x[!is.na(x)]
        x <- do.call(what = agg_fun, args = list(x = x)) # apply aggregation function
        return(x)
      } else {
        return(as.numeric(NA))
      }
    } else {
      return(as.numeric(NA))
    }
  } else {
    return(as.numeric(NA))
  }
}

