#' @title Calculate standardised energy indices
#'
#' @description Inputs a time series of a chosen variable (e.g. residual load or
#' energy demand) and returns a time series of standardised indices. Different
#' types of indices can be calculated, on any timescale that is of interest.
#'
#' @param x_new vector or time series to be converted to standardised indices.
#' @param x_ref vector or time series to be used as reference data when calculating the standardised indices.
#' @param dist string; distribution used to calculate the indices.
#' @param return_fit logical; return parameters and goodness-of-fit statistics for the distribution fit.
#' @param moving_window numeric; length of moving window on which to calculate the indices.
#' @param window_scale string; timescale of \code{moving_window}, default is "days".
#' @param agg_period numeric; the number of values to aggregate over.
#' @param agg_scale string; timescale of \code{agg_period}, default is "days".
#' @param agg_fun string; function used to aggregate the data over the aggregation period, default is "sum".
#' @param rescale string; the timescale that the time series should be rescaled to.
#' @param rescale_fun string; function used to rescale the data, default is "sum".
#' @param index_type string; the type of index: "normal" (default), "probability", or "bounded".
#' @param ignore_na logical; should NAs be ignored when rescaling the time series?
#'
#' @details
#' Details about the std_index function will be added here
#'
#' @return
#' Time series of standardised indices.
#'
#'
#' @references
#' Allen, S. and N. Otero (2022):
#' `Standardised indices to monitor energy droughts',
#' \emph{EarthArXiv preprint} 4752.
#' \doi{10.31223/X51S92}
#'
#' McKee, T. B., Doesken, N. J., & Kleist, J. (1993):
#' `The relationship of drought frequency and duration to time scales',
#' \emph{In Proceedings of the 8th Conference on Applied Climatology} 17, 179-183.
#'
#'
#' @author Sam Allen, Noelia Otero
#'
#' @examples
#' data(data_supply)
#' # consider hourly German energy supply data in 2019
#' supply_de <- subset(data_supply, country == "Germany", select = c("date", "PWS"))
#' supply_de <- xts(supply_de$PWS, order.by = supply_de$date)
#' options(xts_check_TZ = FALSE)
#'
#' # convert to hourly standardised indices
#' supply_de_std <- std_index(supply_de, timescale = "hours")
#' par(mfrow = c(1, 2))
#' hist(supply_de)
#' hist(supply_de_std)
#'
#' # convert to daily or weekly standardised indices
#' supply_de_std <- std_index(supply_de, timescale = "hours", rescale = "days")
#'
#' # convert to weekly standardised indices calculated on each day
#' supply_de_std <- std_index(supply_de, timescale = "hours", rescale = "days", agg_period = 1, agg_scale = "weeks")
#'
#' # calculate standardised indices corresponding to December, based on the previous year
#' dec <- index(supply_de) >= "2019-01-12 UTC"
#' supply_de_std_dec <- std_index(x_new = supply_de[dec], x_ref = supply_de[!dec], timescale = "hours")
#'
#' # calculate standardised indices using a 100 day moving window
#' supply_de_std_dec <- std_index(supply_de[dec], timescale = "hours", rescale = "days", moving_window = 100)
#'
#' # suppose we are interested in the daily maximum rather than the daily total
#' supply_de_std <- std_index(supply_de, timescale = "hours", rescale = "days", rescale_fun = "max")
#' supply_de_std <- std_index(supply_de, timescale = "hours", rescale = "days", rescale_fun = "mean") # or average
#'
#' # the default uses the empirical distribution, but this requires more data than
#' # parametric distributions, meaning it is not ideal when data is short, e.g. in weekly case
#' supply_de_std <- std_index(supply_de, timescale = "hours", rescale = "weeks") # warning
#' # instead, we can use a parametric distribution, e.g. a gamma distribution
#' supply_de_std <- std_index(supply_de, timescale = "hours", rescale = "weeks", dist = "gamma")
#' # we can check the fit by checking whether the indices resemble a standard normal distribution
#' hist(supply_de)
#' hist(supply_de_std)
#' # we can also look at the properties of the fit
#' supply_de_std <- std_index(supply_de, timescale = "hours", rescale = "weeks", dist = "gamma", return_fit = TRUE)
#'
#' # alternatively, we can use kernel density estimation, which is a flexible compromise between the two
#' supply_de_std <- std_index(supply_de, timescale = "hours", rescale = "weeks", dist = "kde")
#'
#' @name std_index
NULL

#' @rdname std_index
#' @export
std_index <- function(x_new,
                      x_ref = x_new,
                      timescale = "days",
                      dist = "empirical",
                      return_fit = FALSE,
                      moving_window = NULL,
                      window_scale = NULL,
                      agg_period = NULL,
                      agg_scale = NULL,
                      agg_fun = "sum",
                      rescale = NULL,
                      rescale_fun = "sum",
                      index_type = "normal",
                      ignore_na = FALSE) {

  # check inputs
  inputs <- as.list(environment())
  check_inputs(inputs)

  # scale data
  if (!is.null(rescale)) {
    if (rescale == "days") {
      apply_rescale <- apply.daily
    } else if (rescale == "weekly") {
      apply_rescale <- apply.weekly
    } else if (rescale == "monthly") {
      apply_rescale <- apply.monthly
    } else if (rescale == "quarterly") {
      apply_rescale <- apply.quarterly
    } else if (rescale == "yearly") {
      apply_rescale <- apply.yearly
    }
    x_new <- apply_rescale(x_new, rescale_fun, na.rm = ignore_na)
    x_ref <- apply_rescale(x_ref, rescale_fun, na.rm = ignore_na)
    timescale <- rescale
  }

  # aggregate data
  if (!is.null(agg_period)) {
    if (is.null(agg_scale)) agg_scale <- timescale
    x_new <- aggregate_xts(x_new, len = agg_period, scale = agg_scale, fun = agg_fun, timescale = timescale)
    x_ref <- aggregate_xts(x_ref, len = agg_period, scale = agg_scale, fun = agg_fun, timescale = timescale)
  }

  # calculate pit values
  if (is.null(moving_window)) {
    fit <- get_pit(x_ref, x_new, dist = dist, return_fit = return_fit)
  } else {
    if (is.null(window_scale)) window_scale <- timescale
    fit <- lapply(index(x_new), function(date) {
      from <- date - as.difftime(moving_window, units = window_scale) + as.difftime(1, units = timescale)
      to <- date
      data <- x_ref[paste(from, to, sep = "/")]
      get_pit(data, x_new[date], dist = dist, return_fit = return_fit)
    })
    fit_names <- names(fit[[1]])
    fit <- lapply(fit_names, function(x) sapply(fit, function(z) z[[x]]))
    names(fit) <- fit_names
  }

  # convert to index
  if (index_type == "normal") {
    fit$si <- qnorm(fit$pit)
  } else if (index_type == "bounded") {
    fit$si <- 2*fit$pit - 1
  } else if (index_type == "probability") {
    fit$si <- fit$pit
  }
  fit$si <- xts(fit$si, order.by = index(x_new))
  xtsAttributes(fit$si) <- xtsAttributes(x_new)

  if (return_fit) {
    fit$F_x <- fit$pit <- NULL
    return(fit)
  } else {
    return(fit$si)
  }

}


check_inputs <- function(inputs) {
  for(i in seq_along(inputs)) assign(names(inputs)[i], inputs[[i]])

  # x_new
  if (!is.numeric(x_new)) {
    stop("x_new must be a numeric vector")
  }
  if (!is.xts(x_new) & (!is.null(agg_period) | !is.null(rescale_period))) {
    stop("x_new cannot be aggregated or rescaled if it is not an xts object")
  }
  if (!is.xts(x_new) & !is.null(moving_window)) {
    stop("standardised indices cannot be calculated from a moving window if
    x_new and x_ref are not xts objects")
  }

  # x_ref
  if (!is.numeric(x_ref) & is.null(moving_window)) {
    stop("x_ref must be a numeric vector")
  }
  if (!is.xts(x_ref) & (!is.null(agg_period) | !is.null(rescale_period))) {
    stop("x_ref cannot be aggregated or rescaled if it is not an xts object")
  }
  if (!is.xts(x_ref) & !is.null(moving_window)) {
    stop("standardised indices cannot be calculated from a moving window if
    x_new and x_ref are not xts objects")
  }

  # dist
  available_dists <- c("empirical", "kde", "gamma", "weibull", "gev", "glogis")
  if (!(dist %in% available_dists)) {
    stop("the specified distribution is not available - see details for a list of
         available distributions")
  }

  # moving_window
  if (!is.null(moving_window)) {
    if (!is.numeric(moving_window)) {
      stop("moving_window must be a single numeric value")
    }
    if (!identical(length(moving_window), 1L)) {
      stop("moving_window must be a single numeric value")
    }
    if (moving_window > length(x_ref)) {
      stop("moving_window exceeds length of reference data")
    }
  }

  # agg_period
  if (!is.null(agg_period)) {
    if (!is.numeric(agg_period)) {
      stop("agg_period must be a single numeric value")
    }
    if (!identical(length(agg_period), 1L)) {
      stop("agg_period must be a single numeric value")
    }
    if (agg_period > length(x_new) | agg_period > length(x_ref)) {
      stop("agg_period exceeds length of data set")
    }
  }

  # agg_fun
  if (!is.null(agg_fun)) {
    if (!(agg_fun %in% c("sum", "mean", "min", "max"))) {
      stop("agg_fun must be one of 'sum', 'mean', 'min', or 'max'")
    }
  }

  # timescale
  if (!is.null(timescale)) {
    if (!(timescale %in% c("hours", "days", "weeks", "months", "quarters", "years"))) {
      stop("timescale must be one of 'hours', 'days', 'weeks', 'months', 'quarters', or 'years'")
    }
  }

  # rescale
  if (!is.null(rescale)) {
    if (!(rescale %in% c("days", "weeks", "months", "quarters", "years"))) {
      stop("rescale must be one of 'days', 'weeks', 'months', 'quarters', or 'years'")
    }
  }

  # rescale_fun
  if (!is.null(rescale_fun)) {
    if (!(rescale_fun %in% c("sum", "mean", "min", "max"))) {
      stop("rescale_fun must be one of 'sum', 'mean', 'min', or 'max'")
    }
  }

  # index_type
  if (!(index_type %in% c("normal", "bounded", "probability"))) {
    stop("index_type must be one of 'normal', 'bounded', or 'probability'")
  }


}
