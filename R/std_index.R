#' @title Calculate standardised energy indices
#'
#' @description Inputs a time series of a chosen variable (e.g. residual load or
#' energy demand) and returns a time series of standardised indices. Different
#' types of indices can be calculated, on any timescale that is of interest.
#'
#' @param x_new vector or time series to be converted to standardised indices.
#' @param x_ref vector or time series to be used as reference data when calculating the standardised indices.
#' @param dist string; distribution used to calculate the indices.
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
#' Examples of the std_index function will be added here
#'
#' @name std_index
NULL

#' @rdname std_index
#' @export
std_index <- function(x_new,
                      x_ref = x_new,
                      dist = "empirical",
                      moving_window = NULL,
                      window_scale = "days",
                      agg_period = NULL,
                      agg_scale = "days",
                      agg_fun = "sum",
                      rescale = NULL,
                      rescale_fun = "sum",
                      index_type = "normal",
                      ignore_na = FALSE) {

  # check inputs
  inputs <- as.list(environment())
  check_inputs(inputs)

  # scale data
  if (!is.null(rescale_period)) {
    apply_rescale <- eval(parse(text = paste0("apply.", rescale_period)))
    x_new <- apply_rescale(x_new, rescale_fun, na.rm = ignore_na)
    x_ref <- apply_rescale(x_ref, rescale_fun, na.rm = ignore_na)
  }

  # aggregate data
  if (!is.null(agg_period)) {
    x_new <- aggregate_xts(x_new, len = agg_period, scale = agg_scale, fun = agg_fun)
    x_ref <- aggregate_xts(x_ref, len = agg_period, scale = agg_scale, fun = agg_fun)
  }

  # calculate pit values
  if (is.null(moving_window)) {
    pit <- get_pit(x_ref, x_new, dist = dist)
  } else {
    pit <- sapply(index(x_new), function(date) {
      from <- date - as.difftime(moving_window - 1, units = window_scale)
      to <- date
      data <- x_ref[paste(from, to, sep = "/")]
      get_pit(data, x_new[date], dist = dist)
    })
  }

  # convert to index
  if (index_type == "normal") {
    si <- qnorm(pit)
  } else if (index_type == "bounded") {
    si <- 2*pit - 1
  } else if (index_type == "probability") {
    si <- pit
  }
  si <- xts(si, order.by = index(x_new))
  xtsAttributes(si) <- xtsAttributes(x_new)

  return(si)
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
  available_dists <- c("empirical", "normal", "fit_dist", "kde")
  if (!(dist %in% available_dists)) {
    stop("the specified distribution is not available - see details for a list of
         available distributions")
  }

  # moving_window
  if (!is.null(moving_window)) {
    if (is.numeric(moving_window)) {
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
    if (is.numeric(agg_period)) {
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

  # rescale_period
  if (!is.null(rescale_period)) {
    if (!(rescale_period %in% c("daily", "weekly", "monthly", "quarterly", "yearly"))) {
      stop("rescale_period must be one of 'daily', 'weekly', 'monthly', 'quarterly', or 'yearly'")
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
