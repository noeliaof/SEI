#' @title Calculate standardised indices
#'
#' @description Inputs a time series of a chosen variable (e.g. precipitation,
#' energy demand, residual load etc.) and returns a time series of standardised indices.
#' Indices can be calculated on any timescale.
#'
#' @param x_new vector or time series to be converted to standardised indices.
#' @param x_ref vector or time series containing reference data to use when calculating the standardised indices.
#' @param gr_new vector of factors for which separate distributions should be applied to \code{x_new}.
#' @param gr_ref vector of factors for which separate distributions should be fit to \code{x_ref}.
#' @param moving_window length of moving window on which to calculate the indices.
#' @param window_scale timescale of \code{moving_window}; default is the timescale of the data.
#' @param rescale the timescale that the time series should be rescaled to;
#'  one of `"days"`, `"weeks"`, `"months"`, `"quarters"`, and `"years"`.
#' @param rescale_fun string specifying the function used to rescale the data; default is `"sum"`.
#' @param index_type the type of standardised index: `"normal"` (default), `"prob01"`,
#'  or `"prob11"` (see details).
#' @param ignore_na logical specifying whether to ignore NAs when rescaling the time series.
#' @inheritParams aggregate_xts
#' @inheritParams get_pit
#' @inheritParams fit_dist
#'
#' @details
#' Standardised indices are calculated by estimating the cumulative distribution function (CDF)
#' of the variable of interest, and using this to transform the measurements to
#' a standardised scale.
#'
#' \code{std_index()} estimates the CDF using a time series of reference data \code{x_ref},
#' and applies the resulting transformation to the time series \code{x_new}. The result is
#' a time series of standardised \code{x_new} values. These standardised indices quantify
#' how extreme the \code{x_new} values are in reference to \code{x_ref}.
#' \code{x_new} and \code{x_ref} should therefore contain values of the same variable.
#' If \code{x_ref} is not specified, then \code{x_new} is also used to estimate the CDF.
#'
#' \code{x_new} and \code{x_ref} can either be provided as vectors or xts time series.
#' In the latter case, the time series can be aggregated across timescales or rescaled.
#' This is useful, for example, if \code{x_new} contains hourly data, but interest is
#' on daily accumulations or averages of the hourly data.
#'
#' The argument \code{rescale} converts the data to a different timescale. The original
#' timescale of the data can be manually specified using the argument \code{timescale}.
#' \code{timescale} is required if the time series is to be aggregated or rescaled.
#' Otherwise, the function will try to automatically determine the timescale of the data.
#' Manually specifying the timescale of the data is generally more robust. The rescaling
#' is performed using the function \code{rescale_fun}. By default, this is assumed to be
#' \code{rescale_fun = "sum"}, so that values are added across the timescale of interest.
#' This can be changed to any user-specified function.
#'
#' The argument \code{agg_period} aggregates the data across the timescale of interest.
#' This differs from \code{rescale} in that the resolution of the data remains the same.
#' \code{agg_period} is a number specifying how long the data should be aggregated across.
#' By default, it is assumed that \code{agg_period} is on the same timescale as \code{x_new}
#' and \code{x_ref}. For example, if the data is hourly and \code{agg_period = 24}, then
#' this assumes the data is to be aggregated over the past 24 hours. The scale of the
#' aggregation period can also be specified manually using \code{agg_scale}. For example,
#' one could also specify \code{agg_period = 1} and \code{agg_scale = "days"}, and this
#' would also aggregate the data over the past day. \code{agg_fun} specifies how the
#' data is to be aggregated, the default is \code{agg_fun = "sum"}.
#'
#' \code{dist} is the distribution used to estimate the CDF from \code{x_ref}.
#' Currently, functionality is available to fit one of the following distributions to the data:
#' Normal ('norm'), Log-normal ('lnorm'), Logistic ('logis'), Log-logistic ('llogis'),
#' Exponential ('exp'), Gamma ('gamma'), and Weibull ('weibull').
#' Alternatively, the CDF can be estimated empirically (\code{dist = "empirical"})
#' based on the values in \code{x_ref}, or using kernel density estimation (\code{dist = "kde"}).
#'
#' If \code{dist} is a parametric family of distributions, then parameters of the
#' distribution are estimated using maximum likelihood estimation from \code{x_ref}.
#' The resulting parameters and corresponding goodness-of-fit statistics can be
#' returned by specifying \code{return_fit = TRUE}.
#'
#' By default, the distribution is estimated over all values in \code{x_ref}. Alternatively,
#' if \code{x_new} is an xts object, parameters can be estimated sequentially using a
#' moving window of values. \code{moving_window} determines the length of the moving window.
#' This is a single value, assumed to be on the same timescale as \code{x_new}.
#' This can also be specified manually using \code{window_scale}. \code{window_scale}
#' must also be one of "days", "weeks", "months", "quarters", and "years".
#'
#' The function returns a vector of time series (depending on the format of \code{x_new})
#' containing the standardised indices corresponding to \code{x_new}. Three different
#' types of indices are available, which are explained in detail in the vignette.
#' The index type can be chosen using \code{index_type}, which must be one of
#' "normal" (default), "prob01", and "prob11".
#'
#'
#' @return
#' Time series of standardised indices.
#'
#' @references
#' Allen, S. and N. Otero (2023):
#' `Standardised indices to monitor energy droughts',
#' \emph{Renewable Energy} 217, 119206
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
#' #options(xts_check_TZ = FALSE)
#'
#' # convert to hourly standardised indices
#' supply_de_std <- std_index(supply_de, timescale = "hours")
#' hist(supply_de, main = "Raw values")
#' hist(supply_de_std, main = "Standardised values")
#'
#' # convert to daily or weekly standardised indices
#' supply_de_std <- std_index(supply_de, timescale = "hours", rescale = "days")
#'
#' # convert to weekly standardised indices calculated on each day
#' supply_de_std <- std_index(supply_de, timescale = "hours", rescale = "days",
#'                            agg_period = 1, agg_scale = "weeks")
#'
#' # calculate standardised indices corresponding to December, based on the previous year
#' dec <- zoo::index(supply_de) > "2019-12-01 UTC"
#' supply_de_std_dec <- std_index(x_new = supply_de[dec], x_ref = supply_de[!dec],
#'                                timescale = "hours")
#'
#' # calculate standardised indices using a 100 day moving window
#' supply_de_std_dec <- std_index(supply_de[dec], supply_de, timescale = "hours",
#'                                rescale = "days", moving_window = 100)
#'
#' # suppose we are interested in the daily maximum rather than the daily total
#' supply_de_std <- std_index(supply_de, timescale = "hours", rescale = "days",
#'                            rescale_fun = "max")
#' supply_de_std <- std_index(supply_de, timescale = "hours", rescale = "days",
#'                            rescale_fun = "mean") # or average
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
#' supply_de_std <- std_index(supply_de, timescale = "hours", rescale = "weeks",
#'                            dist = "gamma", return_fit = TRUE)
#'
#' # we could also use kernel density estimation, which is a flexible compromise between the two
#' supply_de_std <- std_index(supply_de, timescale = "hours", rescale = "weeks", dist = "kde")
#'
#'
#' # calculate separate indices for each quarter of 2019
#' season <- ceiling(lubridate::month(zoo::index(supply_de)) / 3)
#' season <- c("Q1", "Q2", "Q3", "Q4")[season]
#' supply_de_std <- std_index(supply_de, timescale = "hours", rescale = "days",
#'                            gr_new = season, dist = "kde", return_fit = TRUE)
#'
#'
#'
#' @name std_index
#' @importFrom stats qnorm
NULL

#' @rdname std_index
#' @export
std_index <- function(x_new,
                      x_ref = x_new,
                      timescale = NULL,
                      dist = "empirical",
                      method = "mle",
                      return_fit = FALSE,
                      index_type = "normal",
                      preds_new = NULL,
                      preds_ref = preds_new,
                      gr_new = NULL,
                      gr_ref = gr_new,
                      moving_window = NULL,
                      window_scale = NULL,
                      agg_period = NULL,
                      agg_scale = NULL,
                      agg_fun = "sum",
                      rescale = NULL,
                      rescale_fun = "sum",
                      ignore_na = FALSE,
                      n_thres = 20,
                      na_thres = 10,
                      lower = -Inf,
                      upper = Inf,
                      cens = index_type,
                      ...) {

  # check inputs
  inputs <- as.list(environment())
  check_inputs(inputs)

  # group data
  if (!is.null(gr_new)) {
    gr_out <- lapply(unique(gr_new), function(i) {
      ind <- gr_new == i
      ind_ref <- gr_ref == i
      std_index(x_new[ind], x_ref[ind], timescale = timescale, dist = dist, return_fit = return_fit,
                index_type = index_type, agg_period = agg_period, agg_scale = agg_scale,
                agg_fun = agg_fun, rescale = rescale, ignore_na = ignore_na, na_thres = na_thres,
                lower = lower, upper = upper, cens = cens)
    })
    if (return_fit) {
      params <- lapply(seq_along(gr_out), function(i) gr_out[[i]]$params)
      fit <- lapply(seq_along(gr_out), function(i) gr_out[[i]]$fit)
      si <- lapply(seq_along(gr_out), function(i) gr_out[[i]]$si)
      si <- do.call(rbind, si)
      names(params) <- names(fit) <- unique(gr_new)
      return(list(params = params, fit = fit, si = si))
    } else {
      si <- do.call(rbind, gr_out)
      return(si)
    }
  }

  # get timescale (if not given)
  if (!(is.null(rescale) | is.null(agg_period) | is.null(moving_window)) & is.null(timescale)) {
    timedif <- diff(zoo::index(x_ref))[1] # timedif <- deltat(x_ref)/3600
    if (timedif == 1) {
      timescale <- units(timedif)
    } else if (timedif == 24 && units(timedif) == "hours") {
      timescale <- "days"
    } else if (timedif == 7 && units(timedif) == "days") {
      timescale <- "weeks"
    } else if (timedif == 365 && units(timedif) == "days") {
      timescale <- "years"
    } else {
      stop("could not automatically determine the timescale of the time series,
           please specify this manually")
    }
  }

  # scale data
  if (!is.null(rescale)) {
    if (rescale == "days") {
      apply_rescale <- xts::apply.daily
    } else if (rescale == "weeks") {
      apply_rescale <- xts::apply.weekly
    } else if (rescale == "months") {
      apply_rescale <- xts::apply.monthly
    } else if (rescale == "quarters") {
      apply_rescale <- xts::apply.quarterly
    } else if (rescale == "years") {
      apply_rescale <- xts::apply.yearly
    }
    x_new <- apply_rescale(x_new, rescale_fun, na.rm = ignore_na)
    x_ref <- apply_rescale(x_ref, rescale_fun, na.rm = ignore_na)
    timescale <- rescale
  }

  # aggregate data
  if (!is.null(agg_period)) {
    if (is.null(agg_scale)) agg_scale <- timescale
    x_new <- aggregate_xts(x_new, len = agg_period, scale = agg_scale, fun = agg_fun,
                           timescale = timescale, na_thres = na_thres)
    x_ref <- aggregate_xts(x_ref, len = agg_period, scale = agg_scale, fun = agg_fun,
                           timescale = timescale, na_thres = na_thres)
  }

  # calculate pit values
  if (cens %in% c("prob01", "prob11")) cens <- "prob"
  if (is.null(preds)) { # stationary distribution estimation
    if (is.null(moving_window)) {
      fit <- get_pit(x_ref, x_new, dist = dist, method = method, return_fit = return_fit,
                     lower = lower, upper = upper, cens = cens, n_thres = n_thres, ...)
    } else {
      if (is.null(window_scale)) window_scale <- timescale
      fit <- lapply(zoo::index(x_new), function(date) {
        from <- date - as.difftime(moving_window, units = window_scale)
        to <- date - as.difftime(1, units = timescale)
        data <- x_ref[paste(from, to, sep = "/")]
        get_pit(data, x_new[date], dist = dist, return_fit = return_fit, n_thres = n_thres, ...)
      })
      if (return_fit) {
        fit_names <- names(fit[[1]])
        fit <- lapply(fit_names, function(x) sapply(fit, function(z) z[[x]]))
        names(fit) <- fit_names
      } else {
        fit <- unlist(fit)
      }
    }
  } else { # non-stationary distribution estimation with gamlss
    if (is.null(moving_window)) {
      fit <- get_pit(x_ref, x_new, preds_new = preds_new, preds_ref = preds_ref,
                     dist = dist, method = method, return_fit = return_fit,
                     lower = lower, upper = upper, cens = cens, n_thres = n_thres, ...)
    } else {
      if (is.null(window_scale)) window_scale <- timescale
      fit <- lapply(zoo::index(x_new), function(date) {
        from <- date - as.difftime(moving_window, units = window_scale)
        to <- date - as.difftime(1, units = timescale)
        data <- x_ref[paste(from, to, sep = "/")]
        ind <- x_ref %in% data
        ref_preds <- preds_ref[ind, , drop = FALSE]
        ind <- x_ref %in% x_new[date]
        new_preds <- preds_new[ind, , drop = FALSE]
        get_pit(data, x_new[date], ref_preds = ref_preds, new_preds = new_preds,
                dist = dist, return_fit = return_fit, n_thres = n_thres, ...)
      })
      if (return_fit) {
        fit_names <- names(fit[[1]])
        fit <- lapply(fit_names, function(x) sapply(fit, function(z) z[[x]]))
        names(fit) <- fit_names
      } else {
        fit <- unlist(fit)
      }
    }
  }


  # convert to index
  if (return_fit) {
    pit <- fit$pit
  } else {
    pit <- fit
  }
  if (index_type == "normal") {
    si <- qnorm(pit)
  } else if (index_type == "prob11") {
    si <- 2*pit - 1
  } else if (index_type == "prob01") {
    si <- pit
  }
  if (xts::is.xts(x_new)) {
    si <- xts::xts(si, order.by = zoo::index(x_new))
    xts::xtsAttributes(si) <- xts::xtsAttributes(x_new)
  }

  # return indices
  if (return_fit) {
    fit$si <- si
    fit$F_x <- fit$pit <- NULL
    return(fit)
  } else {
    return(si)
  }

}


check_inputs <- function(inputs) {

  # x_new
  if (!is.numeric(inputs$x_new)) {
    stop("x_new must be a numeric vector")
  }
  if (!xts::is.xts(inputs$x_new) & (!is.null(inputs$agg_period) | !is.null(inputs$rescale))) {
    stop("x_new cannot be aggregated or rescaled if it is not an xts object")
  }
  if (!xts::is.xts(inputs$x_new) & !is.null(inputs$moving_window)) {
    stop("standardised indices cannot be calculated from a moving window if
    x_new and x_ref are not xts objects")
  }

  # x_ref
  if (!is.numeric(inputs$x_ref) & is.null(inputs$moving_window)) {
    stop("x_ref must be a numeric vector")
  }
  if (!xts::is.xts(inputs$x_ref) & (!is.null(inputs$agg_period) | !is.null(inputs$rescale))) {
    stop("x_ref cannot be aggregated or rescaled if it is not an xts object")
  }
  if (!xts::is.xts(inputs$x_ref) & !is.null(inputs$moving_window)) {
    stop("standardised indices cannot be calculated from a moving window if
    x_new and x_ref are not xts objects")
  }

  # dist
  available_dists <- c("empirical", "kde", "norm", "lnorm",
                       "logis", "llogis", "exp", "gamma", "weibull")
  if (!(inputs$dist %in% available_dists)) {
    stop("the specified distribution is not available - see details for a list of
         available distributions")
  }

  # moving_window
  if (!is.null(inputs$moving_window)) {
    if (!is.numeric(inputs$moving_window)) {
      stop("moving_window must be a single numeric value")
    }
    if (!identical(length(inputs$moving_window), 1L)) {
      stop("moving_window must be a single numeric value")
    }
    if (inputs$moving_window > length(inputs$x_ref)) {
      stop("moving_window exceeds length of reference data")
    }
  }

  # window_scale
  if (!is.null(inputs$window_scale)) {
    if (!(inputs$window_scale %in% c("days", "weeks", "months", "quarters", "years"))) {
      stop("window_scale must be one of 'days', 'weeks', 'months', 'quarters', or 'years'")
    }
  }

  # agg_period
  if (!is.null(inputs$agg_period)) {
    if (!is.numeric(inputs$agg_period)) {
      stop("agg_period must be a single numeric value")
    }
    if (!identical(length(inputs$agg_period), 1L)) {
      stop("agg_period must be a single numeric value")
    }
    if (inputs$agg_period > length(inputs$x_new) | inputs$agg_period > length(inputs$x_ref)) {
      stop("agg_period exceeds length of data set")
    }
  }

  # agg_scale
  if (!is.null(inputs$agg_scale)) {
    if (!(inputs$agg_scale %in% c("days", "weeks", "months", "quarters", "years"))) {
      stop("agg_scale must be one of 'days', 'weeks', 'months', 'quarters', or 'years'")
    }
  }

  # agg_fun
  if (!is.null(inputs$agg_fun)) {
    if (!(inputs$agg_fun %in% c("sum", "mean", "min", "max"))) {
      stop("agg_fun must be one of 'sum', 'mean', 'min', or 'max'")
    }
  }

  # timescale
  if (!is.null(inputs$timescale)) {
    if (!(inputs$timescale %in% c("hours", "days", "weeks", "months", "quarters", "years"))) {
      stop("timescale must be one of 'hours', 'days', 'weeks', 'months', 'quarters', or 'years'")
    }
  }

  # rescale
  if (!is.null(inputs$rescale)) {
    if (!(inputs$rescale %in% c("days", "weeks", "months", "quarters", "years"))) {
      stop("rescale must be one of 'days', 'weeks', 'months', 'quarters', or 'years'")
    }
  }

  # rescale_fun
  if (!is.null(inputs$rescale_fun)) {
    if (!(inputs$rescale_fun %in% c("sum", "mean", "min", "max"))) {
      stop("rescale_fun must be one of 'sum', 'mean', 'min', or 'max'")
    }
  }

  # index_type
  if (!(inputs$index_type %in% c("normal", "prob01", "prob11"))) {
    stop("index_type must be one of 'normal', 'prob01', or 'prob11'")
  }


}
