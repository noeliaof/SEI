#' @title Fit a distribution to data
#'
#' @description Function to fit a specified distribution a vector of data. Returns
#' the estimated distribution and relevant goodness-of-fit statistics.
#'
#' @param data vector of data
#' @param dist character string specifying the distribution, see details
#' @param n_thres number of data points required to estimate the distribution
#'
#' @details
#' This has been adapted from code available at
#' \url{https://github.com/WillemMaetens/standaRdized}.
#'
#' \code{data} is a numeric vector of data from which the distribution is to be estimated.
#'
#' \code{dist} is the specified distribution to be fit to \code{data}. This must be one of
#' 'empirical' (the empirical distribution given \code{data}), 'kde' (kernel density estimation),
#' 'norm', 'lnorm', 'logis', 'llogis', 'exp', 'gamma', and 'weibull'.
#'
#' By default, \code{dist = "empirical"}, in which case
#' the distribution is estimated empirically from \code{data}. This is only
#' recommended if there are at least 100 values in \code{data}, and a warning
#' message is returned otherwise.
#'
#' \code{n_thres} is the minimum number of observations required to fit the distribution.
#' The default is \code{n_thres = 20}. If the number of values in \code{data} is
#' smaller than \code{na_thres}, an error is returned. This guards against over-fitting,
#' which can result in distributions that do not generalise well out-of-sample.
#'
#' Where relevant, parameter estimation is performed using maximum likelihood
#' estimation.
#'
#'
#' @return
#' A list containing the estimated distribution function, its parameters,
#' and Kolmogorov-Smirnov goodness-of-fit statistics.
#'
#' @examples
#' N <- 1000
#' shape <- 3
#' rate <- 2
#'
#'
#' # gamma distribution
#' data <- rgamma(N, shape, rate)
#' out <- fit_dist(data, dist = "gamma")
#' hist(data, breaks = 30, probability = TRUE)
#' lines(seq(0, 10, 0.01), dgamma(seq(0, 10, 0.01), out$params[1], out$params[2]), col = "blue")
#'
#'
#' # weibull distribution
#' data <- rweibull(N, shape, 1/rate)
#' out <- fit_dist(data, dist = "weibull")
#' hist(data, breaks = 30, probability = TRUE)
#' lines(seq(0, 10, 0.01), dweibull(seq(0, 10, 0.01), out$params[1], out$params[2]), col = "blue")
#'
#' @name fit_dist
#' @importFrom stats bw.nrd ecdf pnorm
NULL

#' @rdname fit_dist
#' @export
fit_dist <- function(data, dist, n_thres = 20){

  # check inputs
  inputs <- as.list(environment())
  check_distribution(inputs)

  # initialise data properties and goodness-of-fit statistics
  fit_props <- rep(NA, 5)
  names(fit_props) <- c("n_obs", "n_na", "pct_na", "aic", "ks_pval")

  fit_props['n_obs'] <- as.integer(length(data))
  fit_props['n_na'] <- sum(is.na(data))
  fit_props['pct_na'] <- (fit_props['n_na']/fit_props['n_obs'])*100

  # check sample size
  data <- data[!is.na(data)]
  n <- length(data)
  if (n < n_thres) {
    warning(paste(n_thres, "values are required to fit the distribution - distribution has not been fit"))
    return(list(F_x = function(x, params) as.numeric(NA), params = NA, fit_props = fit_props))
  }

  # fit distribution
  if (dist == "empirical") {
    emp_func  <- ecdf(data)
    params <- list(emp_func = emp_func, n = n)
    F_x <- function(x, params) (1 + params$emp_func(x)*params$n)/(params$n + 2)
  } else if (dist == "kde") {
    params <- list(data = data, bw = bw.nrd(data))
    F_x <- function(x, params) {
      if (length(x) > 1) {
        sapply(x, function(z) mean(pnorm(z, mean = params$data, sd = params$bw)))
      } else {
        mean(pnorm(x, mean = params$data, sd = params$bw))
      }
    }
  } else {
    fit <- try(fitdistrplus::fitdist(data = data, distr = dist, method = "mle"))
    if (!inherits(fit, "try-error")){
      params <- fit$estimate # parameters
      F_x <- function(x, params) do.call(paste0("p", dist), c(list(q = x), as.list(params))) # cdf
      fit_props['aic'] <- fit$aic # aic
      fit_props['ks_pval'] <- do.call('ks.test', c(list(x = data, y = paste0('p', dist)), as.list(params)))$p.value # ks p-value
    } else {
      warning("distribution fitting failed")
      return(list(F_x = function(x, params) NA, params = NULL, fit_props = fit_props))
    }
  }

  return(list(F_x = F_x, params = params, fit_props = fit_props))
}


check_distribution <- function(inputs) {

  data <- inputs$data[!is.na(inputs$data)]

  if (!(inputs$dist %in% c("empirical", "kde", "norm", "lnorm",
                    "logis", "llogis", "exp", "gamma", "weibull"))) {
    stop("dist must be one of 'empirical', 'kde', 'norm', 'lnorm',
         'logis', 'llogis', 'exp', 'gamma', 'weibull'")
  }

  if (inputs$dist == "empirical") {
    if (length(data) < 100) {
      warning("using the empirical distribution is only recommended when at least
              100 values are available when fitting the distribution")
    }
  }

  if (inputs$dist %in% c("lnorm", "llogis", "exp", "gamma", "weibull")) {
    if (any(data < 0)) {
      stop(paste("the", inputs$dist, "distribution has positive support, but the data contains negative values"))
    } else if (any(data == 0)) {
      stop(paste("the", inputs$dist, "distribution has positive support, but the data contains values equal to zero"))
    }
  }

}


