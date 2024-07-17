#' @title Fit a distribution to data
#'
#' @description Function to fit a specified distribution to a vector of data.
#' Returns the estimated distribution and relevant goodness-of-fit statistics.
#'
#' @param dist character string specifying the distribution to be fit to the data;
#'  one of `"empirical"`, `"kde"`, `"norm"`, `"lnorm"`, `"logis"`, `"llogis"`,
#'  `"exp"`, `"gamma"`, and `"weibull"`.
#' @inheritParams fitdistrplus::fitdist
#' @param n_thres minimum number of data points required to estimate the distribution;
#'  default is 20.
#' @param ... additional arguments to be passed to \code{\link{fitdistrplus::fitdist}}
#'
#' @details
#' This has been adapted from code available at
#' \url{https://github.com/WillemMaetens/standaRdized}.
#'
#' \code{data} is a numeric vector of data from which the distribution is to be estimated.
#'
#' \code{dist} is the specified distribution to be fit to \code{data}. This must be one of
#' 'empirical', 'kde', 'norm', 'lnorm', 'logis', 'llogis', 'exp', 'gamma', and 'weibull'.
#' 'empirical' returns the empirical distribution function of \code{data}, 'kde' applies
#' (normal) kernel density estimation to \code{data}, while 'norm', 'lnorm', 'logis',
#' 'llogis', 'exp', 'gamma', and 'weibull' correspond to the normal, log-normal, logistic,
#' log-logistic, exponential, gamma, and Weibull distributions, respectively.
#'
#' By default, \code{dist = 'empirical'}, in which case the distribution is estimated
#' empirically from \code{data}. This is only recommended if there are at least 100 values
#' in \code{data}, and a warning message is returned otherwise.
#'
#' \code{n_thres} is the minimum number of observations required to fit the distribution.
#' The default is \code{n_thres = 20}. If the number of values in \code{data} is
#' smaller than \code{na_thres}, an error is returned. This guards against over-fitting,
#' which can result in distributions that do not generalise well out-of-sample.
#'
#' \code{method} determines the method used to estimate the distribution parameters.
#' This argument is redundant if \code{dist = 'empirical'} or \code{dist = 'kde'}.
#' Otherwise, \code{fit_dist} essentially provides a wrapper for \code{\link[fitdistrplus]{fitdist}},
#' and further details can be found in the corresponding documentation. Additional arguments
#' to \code{\link[fitdistrplus]{fitdist}} can also be specified via \code{...}.
#' Where relevant, the default is to estimate parameters using maximum likelihood estimation.
#' Parameter estimation is also possible using L-moment matching (\code{method = 'lmme'}),
#' for all distribution choices except the log-logistic distribution.
#'
#'
#' @return
#' A list containing the estimated distribution function, its parameters,
#' and Kolmogorov-Smirnov goodness-of-fit statistics.
#'
#' @seealso \code{\link[fitdistrplus]{fitdist}}
#'
#' @examples
#' N <- 1000
#' shape <- 3
#' rate <- 2
#'
#'
#' ### gamma distribution
#'
#' # maximum likelihood
#' data <- rgamma(N, shape, rate)
#' out <- fit_dist(data, dist = "gamma")
#' hist(data, breaks = 30, probability = TRUE)
#' lines(seq(0, 10, 0.01), dgamma(seq(0, 10, 0.01), out$params[1], out$params[2]), col = "blue")
#'
#' # method of moments
#' out <- fit_dist(data, dist = "gamma", method = "mme")
#' hist(data, breaks = 30, probability = TRUE)
#' lines(seq(0, 10, 0.01), dgamma(seq(0, 10, 0.01), out$params[1], out$params[2]), col = "blue")
#'
#' # method of l-moments
#' out <- fit_dist(data, dist = "gamma", method = "lmme")
#' hist(data, breaks = 30, probability = TRUE)
#' lines(seq(0, 10, 0.01), dgamma(seq(0, 10, 0.01), out$params[1], out$params[2]), col = "blue")
#'
#'
#' ## weibull distribution
#'
#' # maximum likelihood
#' data <- rweibull(N, shape, 1/rate)
#' out <- fit_dist(data, dist = "weibull")
#' hist(data, breaks = 30, probability = TRUE)
#' lines(seq(0, 10, 0.01), dweibull(seq(0, 10, 0.01), out$params[1], out$params[2]), col = "blue")
#'
#' # method of l-moments
#' out <- fit_dist(data, dist = "weibull", method = "lmme")
#' hist(data, breaks = 30, probability = TRUE)
#' lines(seq(0, 10, 0.01), dweibull(seq(0, 10, 0.01), out$params[1], out$params[2]), col = "blue")
#'
#'
#' ## exponential distribution
#'
#' # method of moments
#' out <- fit_dist(data, dist = "exp", method = "mme")
#' hist(data, breaks = 30, probability = TRUE)
#' lines(seq(0, 10, 0.01), dexp(seq(0, 10, 0.01), out$params), col = "blue")
#'
#'
#' ## logistic distribution
#'
#' # maximum likelihood
#' data <- rlogis(N, shape, rate)
#' out <- fit_dist(data, dist = "logis")
#' hist(data, breaks = 30, probability = TRUE)
#' lines(seq(-10, 20, 0.01), dlogis(seq(-10, 20, 0.01), out$params[1], out$params[2]), col = "blue")
#'
#'
#' @name fit_dist
#' @importFrom stats bw.nrd ecdf pnorm qnorm shapiro.test ks.test
#' @importFrom flexsurv pllogis dllogis
NULL

#' @rdname fit_dist
#' @export
fit_dist <- function(data, dist, method = "mle", n_thres = 20, ...){

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
    if (method == "lmme") {

      samp_mom <- lmom::samlmu(data)
      if (dist == "norm") {
        params <- lmom::pelnor(samp_mom)
        names(params) <- c("mean", "sd")
      } else if (dist == "lnorm") {
        params <- lmom::pelln3(samp_mom, bound = 0)
        params <- params[2:3]
        names(params) <- c("meanlog", "sdlog")
      } else if (dist == "logis") {
        params <- samp_mom[1:2]
        names(params) <- c("location", "scale")
      } else if (dist == "llogis") {
        stop("method 'lmme' is not available for the log-logistic distribution")
      } else if (dist == "exp") {
        params <- 1/samp_mom[1]
        names(params) <- "rate"
      } else if (dist == "gamma") {
        params <- lmom::pelgam(samp_mom)
        params[2] <- 1/params[2]
        names(params) <- c("shape", "rate")
      } else if (dist == "weibull") {
        params <- lmom::pelwei(samp_mom, bound = 0)
        params <- params[3:2]
        names(params) <- c("shape", "scale")
      }
      F_x <- function(x, params) do.call(paste0("p", dist), c(list(q = x), as.list(params))) # cdf
      f_x <- function(x, params) do.call(paste0("d", dist), c(list(x = x), as.list(params))) # pdf
      fit_props['aic'] <- 2*length(params) - 2*sum(log(f_x(data, params)))

    } else {
      fit <- try(fitdistrplus::fitdist(data = data, distr = dist, method = method, ...))
      if (!inherits(fit, "try-error")){
        params <- fit$estimate # parameters
        F_x <- function(x, params) do.call(paste0("p", dist), c(list(q = x), as.list(params))) # cdf
        fit_props['aic'] <- fit$aic # aic
      } else {
        warning("distribution fitting failed")
        return(list(F_x = function(x, params) NA, params = NULL, fit_props = fit_props))
      }
    }
  }

  pit <- F_x(data, params)
  fit_props['ks_pval'] <- ks.test(pit, "punif")$p.value # ks p-value

  return(list(F_x = F_x, params = params, fit = fit_props))
}


check_distribution <- function(inputs) {

  data <- inputs$data[!is.na(inputs$data)]

  if (!(inputs$dist %in% c("empirical", "kde", "norm", "lnorm",
                    "logis", "llogis", "exp", "gamma", "weibull"))) {
    stop("dist must be one of 'empirical', 'kde', 'norm', 'lnorm',
         'logis', 'llogis', 'exp', 'gamma', 'weibull'")
  }

  if (!(inputs$method %in% c("mle", "mme", "qme", "mge", "mse", "lmme"))) {
    stop("method must be one of 'mle', 'mme', 'qme', 'mge', 'mse', 'lmme'")
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


