#' @title Fit a distribution to data
#'
#' @description Function to fit a specified distribution to a vector of data.
#' Returns the estimated distribution and relevant goodness-of-fit statistics.
#'
#'
#' @param dist character string specifying the distribution to be fit to the data;
#'  one of `'empirical'`, `'kde'`, `'norm'`, `'lnorm'`, `'logis'`, `'llogis'`,
#'  `'exp'`, `'gamma'`, and `'weibull'`.
#' @param preds data frame of predictor variables on which the estimated distribution
#'  should depend.
#' @param n_thres minimum number of data points required to estimate the distribution;
#'  default is 10.
#' @param ... additional arguments to be passed to \code{\link[fitdistrplus]{fitdist}} or
#'  \code{\link[gamlss]{gamlss}}
#' @inheritParams fitdistrplus::fitdist
#'
#' @details
#' This has been adapted from code available at
#' \url{https://github.com/WillemMaetens/standaRdized}.
#'
#' \code{data} is a numeric vector of data from which the distribution is to be estimated.
#'
#' \code{dist} is the specified distribution to be fit to \code{data}. This must be one of
#' \code{'empirical'}, \code{'kde'}, \code{'norm'}, \code{'lnorm'}, \code{'logis'}, \code{'llogis'},
#' \code{'exp'}, \code{'gamma'}, and \code{'weibull'}. These correspond to the following
#' distributions: \code{'empirical'} returns the empirical distribution function of \code{data},
#' \code{'kde'} applies (normal) kernel density estimation to \code{data}, while \code{'norm'},
#' \code{'lnorm'}, \code{'logis'}, \code{'llogis'}, \code{'exp'}, \code{'gamma'}, and
#' \code{'weibull'} correspond to the normal, log-normal, logistic, log-logistic, exponential,
#' gamma, and Weibull distributions, respectively.
#'
#' By default, \code{dist = 'empirical'}, in which case the distribution is estimated
#' empirically from \code{data}. This is only recommended if there are at least 100 values
#' in \code{data}, and a warning message is returned otherwise. Parametric distributions
#' are more appropriate when there is relatively little data,
#' or good reason to expect that the data follows a particular distribution.
#' Kernel density estimation \code{dist = 'kde'} provides a flexible compromise between
#' using empirical methods and parametric distributions.
#'
#' \code{n_thres} is the minimum number of observations required to fit the distribution.
#' The default is \code{n_thres = 10}. If the number of values in \code{data} is
#' smaller than \code{na_thres}, an error is returned. This guards against over-fitting,
#' which can result in distributions that do not generalise well out-of-sample.
#'
#' \code{method} specifies the method used to estimate the distribution parameters.
#' This argument is redundant if \code{dist = 'empirical'} or \code{dist = 'kde'}.
#' Otherwise, \code{fit_dist} essentially provides a wrapper for
#' \code{\link[fitdistrplus]{fitdist}}, and further details can be found in the corresponding
#' documentation. Additional arguments to \code{\link[fitdistrplus]{fitdist}}
#' can also be specified via \code{...}.
#' Where relevant, the default is to estimate parameters using maximum likelihood estimation,
#' \code{method = "mle"}, though several alternative methods are also available; see
#' \code{\link[fitdistrplus]{fitdist}}. Parameter estimation is also possible using L-moment
#' matching (\code{method = 'lmme'}), for all distribution choices except the log-logistic
#' distribution. In this case, \code{fit_dist} is essentially a wrapper for the
#' \code{\link[lmom]{lmom}} package.
#'
#' The distribution can also be non-stationary, by depending on some predictor variables or covariates.
#' These predictors can be included via the argument \code{preds}, which should be a data frame
#' with a separate column for each predictor, and with a number of rows equal to the length of
#'  \code{data}. In this case, a Generalized Additive Model for
#' Location, Scale, and Shape (GAMLSS) is fit to \code{data} using the predictors in \code{preds}.
#' It is assumed that the mean of the distribution depends linearly on all of the predictors.
#' Variable arguments in \code{...} can also be used to specify relationships between the
#' scale and shape parameters of the distribution and the predictors; see examples below.
#' In this case, \code{fit_dist} is essentially a wrapper for \code{\link[gamlss]{gamlss}},
#' and users are referred to the corresponding documentation for further implementation details.
#'
#'
#' @return
#' A list containing the estimated distribution function (\code{F_x}), its parameters
#' (\code{params}), and properties of the fit such as the AIC and
#' Kolmogorov-Smirnov goodness-of-fit statistic (\code{fit}). If the estimated distribution
#' function depends on covariates, then the \code{gamlss} model fit is returned as the
#' parameters.
#'
#'
#' @references
#'
#' Rigby, R. A., & Stasinopoulos, D. M. (2005):
#' `Generalized additive models for location, scale and shape',
#' \emph{Journal of the Royal Statistical Society Series C: Applied Statistics} 54, 507-554.
#' \doi{https://doi.org/10.1111/j.1467-9876.2005.00510.x}
#'
#' Delignette-Muller, M. L., & Dutang, C. (2015):
#' `fitdistrplus: An R package for fitting distributions',
#' \emph{Journal of Statistical Software} 64, 1-34.
#' \doi{https://doi.org/10.18637/jss.v064.i04}
#'
#' Allen, S. & N. Otero (2023):
#' `Standardised indices to monitor energy droughts',
#' \emph{Renewable Energy} 217, 119206.
#' \doi{https://doi.org/10.1016/j.renene.2023.119206}
#'
#'
#' @author Sam Allen, Noelia Otero
#'
#'
#' @seealso \code{\link[fitdistrplus]{fitdist}} \code{\link[gamlss]{gamlss}} \code{\link[lmom]{lmom}}
#'
#'
#' @examples
#' N <- 1000
#' shape <- 3
#' rate <- 2
#'
#' x <- seq(0, 10, 0.01)
#'
#' ### gamma distribution
#'
#' # maximum likelihood
#' data <- rgamma(N, shape, rate)
#' out <- fit_dist(data, dist = "gamma")
#' hist(data, breaks = 30, probability = TRUE)
#' lines(x, dgamma(x, out$params[1], out$params[2]), col = "blue")
#'
#' # method of moments
#' out <- fit_dist(data, dist = "gamma", method = "mme")
#' hist(data, breaks = 30, probability = TRUE)
#' lines(x, dgamma(x, out$params[1], out$params[2]), col = "blue")
#'
#' # method of l-moments
#' out <- fit_dist(data, dist = "gamma", method = "lmme")
#' hist(data, breaks = 30, probability = TRUE)
#' lines(x, dgamma(x, out$params[1], out$params[2]), col = "blue")
#'
#'
#' ## weibull distribution
#'
#' # maximum likelihood
#' data <- rweibull(N, shape, 1/rate)
#' out <- fit_dist(data, dist = "weibull")
#' hist(data, breaks = 30, probability = TRUE)
#' lines(x, dweibull(x, out$params[1], out$params[2]), col = "blue")
#'
#' # method of l-moments
#' out <- fit_dist(data, dist = "weibull", method = "lmme")
#' hist(data, breaks = 30, probability = TRUE)
#' lines(x, dweibull(x, out$params[1], out$params[2]), col = "blue")
#'
#'
#' ## exponential distribution
#'
#' # method of moments
#' out <- fit_dist(data, dist = "exp", method = "mme")
#' hist(data, breaks = 30, probability = TRUE)
#' lines(x, dexp(x, out$params), col = "blue")
#'
#'
#' ## logistic distribution
#'
#' x <- seq(-10, 20, 0.01)
#'
#' # maximum likelihood
#' data <- rlogis(N, shape, rate)
#' out <- fit_dist(data, dist = "logis")
#' hist(data, breaks = 30, probability = TRUE)
#' lines(x, dlogis(x, out$params[1], out$params[2]), col = "blue")
#'
#'
#'
#' ##### non-stationary estimation using gamlss
#'
#' ## normal distribution
#' x <- seq(-10, 20, length.out = N)
#' data <- rnorm(N, x + shape, exp(x/10))
#' plot(data)
#' preds <- data.frame(t = x)
#'
#' out_st <- fit_dist(data, dist = "norm")
#' out_nst <- fit_dist(data, dist = "norm", preds = preds)
#' out_nst2 <- fit_dist(data, dist = "norm", preds = preds, sigma.formula = ~ .)
#'
#' # pit values without trend
#' pit_st <- out_st$F_x(data, out_st$params)
#' hist(pit_st, breaks = 30, probability = TRUE, main = "No trend")
#' abline(1, 0, col = "red", lty = "dotted")
#' # pit values with trend in mean
#' pit_nst <- out_nst$F_x(data, out_nst$params, preds)
#' hist(pit_nst, breaks = 30, probability = TRUE, main = "Trend in mean")
#' abline(1, 0, col = "red", lty = "dotted")
#' # pit values with trend in mean and sd
#' pit_nst2 <- out_nst2$F_x(data, out_nst2$params, preds)
#' hist(pit_nst2, breaks = 30, probability = TRUE, main = "Trend in mean and standard deviation")
#' abline(1, 0, col = "red", lty = "dotted")
#'
#'
#' ## log normal distribution
#' x <- seq(0.01, 10, length.out = N)
#' data <- rlnorm(N, (x + shape)/3, 1/rate)
#' plot(data)
#' preds <- data.frame(t = x)
#'
#' out <- fit_dist(data, dist = "lnorm", preds = preds)
#' pit <- out$F_x(data, out$params, preds)
#' hist(pit, breaks = 30, probability = TRUE, main = "PIT values for non-stationary fit")
#' abline(1, 0, col = "red", lty = "dotted")
#'
#'
#' @name fit_dist
#' @importFrom stats bw.nrd ecdf pnorm qnorm predict ks.test
#' @importFrom flexsurv pllogis dllogis
#' @importFrom gamlss.dist NO LOGNO LO EXP GA WEI pNO pLOGNO pLO pEXP pGA pWEI
NULL

#' @rdname fit_dist
#' @export
fit_dist <- function(data, dist, method = "mle", preds = NULL, n_thres = 10, ...) {

  # check inputs
  inputs <- as.list(environment())
  check_distribution(inputs)

  # format preds_ref and preds_new
  if (!is.null(preds)) {
    if (is.matrix(preds) || is.vector(preds)) preds <- as.data.frame(preds)
  }


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
  } else if (method == "lmme") {
      params <- fit_dist_lmme(data = data, dist = dist)
      F_x <- function(x, params) do.call(paste0("p", dist), c(list(q = x), as.list(params))) # cdf
      f_x <- function(x, params) do.call(paste0("d", dist), c(list(x = x), as.list(params))) # pdf
      fit_props['aic'] <- 2*length(params) - 2*sum(log(f_x(data, params)))
  } else if (!is.null(preds)) {
    fit <- try(fit_dist_gamlss(data = data, preds = preds, dist = dist, ...))
    if (!inherits(fit, "try-error")){
      params <- fit$params # parameters
      F_x <- fit$F_x # cdf
      fit_props['aic'] <- fit$aic # aic
    } else {
      warning("distribution fitting failed")
      return(list(F_x = function(x, params) NA, params = NULL, fit_props = fit_props))
    }
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

  if (is.null(preds)) {
    pit <- F_x(data, params)
  } else {
    pit <- F_x(data, params, preds)
  }
  fit_props['ks_pval'] <- ks.test(pit, "punif")$p.value # ks p-value

  return(list(F_x = F_x, params = params, fit = fit_props))
}


# estimate distribution parameters using l-moment matching
fit_dist_lmme <- function(data, dist) {
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
  return(params)
}


# estimate non-stationary distributions using gamlss
fit_dist_gamlss <- function(data, preds, dist, ...) {
  data_df <- data.frame(obs = data, preds)
  if (dist == "norm") {
    fit <- gamlss::gamlss(obs ~ ., data = data_df, trace = FALSE, ...)
    F_x <- function(x, params, z) {
      mu <- predict(params, new.data = z, what = "mu", type = "response")
      sig <- predict(params, new.data = z, what = "sigma", type = "response")
      pNO(x, mu = mu, sigma = sig)
    }
  } else if (dist == "lnorm") {
    fit <- gamlss::gamlss(obs ~ ., data = data_df, family = LOGNO(), ...)
    F_x <- function(x, params, z) {
      mu <- predict(params, new.data = z, what = "mu", type = "response")
      sig <- predict(params, new.data = z, what = "sigma", type = "response")
      pLOGNO(x, mu = mu, sigma = sig)
    }
  } else if (dist == "logis") {
    fit <- gamlss::gamlss(obs ~ ., data = data_df, family = LO(), ...)
  } else if (dist == "llogis") {
    stop("non-stationary estimation is not available for the log-logistic distribution")
  } else if (dist == "exp") {
    fit <- gamlss::gamlss(obs ~ ., data = data_df, family = EXP(), ...)
    F_x <- function(x, params, z) {
      mu <- predict(params, new.data = z, what = "mu", type = "response")
      pEXP(x, mu = mu)
    }
  } else if (dist == "gamma") {
    fit <- gamlss::gamlss(obs ~ ., data = data_df, family = GA(), ...)
    F_x <- function(x, params, z) {
      mu <- predict(params, new.data = z, what = "mu", type = "response")
      sig <- predict(params, new.data = z, what = "sigma", type = "response")
      pGA(x, mu = mu, sigma = sig)
    }
  } else if (dist == "weibull") {
    fit <- gamlss::gamlss(obs ~ ., data = data_df, family = WEI(), ...)
    F_x <- function(x, params, z) {
      mu <- predict(params, new.data = z, what = "mu", type = "response")
      sig <- predict(params, new.data = z, what = "sigma", type = "response")
      pWEI(x, mu = mu, sigma = sig)
    }
  }
  aic <- fit$aic
  out <- list(params = fit, F_x = F_x, aic = aic)
  return(out)
}


# check the inputs of fit_dist
check_distribution <- function(inputs) {

  data <- inputs$data[!is.na(inputs$data)]

  # dist
  if (!(inputs$dist %in% c("empirical", "kde", "norm", "lnorm",
                    "logis", "llogis", "exp", "gamma", "weibull"))) {
    stop("dist must be one of 'empirical', 'kde', 'norm', 'lnorm',
         'logis', 'llogis', 'exp', 'gamma', 'weibull'")
  } else if (inputs$dist == "empirical") {
    if (length(data) < 100) {
      warning("using the empirical distribution is only recommended when at least
              100 values are available when fitting the distribution")
    }
  } else if (inputs$dist %in% c("lnorm", "llogis", "exp", "gamma", "weibull")) {
    if (any(data < 0)) {
      stop(paste("the", inputs$dist, "distribution has positive support, but the data contains negative values"))
    } else if (any(data == 0)) {
      stop(paste("the", inputs$dist, "distribution has positive support, but the data contains values equal to zero"))
    }
  }

  # method
  if (!(inputs$method %in% c("mle", "mme", "qme", "mge", "mse", "lmme"))) {
    stop("method must be one of 'mle', 'mme', 'qme', 'mge', 'mse', 'lmme'")
  }

  # preds
  if (!is.null(inputs$preds)) {
    if (!is.data.frame(inputs$preds) & !is.vector(inputs$preds) & !is.matrix(inputs$preds)) {
      stop("'preds' must be a data frame, vector, or matrix")
    } else {
      if (is.matrix(inputs$preds) | is.vector(inputs$preds)) {
        inputs$preds <- as.data.frame(inputs$preds)
      }
      if (nrow(inputs$preds) != length(inputs$data)) {
        stop ("'preds' must have the same number of rows as the length of 'data'")
      }
    }
  }

  # n_thres
  if (!is.numeric(inputs$n_thres) | length(inputs$n_thres) > 1) {
    stop("'n_thres' must be a single numeric value")
  }

}

