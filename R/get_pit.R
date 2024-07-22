#' @title Calculate probability integral transform values
#'
#' @description Function to estimate the cumulative distribution function (CDF)
#' from a set of observations, and return the corresponding probability integral
#' transform (PIT) values.
#'
#'
#' @param x_ref numeric vector from which to estimate the CDF.
#' @param x_new numeric vector from which to calculate the PIT values.
#' @param preds_ref data frame of predictor variables if a non-stationary distribution is
#'  to be estimated, corresponding to the reference observations \code{x_ref}.
#' @param preds_new data frame of predictor variables if a non-stationary distribution is
#'  modelled, corresponding to the new observations \code{x_new}.
#' @param return_fit logical specifying whether to return parameters and goodness-of-fit
#'  statistics for the distribution fit.
#' @param lower,upper numeric values specifying the lower and upper bounds at which censoring
#'  is to be performed.
#' @param cens values to assign to censored values; either a string (`'normal'` or `'prob'`),
#'  corresponding to common choices, or a custom numeric vector of length two.
#' @inheritParams fit_dist
#'
#'
#' @details
#' \code{dist} specifies the distribution used to estimate the cumulative distribution
#' function of the observations. By default, \code{dist = "empirical"}, in which case
#' the CDF is estimated empirically from the values \code{x_ref}. This is only
#' recommended if there are at least 100 values in \code{x_ref}, and a warning
#' message is returned otherwise.
#'
#' Parametric distributions are more appropriate when there is relatively little data,
#' and good reason to expect that the data follows a particular distribution. To
#' check that the chosen parametric distribution is appropriate, the argument
#' \code{return_fit} can be used to return the estimated parameters of the
#' distribution, as well as Kolmogorov-Smirnov goodness-of-fit test statistics.
#'
#' A flexible compromise between using empirical methods and parametric distributions is to
#' use kernel density estimation, \code{dist = "kde"}.
#'
#' \code{dist} must be one of: 'empirical' (the empirical distribution given \code{data}),
#' 'kde' (kernel density estimation), norm', 'lnorm', 'logis', 'llogis', 'exp', 'gamma', and 'weibull'.
#' For the parametric distributions, parameters are estimated using maximum likelihood estimation.
#'
#'
#' @return
#' A vector of PIT values if return_fit = F, or, if return_fit = T, a list containing
#' the estimated CDF (\code{F_x}), the corresponding parameters (\code{params}), and
#' properties of the fit (\code{fit_props}).
#'
#'
#' @author Sam Allen, Noelia Otero
#'
#'
#' @examples
#' N <- 1000
#' shape <- 3
#' rate <- 2
#'
#' x_ref <- rgamma(N, shape, rate)
#' x_new <- rgamma(N, shape, rate)
#'
#' # empirical distribution
#' pit <- get_pit(x_ref, x_new)
#' hist(pit)
#'
#' # gamma distribution
#' pit <- get_pit(x_ref, x_new, dist = "gamma", return_fit = TRUE)
#' hist(pit$pit)
#'
#' hist(x_ref, breaks = 30, probability = TRUE)
#' lines(seq(0, 10, 0.01), dgamma(seq(0, 10, 0.01), pit$params[1], pit$params[2]), col = "blue")
#'
#'
#' # weibull distribution
#' pit <- get_pit(x_ref, x_new, dist = "weibull", return_fit = TRUE)
#' hist(pit$pit)
#'
#' hist(x_ref, breaks = 30, probability = TRUE)
#' lines(seq(0, 10, 0.01), dweibull(seq(0, 10, 0.01), pit$params[1], pit$params[2]), col = "blue")
#'
#'
#' # exponential distribution
#' pit <- get_pit(x_ref, x_new, dist = "exp", return_fit = TRUE)
#' hist(pit$pit)
#'
#' hist(x_ref, breaks = 30, probability = TRUE)
#' lines(seq(0, 10, 0.01), dexp(seq(0, 10, 0.01), pit$params[1]), col = "blue")
#'
#'
#' # gamma distribution with censoring
#' x_ref <- c(x_ref, numeric(N))
#' pit <- get_pit(x_ref, dist = "gamma", lower = 0, cens = "prob")
#' hist(pit)
#' mean(pit) # = 1/2
#' mean(qnorm(pit)) # != 0
#'
#' pit <- get_pit(x_ref, dist = "gamma", lower = 0, cens = "normal")
#' hist(qnorm(pit))
#' mean(pit) # != 1/2
#' mean(qnorm(pit)) # = 0
#'
#'
#' ## normal distribution with trend in mean
#' x <- seq(-10, 20, length.out = N)
#' x_ref <- rnorm(N, x + shape, 2)
#' plot(x_ref)
#' preds <- data.frame(t = x)
#'
#' pit <- get_pit(x_ref, preds_ref = preds, dist = "norm")
#' hist(pit)
#'
#' ## normal distribution with trend in mean and standard deviation
#' x_ref <- rnorm(N, x + shape, exp(x/10))
#' plot(x_ref)
#' preds <- data.frame(t = x)
#'
#' pit <- get_pit(x_ref, preds_ref = preds, dist = "norm", sigma.formula = ~ .)
#' hist(pit)
#'
#'
#' @name get_pit
#' @importFrom stats dnorm pnorm qnorm na.omit
NULL

#' @rdname get_pit
#' @export
get_pit <- function(x_ref,
                    x_new = NULL,
                    preds_ref = NULL,
                    preds_new = NULL,
                    dist = "empirical",
                    method = "mle",
                    return_fit = FALSE,
                    lower = -Inf,
                    upper = Inf,
                    cens = NULL,
                    n_thres = 20,
                    ...) {

  if (is.null(x_new)) x_new <- x_ref
  x_ref <- as.vector(x_ref)
  x_new <- as.vector(x_new)
  pit_na <- rep(NA, length(x_new))

  # remove na's
  x_ref <- na.omit(x_ref)
  na_ind <- is.na(x_new)
  x_new <- na.omit(x_new)
  pit <- rep(NA, length(x_new))

  # find indices where data is censored
  ref_cind <- (x_ref <= lower) | (x_ref >= upper)
  new_cind <- (x_new <= lower) | (x_new >= upper)

  if (!is.null(preds_ref)) preds_ref <- preds_ref[!ref_cind, , drop = FALSE]
  if (!is.null(preds_new)) preds_new <- preds_new[!new_cind, , drop = FALSE]

  # fit distribution to uncensored data

  fit <- fit_dist(x_ref[!ref_cind], dist, method = method, preds = preds_ref, n_thres = n_thres, ...)

  # get pit values
  if (is.null(preds_ref)) {
    pit[!new_cind] <- fit$F_x(x_new[!new_cind], fit$params)
  } else {
    pit[!new_cind] <- fit$F_x(x_new[!new_cind], fit$params, preds_new)
  }

  # censoring
  if (any(ref_cind)) pit <- pit_cens(pit, x_ref, x_new, lower, upper, cens)

  # reinsert NAs for missing values
  pit_na[!na_ind] <- pit
  fit$pit <- pit_na

  if (return_fit) {
    return(fit)
  } else {
    return(fit$pit)
  }
}


pit_cens <- function(pit, x_ref, x_new, lower, upper, cens) {

  low_ind <- (x_new <= lower)
  upp_ind <- (x_new >= upper)
  cen_ind <- low_ind | upp_ind

  p_l <- mean(x_ref <= lower)
  p_u <- mean(x_ref >= upper)
  pit[!cen_ind] <- p_l + (1 - p_l - p_u)*pit[!cen_ind]

  if (lower != -Inf && upper != Inf) {
    if (is.numeric(cens)) {
      x_l <- cens[1]
      x_u <- cens[2]
    } else {
      stop("if the distribution is censored above and below, 'cens' must be a
              numeric vector containing the two censoring points")
    }
  } else {
    if (is.null(cens)) {
      x_l <- p_l
      x_u <- p_u
    } else if (cens == "normal") {
      x_l <- pnorm( - dnorm( qnorm(p_l) ) / p_l)
      x_u <- pnorm( dnorm( qnorm(1 - p_u) ) / p_u)
    } else if (cens == "prob") {
      x_l <- p_l/2
      x_u <- 1 - p_u/2
    }
  }
  pit[low_ind] <- x_l
  pit[upp_ind] <- x_u

  return(pit)
}

