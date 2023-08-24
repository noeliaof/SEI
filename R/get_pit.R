#' @title Calculate probability integral transform values
#'
#' @description Function to estimate the cumulative distribution function (CDF)
#' from a set of observations, and return the corresponding probability integral
#' transform (PIT) values.
#'
#' @param ref_data numeric vector from which to estimate the CDF.
#' @param new_data numeric vector from which to calculate the PIT values.
#' @param dist string; distribution used to estimate the CDF.
#' @param return_fit logical; return parameters and goodness-of-fit statistics.
#'
#' @details
#' \code{dist} specifies the distribution used to estimate the cumulative distribution
#' function of the observations. By default, \code{dist = "empirical"}, in which case
#' the CDF is estimated empirically from the values \code{ref_data}. This is only
#' recommended if there are at least 100 values in \code{ref_data}, and a warning
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
#' @return
#' A vector of PIT values if return_fit = F, or, if return_fit = T, a list containing
#' the estimated CDF (\code{F_x}), the corresponding parameters (\code{params}), and
#' properties of the fit (\code{fit_props}).
#'
#' @author Sam Allen, Noelia Otero
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
#' @name get_pit
NULL

#' @rdname get_pit
#' @export
get_pit <- function(ref_data,
                    new_data,
                    dist = "empirical",
                    return_fit = FALSE) {

  fit <- fit_dist(as.vector(ref_data), dist)
  fit$pit <- fit$F_x(as.vector(new_data), fit$params)

  if (return_fit) {
    return(fit)
  } else {
    return(fit$pit)
  }
}


