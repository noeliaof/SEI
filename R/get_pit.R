#' @title Calculate probability integral transform values
#'
#' @description Function to estimate the cumulative distribution function (CDF)
#' from a set of observations, and return the corresponding probability integral
#' transform (PIT) values.
#'
#'
#' @param x_ref numeric vector from which to estimate the CDF.
#' @param x_new numeric vector from which to calculate the PIT values.
#' @param preds_ref data frame of predictor variables on which the estimated distribution
#'  should depend, corresponding to the reference observations \code{x_ref}.
#' @param preds_new data frame of predictor variables on which the estimated distribution
#'  should depend, corresponding to the new observations \code{x_new}.
#' @param return_fit logical specifying whether to return parameters and goodness-of-fit
#'  statistics for the distribution fit.
#' @param lower,upper numeric values specifying the lower and upper bounds at which the
#'  values in \code{x_ref} and \code{x_new} are censored.
#' @param cens method used to deal with censoring of the PIT values; either a string
#'  (`'none'`, `'normal'` or `'prob'`), corresponding to common choices, or a custom numeric value.
#' @inheritParams fit_dist
#'
#'
#' @details
#'
#' **Continuous data**
#'
#' If \eqn{X} is a continuous random variable with cumulative distribution function (CDF)
#' \eqn{F}, then the probability integral transform (PIT) \eqn{F(X)} is uniformly distributed
#' between 0 and 1. Given a vector \eqn{x_{1}, \dots, x_{n}} of realisations of \eqn{X},
#' \code{get_pit} produces an estimate \eqn{\hat{F}} of the CDF \eqn{F}, and returns a
#' vector of PIT values corresponding to another set of realisations \eqn{z_{1}, \dots, z_{N}},
#' \deqn{\hat{F}(z_{1}), \dots, \hat{F}(z_{n}).}
#'
#' \code{x_ref} contains the values \eqn{x_{1}, \dots, x_{n}} from which the CDF estimate
#' \eqn{\hat{F}} is obtained. \code{x_new} contains the values \eqn{z_{1}, \dots, z_{n}}
#' from which the PIT values \eqn{\hat{F}(z_{1}), \dots, \hat{F}(z_{n})} are calculated.
#' By default, \code{x_ref} and \code{x_new} are the same, so that the PIT values are
#' calculated in-sample.
#'
#' To estimate the distribution, \code{get_pit} calls \code{\link{fit_dist}}. The arguments
#' \code{dist}, \code{method} and \code{n_thres} are
#' documented in detail in the corresponding help page.
#'
#' To check that the chosen distribution adequately fits the data, the argument
#' \code{return_fit = TRUE} can be used to return the estimated parameters of the
#' distribution, as well as properties of the fit such as the AIC and a p-value
#' for the Kolmogorov-Smirnov goodness-of-fit test.
#'
#'
#' **Non-stationary distributions**
#'
#' The estimated distribution can also be non-stationary, by depending on some predictor variables or covariates.
#' These predictors can be included via the arguments \code{preds_ref} and \code{preds_new},
#' which should be data frames with a separate column for each predictor, and with
#' numbers of rows equal to the lengths of \code{x_ref} and \code{x_new}, respectively.
#' In this case, a Generalized Additive Model for Location, Scale, and Shape (GAMLSS) is
#' fit to \code{x_ref} using the predictors in \code{preds_ref}.
#' The PIT values corresponding to \code{x_new} are then calculated by applying the
#' estimated distribution with predictors \code{preds_new}.
#' If a non-stationary distribution is to be estimated, both \code{preds_ref} and
#' \code{preds_new} must be provided. By default, \code{preds_new} is assumed to be
#' the same as \code{preds_ref}, to align with \code{x_new} being the same as \code{x_ref}.
#'
#'
#' **Censored data**
#'
#' If the random variable \eqn{X} is not continuous, the PIT will not be uniformly distributed.
#' A relevant case is when \eqn{X} is censored. For example, precipitation is censored below
#' at zero. This results in several PIT values being equal to \eqn{F(0)}. The \code{lower}
#' and \code{upper} arguments to \code{get_pit} allow the user to specify the lower and upper
#' bounds at which the data is censored; the default is \code{lower = -Inf} and \code{upper = Inf},
#' i.e. there is no censoring.
#'
#' If the PIT values are used to construct standardised indices, this censoring can lead to
#' unintuitive index values.
#' To deal with censored data, it has been proposed to map the PIT values of the censored
#' values to a different constant \eqn{c}; see references. For example, for precipitation, the PIT
#' values would become
#' \deqn{F(X) \quad \text{if} \quad X > 0,}
#' \deqn{c \quad \text{if} \quad X = 0.}
#' The constant \eqn{c} can be chosen so that the PIT values satisfy some desired property.
#' For example, if \eqn{F(X)} is uniformly distributed between 0 and 1, then it has mean equal
#' to \eqn{1/2}. Hence, \eqn{c} could be chosen such that the mean of the PIT values of the
#' censored distribution are equal to \eqn{1/2}.
#' Alternatively, if \eqn{F(X)} is uniformly distributed between 0 and 1, then
#' the transformed PIT value \eqn{\Phi^{-1}(F(X))} (where \eqn{\Phi^{-1}} is the quantile function
#' of the standard normal distribution) follows a standard normal distribution, and therefore
#' has mean equal to 0. The constant \eqn{c} could therefore be chosen such that the mean of the
#' transformed PIT values of the censored distribution are equal to 0.
#'
#' The argument \code{cens} in \code{get_pit} can be used to treat censored data. \code{cens}
#' can be one of four options: a single numeric value containing the value \eqn{c} at which to
#' assign the PIT values of the censored realisations; the string \code{'none'} if no censoring
#' is to be performed; the string \code{'prob'} if \eqn{c}
#' is to be chosen automatically so that the mean of the PIT values is equal to \eqn{1/2};
#' or the string \code{'normal'} if \eqn{c} is to be chosen automatically so that the mean of
#' the transformed PIT values is equal to 0. If the data is censored both above and below,
#' then \code{cens} must be a numeric vector of length two, specifying the values to assign
#' the realisations that are censored both below and above.
#'
#' When the data is censored, \code{dist} corresponds to the distribution used to estimate the
#' uncensored realisations, e.g. positive precipitations. The probability of being at the boundary
#' points is estimated using the relative frequency of censored observations in \code{x_ref}.
#'
#'
#' @return
#' A vector of PIT values if \code{return_fit = FALSE}, or, if \code{return_fit = TRUE},
#' a list containing the estimated distribution function (\code{F_x}), its parameters
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
#' \doi{10.1111/j.1467-9876.2005.00510.x}
#'
#' Stagge, J. H., Tallaksen, L. M., Gudmundsson, L., Van Loon, A. F., & Stahl, K. (2015):
#' `Candidate distributions for climatological drought indices (SPI and SPEI)',
#' \emph{International Journal of Climatology} 35, 4027-4040.
#' \doi{10.1002/joc.4267}
#'
#' Allen, S. & N. Otero (2023):
#' `Calculating standardised indices using SEI',
#' \emph{EarthArXiv pre-print}.
#' \doi{10.31223/X5GM4G}
#'
#'
#' @author Sam Allen, Noelia Otero
#'
#'
#' @seealso \code{\link{fit_dist}}
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
#' # sigma.formula is an optional argument in the gamlss::gamlss function
#'
#'
#' @name get_pit
#' @importFrom stats dnorm pnorm qnorm na.omit
NULL

#' @rdname get_pit
#' @export
get_pit <- function(x_ref,
                    x_new = x_ref,
                    dist = "empirical",
                    preds_ref = NULL,
                    preds_new = preds_ref,
                    method = "mle",
                    return_fit = FALSE,
                    lower = -Inf,
                    upper = Inf,
                    cens = 'none',
                    n_thres = 10,
                    ...) {

  # format x_ref and x_new
  x_ref <- as.vector(x_ref)
  x_new <- as.vector(x_new)

  # check inputs
  inputs <- as.list(environment())
  check_getpit(inputs)

  # remove na's
  pit_na <- rep(NA, length(x_new))
  x_ref <- na.omit(x_ref)
  na_ind <- is.na(x_new)
  x_new <- na.omit(x_new)
  pit <- rep(NA, length(x_new))

  # find indices where data is censored
  ref_cind <- (x_ref <= lower) | (x_ref >= upper)
  new_cind <- (x_new <= lower) | (x_new >= upper)

  # format preds_ref and preds_new
  if (!is.null(preds_ref)) {
    if (is.matrix(preds_ref) | is.vector(preds_ref)) preds_ref <- as.data.frame(preds_ref)
    preds_ref <- preds_ref[!ref_cind, , drop = FALSE]
  }
  if (!is.null(preds_new)) {
    if (is.matrix(preds_new) | is.vector(preds_new)) preds_new <- as.data.frame(preds_new)
    preds_new <- preds_new[!new_cind, , drop = FALSE]
  }

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
    x_l <- cens[1]
    x_u <- cens[2]
  } else {
    if (cens == "none") {
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


# check the inputs of get_pit
check_getpit <- function(inputs) {

  # x_ref
  if (!is.vector(inputs$x_ref)) {
    stop("'x_ref' must be a numeric vector")
  }

  # x_new
  if (!is.vector(inputs$x_new)) {
    stop("'x_new' must be a numeric vector")
  }

  # return_fit
  if (!is.logical(inputs$return_fit) | length(inputs$return_fit) > 1) {
    stop("'return_fit' must either be TRUE or FALSE")
  }

  # lower
  if (!is.numeric(inputs$lower) | length(inputs$lower) > 1) {
    stop("'lower' must be a single numeric value")
  }

  # upper
  if (!is.numeric(inputs$upper) | length(inputs$upper) > 1) {
    stop("'upper' must be a single numeric value")
  }

  if (inputs$lower >= inputs$upper) {
    stop("'lower' must be smaller than 'upper'")
  }

  # preds_ref
  if (!is.null(inputs$preds_ref)) {
    if (!is.data.frame(inputs$preds_ref) & !is.vector(inputs$preds_ref) & !is.matrix(inputs$preds_ref)) {
      stop("'preds_ref' must be a data frame, vector, or matrix")
    } else {
      if (is.matrix(inputs$preds_ref) | is.vector(inputs$preds_ref)) {
        inputs$preds_ref <- as.data.frame(inputs$preds_ref)
      }
      if (nrow(inputs$preds_ref) != length(inputs$x_ref)) {
        stop ("'preds_ref' must have the same number of rows as the length of 'x_ref'")
      }
    }
    if (is.null(inputs$preds_new)) {
      stop("both 'preds_new' and 'preds_ref' must be specified")
    }
  }

  # preds_new
  if (!is.null(inputs$preds_new)) {
    if (!is.data.frame(inputs$preds_new) & !is.vector(inputs$preds_new) & !is.matrix(inputs$preds_new)) {
      stop("'preds_new' must be a data frame, vector, or matrix")
    } else {
      if (is.matrix(inputs$preds_new) | is.vector(inputs$preds_new)) {
        inputs$preds_new <- as.data.frame(inputs$preds_new)
      }
      if (nrow(inputs$preds_new) != length(inputs$x_new)) {
        stop ("'preds_new' must have the same number of rows as the length of 'x_new'")
      }
    }
    if (is.null(inputs$preds_ref)) {
      stop("both 'preds_new' and 'preds_ref' must be specified")
    }
    if (ncol(inputs$preds_ref) != ncol(inputs$preds_new)) {
      stop("'preds_new' and 'preds_ref' must have the same number of columns")
    }
  }

  # censoring
  if (inputs$lower != -Inf && inputs$upper != Inf) {
    if (!is.numeric(inputs$cens) | length(inputs$cens) != 2) {
      stop("when the data is bounded both below and above, 'cens' must be a
           numeric vector of length 2, specifying the censoring points for the
           lower and upper boundary points")
    }
  } else {
    if (is.numeric(inputs$cens)){
      if (length(inputs$cens) > 1) {
        stop("'cens' must be either 'none', 'prob', 'normal', or a
           single numeric value specifying the censoring point")
      }
    } else if (!(inputs$cens %in% c("none", "prob", "normal"))) {
        stop("'cens' must be either 'none', 'prob', 'normal', or a
             single numeric value specifying the censoring point")
    }
  }

}

