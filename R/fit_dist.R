#' @title Calculate distribution parameters and statistics
#'
#' @description This function calculates the distribution parameters and a number
#'  of statistics for a numeric vector and returns them as a named vector.
#'  This function is used internally by the standardized.index function or
#'  can be used to provide input to it.
#'
#' @param data vector of data
#' @param distr character string specifying the distribution, see details
#' @param n_thres number of data points required to estimate the distribution
#'
#' @details
#' This has been adapted from code available at
#' \url{https://github.com/WillemMaetens/standaRdized}.
#'
#' where relevant, parameter estimation is performed using maximum likelihood
#' estimation.
#'
#' @return a list containing the estimated distribution function, its parameters,
#' and relevant goodness-of-fit statistics
#'
#' @examples
#' data(Ukkel_RR)
#'
#' @name fit_dist
NULL

#' @rdname fit_dist
#' @export
fit_dist <- function(data, dist, n_thres = 20){

  # check inputs
  inputs <- as.list(environment())
  check_distribution(inputs)

  # initialise data properties and goodness-of-fit statistics
  fit_props <- c(n_obs = as.integer(NA),
                 n_na = as.numeric(NA),
                 pct_na = as.numeric(NA),
                 ks_pval = as.numeric(NA),
                 ad_pval = as.numeric(NA))

  fit_props['n_obs'] <- as.integer(length(data))
  fit_props['n_na'] <- length(data[which(is.na(data))])
  fit_props['pct_na'] <- (fit_props['n_na']/fit_props['n_obs'])*100

  # initialise parameters
  if (dist == "empirical") {
    params <- list(emp_func = NA, n = NA)
  } else if (dist == "kde") {
    params <- list(data = NA, bw = NA)
  } else if (dist == "gamma"){
    params <- c(shape = as.numeric(NA), rate = as.numeric(NA))
  } else if (dist == "weibull"){
    params <- c(shape = as.numeric(NA), scale = as.numeric(NA))
  } else if (dist == "gev"){
    params <- c(shape = as.numeric(NA), scale = as.numeric(NA), location = as.numeric(NA))
  } else if (dist == "glogis"){
    params <- c(shape = as.numeric(NA), scale = as.numeric(NA), location = as.numeric(NA))
  }

  # check sample size
  data <- data[!is.na(data)]
  n <- length(data)
  if (n < 20) {
    warning(paste(n_thres, "values are required to fit the distribution - distribution has not been fit"))
    return(list(F_x = function(x, params) NA, params = params, fit_props = fit_props))
  }

  # initial estimates
  if (dist == 'gev' | dist == 'glogis'){
    start <- list(shape = 1, scale = 1, location = 0)
  } else {
    start <- NULL
  }

  # fit distribution
  if (dist == "empirical") {
    emp_func  <- ecdf(data)
    params$emp_func <- emp_func
    params$n <- n
    F_x <- function(x, params) (1 + params$emp_func(x)*params$n)/(params$n + 2)
  } else if (dist == "kde") {
    params$data <- data
    params$bw <- bw.nrd(data)
    F_x <- function(x, params) {
      if (length(x) > 1) {
        sapply(x, function(z) mean(pnorm(z, mean = params$data, sd = params$bw)))
      } else {
        mean(pnorm(x, mean = params$data, sd = params$bw))
      }
    }
  } else {
    fit <- try(fitdistrplus::fitdist(data = data, distr = dist, method = "mle", start = start))
    if (!inherits(fit, "try-error")){
      params <- fit$estimate
      F_x <- function(x, params) do.call(paste0("p", dist), c(list(q = x), as.list(params)))
    } else {
      warning("distribution fitting failed")
      return(list(F_x = function(x, params) NA, params = params, fit_props = fit_props))
    }
  }

  # calculate goodness-of-fit statistics
  if (!(dist %in% c("empirical", "kde"))) {
    if(!any(is.na(params))){
      ks_pval <- try(do.call('ks.test', c(list(x = data, y = paste0('p', dist)), as.list(params)))$p.value)
      if (!inherits(ks_pval, 'try-error')) fit_props['ks.pval'] <- ks_pval
      ad_pval <- try(do.call('ad.test', c(list(x = data, null = paste0('p', dist)), as.list(params)))$p.value)
      if (!inherits(ad_pval, 'try-error')) fit_props["ad.pval"] <- ad_pval
    }
  }

  return(list(F_x = F_x, params = params, fit_props = fit_props))
}


check_distribution <- function(inputs) {
  for(i in seq_along(inputs)) assign(names(inputs)[i], inputs[[i]])
  data <- data[!is.na(data)]

  if (!(dist %in% c("empirical", "kde", "gamma", "weibull", "gev", "glogis"))) {
    stop("dist must be one of 'empirical', 'kde', 'gamma', 'weibull', 'gev', 'glogis'")
  }

  if (dist == "empirical") {
    if (length(data) < 100) {
      warning("using the empirical distribution is only recommended when at least
              100 values are available when fitting the distribution")
    }
  }

  if (dist == "gamma" | dist == "weibull") {
    if (any(data < 0)) {
      stop(paste("the", dist, "distribution has positive support, but the data contains negative values"))
    } else if (any(data == 0)) {
      stop(paste("the", dist, "distribution has positive support, but the data contains values equal to zero"))
    }
  }

}

