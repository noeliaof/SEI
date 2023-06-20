#' @title Calculate probability integral transform values
#'
#' @description Functions to estimate the cumulative distribution function (CDF)
#' of a set of observations, and return the corresponding probability integral
#' transform (PIT) values.
#'
#' @param ref_data numeric vector from which to estimate the CDF.
#' @param new_data numeric vector from which to calculate the PIT values.
#' @param dist string; distribution used to estimate the CDF.
#' @param return_fit logical; return parameters and goodness-of-fit statistics.
#'
#' @details
#' Functionality is currently available to calculate parameters using maximum
#' likelihood estimation, though alternative methods may be introduced later.
#'
#' @return
#' A vector of PIT values.
#'
#' @author Sam Allen, Noelia Otero
#'
#' @examples
#' Examples of the calculate_pit function will be added here
#'
#' @name get_pit
NULL

#' @rdname get_pit
#' @export
get_pit <- function(ref_data,
                    new_data,
                    dist = "empirical",
                    return_fit = F) {

  fit <- fit_dist(as.vector(ref_data), dist)
  fit$pit <- fit$F_x(as.vector(new_data), fit$params)

  if (return_fit) {
    return(fit)
  } else {
    return(list(pit = fit$pit))
  }
}


