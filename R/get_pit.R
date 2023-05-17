#' @title Calculate probability integral transform values
#'
#' @description Functions to estimate the cumulative distribution function (CDF)
#' of a set of observations, and return the corresponding probability integral
#' transform (PIT) values.
#'
#' @param ref_data numeric vector from which to estimate the CDF.
#' @param new_data numeric vector from which to calculate the PIT values.
#' @param dist string; distribution used to estimate the CDF.
#' @param method string; method used to estimate distribution parameters.
#'
#' @details
#' Details of the calculate_pit function will be added here
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
                    method = "mle") {

  F_x <- fit_dist(ref_data, dist)
  pit <- F_x(new_data)

  return(pit)
}


