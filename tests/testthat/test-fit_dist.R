library(testthat)
library(fitdistrplus)
library(gamlss)
library(lmom)
library(flexsurv)

# Sample data for tests
set.seed(123)


test_that("fit_dist fits gamma distribution using MLE", {
  data_gamma <- rgamma(1000, shape = 3, rate = 2)
  result <- fit_dist(data_gamma, dist = "gamma")
  expect_true(is.list(result))
  expect_equal(length(result$params), 2)
  expect_true(all(names(result$params) == c("shape", "rate")))
  expect_true(!is.na(result$fit["aic"]))
  expect_true(!is.na(result$fit["ks_pval"]))
})



test_that("fit_dist fits log-normal distribution", {
  data_lnorm <- rlnorm(1000, meanlog = 1, sdlog = 0.5)
  result <- fit_dist(data_lnorm, dist = "lnorm", method = 'lmme')
  expect_true(is.list(result))
  expect_true(!is.na(result$fit["aic"]))
  expect_true(!is.na(result$fit["ks_pval"]))
})

test_that("fit_dist fits log-normal distribution", {
  data_weibull <- rweibull(1000, shape = 3, scale = 2)
  result <- fit_dist(data_weibull, dist = "weibull", method = "lmme")
  expect_true(is.list(result))
  expect_true(!is.na(result$fit["aic"]))
  expect_true(!is.na(result$fit["ks_pval"]))
})


