library(testthat)
library(SEI)
library(xts)
library(zoo)

data_sample <- data.frame(
  date = seq(as.POSIXct("2020-01-01 00:00"), as.POSIXct("2020-01-10 23:00"), by = "hour"),
  value = rnorm(240)
)
xts_sample <- xts(data_sample$value, order.by = data_sample$date)

test_that("aggregate_xts correctly aggregates daily data", {
  result <- aggregate_xts(xts_sample, agg_period = 1, timescale = "hours")
  expect_true(is.xts(result))
  expect_equal(ncol(result), 1)
})


test_that("aggregate_xts correctly aggregates weekly data", {
  result <- aggregate_xts(xts_sample, agg_period = 1, agg_scale = "weeks", timescale = "hours")
  expect_true(is.xts(result))
  expect_equal(ncol(result), 1)
})

test_that("aggregate_xts returns NA when NA threshold is exceeded", {
  xts_sample_na <- xts_sample
  xts_sample_na[1:24] <- NA
  result <- aggregate_xts(xts_sample_na, agg_period = 1, timescale = "hours", na_thres = 10)
  expect_true(is.xts(result))
  # Since the first day has all values as NA, the first value should be NA
  expect_true(is.na(result[1]))
  expect_false(all(is.na(result))) # Not all values should be NA
})