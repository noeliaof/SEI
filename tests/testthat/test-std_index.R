library(testthat)
library(xts)
library(SEI)

# Create mock data
set.seed(123)
dates <- seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = 365)
x_new <- xts(rnorm(365, 10, 5), order.by = dates)
x_ref <- xts(rnorm(365, 10, 5), order.by = dates)

test_that("Basic functionality", {
  # Test basic functionality without additional parameters
  result <- SEI::std_index(x_new, x_ref, rescale = "days")
  expect_true(is.xts(result))
  expect_equal(length(result), length(x_new))
})

test_that("Rescaling data", {
  # Test rescaling from daily to weekly 
  result <- SEI::std_index(x_new, x_ref, rescale = "weeks",dist = "kde")
  expect_true(is.xts(result))
  expect_equal(length(result), 53)
})

test_that("Different distributions", {
  x_new <- rnorm(100)
  x_ref <- rnorm(100)
  dists <- c("empirical", "norm", "logis")
  results <- lapply(dists, function(d) SEI::std_index(x_new, x_ref, dist = d))
  expect_true(all(sapply(results, function(res) is.numeric(res) && length(res) == length(x_new))))
})

