library(testthat)
library(xts)
library(zoo)

# Create a sample data set for testing
set.seed(123)
dates <- seq(as.POSIXct("2023-01-01"), as.POSIXct("2023-01-10"), by = "day")
values <- rnorm(length(dates))
sample <- xts(values, order.by = dates)

# Let's get the standardized values (only for test purposes)
std_index <- function(x, timescale) {
  return((x - mean(x)) / sd(x))
}

sample_std <- std_index(sample, timescale = "days")


test_that("get_drought works correctly with xts input", {
  drought_df <- get_drought(sample_std, thresholds = c(-1, -1.5, -2), exceed = FALSE)
  expect_equal(ncol(drought_df), 6) # "Index" "x"     "ins"   "occ"   "dur"   "mag" 
  expect_equal(colnames(drought_df), c("Index", "x", "ins", "occ", "dur", "mag"))
  expect_true(all(drought_df$occ %in% c(0, 1)))
})
