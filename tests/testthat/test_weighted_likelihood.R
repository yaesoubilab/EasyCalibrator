library(EasyCalibrator)
library(testthat)

context("Weighted likelihood")

test_that("Invalid input errors", {
  expect_error(ThresholdNorm(1:10, base=0, threshold=5))
  expect_error(ThresholdNorm(-40:-20, base=0.5, threshold=-10))
  expect_error(ThresholdNorm(-40:-20, base=seq(2), threshold=-10))
  expect_error(ThresholdNorm(-40:-20, base=10, threshold=seq(2)))
  expect_error(ThresholdNorm(-20:-20, base=10, threshold=0))
})

test_that("Things are returned in original order", {
  logs <- -20:-1
  base <- 10
  threshold <- -40

  result <- ThresholdNorm(logs, base=base, threshold=threshold)

  expect_equal(result, sort(result))
})

test_that("Things sum to 1", {
  expect_equal(sum(ThresholdNorm(-400:-1, -20, 10)), 1)
})

test_that("Input containing positive numbers appears to normalize", {
  expect_equal(sum(ThresholdNorm(-10:10, -1, 10)), 1)
  expect_equal(ThresholdNorm(0:10, -10, 10),
               10^(0:10)/sum(10^(0:10)))
})
