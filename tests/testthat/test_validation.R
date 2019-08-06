source('example_data.R')

library(testthat)
library(EasyCalibrator)

context('Target format validation')

#########################################################
## Tests: Target format validation
#########################################################

test_that("Simple valid target is recognized as valid", {
	expect_true(IsTarget(valid_target_2))
})

test_that("Rate-based valid target is recognizesd as valid", {
	expect_true(IsTarget(valid_target_1))
})

test_that("ValidateTargets works on valid targets", {
	expect_true(ValidateTargets(valid_targets))
	expect_true(ValidateTargets(valid_targets_dup))
})

test_that("Invalid targets fail", {
	expect_false(IsTarget(invalid_target_1))
	expect_false(IsTarget(invalid_target_2))
	expect_false(ValidateTargets(list(invalid_target_1, invalid_target_2)))
})

test_that("Mix of valid/invalid targets fail", {
	expect_false(ValidateTargets(list(invalid_target_1, valid_target_1)))
})

