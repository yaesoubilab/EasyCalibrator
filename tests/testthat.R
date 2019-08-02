source('likelihood.R')
library(testthat)

#########################################################
## Example data
#########################################################

valid_observations <- tibble(
  years=seq(2002,2008),
  values=10*seq(1002,1008) # Made-up values
)

# Example of a target with two time-series: 'tbLatent' is divided by
# 'populationSize' for each year in 'tbLatent' that is also a year
# in 'populationSize'
valid_target_1    <- list(type="TS",
                          model=c("tbLatent", "populationSize"),
                          observed=valid_observations)

# Example of a target with one time-series
valid_target_2    <- list(type="TS",
                          model=c("tbSusceptible"),
                          observed=valid_observations)

valid_targets     <- list(valid_target_1, valid_target_2)

# It's conceivable that two identical, or very-similar targets could be
# included in the list of targets, so this is there to illustrate that
# capability.
valid_targets_dup <- list(valid_target_1, valid_target_1, valid_target_2)

# Invalid because a 'PTS' (PyramidTimeSeries) can't be used with
# 'valid_observations', which is a TimeSeries data
invalid_target_1 <- list(type="PTS",
                         model=c("mydata2"),
                         observed=valid_observations)

# Invalid because of the '.csv': there shouldn't be file extensions
invalid_target_2 <- list(type="TS",
                         model=c("mydata1", "mydata2.csv"),
                         observed=valid_observations)

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

#########################################################
## Tests: Target dependency resolution 
#########################################################

test_that("Generated dependencies are unique", {
	expect_length(GenAllDeps(valid_targets), 3)

	dups <- list(valid_target_1, valid_target_1, valid_target_2)
	expect_length(GenAllDeps(dups), 3)
})

test_that("Valid files get loaded without error", {
	expect_message(GenLibrary(GenAllDeps(valid_targets)))	
})

test_that('nonexistent files error', {
	expect_error(GenLibrary('cats.csv'))

	# Mix of valid/invlaid fnames errors	
	expect_error(GenLibrary(c('cats.csv', 'populationSize.csv')))
})

test_that('InjectFromLibrary basically works', {
	tars <- list(valid_target_1)
	lib <- LibraryForTargets(tars)	
	
	injected <- InjectFromLibrary(tars[[1]], lib)
	
	expect_s3_class(injected$model[[1]], 'tbl_df')
	expect_s3_class(injected$model[[2]], 'tbl_df')
})

#########################################################
## Tests: Target dependency resolution 
#########################################################
