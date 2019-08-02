source('likelihood.R')
library(testthat)

#########################################################
## Example data
#########################################################

valid_observations <- tibble(
  year=seq(2002,2008),
  value=10*seq(1002,1008) # Made-up values
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
## Tests: Cleaning
#########################################################

test_that('Two identical variables pass SameVars', {
	a <- tibble(lol=integer(), cat=integer())
	b <- tibble(lol=integer(), cat=integer())

	expect_true(SameVars(a,b))
})

test_that('Three identical tibbles pass SameVars', {
	a <- tibble(lol=integer(), cat=integer())

	expect_true(SameVars(a,a,a))
})

test_that('Two tibbles, one with a superset of vars, fails SameVars', {
	a <- tibble(lol=integer())
	b <- tibble(lol=integer(), cat=integer())

	expect_false(SameVars(a,b))
})

test_that('Invalid model-pairs error', {
	a <- tibble(period=integer(), trajectory=integer(), value=integer())
	b <- tibble(period=integer(), value=integer())

	target <- list(model=list(a, b))

	expect_error(CleanModelData(target))
})

transformed_tars <- function() {
	injected_tars <- InjectAllTargets(valid_targets,
																		LibraryForTargets(valid_targets))
	
	cleaned_tars <- CleanInjectedTargets(injected_tars)

	TransformAllTargets(cleaned_tars, 1990)
}

test_that('CleanModelData leaves no groups', {
	expect_false(some(transformed_tars(), is_grouped_df))
})

# This test is NON-FUNCTIONAL and ERRORS!
test_that('CleanModelData deals with 2-element model keys correctly', {
	tars_2_models <- Filter(x=transformed_tars(),
													f=function(x) length(x$model) == 2)
	print(tars_2_models)
	lmap(tars_2_models, ~expect_s3_class(.$model[[1]], 'tbl_df'))
	lmap(tars_2_models, ~expect_s3_class(.$model[[2]], 'tbl_df'))
})

