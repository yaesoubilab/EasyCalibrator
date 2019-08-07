source('example_data.R')

library(testthat)
library(EasyCalibrator)

context('Dependency resolution')

#########################################################
## Tests: Target dependency resolution 
#########################################################

test_that("Generated dependencies are unique", {
	expect_length(GenAllDeps(valid_targets), 3)

	dups <- list(valid_target_1, valid_target_1, valid_target_2)
	expect_length(GenAllDeps(dups), 3)
})

test_that("Valid files get loaded without error", {
	expect_silent(GenLibrary(GenAllDeps(valid_targets)))	
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

