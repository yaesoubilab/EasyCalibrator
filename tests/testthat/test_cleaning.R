source('example_data.R')

library(testthat)
library(EasyCalibrator)

context("Cleaning")

#########################################################
## Tests: Cleaning
#########################################################

test_that('Two identical variables pass SameVars', {
  a <- tibble::tibble(lol=integer(), cat=integer())
  b <- tibble::tibble(lol=integer(), cat=integer())
  
  expect_true(SameVars(a,b))
})

test_that('Three identical tibbles pass SameVars', {
            a <- tibble::tibble(lol=integer(), cat=integer())

            expect_true(SameVars(a,a,a))
})

test_that('Two tibbles, one with a superset of vars, fails SameVars', {
            a <- tibble::tibble(lol=integer())
            b <- tibble::tibble(lol=integer(), cat=integer())

            expect_false(SameVars(a,b))
})

test_that('Invalid model-pairs error', {
            a <- tibble::tibble(period=integer(), trajectory=integer(), value=integer())
            b <- tibble::tibble(period=integer(), value=integer())

            target <- list(model=list(a, b))

            expect_error(CleanModelData(target))
})

test_that('CleanModelData leaves no groups', {

  injected_tars <- InjectAllTargets(valid_targets,
                                    LibraryForTargets(valid_targets))

  cleaned_tars <- CleanInjectedTargets(injected_tars)
  transformed <- TransformAllTargets(cleaned_tars, 1990)

  expect_false(purrr::some(transformed, dplyr::is_grouped_df))
})

# This test is NON-FUNCTIONAL and ERRORS!
# test_that('CleanModelData deals with 2-element model keys correctly', {
#   tars_2_models <- Filter(x=transformed_tars(),
#                           f=function(x) length(x$model) == 2)
#   print(tars_2_models)
#   lmap(tars_2_models, ~expect_s3_class(.$model[[1]], 'tbl_df'))
#   lmap(tars_2_models, ~expect_s3_class(.$model[[2]], 'tbl_df'))
# })
