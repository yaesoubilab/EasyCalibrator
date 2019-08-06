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

transform_tars <- function() {
  injected_tars <- InjectAllTargets(valid_targets,
                                    LibraryForTargets(valid_targets))
  cleaned_tars <- CleanInjectedTargets(injected_tars)
  transformed <- TransformAllTargets(cleaned_tars, 1990)

  transformed
}

test_that('CleanModelData leaves no groups', {
  expect_false(purrr::some(transform_tars(), dplyr::is_grouped_df))
})

