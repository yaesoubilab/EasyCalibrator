requireNamespace("assertthat")
requireNamespace("dplyr")
requireNamespace("magrittr")
requireNamespace("purrr")
requireNamespace("readr")
requireNamespace("readr")
requireNamespace("rlang")
requireNamespace("stringr")
requireNamespace("tibble")

#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that

#########################################################
## Data: Cleaning
##
## The purpose of this section is to clean the model
## data so that it better conforms to the observed data.
## This involves a few steps:
##
## 1. Get rid of the 'trajectory' variable
## 2. Rename the 'period' variable to 'year'
## 3. Separate the '15-25' format of specifying age-
##    -groups into 'agemin' and 'agemax' variables
## 4. Group the 'model' data by the variables that are
##    present in the 'observed' data, and sum these
##    groups
#########################################################

# list(tibbles...) -> bool
# Do all the tibbles have the same set of variables?
SameVars <- function(...) {
  tibbles <- list(...)
  name_lists <- purrr::map(tibbles, names)
  head <- dplyr::first(name_lists)

  purrr::reduce(purrr::map(name_lists, ~setequal(head, .)), all)
}

# tibble -> tibble
# Get rid of any 'trajectory' variable
RemoveTrajectory <- function(tbl) dplyr::select(tbl, -trajectory)

# tibble -> tibble
# RenameTime <- purrr::partial(dplyr::rename, year=period)
RenameTime <- function(tbl) dplyr::rename(tbl, year=period)

# tibble -> tibble
# Spread the AG column into agemin, agemax columns
# UNIMPLEMENTED
SpreadAG <- function(tibble) tibble

# tibble, tibble -> char_vec
# Identify shared variables between two tibbles
SharedVars <- function(t1, t2) intersect(names(t1), names(t2))

# tibble_model, tibble_observational -> chr_vec
# Identify variables besides 'value' that are present in both tibbles
IDGroupingVars <- function(t1, t2) setdiff(SharedVars(t1, t2), "value")

# tibble, tibble -> tibble
# Group t1, by shared (between t1, t2), non-'value' variables, then sum
GroupAndSummarize <- function(t1, t2)
  IDGroupingVars(t1, t2) %>%
  dplyr::group_by_at(t1, .) %>%
  dplyr::summarize(value = sum(value))

CleanModelData <- function(tar_injected) {
  if (!purrr::lift_dl(SameVars)(tar_injected$model))
    stop("Model datas have different variable-sets")

  Summarizer <- purrr::partial(GroupAndSummarize, ...=, t2=tar_injected$observed)

  pipeline <- purrr::compose(Summarizer, SpreadAG, RenameTime, RemoveTrajectory)

  new_model <- purrr::map(tar_injected$model, pipeline)
  tar_injected$model <- new_model
  tar_injected
}

# CleanInjectedTargets: targets -> cleaned targets
CleanInjectedTargets <- purrr::partial(purrr::map, ...=, CleanModelData)

