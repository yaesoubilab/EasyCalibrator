# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

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
## Target format validation
#########################################################

# Takes a 'target' object and determines that it has the following 
# properties:
#	- has keys 'type', 'model', 'observed'
# - where 'type' == 'TS' (TimeSeries)
# - where 'model' is a chr_vec of length 1,2
# - where none of the elements of 'model' end in '.csv'
#
# IsTarget_impl: tar -> logical_vec
IsTarget_impl <- function(tar) {
  target_keys <- c("type", "model", "observed")
  possible_types <- c('TS')

  filenames <- tar$model

  # Does the target have the right keys?
  HAS_NAMES <- setequal(target_keys, names(tar))

  # Is the 'type' key correct?
  TYPE_CORRECT <- any(tar$type %in% possible_types)

  MODEL_CORRECT <- all(typeof(tar$model) == "character",
                       length(tar$model) %in% seq(1,2))

  # Do any of the filenames include an extension?
  filename_valid <- function(s) !stringr::str_detect(s, '\\.csv$')
  FNAMES_CORRECT <- purrr::every(filenames, filename_valid)

  c(HAS_NAMES, TYPE_CORRECT, MODEL_CORRECT, FNAMES_CORRECT)
}

# Takes a target and determines that it has the properties detailed in the
# specification of 'IsTarget_impl'. Implemented as a composition of 'IsTarget_impl"
# and 'all'
#
# IsTarget: target -> bool
IsTarget <- purrr::compose(all, IsTarget_impl)


# ValidateTargets: list of targets -> bool
#
# Validates a list of targets using 'IsTarget'
ValidateTargets <- function(targets) all(purrr::map_lgl(targets, IsTarget))

#########################################################
## Target dependency resolution
##
## This section of code is responsible for injecting
## dependencies of targets. It identifies .csv files that
## need to be loaded, performs the load, and replaces
## references to these files (made in the 'model' section
## of a target) with the file, represented as a tibble.
#########################################################

# GenTargetDeps: target -> chr_vec of filenames
#
# List all the .csv files that need to be loaded for a target
GenTargetDeps <- function(tar) paste0(tar$model, '.csv')

# GenAllDeps: list of targets -> chr_vec of filenames
#
# List all the unique .csv files that need to be loaded for a collection
# of targets
GenAllDeps <- function(tars) {
  mapper <- purrr::partial(purrr::map, ...=, GenTargetDeps)
  purrr::compose(unique, as.character, purrr::flatten, mapper)(tars)
}

# GenLibrary: chr_vec of unique files -> list of [basename, tibble]
#		where:
#			'deps': chr_vec of filenames, likely ending in '.csv'
#			result: 'basename' is the filename sans extension
GenLibrary <- function(deps) {
  
  col_types <- readr::cols(period=readr::col_integer(),
                           trajectory=readr::col_character(),
                           value=readr::col_double())

  # Options to the csv reader: columns are named, types are as according 
  # to spec, and empty cells should be represented as the empty string
  readr_opts <- list(col_names=TRUE,
                     col_types=col_types, 
                     na="")

  # Partialize the 'read_csv' function to configure it
  loader <- purrr::partial(purrr::lift_dl(readr::read_csv),
                           readr_opts)

  # Get rid of the file extension
  # KNOWN BUG: Will fail when fname has two periods, i.e. 'fname.xls.csv'
  namer <- purrr::partial(stringr::str_extract, pattern='^\\w+')

  # Attempt to load each dependency into a list, keyed on 'basename'
  # Catch errors gracefully
  accumulator <- function(acc, fname) {
    acc[[namer(fname)]] <- tryCatch(loader(fname),
                                    error=function(c) {
                                      message(c, "\n")
                                      stop("GenLibrary: Couldn't open file: ", fname)
                                    }
    )
    acc
  }

  # Use the accumulator to reduce the dependency list
  purrr::reduce(deps, accumulator, .init=list())
}

# list of targets -> library
# Takes a list of dependencies and produces a library
LibraryForTargets <- purrr::compose(GenLibrary, GenAllDeps)

# target, library -> target
# Performs dependency injection on the targets, replacing each instance of a
# referenced file with its representation as a tibble
InjectFromLibrary <- function(tar, lib)
  purrr::update_list(tar, model=rlang::quo(purrr::map(model, ~lib[[.]]) ))

# Injects all of the targets with their dependencies
# list of targets, library -> list of targets
# [todo] This could be made a little safer
InjectAllTargets <- function(tars, lib) purrr::map(tars, InjectFromLibrary, lib)

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

#########################################################
## Data: Transformations
##
## This section applies transformations to the 'model'
## data in attempting to join it to the observational
## data.
#########################################################

# tibble, var name, number -> tibble
# Mutates 'var name' in-place by adding 'number' to it
OffsetVar <- function(tbl, var, by) {
  assert_that(all(var %in% names(tbl)))
  assert_that(all.equal(length(var), 1))
  assert_that(is.numeric(by))

  dplyr::mutate_at(tbl, var, ~. + by)
}

# tibble, by[integer] -> tibble with 'year' column modified
OffsetYear <- purrr::partial(OffsetVar, var='year')

# tibble -> int
# Number of rows in tibble
NRows <- nrow

# tbl_l, tbl_r, tbl_joined -> bool
# Decides whether or not the join went well
JoinWentWell <- function(l, r, joined) {
  LengthGood <- `==`(NRows(l), NRows(joined))
  NoNAExceptLast <- dplyr::pull(joined, value.y)[1:length(joined$value.y)-1] %>%
    purrr::every(purrr::negate(is.na))

  all(LengthGood, NoNAExceptLast)
}

# tibble_x, tibble_y -> tibble
# Takes two joinable tibbles and joins them, dividing their 'value' variables
# (x/y) and returning a 'value' column with the results of the division, along
# with all other columns present in the two tibbles
JoinAndDivide <- function(tbl1, tbl2) {
  joining_vars <- IDGroupingVars(tbl1, tbl2)

  joined <- dplyr::left_join(tbl1, tbl2, joining_vars)

  if( !JoinWentWell(tbl1, tbl2, joined) ) {
    stop("Join failed!")
  }

  divide <- purrr::partial(dplyr::mutate, value = value.x/value.y)
  consolidate <- purrr::partial(dplyr::select, ...=, -value.x, -value.y)

  purrr::compose(consolidate, divide)(joined)
}

# tibble, list of AGPairs -> tibble
# where: AGPair is a two-element numeric vector with an agemin, agemax
# UNIMPLEMENTED
CoalesceAGs <- function(tbl, ag_pairs) tbl

TransformCleanedModel <- function(obj, year.offset=0) {
  # model_transformers <- list(
  #   purrr::partial(OffsetYear, by=year.offset)
  # )

  # model_transformers <- purrr::lift_dl(purrr::compose)(model_transformers)

  # model_transformers(obj)
  OffsetYear(obj, by=year.offset)
}

TransformCleanedTarget <- function(tar, year.offset=0) {
  SingleTblTransforms <- purrr::map(tar$model, TransformCleanedModel, year.offset)

  MaybeJoined <- switch(length(SingleTblTransforms),
                        SingleTblTransforms,
                        JoinAndDivide(SingleTblTransforms[[1]],
                                      SingleTblTransforms[[2]]))

  tar$model <- MaybeJoined
  tar
}

TransformAllTargets <- function(tars, year.offset=0) 
  purrr::map(tars, TransformCleanedTarget, year.offset)


#########################################################
## Data: Filter
#########################################################

FilterOn <- function(observed) setdiff(names(observed), 'value')

# string -> quosure
Sym <- rlang::sym

GenInPred <- function(vec, var_name) rlang::quo(!!Sym(var_name) %in% vec)

GenTblPred <- function(tbl, var_name)
  GenInPred(dplyr::pull(tbl, var_name), var_name)

GenTblPreds <- function(tbl, var_names)
  purrr::map(var_names, ~GenTblPred(tbl, .))

FilterModel <- function(model, observed)
  dplyr::filter(model, !!! GenTblPreds(observed, FilterOn(observed)))

FilterTarget <- function(tar)
  purrr::modify_at(tar, "model", ~FilterModel(., tar$observed))

FilterTargets <- purrr::partial(purrr::map, ...=, FilterTarget)

#########################################################
## Data: Join
#########################################################

# mod, obs -> tbl[model, observed]
# Joins model data to observed data using the grouping vars
JoinModelObserved <- function(mod, obs)
  dplyr::inner_join(mod,
                    obs,
                    IDGroupingVars(mod, obs),
                    suffix=c(".mod", ".obs")) %>%
  dplyr::mutate(model     = value.mod,
                observed  = value.obs,
                value.mod = NULL,
                value.obs = NULL)

# obs [tbl] and [joined] -> bool
# Check to make sure that every observed value was joined to
# a model value
CheckJoin <- function(obs, joined) 
  all.equal(NRows(joined), NRows(obs))

# tar -> list[type=string, data[ tbl[model,observed] ]]
# Take a target which is ready for its join, do the join, reorganize
# the target
JoinTarget <- function(tar)
  list(type=tar$type,
       data=JoinModelObserved(tar$model, tar$observed))

# list of trargets -> list of joined targets
JoinAllTargets <- purrr::partial(purrr::map, ...=, JoinTarget)

#########################################################
## Distribution generator
#########################################################

# Partialize a binomial distribution on a population of size 'size'
# and configure it to output values as represented by their natural
# logarithm.
DistGenOnSize <- function(size) purrr::partial(stats::dbinom, size=size, log=TRUE)

# Currying function
Likelihood <- function(size) {
  assert_that(length(size) == 1)
  assert_that(is.integer(size))
  assert_that(size > 1)
  
  function(m,o) { 
    assert_that(all(is.double(m)))

    DistGenOnSize(size)(x=o, prob=m)
  }
}

# Currying function
Likelihoods <- function(model, obs, size) purrr::map2_dbl(model, obs, Likelihood(size))

#
CalculateLikelihoods <- function(tbl_data, size)
  dplyr::mutate(tbl_data,
                likelihood=Likelihoods(model,
                                       round(size*observed),
                                       size))

LikelihoodOnTarget <- function(tar, size=as.integer(36500))
  list(type=tar$type,
       likelihoods=CalculateLikelihoods(tar$data, size))

LikelihoodOnTargets <- purrr::partial(purrr::map, ...=, LikelihoodOnTarget)

#########################################################
## Summarize each target
#########################################################

SumTarget <- function(tar)
  purrr::modify_at(tar, "likelihoods", ~dplyr::summarize(., log.sum = sum(likelihood)))

SumTargets <-
  purrr::compose(sum,
                 purrr::partial(purrr::map_dbl, ...=, ~sum(dplyr::pull(.$likelihoods, log.sum))),
                 purrr::partial(purrr::map, ...=, SumTarget))

#' @export
CalibrateTargets <- function(targets) {

  start_year <- 1990
  pop_size   <- as.integer(36500)

  if (!ValidateTargets(targets))
    stop("TryIt: One or more targets failed to validate")

  lib <- LibraryForTargets(targets)

  Transformer <- purrr::partial(TransformAllTargets, year.offset=start_year)
  Injector <- purrr::partial(InjectAllTargets, lib=lib)

  pipeline <- purrr::compose(SumTargets,
                             purrr::partial(LikelihoodOnTargets, size=pop_size),
                             JoinAllTargets,
                             FilterTargets,
                             Transformer,
                             CleanInjectedTargets,
                             Injector)

  pipeline(targets)
}

#########################################################
## Real data: preprocessing functions
#########################################################

ReformatRawCalibData <- function(d) {
  vars <- setdiff(names(d), 'year')

  # Reformat each target into a tibble with a 'year' variable
  # and a 'value' variable
  tibbles <- purrr::map_at(d, vars, ~tibble::tibble(year=d$year, value=.))

  # For each target, remove entries that are NA-valued
  purrr::map(tibbles[vars], ~dplyr::filter(., !is.na(value)))
}

#########################################################
## Real data
#########################################################

calibrationData_raw <- tibble::tibble(
  year = 2002:2008,
  populationChildren = c(10427, 10531, 10637, 10743, 10850, 10959, 11068),
  populationAdults =   c(25903, 26162, 26424, 26688, 26955, 27224, 27497),
  notifiedTBChildren = c(82, 60, 66, 69, 73, 77, 69),
  notifiedTBExperiencedAdults = c(105, 119, 130, 109, 130, 126, 137),
  notifiedTBNaiveAdults = c(172, 234, 200, 224, 216, 233, 210),
  prevalenceExperiencedAdults = 100*c(0.097, NA, NA, NA, NA, NA, NA),
  prevalenceHIV = c(0.052, NA, NA, NA, NA, NA, NA),
  prevalenceInfectiousNaiveAdults = 100*c(0.0051, NA, NA, NA, NA, NA, NA),
  prevalenceInfectiousExperiencedAdults = 100*c(0.0299, NA, NA, NA, NA, NA, NA)
)

calibrationData <- ReformatRawCalibData(calibrationData_raw)

tar_prevalence_HIV <- list(type='TS',
                           model=c('hivPositive', 'populationSize'),
                           observed=calibrationData$prevalenceHIV)

targets <- list(HIVPrevalence=tar_prevalence_HIV)

fakePopSize <- 36500

