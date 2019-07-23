library(readr)
library(tibble)
library(purrr)
library(dplyr)
library(stringr)

#########################################################
## Target format validation
#########################################################

# Takes a 'target' object and determines that it has the following properties:
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
  filename_valid <- function(s) !str_detect(s, '\\.csv$')
  FNAMES_CORRECT <- every(filenames, filename_valid)

  c(HAS_NAMES, TYPE_CORRECT, MODEL_CORRECT, FNAMES_CORRECT)
}

# Takes a target and determines that it has the properties detailed in the
# specification of 'IsTarget_impl'. Implemented as a composition of 'IsTarget_impl"
# and 'all'
#
# IsTarget: target -> bool
IsTarget <- compose(all, IsTarget_impl)


# ValidateTargets: list of targets -> bool
#
# Validates a list of targets using 'IsTarget'
ValidateTargets <- function(targets) all(map_lgl(targets, IsTarget))

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
  mapper <- partial(map, ...=, GenTargetDeps)
  compose(unique, as.character, flatten, mapper)(tars)
}

# GenLibrary: chr_vec of unique files -> list of [basename, tibble]
#		where:
#			'deps': chr_vec of filenames, likely ending in '.csv'
#			result: 'basename' is the filename sans extension
GenLibrary <- function(deps) {
	# Options to the csv reader: columns are named, types should be inferred, and 
	# empty cells should be represented as the empty string
  readr_opts <- list(col_names=TRUE, col_types=NULL, na="")

	# Partialize the 'read_csv' function to configure it
  loader <- partial(lift_dl(read_csv), readr_opts)

	# Get rid of the file extension
	# KNOWN BUG: Will fail when fname has two periods, i.e. 'fname.xls.csv'
  namer <- partial(str_extract, pattern='^\\w+')
  
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
  reduce(deps, accumulator, .init=list())
}

# list of targets -> library
# Takes a list of dependencies and produces a library
LibraryForTargets <- compose(GenLibrary, GenAllDeps)

# target, library -> target
# Performs dependency injection on the targets, replacing each instance of a
# referenced file with its representation as a tibble
InjectFromLibrary <- function(tar, lib)
  update_list(tar, model=quo(map(model, ~lib[[.]]) ))

# Injects all of the targets with their dependencies
# list of targets, library -> list of targets
# [todo] This could be made a little safer
InjectAllTargets <- function(tars, lib) map(tars, InjectFromLibrary, lib)

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
  name_lists <- map(tibbles, names)
  head <- first(name_lists)

  reduce(map(name_lists, ~setequal(head, .)), all)
}

# tibble -> tibble
# Get rid of any 'trajectory' variable
RemoveTrajectory <- function(tbl) select(tbl, -trajectory)

# tibble -> tibble
RenameTime <- partial(rename, year=period)

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
  group_by_at(t1, .) %>% 
  summarize(value = sum(value))

CleanModelData <- function(tar_injected) {
  if (!lift_dl(SameVars)(tar_injected$model))
    stop("Model datas have different variable-sets")

  Summarizer <- partial(GroupAndSummarize, ...=, t2=tar_injected$observed)

  pipeline <- compose(Summarizer, SpreadAG, RenameTime, RemoveTrajectory)

  new_model <- map(tar_injected$model, pipeline)
  tar_injected$model <- new_model
  tar_injected
}

# CleanInjectedTargets: targets -> cleaned targets
CleanInjectedTargets <- partial(map, ...=, CleanModelData)

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
  var <- enquo(var)
  mutate(tbl, !!var := !!var + by)
}

# tibble, var name, year
OffsetYear <- partial(OffsetVar, var=year)

# tibble -> int
# Number of rows in tibble
NRows <- function(tbl) count(tbl) %>% pull(n)

# tbl_l, tbl_r, tbl_joined -> bool
# Decides whether or not the join went well
JoinWentWell <- function(l, r, joined) {
  LengthGood <- `==`(NRows(l), NRows(joined))
  NoNAExceptLast <- pull(joined, value.y)[1:length(joined$value.y)-1] %>%
    every(negate(is.na))

  all(LengthGood, NoNAExceptLast)
}

# tibble_x, tibble_y -> tibble
# Takes two joinable tibbles and joins them, dividing their 'value' variables
# (x/y) and returning a 'value' column with the results of the division, along
# with all other columns present in the two tibbles
JoinAndDivide <- function(tbl1, tbl2) {
  joining_vars <- IDGroupingVars(tbl1, tbl2)
  
  joined <- left_join(tbl1, tbl2, joining_vars)

  if( !JoinWentWell(tbl1, tbl2, joined) ) {
    print("Join failed!")
    return()
  }

  divide <- partial(mutate, value = value.x/value.y)
  consolidate <- partial(select, ...=, -value.x, -value.y)

  compose(consolidate, divide)(joined)
}

# tibble, list of AGPairs -> tibble
# where: AGPair is a two-element numeric vector with an agemin, agemax
# UNIMPLEMENTED
CoalesceAGs <- function(tbl, ag_pairs) tbl

TransformCleanedModel <- function(obj, year.offset=0) {
  model_transformers <- list(
    partial(OffsetYear, by=year.offset)
  )

  model_transformers <- lift_dl(compose)(model_transformers)

  model_transformers(obj)
}

TransformCleanedTarget <- function(tar, year.offset=0) {
  SingleTblTransforms <- map(tar$model, TransformCleanedModel, year.offset)

  MaybeJoined <- switch(length(SingleTblTransforms),
                        SingleTblTransforms,
                        JoinAndDivide(SingleTblTransforms[[1]],
                                      SingleTblTransforms[[2]]))

  tar$model <- MaybeJoined
  tar
}

TransformAllTargets <- function(tars, year.offset=0) 
  map(tars, TransformCleanedTarget, year.offset)

#########################################################
## Data: Filter
#########################################################

FilterOn <- function(observed) setdiff(names(observed), 'value')

# string -> quosure
Sym <- rlang::sym

GenInPred <- function(vec, var_name) quo(!!Sym(var_name) %in% vec)

GenTblPred <- function(tbl, var_name) GenInPred(pull(tbl, var_name), var_name)

GenTblPreds <- function(tbl, var_names) map(var_names, ~GenTblPred(tbl, .))

FilterModel <- function(model, observed)
  filter(model, !!! GenTblPreds(observed, FilterOn(observed)))

FilterTarget <- function(tar)
  modify_at(tar, "model", ~FilterModel(., tar$observed))

FilterTargets <- partial(map, ...=, FilterTarget)

#########################################################
## Data: Join
#########################################################

JoinModelObserved <- function(mod, obs)
  inner_join(mod, obs, IDGroupingVars(mod, obs), suffix=c(".mod", ".obs")) %>%
  mutate(model     = value.mod,
         observed  = value.obs,
         value.mod = NULL,
         value.obs = NULL)

CheckJoin <- function(obs, joined) NRows(joined) == NRows(obs)

JoinTarget <- function(tar)
  list(type=tar$type, 
       data=JoinModelObserved(tar$model, tar$observed))

JoinAllTargets <- partial(map, ...=, JoinTarget)

#########################################################
## Distribution generator
#########################################################

DistGenOnSize <- function(size) partial(dbinom, size=size, log=TRUE)

Likelihood <- function(size) function(m,o) DistGenOnSize(size)(x=o, prob=m)

Likelihoods <- function(model, obs, size) map2_dbl(model, obs, Likelihood(size))

CalculateLikelihoods <- function(tbl, size) 
  mutate(tbl, likelihood=Likelihoods(model, round(size*observed), size))

LikelihoodOnTarget <- function(tar, size=36500)
  list(type=tar$type, 
       likelihoods=CalculateLikelihoods(tar$data, size))

LikelihoodOnTargets <- partial(map, ...=, LikelihoodOnTarget)

#########################################################
## Summarize each target
#########################################################

SumTarget <- function(tar) 
  modify_at(tar, "likelihoods", ~summarize(., log.sum = sum(likelihood)))

SumTargets <- 
  compose(sum,
          partial(map_dbl, ...=, ~sum(pull(.$likelihoods, log.sum))),
          partial(map, ...=, SumTarget))

TryIt <- function() {

  start_year <- 1990
  pop_size   <- 36500

  if (!ValidateTargets(targets))
    stop("TryIt: One or more targets failed to validate")

  lib <- LibraryForTargets(targets)

  Transformer <- partial(TransformAllTargets, year.offset=start_year)
  Injector <- partial(InjectAllTargets, lib=lib)

  injected    <- try(Injector(targets))
  cleaned     <- try(CleanInjectedTargets(injected))
  transformed <- try(Transformer(cleaned))
  filtered    <- try(FilterTargets(transformed))
  joined      <- try(JoinAllTargets(filtered))
  calculated  <- try(LikelihoodOnTargets(joined))
  summed      <- try(SumTargets(calculated))

  dbg <- list(injected=injected, 
              cleaned=cleaned,
              transformed=transformed, 
              filtered=filtered,
              joined=joined,
              calculated=calculated,
              summed=summed)

  pipeline <- compose(SumTargets,
                      partial(LikelihoodOnTargets, size=pop_size),
                      JoinAllTargets,
                      FilterTargets,
                      Transformer,
                      CleanInjectedTargets,
                      Injector)

  list(pipeline = pipeline(targets),
			 stepped  = dbg)
}


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
## Real data: preprocessing functions
#########################################################

ReformatRawCalibData <- function(d) {
  vars <- setdiff(names(d), 'value')

	# Reformat each target into a tibble with a 'year' variable
	# and a 'value' variable
  tibbles <- map_at(d, vars, ~tibble(year=d$year, value=.))

	# For each target, remove entries that are NA-valued
  map(tibbles[vars], ~filter(., !is.na(value)))
}

#########################################################
## Real data
#########################################################

calibrationData_raw <- tibble(
  year = 2002:2008,
  populationChildren = c(10427, 10531, 10637, 10743, 10850, 10959, 11068),
  populationAdults =   c(25903, 26162, 26424, 26688, 26955, 27224, 27497),
  notifiedTBChildren = c(82, 60, 66, 69, 73, 77, 69),
  notifiedTBExperiencedAdults = c(105, 119, 130, 109, 130, 126, 137),
  notifiedTBNaiveAdults = c(172, 234, 200, 224, 216, 233, 210),
  prevalenceExperiencedAdults = 100*c(0.097, NA, NA, NA, NA, NA, NA),
  prevalenceHIV = c(0.052, NA, NA, NA, NA, NA, NA),
  prevalenceInfectiousNaiveAdults = 100*c(0.0051, NA, NA, NA, NA, NA, NA),
  prevalenceInfectiousExperiencedAdults = 100*c(0.0299, NA, NA, NA, NA, NA, NA),
)

calibrationData <- ReformatRawCalibData(calibrationData_raw)

tar_prevalence_HIV <- list(type='TS',
                           model=c('hivPositive', 'populationSize'),
                           observed=calibrationData$prevalenceHIV)

targets <- list(HIVPrevalence=tar_prevalence_HIV)

fakePopSize <- 36500

