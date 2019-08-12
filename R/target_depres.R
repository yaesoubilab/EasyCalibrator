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

