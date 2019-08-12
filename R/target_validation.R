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

