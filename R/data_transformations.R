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


