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
## Data: Filter
#########################################################

# Returns all variables named in 'observed', except value
FilterOn <- function(observed) setdiff(names(observed), 'value')

# string -> quosure
Sym <- rlang::sym

# Creates a quosure, asking whether 'var_name' is in 'vec', where
# 'var_name' eventually is evaluated within the context of a dpylr
# expression that supports quasiquotation
GenInPred <- function(vec, var_name) rlang::quo(!!Sym(var_name) %in% vec)

# Creates a predicate that asks whether values in 'var_name' are in the 
# vector pulled from tbl$var_name
GenTblPred <- function(tbl, var_name)
  GenInPred(dplyr::pull(tbl, var_name), var_name)

GenTblPreds <- function(tbl, var_names)
  purrr::map(var_names, ~GenTblPred(tbl, .))

# Filters the 'model' data, such that every row in the result must
# correspond to some row in 'observed', along every variable EXCEPT the
# 'value' variable
FilterModel <- function(model, observed)
  dplyr::filter(model, !!! GenTblPreds(observed, FilterOn(observed)))

FilterTarget <- function(tar) {
  tar <- purrr::modify_at(tar, "model", ~FilterModel(., tar$observed))

  if (tibble::is_tibble(tar$size)) {
    tar$size <- FilterModel(tar$size, tar$observed)$value
  }

  tar
}

FilterTargets <- purrr::partial(purrr::map, ...=, FilterTarget)

