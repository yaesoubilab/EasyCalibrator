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

