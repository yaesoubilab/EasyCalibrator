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
## Summarize each target
#########################################################

SumTarget <- function(tar) {
  purrr::modify_at(tar, "likelihoods", ~dplyr::summarize(., log.sum = sum(likelihood)))
}

SumTargets <- 
  purrr::compose(sum,
                 purrr::partial(purrr::map_dbl, ...=, ~sum(dplyr::pull(.$likelihoods, log.sum))),
                 purrr::partial(purrr::map, ...=, SumTarget))

SumTargetsVerbose <- function(targets) {
  sum.for.each.target <- purrr::map(targets, SumTarget)
  map(sum.for.each.target, ~dplyr::pull(.$likelihoods, log.sum))
}

#' @export
CalibrateTargets <- function(targets, verbose=FALSE) {

  start_year <- 1990

  if (!ValidateTargets(targets))
    stop("TryIt: One or more targets failed to validate")

  lib <- LibraryForTargets(targets)

  Transformer <- purrr::partial(TransformAllTargets, year.offset=start_year)
  Injector <- purrr::partial(InjectAllTargets, lib=lib)

  pipeline <- purrr::compose(SumTargets,
                             LikelihoodOnTargets,
                             JoinAllTargets,
                             FilterTargets,
                             Transformer,
                             CleanInjectedTargets,
                             Injector)

  if (identical(verbose, TRUE)) {
    pipeline <- purrr::compose(SumTargetsVerbose,
                               LikelihoodOnTargets,
                               JoinAllTargets,
                               FilterTargets,
                               Transformer,
                               CleanInjectedTargets,
                               Injector)
  }

  pipeline(targets)
}

