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
# LikelihoodOnTarget <- function(tar, size=as.integer(36500)) {
#   purrr::modify_at(tar, "likelihoods", ~CalculateLikelihoods(tar$data, tar$size))
# }

LikelihoodOnTargets <- purrr::partial(purrr::map, ...=, LikelihoodOnTarget)

