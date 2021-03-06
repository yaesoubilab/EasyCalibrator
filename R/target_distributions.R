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
  assert_that(size > 1)
  
  function(m,o) { 
    assert_that(all(is.double(m)))

    DistGenOnSize(as.integer(size))(x=o, prob=m)
  }
}

# Currying function
Likelihoods <- function(model, obs, size) {
  purrr::pmap_dbl(list(model, obs, size),
                  function(m,o,s) Likelihood(s)(m,o))
}

CalculateLikelihoods <- function(tbl_data, size)
  dplyr::mutate(
    tbl_data,
    likelihood=
      Likelihoods(model,
                  ifelse(observed < 1, round(observed*size), observed),
                  size)
  )

LikelihoodOnTarget <- function(tar)
  list(type=tar$type,
       likelihoods=CalculateLikelihoods(tar$data, tar$size),
       size=tar$size)

LikelihoodOnTargets <- purrr::partial(purrr::map, ...=, LikelihoodOnTarget)

