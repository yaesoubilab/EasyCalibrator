requireNamespace("assertthat")

#' @importFrom assertthat assert_that

# T/F on whether 'x' is a whole number
is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

# A_i: dbl_vec, threshold, base -> list
#   dbl_vec: double vector of logarithms. All logarithms must be of the same
#     base b>1
#
#   threshold: integer representing the number of digits of precision. For 
#     example, if the smallest number of consequence is 0.001, set threshold
#     to -3. Threshold is ALWAYS defined in terms of powers of 10.
#
#   base: the base of the logarithm used, must be a single value >1
#   
#   The output is a list:
#
#   $permutation: A_i is returned having been sorted in increasing order.
#     $permutation contains the permutation of this sort, so that $result
#     can easily be returned to its original order
#   $result: a dbl_vec containing A_i for each element of the input, where
#     A_i = 0 if an element is <= ln(10^threshold) - ln(length(input)), and
#     A_i = input[i] - max(input) otherwise
A_i <- function(input, threshold, base=exp(1)) {
  assert_that(is.numeric(input), msg="Input was not a numeric vector")
  assert_that(length(input) > 1, msg="Input was of length 0 or 1")

  assert_that(length(base) == 1, msg="'base' must be a single number")
  assert_that(base > 1,          msg="'base' must be > 1")

  assert_that(is.wholenumber(threshold), msg="Threshold must be a whole number")
  assert_that(length(threshold) == 1,    msg="Threshold must be a 1-el intvec")
  assert_that(threshold < 0,             msg="Threshold must be < 0")

  input_length <- length(input)

  # Sort the input vector, but also grab what the permutation of the sort
  # is so that the original order can be reconstructed later by the caller
  sort_results <- sort(input, index.return=TRUE)

  sorted       <- sort_results$x
  permutation  <- sort_results$ix
  max_val      <- sorted[[input_length]]
  
  # For each element of the input, we calculate whether that element
  # is too small to care about. 'too_small' is than a logical vector.
  too_small <- sorted - max_val <= log(10^threshold) - log(input_length) 

  A_is <- ifelse(too_small, 0, base^(sorted - max_val))

  A_is[order(permutation)]
}

Normalize <- function(vec) vec/sum(vec)

#' @export
ThresholdNorm <- function(input, threshold, base=exp(1)) {
  A_is <- A_i(input, threshold, base)
  Normalize(A_is)
}
