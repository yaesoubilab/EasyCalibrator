# thresholdNorm is a function that normalizes values in a numeric vector 
requireNamespace("assertthat")
#' @importFrom assertthat assert_that

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

#thresholdNorm: vec, threshold -> vec
#	where vec is vector containing values 
#	where d is what the base will be raised to (ex. 10^ d)

#' @export
ThresholdNorm <- function(input, threshold, base = 10){
  
  assert_that(length(input) > 1, msg="Input was of length 0 or 1")
  assert_that(is.numeric(input), msg="Input was not a numeric vector")
  #assert_that(all(input < 0),    msg="Input had a nonnegative element")
  assert_that(length(base) == 1,    msg="'base' must be one number")
  assert_that(is.wholenumber(base), msg="'base must be whole number")
  assert_that(base > 1,             msg="'base' must be >1")
  assert_that(length(threshold) == 1,    msg="'threshold' must be one number")
  assert_that(is.wholenumber(threshold), msg="'threshold' must be whole number")
  assert_that(threshold < 0,             msg="'threshold' must be <0")

  #FIRST PART 
  vec_sorted <- sort(input, index.return = TRUE)
  vec_count <- NULL
  vec_n <- length(input)
  vec_max <- vec_sorted$x[vec_n]

  sort_order <- vec_sorted$x
  init_order <- vec_sorted$ix 
  a_i <- ifelse((sort_order - vec_max) > (log(10^threshold) - log(vec_n)), 10^(sort_order - vec_max), 0)
  a_i_sum <- sum(a_i)

  norm_a_i <- a_i / a_i_sum
  
  norm_a_i[init_order]
}

#ThresholdNorm(-10:10, -1, 10)
