# thresholdNorm is a function that normalizes values in a numeric vector 

# FIRST PART: sorts, sums up, and finds the max of the given vector the for
# loop runs a check to compare the each value in a vector to the smallest
# possible value in a given vector then keeps anything greater than 0
# and sets anything less to 0

#SECOND PART: sorts, sums, and finds n

#thresholdNorm: vec, threshold -> vec
#	where vec is vector containing values 
#	where d is what the base will be raised to (ex. 10^ d)

thresholdNorm <- function(input, base=10, threshold){
  
  #FIRST PART 
  vec_sorted <- sort(input, decreasing = TRUE, index.return = TRUE)
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

#thresholdNorm(c(-0.001, -0.000000000001, -0.0000000000000001), base = 10, 15)
