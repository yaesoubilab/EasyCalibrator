# thresholdNorm is a function that normalizes values in a numeric vector such
# that every value in the vector can be better compared

# FIRST PART: sorts, sums up, and finds the max of the given vector the for
# loop runs a check to compare the each value in a vector to the smallest
# possible value in a given vector

#SECOND PART: sorts, sums, and finds
# max of resulting vector from FIRST PART normalizes the vector by dividing
# each value in vector by sum of vector

#thresholdNorm: vec, d
#	where vec is vector containing the different likelihoods where d is the
#	value of the exponent 

thresholdNorm <- function(vec, d){
  
  #FIRST PART 
  vec_sorted <- sort(vec, decreasing = TRUE)
  vec_sum <- sum(vec_sorted)
  vec_max <- max(vec_sorted)
  vec_count <- NULL
  a_i <- c(double())
  vec_n <- length(vec)
  
  #for loop that goes through sorted vector and compares 
  # the value minus the max to the smallest possible value in a given vector 
  # if smaller, value is removed & vec_count + 1
  # 	to keep track of removed values
  # if greater, add value to new vector called a_i
  for(i in vec_sorted){
      if (i - vec_max > log(10 ^ -(d)) - log(vec_n)){
      a_i <- c(a_i, i-vec_max)
    }
    else{
      vec_count <- vec_count + 1
    }
  }
  
  #SECOND PART
  a_sort <- sort(a_i, decreasing = TRUE)
  sum_final <- sum(a_sort)
  final_sum <- c(double())
  
  #for loop that goes through a_sort and divides each value by the sum 
  for(j in a_sort){
    
    divide_sum <- j/sum_final
    final_sum <- c(final_sum, divide_sum)
  }
  
  return(final_sum)
}

#thresholdNorm(c(-0.001, -0.000000000001, -0.0000000000000001), 15)
