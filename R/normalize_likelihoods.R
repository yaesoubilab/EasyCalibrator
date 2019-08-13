# norm is a function that takes in a vector and returns the normalized version of the vector 
# FIRST PART: sorts, sums up, and finds the max of the argument vector then run for loop that runs check 
# SECOND PART: sorts, sums, and finds max of sorted and checked vector then normalizes by dividing by sum
norm <- function(vec, decreasing = TRUE){
	#FIRST PART 
	sorted <- sort(vec)
	sum <- sum(sorted)
	max <- max(sorted)
	count <<- NULL
	a <<- vector()
	
	#for loop that goes through sorted vector and checks the value of the value minus the max to the smallest possible value
	# if smaller, then is taken out of the vector and adds one to the count value that keeps track of removed values 
	# if greater, add to new vector that is contains all the ones that are greater
	for(i in sorted){
		if (i - max > log(10 ^ (-15)) - log(length(vec))){
			a <- c(a, i - max)
		}
		else{
			count <- count + 1
		}
	}
	
	#SECOND PART
	a_sort <- sort(a, decreasing = TRUE)
	sum_final <- sum(a_sort)
	final_sum <<- vector()
	
	#for loop that goes through a_sort and divides each value by the sum 
	for(j in a_sort){
		val <- j/sum_final
		final_sum <- c(final_sum, val)
	}
	
	#PRINT RESULT
	print("FINAL RESULT")
	for(k in final_sum){
		print(k)
	}
}

norm(c(-0.001, -0.000000000001, -0.0000000000000001))
