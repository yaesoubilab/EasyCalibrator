norm <- function(vec, decreasing = TRUE){
sorted <- sort(vec)
sum <- sum(sorted)
max <- max(sorted)
count <<- NULL
a <<- vector()
for(i in sorted){
if (i - max > log(10 ^ (-15)) - log(length(vec))){
a <- c(a, i)
}
else{
count <- count + 1
}
}

a_sort <- sort(a, decreasing = TRUE)
sum_final <- sum(a_sort)
final_sum <<- vector()
for(j in a_sort){
val <- j/sum_final
final_sum <- c(final_sum, val)
}

print("FINAL RESULT")
for(k in final_sum){
print(k)
}
}

norm(c(-0.001, -0.000000000001, -0.0000000000000001))
