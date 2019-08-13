norm <- function(vec){
sorted <- sort(vec)
sum <- sum(sorted)
max <- max(sorted)
count <<- NULL
a <<- vector()
for(i in sorted){
if (i - max > log(10 ^ (-15) - log(length(vec)))){
append(a, i)
}
else{
count <- count + 1
}
}

a_sort <- sort(a)
sum_final <- sum(a_sort)

final_sum <<- vector()
for(j in a_sort){
val <- j/sum_final
append(final_sum, val)
}

print(final_sum)
}

norm(c(1, 123), c(2, 234), c(5, 567))
