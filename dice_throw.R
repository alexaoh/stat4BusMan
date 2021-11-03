# Simulate and calculate the probability of throwing 2 dices and the sum of the result is equal or larger than 10.

set.seed(1222)
N <- 10000 # Number of repetitions.
throws <- rep(0,N)
for (i in 1:N){
  throws[i] <- sample(x = c(1,2,3,4,5,6), size = 1) + sample(x = c(1,2,3,4,5,6), size = 1) 
}

prob <- length(throws[throws>=10])/N
prob

# This is obviously not the most efficient code for simulation (especially because of the loop). 
# but I did not have time to optimize it during the test. Could be optimized using apply, lapply, replicate
# or similar functions. 
# The correct result should be 6/36 = 1/6, which the simulated value is not too far off with 10000 repetitions. 
