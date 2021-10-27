### SIMULATION_2_Lab_ Simulations in R.pdf

## Getting started step 1
vecpoisson =   rpois(100,5)
mean(vecpoisson)

## Getting started step 2
set.seed(198911)
vecpoisson=rpois(100,5)
mean(vecpoisson)

## Getting started step 3
set.seed(198911)
vecpoisson=rpois(100,5)
mean(vecpoisson)


## Using the apply function

## (generate exponential random variable)
rexp(3, 0.1)

## run 50.000 simulations
reps <- 50000
nexps <- 5
rate <- 0.1
set.seed(0)
system.time(x1 <- replicate(reps, sum(rexp(n=nexps, rate=rate))) ) # replicate

head(x1)

## check histogram (sum of 5 exponentials with rate =0.1 is a gamma with shape=5, scale=10)
## https://en.wikipedia.org/wiki/Relationships_among_probability_distributions#:~:text=The%20sum%20of%20n%20exponential,with%20N%20degrees%20of%20freedom.

require(ggplot2)
ggplot(data.frame(x1), aes(x1)) + 
  geom_histogram(aes(y=..density..)) + 
  stat_function(fun=function(x) dgamma(x, shape=nexps, scale=1/rate), 
                color="red", size=2)

## different ways to do this.
set.seed(0)
system.time(x1 <- sapply(1:reps, function(i){sum(rexp(n=nexps, rate=rate))})) # simple apply
head(x1)

set.seed(0)
system.time(x1 <- lapply(1:reps, function(i){sum(rexp(n=nexps, rate=rate))})) # list apply
head(x1)

set.seed(0)
system.time(x1 <- apply(matrix(rexp(n=nexps*reps, rate=rate), nrow=nexps),2,sum)) # apply on a matrix
head(x1)

set.seed(0)
system.time(x1 <- colSums(matrix(rexp(n=nexps*reps, rate=rate), nrow=nexps))) # using colSums
head(x1)

require(parallel)
set.seed(0)
system.time(x1 <- mclapply(1:reps, function(i){sum(rexp(n=nexps, rate=rate))})) # multi-cluster apply

### Question 1
# Let \(U_1, U_2, U_3\) all come from a uniform(0,1) distribution.
# Let \(M = \max(U_1, U_2, U_3)\).
# Estimate (to 3 significant digits) the probability \(\mathbb{P}(M > 0.75)\).

set.seed(0)
reps=3000000
x1<-replicate(reps, max(runif(3)))
sum((x1>0.75)*1)/reps
                      

## Question 2
# Let \(Z_{(n)}\) be maximum of \(n\) standard normal observations.
# Estimate what \(n\) should be so that \(\mathbb{P}(Z_{(n)} > 4) = 0.25\).

set.seed(0)
n=7820
reps=700
x1=sum(apply(matrix(rnorm(reps*n, c(0,0),sd=c(1,1)),ncol=2),1,max)>4)/reps
x1

## Question 3
# Let \(X_1, \dots, X_n\) be Poisson random variables with parameter \(\lambda = 0.5\) 
# and where \(n=21\).
# Estimate the probability that the sample mean is greater than the sample median.

reps=51000
x1=matrix(replicate(reps, rpois(21,5)), ncol=21)
sum(apply(x1,1,mean)>apply(x1,1,median))/reps


## Question 4 (to be done)
# Let \(U\) come from a uniform(0,1) distribution and \(Z\) come from a standard normal distribution.
# Let \(X = pZ + (1-p)U\).
# Estimate \(p\) when \(X\) has a variance of 0.4.

## Generating normal random variables
samples = rnorm(1000, 0, 1)

## Question 5
# Check that these are from \(N(0,1)\) using a quantile-quantile plot (Q-Q plot). 
# Use the stat_qq() function in the ggplot2 package.

ggplot(data.frame(samples), aes(sample=samples)) + 
  stat_qq()

load("mystery_samples.RData")
head(samples)

## Question 6
# Which distribution do you think the samples comes from? 
# Hint: look at the histogram, and the displot() function from the vcd
# package might be useful. You might also want to check the mean and variances.
mean(samples)
sd(samples)
hist(samples)

#install.packages("vcd")
require(vcd)
distplot(samples, type = "poisson")
distplot(samples, type = "poisson", lambda=23)
distplot(samples, type = "nbinomial", size=0.1)
distplot(samples, type = "binomial", size = 50)


## Generating Random Mixtures of Normal Data
require(ggplot2)
sampa=rnorm(1000000,0,1)
sampb=rnorm(1500000,3,1)
combined = c(sampa, sampb)
plt = ggplot(data.frame(combined), aes(x=combined)) + 
  stat_bin(binwidth=0.25, position="identity")
plt

pop1=rnorm(2000000)
pop2=rnorm(1000000, 1, 2)
combined = c(pop1, pop2)
plt= ggplot(data.frame(data=c(combined, pop1, pop2),
                       labels=rep(c("combined", "pop1", "pop2"), 
                                  c(3e6, 2e6, 1e6))),
            aes(x=data)) + stat_bin(aes(fill=labels), 
            position="identity", binwidth=0.25, alpha=0.5) + theme_bw()
plt

#install.packages("Biostrings")
require(Biostrings)

## Question 9
# Show how to do this using sample().
# generate a random DNA string with an independent background multinomial 
# of \((p_A,p_C,p_G,p_T)=(0.2,0.3,0.2,0.3)\).

sample(c("A", "C", "G", "T"), 10, replace=T,prob=c(0.2,0.3,0.2,0.3))

## MONTE CARLO Simulation
isEvent = function(numDice, numSides, targetValue, numTrials){
  apply(matrix(sample(1:numSides, numDice*numTrials, replace=TRUE), nrow=numDice), 2, sum) >= targetValue
}

set.seed(0)
#try 5 trials
outcomes = isEvent(2, 6, 7, 5)
mean(outcomes)

# This is far from the theoretical answer of \(\frac{21}{36}=0.58333\). 
# Now try with 10,000 trials:
set.seed(0)
outcomes = isEvent(2, 6, 7, 10000)
mean(outcomes)

require(parallel)

isEventPar = function(numDice, numSides, targetValue, trialIndices){
  sapply(1:length(trialIndices), function(x) sum(sample(1:numSides, numDice, replace=TRUE)) >= targetValue)
}

## pvec() parallelises the vector in pieces
set.seed(0)
outcomes = pvec(1:10000, function(x) isEventPar(2, 6, 7, x))
mean(outcomes)

## Gamma mixture of poissons
lambdas = rgamma(100, shape=2, scale=3)
samples = rep(0, 100)
for (i in 1:100)
  samples[i] = rpois(1, lambdas[i])

## Question 13
# Using the goodfit() and rootogram() functions in the vcd package, 
# what distribution do you think fits the gamma-poisson mixture best? 
# (Hint: Beware of the Chisquare test when some counts are smaller than 5.)

fit=goodfit(samples, "nbinomial")
summary(fit)
fit2=goodfit(samples, "poisson")
summary(fit2)
rootogram(fit)
rootogram(fit2)

## Power calculation

# The power of a statistical test is the probability that the test rejects 
# the null hypothesis if the alternative is true. There is rarely a closed form
# for the power, so we resort to simulation. An important question in many clinical
# trials is how many subjects (samples) do we need to achieve a certain amount of power?

# Suppose we want to find out how many samples are needed to distinguish 
# between the means of two normal distributions, \(N(1, 0.5)\) and
# \(N(2, 0.5)\) with a power of at least 0.8 at the 0.05 significance level.

# We'll take \(n\) samples from each population, and compute the 
# statistic \(\frac{\bar{X}_1-\bar{X}_2}{\sqrt{(0.5^2+0.5^2)/n}}\). 
# Under the null hypothesis that the two means are the same, this statistic 
# has a \(N(0, 1)\) distribution, and the \(p\)-value is 
# \(2P\left(N(0,1)\geq\left|\frac{\bar{X}_1-\bar{X}_2}
# {\sqrt{(0.5^2+0.5^2)/n}}\right|\right)\).

compute_power = function(n, sigma, numTrials){
  sampa = matrix(rnorm(n*numTrials, 1, sigma), ncol=numTrials)
  sampb= matrix(rnorm(n*numTrials, 2, sigma), ncol=numTrials)
  statistics = (apply(sampa, 2, mean) - apply(sampb, 2, mean))/sqrt(2*sigma^2/n)
  return (mean(abs(statistics) >= qnorm(0.975)))
}

set.seed(0)
compute_power(3, 0.5, 10000)
compute_power(4, 0.5, 10000)
