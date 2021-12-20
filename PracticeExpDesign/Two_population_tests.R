# Comparing two populations

setwd("C:\\Clases\\EGE_2021_2022\\Theme IV\\Practice")

x1 <- c(4.1,4.02,4.12,4.11,4.05,4.13,4.1,4.07)
x2 <- c(4.05,3.89,4.09,3.96,3.98,4.05,4.02,4.04)

# R Commands for Testing H0 : mu1 _ mu2 
# Assuming that both standard deviation are the same:

t.test(x1, x2, var.equal=T)

# alternative="greater", "less", "two.sided"
t.test(x1, x2, var.equal=T, alternative= "greater")
t.test(x1, x2, var.equal=T, alternative= "less")
t.test(x1, x2, var.equal=T, alternative= "two.sided")

# conf.level = 0.99
t.test(x1, x2, var.equal=T, conf.level=0.99)

# without Assuming that both standard deviation are the same
t.test(x1, x2, mu= 0, alternative = "greater")
t.test(x1, x2, mu= 0, alternative= "less")
t.test(x1, x2, mu= 0, alternative= "two.sided")



# Test about proportions

# R Commands for the Z Procedure for Testing H0 : p1 ??? p2 = 0
x1 <- 0.156; x2 <- 0.149; n1 <- 20; n2 <- 45
x = c(x1, x2); n = c(n1, n2)
x*n
prop.test(x, n, alternative="greater", correct=F)
# if Ha : p1 - p2 > 0
prop.test(x, n, alternative="less", correct=F)
# if Ha : p1 - p2 < 0
prop.test(x, n, alternative="two.sided", correct=F)
# if Ha : p1 - p2 ne 0


# Example with data: 
# A certain type of tractor is being assembled at two locations, L1 and L2. An investigation into the 
# proportion of tractors requiring extensive adjustments after assembly
# finds that in random samples of 200 tractors from L1 and 400 from L2, the number
# requiring extensive adjustments were 16 and 14, respectively.

# Test: H0: p1 = p2 against H1: p1 > p2 and construct a 99% CI for p1-p2

# Here n1 = 200, n2 = 400, p1 = 0.08, and p2 = 0.035

prop.test(c(16, 14), c(200, 400), correct=F, conf.level=0.99)



# The Rank-Sum Test Procedure

# Example
# The sputum histamine levels (in µg/g) from a sample of size 9 allergic individuals and 13 non-allergic individuals are as follows:
# Allergic 67.7, 39.6, 1,651.0, 100.0, 65.9, 1,112.0, 31.0, 102.4, 64.7
# Non-Allergic 34.3, 27.3, 35.4, 48.1, 5.2, 29.1, 4.7, 41.7, 48.0, 6.6, 18.9, 32.4, 45.5
# Is there a difference between the two populations? Test at level alpha = 0.01, and use R commands to construct a 95% CI for the median of the difference between the histamine levels of an allergic and non-allergic individual.

x1 <- c(67.7,39.6,1651,100,65.9,1112,31,102.4,64.7)
x2 <- c(34.3,27.3,35.4,48.1,5.2,29.1,4.7,41.7,48,6.6,18.9,32.4,45.5)

n1=9; n2=13; n=n1+n2; x=c(x1, x2); r=rank(x)

# or easily this way:
wilcox.test(x1, x2, conf.int=T)


# Comparing Two Variances

## The F Test under normality

#R Commands for the F Test for H0 : sigma2_1 = sigma2_2
var.test(x1, x2, alternative="two.sided") 

## Levene's Test for non-normal populations or small samples.

# install.packages("lawstat")
library(lawstat)
x = c(x1, x2); ind = c(rep(1, length(x1)), rep(2, length(x2)));
levene.test(x, ind)

# The following code also return the value of T^EV_H0:
v1=abs(x1-median(x1)); v2=abs(x2-median(x2));
t.test(v1, v2, var.equal=T)

# Example: 
# Numerous studies have shown that cigarette smokers have a lower plasma concentration of ascorbic acid (vitamin C) than nonsmokers. Given the health benefits
# of ascorbic acid, there is also interest in comparing the variability of the concentration in the two groups. The following data represent the plasma ascorbic
# acid concentration measurements (µmol/l) of five randomly selected smokers and nonsmokers:

# Test the hypothesis H0 : sigma2_1 = sigma2_2 versus Ha : sigma2_1 ne sigma2_2 at alpha = 0.05.

x1=c(41.48, 41.71, 41.98, 41.68, 41.18); x2=c(40.42, 40.68,40.51, 40.73, 40.91)
x = c(x1, x2)
ind = c(rep(1, length(x1)), rep(2, length(x2)));

## The F Test under normality

#R Commands for the F Test for H0 : sigma2_1 = sigma2_2
var.test(x1, x2, alternative="greater") # if Ha : sigma2_1 > sigma2_2
var.test(x1, x2, alternative="less") # if Ha : sigma2_1 < sigma2_2
var.test(x1, x2, alternative="two.sided") # if Ha : sigma2_1 ne sigma2_2
# The Levene's test
levene.test(x, ind)
levene.test(x, ind,kruskal.test = T)


# Paired Data

## The Paired Data T Test

t.test(x1, x2, mu= 0, paired=T, alternative="greater")
# if Ha : µ1 - µ2 > 0 (0 = the value we introduce at mu)
t.test(x1, x2, mu=0, paired=T, alternative="less")
# if Ha : µ1 - µ2 < 0
t.test(x1, x2, mu=0, paired=T, alternative="two.sided")
# if Ha : µ1 - µ2 ne 0

# Example:
# Changes in the turbidity of a body of water are used by environmental or soil engineers as an indication that 
# surrounding land may be unstable, which allows sediments to be pulled into the water. 
# Turbidity measurements using the Wagner test, from 10 locations around a lake taken both before and after a 
# land-stabilization project, can be found in Turbidity.txt. Is there evidence that the land-stabilizing measures 
# have reduced the turbidity?

# (a) Using R commands test the appropriate hypothesis at level alpha= 0.01, give the p-value, and construct a 
# 99% CI. What assumptions, if any, are needed for the validity of this procedure?

tb <- read.table("Turbidity.txt", header = TRUE)
t.test(tb$Before, tb$After, paired=T, alternative="greater")

# Because of the small sample size, the normality assumption is needed for the validity of the procedure. Repeating the command with the default alternative and confidence level specification of 0.99, that is:
t.test(tb$Before, tb$After, paired=T, conf.level=0.99)

# (b) Conduct the test and CI assuming that the two samples are independent and compare the results with those obtained in part (a). Use the default statistic that does not assume equal variances.
t.test(tb$Before, tb$After, alternative="greater")
t.test(tb$Before, tb$After, conf.level=0.99)


## The Paired T Test for proportions

# Example:
# To assess the effectiveness of a political speech in changing public opinion on a proposed reform, a random sample of n = 300 voters was asked, both before and after the speech, if they support the reform. The before-after data are given in the table below:
#	     After
#	    Yes No
# Before Yes 80 100
#        No  10 110
# Was the political speech effective in changing public opinion? Test at alpha = 0.05.

n = 300; Y1=80; Y2=100; Y3=10; Y4 = 110
PairedT <- (Y2/n-Y3/n)/sqrt((Y2/n+Y3/n-(Y2/n-Y3/n)^2)/(n-1))
PairedT
1-pnorm(PairedT)
MN <- (Y2-Y3)/sqrt(Y2+Y3)
MN
1-pnorm(MN)

# Because of the large sample size, we use z_0.95 = 1.96 as the critical point.
# Since both 8.58 and 9.86 are greater than 1.96, we conclude that the political speech was effective in changing 
# public opinion.


## The Wilcoxon Signed-Rank Test
# Example
# The octane ratings of 12 gasoline blends were determined by two standard methods
# Is there evidence, at level alpha = 0.1, that the rating produced by the two methods differs?

# (b) Use R commands to obtain the p-value and to construct a 90% CI for µ_D.

d=c(2.1, 3.5, 1.6, 0.2,-0.6, 2.2, 2.5, 2.8, 2.3, 6.5, -4.6, 1.6)
wilcox.test(d, conf.int=T,conf.level=0.9)
