# Statistical Process Control

setwd("/home/ajo/gitRepos/stat4BusMan/Practice_SPC")

#EXAMPLE 1
# X mean chart with known target 

CShaft <- read.table("CcShaftDM.txt")

mean <- 0.407; stdv <- 0.0003; n <- 5
# mean(c(CShaft[,1], CShaft[,2], CShaft[,3], CShaft[,4], CShaft[,5]))
# sd(c(CShaft[,1], CShaft[,2], CShaft[,3], CShaft[,4], CShaft[,5]))
# ncol(CShaft)

# Control limit calculation (considering with 3 sigma):

# LCL <- mean - 3 * (stdv / sqrt(n)); LCL
# UCL <- mean + 3 * (stdv / sqrt(n)); UCL


# Alternatively using 'qcc' package
# install.packages("qcc")
library(qcc)

# By default, qcc uses sigma = 3. 
qcc(CShaft, type = "xbar",center = mean, std.dev = stdv)

# Changing into sigma = 2
qcc(CShaft, type = "xbar",center = mean, std.dev = stdv, nsigmas = 2)

# Changing the confidence level to construct alternative X mean charts
qcc(CShaft, type = "xbar",center = mean, std.dev = stdv, confidence.level = 0.99)

# Working with another type of data
CShaftD <- read.table("CcShaftD.txt")

# Comparing
head(CShaft);head(CShaftD)

Diam <- qcc.groups(CShaftD$ShaftDiam, CShaftD$Day)
qcc(Diam, type= "xbar", center = mean, std.dev = stdv)

# Number of shafts between the specification limits (mean +- 0.00025):
# sum(CShaft >= mean -0.00025 & CShaft <= mean + 0.00025)


# X mean chart with estimated target values

# library(qcc)
data("pistonrings")
attach(pistonrings)

head(pistonrings)
piston <- qcc.groups(diameter,sample)

# Default std based on ranges
qcc(piston[1:25,], type = "xbar", newdata = piston[26:40,])
qcc(piston[1:25,], type = "xbar", newdata = piston[26:40,],nsigmas = 2)
# The same
qcc(piston[1:25,], type = "xbar", newdata = piston[26:40,], std.dev = "UWAVE-R")
qcc(piston[1:25,], type = "xbar", newdata = piston[26:40,], std.dev = "UWAVE-R")$std.dev

# Based on stds 
qcc(piston[1:25,], type = "xbar", newdata = piston[26:40,], std.dev = "UWAVE-SD")
qcc(piston[1:25,], type = "xbar", newdata = piston[26:40,], std.dev = "UWAVE-SD")$std.dev

# Comands to delete subgroups from the Calibration Data: Out of control during the collection data to estimate target
qcc(piston[-c(14),], type = "xbar", newdata = piston[26:40,])


# X mean chart when n = 1 (example)

x <- pistonrings[,1]
qcc(x,type= "xbar.one")
qcc(x[c(2:66,68:95)], type = "xbar.one", newdata = x[96:length(x)])
#mean and the average moving range are computed from the entired dataset.
x[65:74]
which(x<73.97)
# Average Run Length and Supplemental Rules

Delta <- seq(0, 1.4, .2) 
p <- 1 + pnorm(-3-sqrt(5)*Delta)-pnorm(3-sqrt(5)*Delta)
ARL<-1/p
results<-rbind(p,ARL)
knitr::kable(results,digits=3, caption="Probability of out-of-control and ARL")

## Monitoring process variability ##
# The S and R Charts

qcc(piston, type = "S")
qcc(piston[1:25,], type = "S", newdata = piston[26:40,])
# Neither of these charts suggests that the process variability is out of control.

qcc(piston, type = "R")
qcc(piston[1:25,], type = "R", newdata = piston[26:40,])

# Exercise 3
# Implementation of the Chi-Squared based S chart

stilde <- sqrt(mean(apply(piston[1:25,],1,var)))
n <- 5; LCL <- stilde * sqrt(qchisq(0.0015,n-1)/(n-1))
UCL <- stilde * sqrt(qchisq(0.9985,n-1)/(n-1))

sdv <- apply(piston,1,sd)
plot(1:40,sdv, ylim = c(0.0015,0.0208), pch = 4, main = "Chi square S chart")
lines(1:40,sdv,lty=1)
axis(4,at=c(LCL, UCL), lab = c("LCL", "UCL"))
abline(h=LCL, col = "red"); abline(h=UCL, col = "red");abline(h=(LCL+UCL)/2, col = "blue");



# The p and c Charts
data(orangejuice)
attach(orangejuice)
head(orangejuice)

# 54 samples each of size 50 of orange juice cans collected at half-hour intervals.
# The first 30 samples were taken when the machine was in continuous operation.
# The last 24 were taken after an adjustment was made.
# It is believed that the process was in control before the adjustment. Construct a 3 sigma p chart using the first 30 sample as the calibration data set.

qcc(D[trial], sizes = size[trial], type = "p", newdata = D[!trial], newsizes = size[!trial])
qcc(D[1:30], sizes = size[1:30], type = "p", newdata = D[31:54], newsizes = size[31:54])

# Upon investigation, it turns out that sample 15 used a different batch of cardboard, 
#while sample 23 was obtained when an inexperienced operator was temporarily assigned to the machine

qcc(D[c(1:30)[-c(15,23)]], sizes = size[c(1:30)[-c(15,23)]], type = "p", newdata = D[31:54], newsizes = size[31:54])

# Turning p into np chart
qcc(D[trial], sizes = size[trial], type = "np", newdata = D[!trial], newsizes = size[!trial])
qcc(D[c(1:30)[-c(15,23)]], sizes = size[c(1:30)[-c(15,23)]], type = "np", newdata = D[31:54], newsizes = size[31:54])


# The c Chart
rm(list=setdiff(ls(), "piston"))
data(circuit)
attach(circuit)
head(circuit)
# x: number of nonconformities in each batch.
# size: sample size of each batch.

qcc(x[trial], sizes = size[trial], type = "c", newdata = x[!trial], newsies = size[!trial])

# Upon investigation, it turns out that sample 6 was examined by a new inspector who was not trained to recognize several types of nonconformities.
# The large number of nonconformities in sample 20 resulted from a temperature control problem in the wave soldering machine. 

qcc(x[c(1:26)[-c(6,20)]], sizes = size[c(1:26)[-c(6,20)]], type = "c", newdata = x[!trial], newsies = size[!trial])

# U Chart
# Plots the average number of defectives per unit (still a Poisson)

qcc(x[trial], sizes = size[trial], type = "u", newdata = x[!trial], newsizes = size[!trial])
qcc(x[c(1:26)[-c(6,20)]], sizes = size[c(1:26)[-c(6,20)]], type = "u", newdata = x[!trial], newsizes = size[!trial])


# CUSUM and EWMA Charts

cusum(piston[1:25,], std.dev = "UWAVE-SD")
cusum(piston[1:25,], newdata = piston[26:40,], std.dev = "UWAVE-SD")

# Using different k = 0.5  (in R, se.shift = 1 by default)
# se.shift: The amount of shift to detect in the process, measured in standard errors of the summary statistics.: 
cusum(piston[1:25,], std.dev = "UWAVE-SD", se.shift = 1.5, decision.interval = 6) # Decision.interval by default is 5.
cusum(piston[1:25,], newdata = piston[26:40,], std.dev = "UWAVE-SD", se.shift = 1.5, decision.interval = 6)

# Tabular form k=0.5, h=5
mean(apply(piston[1:25,],1,mean)); #74.00118
mean(apply(piston[1:25,],1,sd)) # 0.00983

z <- (apply(piston[1:25,],1,mean)-74.00118)/(0.009829977/sqrt(5))

SH <- rep(0,25); SH[1] <- max(z[1]-0.5,0)
for(i in 2:25){ SH[i] <- max(SH[i-1] + z[i] - 0.5,0)}

SL <- rep(0,25); SL[1] <- max(z[1]-0.5,0)
for(i in 2:25){ SL[i] <- max(SL[i-1] - z[i] - 0.5,0)}

# Alternativelly
obj <- cusum(piston[1:25,], std.dev = "UWAVE-SD")
SH <- obj$pos; SL <- -obj$neg

matrix <- cbind(1:25,apply(piston[1:25,],1,mean),z,SH,SL)
colnames(matrix)[1:2] <- c("Sample", "x")
matrix


##Subgroup means with + signs, sigma estiamte based on rangs r_i.

ewma(piston[1:25,])
ewma(piston[1:25,], newdata = piston[26:40,])

# To use different values of gamma (called lambda in R) from the default value 0.2, say 0.4, use the option gamma = 0.4

ewma(piston[1:25,], lambda = 0.4)
ewma(piston[1:25,], newdata = piston[26:40,], lambda = 0.4)
