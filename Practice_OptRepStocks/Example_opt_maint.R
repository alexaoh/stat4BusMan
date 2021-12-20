#Set working directory
setwd("/home/ajo/gitRepos/stat4BusMan/Practice_OptRepStocks")

# Reading grouped failure data

fdata<-read.table("Timefailure.csv",sep=";",header = T)

fdata$time<-(fdata$time_lim_l+fdata$time_lim_u)/2
fdata2<-fdata[,c("failures","time")]
# Ungrouping data
nint<-nrow(fdata2)
nint
uno<-matrix(fdata2$time[1],fdata2$failures[1],1)
uno
for(i in 2:nint){
  dos<-rbind(uno,matrix(fdata2$time[i],fdata2$failures[i],1))
  uno<-dos
}

Y<-dos
summary(Y)
hist(Y)
# Fitting time distributions
library(MASS)

## Exponential
fitExp<-fitdistr(Y,"exponential")
fitExp$estimate
fitExp$loglik
AIC(fitExp)

## Weibull
scale<-1/fitExp$estimate
fitWeib<-fitdistr(Y,"weibull",list(shape = 1, scale =scale))
fitWeib$estimate
fitWeib$loglik
AIC(fitWeib)

## Log-normal
fitLn<-fitdistr(Y,"lognormal")
fitLn$estimate
fitLn$loglik
AIC(fitLn)


## Comparint Weibull and Lognormal
t<-as.vector(c(0:2600))
hist(Y,xlim = c(0,2600), probability = T,main = "Time failure")
lines(t,dweibull(t,fitWeib$estimate[1],fitWeib$estimate[2]),lty=1)
lines(t,dlnorm(t,fitLn$estimate[1],fitLn$estimate[2]),lty=2)
legend("topright",c("Weibull","Lognormal"),lty=c(1,2))

# Weibull results
## Hazard function

t<-as.vector(c(1:4000))
lambda<-(fitWeib$estimate[1]/fitWeib$estimate[2])*(t/fitWeib$estimate[2])**(fitWeib$estimate[1]-1)
plot(t,lambda, type="l")


# Average life time
mu0<-fitWeib$estimate[2]*gamma(1+1/fitWeib$estimate[1]) # Is the mean
mu0

# Times between inspection 
R<-0.9 # is Pr(Y>y)
mu2<-fitWeib$estimate[2]*(-log(R))**(1/fitWeib$estimate[1]) # Is the inverse of the reliavility function (or S(y))
mu2

# Number of failures per hours
lambda<-1/mu2
lambda

H<-30*4*8 # Time scale
H
lambdaH<-lambda*H
lambdaH

# Maintenance parameters
# Mean time before failures mu_1 (MTBF)
## Mean failure per hour
one<-1/MTBF 
## Weibull parameters
beta<-fitWeib$estimate[1]
alpha<-fitWeib$estimate[2]

# Failure rate under prevention maintenance
one_num<-factorial(1/beta)
two_num<-1-exp(-(mu2/alpha)**beta)
one_den<-(mu2/alpha)*exp(-(mu2/alpha)**beta)
two_den<-((mu2/alpha)**beta)/beta
lim_lambda_p<-one*one_num*two_num/(one_den+two_den)
mu1<-1/lim_lambda_p
mu1
