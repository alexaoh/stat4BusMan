setwd("/home/ajo/gitRepos/stat4BusMan/Week4")
Y<-read.csv("prod.csv",sep = ",",col.names = 1)
Y<-as.matrix(Y)
head(Y)
summary(Y)
# 2.1
# The optimal limits are:
mu=30
sigma=sqrt(2)
LSL<-(-6)*sigma+mu
USL<-6*sigma+mu
USL
LSL
DPMO<-1000000*(1-pnorm(USL,mu+1.5*sigma,sigma))
DPMO

DPMO<-1000000*(pnorm(LSL,mu-1.5*sigma,sigma))
DPMO

# 2.2
x <- seq(min(Y), max(Y), length = 100)
f1 <- dnorm(x, mean = mu+1.5*sigma, sd = sigma)
f2 <- dnorm(x, mean = mu, sd = sigma)
hist(Y,xlim = c(LSL,USL),ylim=c(0,0.3),probability = T,main = "")
lines(x,f1,type="l",lwd=2,lty=2)
lines(x,f2,type="l",lwd=2,lty=1)
abline(v=LSL,lwd=2,lty=2)
abline(v=USL,lwd=2,lty=2)

# 2.3
faults<-sum((Y>USL)+(Y<LSL))
faults

# 2.4
prob<-faults/nrow(Y)
prob
k<-qnorm(1-prob,1.5,1)
k

# 2.5
Cp_opt<-(USL-LSL)/(6*sigma)
Cp_opt
Cp<-(USL-LSL)/(6*sqrt(var(Y)))
Cp
