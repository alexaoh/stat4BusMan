# PRACTICE 1: Select a random sample of 100 cases and estimate average age.
data <- read.csv('Prob1.csv', sep=';')
summary(data$edat)

set.seed(123)
s1 <- data[sample(nrow(data),100, replace=T),]

# first remove NAs for age column

data.withage=data[!is.na(data$edat),]
head(data.withage)
s2 <- data.withage[sample(nrow(data.withage),100, replace=T),]

summary(s1$edat)
summary(s2$edat)

# using library survey ####
library(survey)

# design = how the data was selected
# ids=1 means every sample is an individual
simple.random.sample <- svydesign(ids=~1, strata=NULL, weights=NULL, data=s2)

svymean(~edat, simple.random.sample, na.rm=TRUE)

# for stratified sampling we can specify the weights
# x.w = x.population / x.random.sample

# Weighted samples in R:
enquesta=data.frame(sexe=c(rep("HOME", 20),rep("DONA", 20))) 
enquesta$pes=0 
enquesta$pes[enquesta$sexe=="HOME"]=40/20
enquesta$pes[enquesta$sexe=="DONA"]=60/20

# method 1
disseny=svydesign(ids=~1,weights=enquesta$pes, data=enquesta) 
svytable(~enquesta$sexe, disseny) 

# method 2
library(dplyr)
count(x=enquesta, sexe, wt=pes)

# method 3
library(questionr) 
wtd.table(x = enquesta$sexe, weights = enquesta$pes)
