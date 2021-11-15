# A/B test of Cookie Cats game.
setwd("/home/ajo/gitRepos/stat4BusMan/Assignment1")
data <- read.csv("cookie_cats.csv")
head(data)
str(data)
data$retention_1 <- as.integer(data$retention_1)
data$retention_7 <- as.integer(data$retention_7)
str(data)

# Make OEC column. 
w1 <- 0.6
w2 <- 0.3
w3 <- 0.1

max_gamerounds <- max(data$sum_gamerounds)
min_gamerounds <- min(data$sum_gamerounds)

# Maybe rescale all values between something else than 0 and 1 (to spread them more out) if this does not work. 
data$OEC <- w1*data$retention_7 + w2*data$retention_1 + w3*(data$sum_gamerounds-min_gamerounds)/(max_gamerounds-min_gamerounds)
head(data)
summary(data)
