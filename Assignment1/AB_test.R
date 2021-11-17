# A/B test of Cookie Cats game.
library(tidyverse)
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

# Check the t-test for retention_7! (just to see what it gives compared to the t-test package).
conversion_subset_control <- data %>% filter(version == "gate_30" & retention_7 == 1)

#Number of Conversions for the control.
(conversions_control <- nrow(conversion_subset_control))

#Number of users for the control.
users_control <- nrow(data %>% filter(version == "gate_30"))

#Conversion_rate_control
(conv_rate_control <-  (conversions_control/users_control))

#let's take a subset of conversions for the treatment.
conversion_subset_treat <- data %>% filter(version == "gate_40" & retention_7 == 1)

#Number of Conversions for variant_A
(conversions_treat <- nrow(conversion_subset_treat))

#Number of users for the treatment. .
users_treat <- nrow(data %>% filter(version == "gate_40"))

#Conversion_rate_A
(conv_rate_treat <-  (conversions_treat/users_treat))

(p_pool <- (conversions_control + conversions_treat)/(users_control + users_treat))

#3. Let's compute the standard error (SE_pool)
(SE_pool<- sqrt(p_pool*(1-p_pool) * ((1/users_control) + (1/users_control))))

#4. Let's compute the margin of error for the pool
(MOE <- SE_pool * qnorm(0.975))
  
#5. Point Estimate or Difference in proportion
(d_hat <- conv_rate_treat - conv_rate_control)

#6. Compute the Z-score so we can determine the p-value
(z_score <- d_hat/SE_pool)

# Let's calculate P-value and confidence interval -------------------------
#Method 1
#7. Let's compute p_value using the z_score value
(p_value <- pnorm(q = -z_score, mean = 0, sd = 1) * 2)

# CONFIDENCE INTERVALS FOR RETENTION RATES
# One confidence interval for each of the two 
(X_hat_treat <- conversions_treat/users_treat)

(se_hat_treat <- sqrt(X_hat_treat*(1-X_hat_treat)/users_treat))

# Compute standard error of test_version_A

(X_hat_control <- conversions_control/users_control)

(se_hat_control <- sqrt(X_hat_control*(1-X_hat_control)/users_control))

# # Compute the 95% confidence interval for B
# # Save the lower and then the upper confidence interval to a variable called `ci`.
# c(-qnorm(.975), qnorm(.975))    #95% confidence interval

#Compute 95% COnfidence Interval for Version B
(ci_treat <- c(X_hat_treat - qnorm(0.975)*se_hat_treat, X_hat_treat + qnorm(0.975)*se_hat_treat))

# Compute the 95% confidence interval for A
(ci_control <- c(X_hat_control - qnorm(0.975)*se_hat_control, X_hat_control + qnorm(0.975)*se_hat_control))
