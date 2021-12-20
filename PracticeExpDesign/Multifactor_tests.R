# Multifactor Experiments

setwd("C:\\Clases\\EGE_2021_2022\\Theme IV\\Practice")
rm(list=ls())


library(conf.design)

# Two-Factor Designs

# R Commands for the ANOVA F Tests and Tukey's (1 ??? alpha)100% Simultaneous CIs
# fit=aov(y???A*B); anova(fit); TukeyHSD(fit, conf.level=1-alpha)

# Example: 
# The data file CloudSeed2w.txt contains the rainfall measurements of the previous example. 
# Use R commands to construct the ANOVA table and to perform Tukey's 95% simultaneous CIs and 
# multiple comparisons for determining the pairs of seasons that are significantly different at 
# experiment-wise error rate of 0.05.

cs=read.table("CloudSeed2w.txt", header=T)
y=cs$rain; A=cs$seeded; B=cs$season
fit=aov(y~A*B); anova(fit)

TukeyHSD(fit)

# Testing de Validity of Assumptions

# R Command for Testing the Homoscedasticity Assumption
# anova(aov(resid(fit)**2???A*B))

# R Command for Testing the Normality Assumption
# shapiro.test(resid(fit))

# plot(fit, which=1); plot(fit, which=2) will display the residuals by groups (labeled by the fitted, or the mujl, values) and produce a Q-Q plot for the combined residuals, respectively.

# Example:
# Check whether the previous data satisfies the homoscedasticity and normality assumptions.

anova(aov(resid(fit)**2~A*B))
plot(fit, which=1)

shapiro.test(resid(fit))
plot(fit, which = 2)


# One Observation per Cell

# Example: 
# Use R commands to generate data from an additive, and also from a non-additive,
# 3 ? 3 design with one observation per cell and non-zero main effects.
# (a) Apply Tukey's one degree of freedom test for interaction to both data sets,
# stating whether or not the additivity assumption is tenable.
# (b) Regardless of the outcome of Tukey's tests in part (a), assume the additive
# model to analyze both data sets, stating whether or not the main effects for
# factors A and B are significantly different from zero at level of significance alpha = 0.05

S=expand.grid(a=c(-1, 0, 1), b=c(-1, 0, 1));
y1=2+S$a+S$b+rnorm(9, 0, 0.5)
y2=2+S$a+S$b+S$a*S$b+rnorm(9, 0, 0.5)

# (a)
A=as.factor(S$a); B=as.factor(S$b); fit=aov(y1~A+B)
fitteds=(fitted(fit))**2; anova(aov(y1~A+B+fitteds))

A=as.factor(S$a); B=as.factor(S$b); fit=aov(y2~A+B)
fitteds=(fitted(fit))**2; anova(aov(y2~A+B+fitteds))

# (b)

anova(aov(y1~A+B))
anova(aov(y2~A+B))



# Three Factor Designs

# R Commands for Fitting the Full and Reduced Models
# out=aov(y~A*B*C); anova(out) # for fitting the full model
# out=aov(y~A+B+C); anova(out) # for fitting the additive model
# out=aov(y~A*B+A*C+B*C); anova(out) # for fitting the model without ABC interactions

# Example: 

# A paper reports on a study sponsored by CIFOR (Center for International Forestry Research) to evaluate 
# the effectiveness of monitoring methods related to water and
# soil management. Part of the study considered soil runoff data from two catchment
# areas (areas number 37 and 92) using runoff plots classified as "undisturbed/control"
# and "harvested." The runoff volume was calculated at each rainfall event, with the
# amount of rainfall serving as an additional factor at three levels (3.5-10 mm, 10- 20 mm, and > 20 mm). 
# The data, consisting of four measurements per factor-level
# combination, is in SoilRunoff3w.txt. Use R commands to complete the following parts using level of significance 
# of alpha = 0.05.

# (a) Construct the ANOVA table corresponding to fitting the full model and report which of the null hypotheses 
# are significant.
# (b) Construct the ANOVA table corresponding to fitting the additive model and
# report which of the null hypotheses are significant.
# (c) Construct the ANOVA table corresponding to fitting the model with no three-factor interactions, 
# and report which of the null hypotheses are significant.
# (d) Using the residuals from fitting the full model, check the homoscedasticity and
# normality assumptions.

# (a)
rm(list=ls())
Data <- read.table("SoilRunoff3w.txt")
attach(Data)

out=aov(y~Rain*Log*Catch); anova(out)

# It follows that the main effects of the factors "Rain" and "Log" (for "logging")
# are significant at level 0.05, and so are their interaction effects. All other effects
# are not significant; thus, the "Catch" factor has no impact on the average (over 
# the other factors) mean runoff volume, does not interact with the other two
# factors, and has no impact on the interaction of the factors "Rain" and "Log"
# (i.e., no significant three-factor interaction).

# (b)
anova(aov(y~Rain+Log+Catch))

# (c)
out1=aov(y~Rain*Log+Rain*Catch+Log*Catch); anova(out1)

# (d)
anova(aov(resid(out)**2~Rain*Log*Catch))
plot(out,which=1)

shapiro.test(resid(out))
plot(out, which=2)

# When the assumptions underlying the validity of the ANOVA F tests appear
# to be violated, as in the previous example, a Q-Q plot of the estimated main effects and
# interactions can add credibility to the significant outcomes of the F tests.

eff=effects(out); qqnorm(eff[-1]); qqline(eff[-1], col="red")

# Looking for an explanation: INTERACTIONS.

interaction.plot(Rain,Log, y, col=c(2,3))
interaction.plot(Rain[Catch=="C37"],Log[Catch=="C37"], y[Catch=="C37"], col=c(2,3))
interaction.plot(Rain[Catch=="C92"],Log[Catch=="C92"], y[Catch=="C92"], col=c(2,3))


