# Comparing k > 2 populations (357-397)

setwd("/home/ajo/gitRepos/stat4BusMan/PracticeExpDesign")

# Anova F Test for means

# R Commands for the ANOVA F Test for H0 : ?1 = ???= ?k
# fit=aov(Value???as.factor(Sample), data=df); anova(fit)

# Example:
# To compare three different mixtures of methacrylic acid and ethyl acrylate for
# stain/soil release effectiveness, 5 cotton fabric specimens are treated with each mixture and tested. 
# The data are given in FabricSoiling.txt. Do the three mixtures differ in the stain/soil release effectiveness? 
# Test at level of significance alpha=0.05 and report the p-value.
fs <- read.table("FabricSoiling.txt")
aov(Value~Sample, data=fs)
anova(aov(Value~Sample, data=fs))

# Example:
# A quantification of coastal water quality converts measurements on several pollutants to a water quality index 
# with values from 1 to 10. An investigation into the after-clean-up water quality of a lake focuses on five areas 
# encompassing the two beaches on the eastern shore and the three beaches on the western shore. Water quality index 
# values are obtained from 12 water samples from each beach. The data are found in WaterQualityIndex.txt. 
# One objective of the study is the comparison of the water quality on the eastern and western shores of the lake.
# (a) Identify the relevant contrast, theta, for the study's objective, and use the data to (i) carry out the test 
# of H0 : theta = 0 vs Ha : theta ne 0 at level of significance alpha = 0.05, and (ii) to construct a 95% CI for theta.
# (b) Test, at alpha= 0.05, the overall hypothesis that the average pollution index is the same for all five beaches.

wq <- read.table("WaterQualityIndex.txt")
attach(wq)

sm=by(Index, Beach, mean); svar=by(Index, Beach, var)
theta=(sm[1]+sm[2])/2-(sm[3]+sm[4]+sm[5])/3
stheta=sqrt(mean(svar)*((1/4)*(2/12)+(1/9)*(3/12)))

# Compute the five sample means, the sample variances, the contrast, and the standard deviation of the contrast, which are stored in the R objects t and st, respectively

TS=theta/stheta; TS # Test Statistic

2*(1-pt(abs(TS),55)) # P-value

# (b)
anova(aov(Index~as.factor(Beach)))


# R Command for Testing the Assumption sigma2_1 = ???= sigma2_k
# anova(aov(resid(aov(df$Value~df$Sample))**2~df$Sample))

# R Command for Testing the Normality Assumption
# shapiro.test(resid(aov(df$Value~df$Sample)))

# fit=aov(df$Value???df$Sample)
# plot(fit, which=1)
# plot(fit, which=2)

# Example: In the context of the water quality measurements, test the validity of the assumptions of equal 
# variances and normality

Sample = as.factor(wq$Beach)
fit=aov(wq$Index ~ Sample)
anova(aov(resid(fit)**2~Sample)) # assumption of equal variances is approximately satisfied
plot(fit, which=1)

shapiro.test(resid(aov(wq$Index~ Sample))) # Normality test
Resid=resid(aov(wq$Index ~ Sample)); boxplot(Resid)
qqnorm(Resid); qqline(Resid, col=2)

# The Kruskal-Wallis Test

# R Command for the Kruskal-Wallis Test
# kruskal.test(x~s)

# Example: 
# Organizations such as the American Society for Testing Materials are interested
# in the flammability properties of clothing textiles. A particular study performed a
# standard flammability test on six pieces from each of three types of fabric used in
# children's clothing. The response variable is the length of the burn mark made when
# the fabric piece is exposed to a flame in a specified way. Using the Kruskal-Wallis
# procedure on the data from this study (Flammability.txt), test the hypothesis that the
# flammability of the three materials is the same, at level of significance alpha = 0.1, and
# compute the p-value

fl <- read.table("Flammability.txt")
x=fl$BurnL; s=fl$Material; kruskal.test(x ~s)

# Because the sample sizes are ni = 6, that is, < 8, a more accurate p-value (computed through Monte-Carlo resampling)
library(coin); kw=kruskal_test(x ~ factor(s), distribution=approximate(B=9999)); pvalue(kw)


# The Chi-Square Test for Proportions

# R Commands for the Chi-Square Test for H0 : p1 = ???= pk
# table=matrix(c(O11, O21, ..., O1k, O2k), nrow=2)
# chisq.test(table)

# Example: 
# A commercial airline is considering four different designs of the control panel for
# the new generation of airplanes. To see if the designs have an effect on the pilot's
# response time to emergency displays, emergency conditions were simulated and the
# response times of pilots were recorded. The sample sizes, ni, and number of times,
# O1i, that the response times were below 3 seconds for the four designs are as follows:
# n1 = 45, O11 = 29; n2 = 50, O12 = 42; n3 = 55, O13 = 28; n4 = 50, O14 = 24.
# Perform the test at level of significance alpha = 0.05.

table=matrix(c(29, 16, 42, 8, 28, 27, 24, 26), nrow=2); chisq.test(table)

# Bonferroni Multiple Comparisons

# Example: 
# Iron concentration measurements from four ore formations are given in FeData.txt.
# Use Bonferroni multiple comparisons, based on rank-sum tests, to determine which
# pairs of ore formations differ, at experiment-wise level of significance alpha = 0.05, in
# terms of iron concentration.

fe <- read.table("FeData.txt")
f1=fe$conc[fe$ind=="V1"]; f2=fe$conc[fe$ind=="V2"]; f3=fe$conc[fe$ind=="V3"]; f4=fe$conc[fe$ind=="V4"]

wilcox.test(f1, f2); 
wilcox.test(f1, f3); 
wilcox.test(f1, f4); 
wilcox.test(f2, f3); 
wilcox.test(f2, f4); 
wilcox.test(f3, f4);

# Those who are less than alpha/6 (0.05/6) = 0.0083 will mean that they are different between each other.

by(fe$conc,fe$ind, mean)


## Tukey Multiple Comparisons and simultaneous CIs

# R Commands for Tukey's (1 ??? ??)100% Simultaneous CIs
# TukeyHSD(aov(y???s), conf.level=1-??)
# plot(TukeyHSD(aov(y???s), conf.level=1-??))

# Example: 
# Four different concentrations of ethanol are compared at level ?? = 0.05 for their
# effect on sleep time. Each concentration was given to a sample of 5 rats and the
# REM sleep time for each rat was recorded. Use the resulting data set (SleepRem.txt)
# to construct Tukey's 95% simultaneous CIs and apply the Tukey multiple comparisons method 
# to identify the pairs of concentrations that are significantly different at
# experiment-wise level of significance 0.05.

sl <- read.table("SleepRem.txt")
by(sl$values, sl$ind, mean)

TukeyHSD(aov(sl$values~sl$ind))

plot(TukeyHSD(aov(sl$values~sl$ind))) 

## Tukey Multiple Comparisons on the ranks

# Example: 
# Consider the setting where three types of fabric are tested for
# their flammability, and use Tukey's multiple comparisons method on the ranks to
# identify which materials differ in terms of flammability at experiment-wise level of
# significance alpha = 0.1.

r=rank(fl$BurnL); s=as.factor(fl$Material)
plot(TukeyHSD(aov(r~s), conf.level=0.9))


# Randomized Block designs

# R Command for the Randomized Block Design
# summary(aov(values~treatment+block))

# Example:
# A random sample of 36 Napa Valley visitors tested and rated four wine varieties on
# a scale of 1-10. For impartiality purposes, the wines were identified only by numbers
# 1-4. The order in which each of the four wines were presented to each visitor was
# randomized. The average rating for each wine, and overall average rating, are X1? =
#   8.97, X2? = 9.04, X3? = 8.36, X4? = 8.31, and X?? = 8.67. Moreover, it is given that
# the sum of squares due to the visitors (blocks) is SSB = 11.38 and the total sum of
# squares is SST = 65.497. With the information given, construct the ANOVA table.
# Is there a significant difference in the rating of the four wines? Test at ?? = 0.05.

wt=read.table("NapaValleyWT.txt", header=T)
st=stack(wt); wine=st$ind; visitor=as.factor(rep(1:36,4))
summary(aov(st$values~wine+visitor))

# Testing the validity of the assumptions

# R Command for Testing the Homoscedasticity Assumption
# anova(aov(resid(aov(values~trt+blk))**2???trt+blk))

# R Command for Testing the Normality Assumption
# shapiro.test(resid(aov(values~trt+blk)))

# Example using the previous situation:
anova(aov(resid(aov(st$values~wine+visitor))**2~wine+visitor))

shapiro.test(resid(aov(st$values~wine+visitor)))


# Friedman's Test and F Test on the Ranks

# For the wine tasting data set, the summary statistics on the ranks
# are R1? = 90.93, R2? = 94.89, R3? = 52.97, R4? = 51.21, and R?? = 72.50. Moreover,
# it is given that the rank sum of squares due to the visitors (blocks) is SSBR = 41,843
# and the rank total sum of squares is SSTR = 248203. With the information given,
# calculate the F statistic on the ranks and test the hypothesis of no difference in the
# ratings of the four wines at ?? = 0.05.

ranks=rank(st$values); wine=st$ind;
visitor=as.factor(rep(1:36,4))
summary(aov(ranks~wine+visitor))

# With the information given, calculate Friedman's test statistic and p-value, and test the hypothesis
# of no difference in the ratings of the four wines at ?? = 0.05.

# R Command for Friedman's Test
# friedman.test(st$values, st$ind, as.factor(rep(1:n, k)))

friedman.test(st$values, st$ind, as.factor(rep(1:36, 4))) 


# Multiple Comparisons

## Bonferroni Multiple Comparisons and Simultaneous CIs

# Example: 

# In the context of the wine tasting data, apply the following
# Bonferroni multiple comparisons procedures to identify which of the 6 pairs of
# wines are significantly different at experiment-wise level of significance ?? = 0.05.
# (a) Construct 95% Bonferroni simultaneous CIs, and perform multiple comparisons based on them.
# (b) Perform the Bonferroni multiple comparisons procedure through pairwise testing using the Wilcoxon's signed-rank procedure.
# (c) Perform the Bonferroni multiple comparisons procedure through pairwise testing using the paired T test procedure.

# (a)
(1-0.05/6)*100 # Confidence level
t.test(wt$W1,wt$W2, paired=T, conf.level=0.99167)
t.test(wt$W1,wt$W3, paired=T, conf.level=0.99167)
t.test(wt$W1,wt$W4, paired=T, conf.level=0.99167)
t.test(wt$W2,wt$W3, paired=T, conf.level=0.99167)
t.test(wt$W2,wt$W4, paired=T, conf.level=0.99167)
t.test(wt$W3,wt$W4, paired=T, conf.level=0.99167)

# The results, given in the last column of the table in (10.4.19), mean that
# each of the wines 1 and 2 is significantly different from wines 3 and 4, but
# wine 1 is not significantly different from wine 2, and wine 3 is not significantly
# different from wine 4.

# (b)

wilcox.test(wt$W1,wt$W2, paired = TRUE); 
wilcox.test(wt$W1,wt$W3, paired = TRUE); 
wilcox.test(wt$W1,wt$W4, paired = TRUE); 
wilcox.test(wt$W2,wt$W3, paired = TRUE); 
wilcox.test(wt$W2,wt$W4, paired = TRUE); 
wilcox.test(wt$W3,wt$W4, paired = TRUE);

# Because the desired experiment-wise level of significance is 0.05, each p-value is compared to 0.05/6 = 0.00833.
# Comparisons with p-values less than 0.00833 are declared significantly different.

# Each of the wines 1 and 2 is significantly different from wines 3 and 4, but wine 1 is not significantly
# different from wine 2, and wine 3 is not significantly different from wine 4.

# (c)

# Same results as (a) abd (b)


# Tukey's Multiple Comparisons and Simultaneous CIs

# Example:
# In the context of the wine tasting data of Example 10.4-3, apply Tukey's method
# to construct 95% simultaneous CIs and multiple comparisons to identify which of
# 6 pairs of wines are significantly different at experiment-wise level of significance alpha = 0.05.

# R Commands for Tukey's (1 -0.5)100% Simultaneous CIs
# TukeyHSD(aov(values~treatment+block), "treatment", conf.level=1-??)
# plot(TukeyHSD(aov(values~treatment+block), "treatment", conf.level=1-??))

st=stack(wt); wine=st$ind; visitor=as.factor(rep(1:36, 4))
TukeyHSD(aov(st$values~wine+visitor),"wine")
plot(TukeyHSD(aov(st$values~wine+visitor),"wine"))


# Tukey's Multiple Comparisons on the Ranks

# Example:
# In the context of the wine tasting data of Example 10.4-3, perform multiple comparisons 
# at experiment-wise level of significance alpha = 0.05 using Tukey's method on the ranks.

st=stack(wt); wine=st$ind; visitor=as.factor(rep(1:36, 4))
TukeyHSD(aov(rank(st$values)~wine+visitor),"wine")

