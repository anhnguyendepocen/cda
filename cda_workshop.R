# Clay Ford (jcf2d)
# UVa StatLab
# Introductory Categorical Data Analysis with R
# Fall 2016

# Ctrl + Enter to submit commands

# packages used in this workshop
pkgs <- c("epiR","epitools","car","effects")
install.packages(pkgs)

library(epitools)
library(DescTools)
library(car)
library(effects)


# Two-Way Contingency Tables ----------------------------------------------

# enter data as matrix and assign dimension names
aspirin <- matrix(data = c(189, 104, 10845, 10933), 
                  ncol=2, 
                  dimnames = list("group" = c("Placebo","Aspirin"),
                                  "MI" = c("Yes","No")))
aspirin
# it's a matrix
class(aspirin)

# turn a matrix into a "table"
aspirinT <- as.table(aspirin)
aspirinT

# looks the same, but different class
class(aspirinT)

# The table class allows us to convert our table into a data frame with a column
# for Frequencies:
as.data.frame(aspirinT)

# Import aspiring data:
aspirin.df <- read.csv("aspirin.csv")

# create tables
table(aspirin.df$group, aspirin.df$MI)

# notice this adds dimension names to output
with(aspirin.df, table(group, MI))

# using xtabs
xtabs(~ group + MI, data = aspirin.df)

# What if we want to switch row and/or column ordering? Set the factor levels
aspirin.df$group <- factor(aspirin.df$group, levels = c("Placebo","Aspirin"))
aspirin.df$MI <- factor(aspirin.df$MI, levels = c("Yes","No"))

table(aspirin.df$group, aspirin.df$MI)
with(aspirin.df, table(group, MI))
xtabs(~ group + MI, data = aspirin.df)

# working with a 2 x 2 table

# calculate cell proportions
prop.table(aspirin) # all cells sum to 1

# Proportion of MI by group
prop.table(aspirin, margin = 1)

# Proportion of group by MI 
prop.table(aspirin, margin = 2)


# calculate row and column marginal totals
margin.table(aspirin) # sum all cells

# total in each group
margin.table(aspirin, margin = 1)

# total of MI results
margin.table(aspirin, margin = 2)


# add margins to a table
addmargins(aspirin)


# chi-square test

# can use a matrix or table:
chisq.test(aspirin) # matrix
chisq.test(aspirinT) # or table

# ...or two vectors from a data frame
chisq.test(aspirin.df$group, aspirin.df$MI)


# prop.test

# can use a table or matrix; just remember that the function assumes "successes"
# (the event of interest) are in the first column and "failures" in the second
aspirin
prop.test(aspirin)

# prop.test can also work directly with successes and number of trials
x <- aspirin[,1]
n <- margin.table(aspirin, margin = 1)
prop.test(x, n)
rm(x,n)


# risk ratio and odds ratio

# The epitools package provides functions for calculating risk ratios and odds
# ratios. The functions are...

# riskratio()
# oddsratio()

# Let's try riskratio() using the aspirin matrix
riskratio(aspirin)

# Notice it produced a different risk ratio estimate than what we saw
# in the presentation (1.817).

# The reason for this is that epitools expects the table rows and columns to be
# in a particular order, as follows (per its documentation):

#                  disease=0   disease=1
# exposed=0 (ref)    n00         n01
# exposed=1          n10         n11	

# The "exposed" rows are the groups. In our case: placebo versus aspirin. 
# The reference level (ref) is what we want to compare against.
# The "disease" columns are the response: MI (yes or no)

# What we got was the risk ratio of not experiencing MI for Aspirin versus
# Placebo.

# What we would like to estimate is the risk ratio of experiencing MI for 
# Placebo versus Aspirin. So we need to switch the order of both rows and
# columns.

# Fortunately the epitools function have a rev argument that allows you to do
# this:

# rev = "rows"
# rev = "colums"
# rev = "both"

# To replicate the results in the presentation:
riskratio(aspirin, rev = "both")

# Notice we get 95% confidence intervals and tests of independence. 

# odds ratio works the same way; however for 2 x 2 tables the ordering doesn't
# matter!

# Odds of MI for Placebo group about 83% higher than the Aspirin group
oddsratio(aspirin, rev = "both")

# Odds of no MI for Aspirin group about 83% higher than the Placebo group
oddsratio(aspirin)


# We can also use vectors instead of tables with oddsratio and riskratio; in 
# this case the functions use the baseline factor levels as the reference level
# and No levels respectively.

levels(aspirin.df$group) # baseline = "Placebo"
levels(aspirin.df$MI) # baseline = "Yes"

riskratio(x = aspirin.df$group, y = aspirin.df$MI)
riskratio(x = aspirin.df$group, y = aspirin.df$MI, rev = "both")

# Again, doesn't matter for oddsratio since both have only two levels
oddsratio(x = aspirin.df$group, y = aspirin.df$MI)
oddsratio(x = aspirin.df$group, y = aspirin.df$MI, rev = "both")

# YOUR TURN!

# Below is data on Florida auto accidents from 1988. Submit the R code and look
# at the table
auto <- matrix(data = c(1601, 510, 162527, 412368),
               ncol = 2,
               dimnames = list("seat belt" = c("No","Yes"),
                               "Injury" = c("Fatal","Nonfatal")))
auto

# what is the proportion of Fatal vs. Nonfatal injuries by seat belt use?
prop.table(auto, margin = 1)

# Estimate the risk ratio of fatality for seat belt use
riskratio(auto, rev = "both")

# Estimate the odds ratio of fatality for seat belt use
oddsratio(auto, rev = "both")

# Back to the presentation!

# Three-Way Contingency Tables --------------------------------------------

# Enter data as array and assign dimension names
lung.cancer <- array(data = c(126,35,100,61,
                              908,497,688,807,
                              913,336,747,598,
                              235,58,172,121,
                              402,121,308,215,
                              182,72,156,98,
                              60,11,99,43,
                              104,21,89,36),
                     dim = c(2,2,8),
                     dimnames = list("smoking" = c("smokers","nonsmokers"),
                                     "lung.cancer" = c("yes","no"),
                                     "city" = c("Beijing","Shanghai","Shenyang",
                                                "Nanjing","Harbin","Zhengzhou",
                                                "Taiyuan","Nanchang")))

lung.cancer

# import data
lc.df <- read.csv("lung_cancer.csv")
head(lc.df)
str(lc.df)

# make a three-way contingency table
# notice groups are placed in alphabetical order
table(lc.df$smoking, lc.df$lung.cancer, lc.df$city)
with(lc.df, table(smoking, lung.cancer, city))

xtabs(~ smoking + lung.cancer + city, data = lc.df)

# save the table and create a data frame with Freq column
lung.cancer2 <- xtabs(~ smoking + lung.cancer + city, data = lc.df)
lc.df2 <- as.data.frame(lung.cancer2)
lc.df2


# working with a three-way table

# total number of subjects
margin.table(lung.cancer2)

# row margin in each city
margin.table(lung.cancer2, margin = c(1,3))

# column margins in each city 
margin.table(lung.cancer2, margin = c(2,3))

# total subject in each city
margin.table(lung.cancer2, margin = 3)

# total smokers in each city
margin.table(lung.cancer2, margin = c(1,3))[2,]

# Collapse into a 2 x 2 table that ignores city
margin.table(lung.cancer2, margin = c(1,2))


# all cells sum to 1
prop.table(lung.cancer2) 

# row-wise proportions for each city (ie, proportion of lung.cancer by smoking)
prop.table(lung.cancer2, margin = c(1,3))

# column-wise proportions for each city
prop.table(lung.cancer2, margin = c(2,3))

# proportion of smokers in each city
prop.table(margin.table(lung.cancer2, margin = c(1,3)), margin = 2)[2,]

# add table margins
addmargins(lung.cancer2)

# Flatten table into two dimensions
ftable(lung.cancer2)
ftable(lung.cancer2, row.vars = c("city","smoking"))


# Examples of extracting and subsetting three-way contingency tables

# first city
lung.cancer2[,,1]
lung.cancer2[,,"Beijing"]

# Can't remember the city names?
dimnames(lung.cancer2)

# to show city name, set drop = FALSE
lung.cancer2[,,1, drop = FALSE]

# Show just the smokers
lung.cancer2[2,,]
lung.cancer2[2,,,drop = FALSE]

# Show just the smokers in Beijing
lung.cancer2[2,,1]
lung.cancer2[2,,1, drop = FALSE]

# Smokers with lung cancer
lung.cancer2[2,2,]
lung.cancer2[2,2,,drop=FALSE]
lung.cancer2["smokers","yes",]




# Cochran-Mantel-Haenszel (CMH) Test

# test the null hypothesis that smoking and lung cancer are conditionally
# independent given city (ie, odds ratio = 1 for each partial table)
mantelhaen.test(lung.cancer2)

# or you can do this:
mantelhaen.test(x = lc.df$smoking, 
                y = lc.df$lung.cancer, 
                z = lc.df$city)

# Notice the common odds ratio estimate. It's basically a weighted average of
# the partial table odds ratios.

# We can get the odds ratio for the first partial table as follows:
oddsratio(lung.cancer2[,,1])
oddsratio(lung.cancer2[,,"Beijing"])

# View just odds ratio without other output:
oddsratio(lung.cancer2[,,1])$measure
oddsratio(lung.cancer2[,,1])$measure[2,] # just the bottom row

# we can get the odds ratios for all partial tables using a "for loop".
# Have to use print() in a for loop to make something print.
for(i in 1:8){
  print(oddsratio(lung.cancer2[,,i])$measure[2,])
}

# Can also use apply functions. We "apply" a function to the indices of the third
# dimension.

# lapply returns a list:
lapply(1:8, function(x)oddsratio(lung.cancer2[,,x])$measure[2,])
# sapply simplifies output to a matrix
sapply(1:8, function(x)oddsratio(lung.cancer2[,,x])$measure[2,])


# Breslow-Day Test

# test the null hypothesis that the odds ratio between smoking and lung cancer
# is the same at each level of city (test of homogeneity of the odds)

# The BreslowDayTest function is in the DescTools package (Tools for Descriptive
# Statistics)
BreslowDayTest(lung.cancer2)

# A high p-value suggests the CMH common odds ratio is valid.


# YOUR TURN!

# R comes within built-in data sets for educational and testing purposes. One 
# such data set is UCBAdmissions. It contains aggregate data on applicants to 
# graduate school at Berkeley for the six largest departments in 1973 classified
# by admission and sex. At issue is whether the data show evidence of sex bias
# in admission practices.
UCBAdmissions

# If we ignore department we see evidence of sex bias (more females being
# rejected):
margin.table(UCBAdmissions, margin = c(1,2)) %>% 
  prop.table(margin = 2)

# this is the same as:
prop.table(
  margin.table(UCBAdmissions, margin = c(1,2)), 
  margin = 2)

# Run the Cochran-Mantel-Haenszel (CMH) Test on the full data to test the null 
# hypothesis that admission and gender are conditionally independent given 
# department.
mantelhaen.test(UCBAdmissions)
sapply(1:6, function(x)oddsratio(UCBAdmissions[,,x])$measure[2,])
prop.table(UCBAdmissions[,,"A"], margin = 2)

# You should notice the evidence for sex bias disappears. This is a famous 
# example of Simpson's Paradox, where an association disappears or reverses
# direction when a third variable is introduced into the analysis.


# Logistic Regression -----------------------------------------------------


# Read in coronary heart disease data
chd <- read.csv("chd.csv")
head(chd)

# perform logistic regression
mod1 <- glm(chd ~ age, data = chd, family = binomial)
summary(mod1)
coef(mod1)
coef(mod1)[2]
exp(coef(mod1)[2]) # odds ratio

# make a prediction for a person age 60
# type="response" returns proportion (or probability)
predict(mod1, type="response", 
        newdata = data.frame(age = 60))

# predictions for data used to fit model
predict(mod1, type="response")


# using the effects package
plot(allEffects(mod1))
plot(allEffects(mod1), type = "response")

# YOUR TURN!

# The 1986 crash of the space shuttle Challenger was linked to failure of O-ring
# seals in the rocket engines. Data was collected on the 23 previous shuttle 
# missions. Each record contains temperature at launch and whether there was 
# damage to the 0-rings (0 = no, 1 = yes). The launch temperature on the day of
# the crash was 31 degrees F.

orings <- read.csv("orings.csv")
# convert damage (0,1) to a factor
orings$damage <- factor(orings$damage)
str(orings)
summary(orings)

# 1, Model damage as a function of temp. 
# 2. Interpret the coefficient on temp.
# 3. Predict the probability of damage at 55 degrees F.
m1 <- glm(damage ~ temp, data = orings, family = binomial)
summary(m1)

exp(coef(m1)[2])

predict(m1, newdata = data.frame(temp = 55), type = "response")

plot(allEffects(m1), type = "response")


# Back to presentation.


# Connecting logistic regression with contingency table analysis ----------


# revist the MI data
oddsratio(x = aspirin.df$group, y = aspirin.df$MI)$measure

# same analysis with logistic regression
mod.asp <- glm(MI ~ group, data = aspirin.df, family = binomial)
summary(mod.asp)
coef(mod.asp) # just the coefficients
coef(mod.asp)[2] 
exp(coef(mod.asp)[2]) # odds ratio
exp(confint(mod.asp, parm = "groupAspirin")) # CI for odds ratio


# revist the lung cancer data
mantelhaen.test(lung.cancer2)

# same analysis with logistic regression
mod.lc <- glm(lung.cancer ~ smoking + city, data = lc.df, family = binomial)
summary(mod.lc)
coef(mod.lc)
coef(mod.lc)[2]
exp(coef(mod.lc)[2]) # almost same as common odds ratio estimated by CMH Test
confint(mod.lc, parm = "smokingsmokers")
exp(confint(mod.lc, parm = "smokingsmokers")) # almost identical to CI given by CMH Test

# The Breslow-Day test is analogous to an interaction in a model.
BreslowDayTest(lung.cancer2)

# same analysis with logistic regression
mod.lc2 <- glm(lung.cancer ~ smoking + city + smoking:city, 
               data = lc.df, family = binomial)
summary(mod.lc2)
anova(mod.lc2)
# use Anova in the car package to get p-value
Anova(mod.lc2) # last line almost identical to Breslow-Day Test




# Appendix ----------------------------------------------------------------

# plots for Logistic regression slides

# plot data
plot(chd ~ jitter(age), data=chd, ylim=c(0,1), axes = "F", xlab = "Age")
axis(side = 1, at = seq(20,70,10)) 
axis(side = 2, at = c(0,1))
box()

# create age groups and calculate proportion of CHD per age group
chd$agrp <- cut(x = chd$age, breaks = c(0, 29, 34, 39, 44, 49, 54, 59, 100), 
                labels = c("<29", "30-34","35-39","40-44","45-49","50-54","55-59",">60"))

library(dplyr)
chd2 <- chd %>% 
  group_by(agrp) %>% 
  summarise(n = n(), 
            absent = sum(chd==0), 
            present = sum(chd==1),
            proportion = round(present/n,2)) %>% 
  as.data.frame()
chd2

# Scatterplot of proportion with CHD by age group
plot(seq(20,70, by=10),seq(0,1,by=0.2), type="n", 
     xlab = "Age", 
     ylab = "Proportion with CHD")
with(chd2, points(x = c(25, 32, 37, 42, 47, 52, 57, 65),
                  y = chd2$proportion))


# add fitted line to scatterplot of proportion with CHD by age group
plot(seq(20,70, by=10),seq(0,1,by=0.2), type="n", xlab = "Age", ylab = "Proportion with CHD")
with(chd2, points(x = c(25, 32, 37, 42, 47, 52, 57, 65),
                  y = chd2$proportion))
lines(x = chd$age, y = predict(mod1, type="response"), col="blue")
