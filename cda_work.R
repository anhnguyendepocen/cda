# cda work

# enter data as matrix and assign dimension names
aspirin <- matrix(data = c(189, 104, 10845, 10933), 
                  ncol=2, 
                  dimnames = list("group" = c("Placebo","Aspirin"),
                                  "MI" = c("Yes","No")))

aspirinT <- as.table(aspirin)
class(aspirinT)
class(aspirin)

as.data.frame(aspirin)
as.data.frame(aspirinT) # returns column of Frequencies

# create csv file of aspirin data
MI <- c(rep("Yes", 189 + 104), rep("No", 10845 + 10933))
group <- rep(c("Placebo","Aspirin","Placebo","Aspirin"), times=c(189, 104,10845,10933))
aspirin.df <- data.frame(group, MI)
aspirin.df <- aspirin.df[sample(nrow(aspirin.df)),]
rownames(aspirin.df) <- NULL
write.csv(aspirin.df, file = "aspirin.csv", row.names = FALSE)

aspirin.df <- read.csv(file = "aspirin.csv")

xtabs(~ group + MI, data = aspirin.df)

# what if we want to switch row and/or column ordering? Set the factor levels
aspirin.df$group <- factor(aspirin.df$group, levels = c("Placebo","Aspirin"))
aspirin.df$MI <- factor(aspirin.df$MI, levels = c("Yes","No"))

xtabs(~ group + MI, data = aspirin.df)


# calculate cell proportions
prop.table(aspirin)
prop.table(aspirin, margin = 1)
prop.table(aspirin, margin = 2)

# calculate row and column marginal totals
margin.table(aspirin)
margin.table(aspirin, margin = 1)
margin.table(aspirin, margin = 2)

# add margins to a table
addmargins(aspirin)

# compare proportions
prop.test(aspirin)
chisq.test(table(aspirin2$group, aspirin2$MI))

asp.tab <- table(aspirin2$group, aspirin2$MI)
chisq.test(asp.tab)

# assumes counts of successes and failures are in the columns;
# gives CI of difference in proportions if only two groups

# can also use counts of success and trials
prop.test(x = c(189, 104), n = c(11034, 11037))

# The epitools package provides functions for odds ratios and risk ratios
library(epitools)

riskratio(x = aspirin)
riskratio(x = aspirin, rev = "columns")
riskratio(x = aspirin, rev = "both")

oddsratio(x = aspirin, rev = "both")


# make an aspirin data frame
# id <- seq(sum(margin.table(aspirin,margin = 1)))
grp <- rep(c("placebo","aspirin"), margin.table(aspirin,margin = 1))
mi <- c(rep(c("yes","no"), aspirin[1,]),rep(c("yes","no"), aspirin[2,]))
aspirin.df <- data.frame(grp, mi = factor(mi, levels = c("yes","no")))
head(aspirin.df)

xtabs(~ grp + mi, data=aspirin.df)
asp.table <- xtabs(~ grp + mi, data=aspirin.df)
summary(asp.table)

as.data.frame(aspirin)
as.data.frame(as.table(aspirin))

aspirin.tab <- as.table(aspirin)

library(vcd)
oddsratio(aspirin)
epitools::oddsratio(aspirin, rev = "both")

# sieve plot?

# gender gap example
genderGap <- matrix(c(279, 165, 73, 47, 225, 191), ncol=3)
rownames(genderGap) <- c("Females","Males")
colnames(genderGap) <- c("Democrat","Independent","Republican")

prop.table(genderGap, margin = 1)
chisq.test(genderGap)
c.out <- chisq.test(genderGap)
str(c.out)
c.out$expected
c.out$residuals
c.out$stdres # adjusted residuals, Table 2.6

# visualize the table and the residuals
mosaicplot(genderGap)
mosaicplot(genderGap, shade = TRUE)

genderGap
genderGap2 <- matrix(c(apply(genderGap[,-3],1,sum), genderGap[,3]),ncol=2)
rownames(genderGap2) <- c("Females","Males")
colnames(genderGap2) <- c("Democrat/Independent","Republican")
genderGap2
chisq.test(genderGap2)

# Notes on chi-square tests
# - simply indicate degree of evidence for association
# - study residuals to investigate nature of association
# - estimate parameters such as odds ratios to estimate strength of association
# - like fire, the chi-square test is an excellent servant and a bad master
#   - Sir Austin Bradford Hill

infantMal <- matrix(c(17066,14464,788,126,37,48,38,5,1,1),ncol=2)
rownames(infantMal) <- c("0","<1","1-2","3-5",">=6")
colnames(infantMal) <- c("Absent","Present")

prop.trend.test(x = infantMal[,2], n = margin.table(infantMal,1), 
                score = c(0, 0.5, 1.5 ,4, 7))

IM2 <- as.data.frame(as.table(infantMal))
names(IM2) <- c("Alcohol.Consumption","Malformation","Freq")



library(DescTools)
CochranArmitageTest(x = t(infantMal))

# Three way contingency tables

# Death penalty example
# Table 3.1
# 2 x 2 x 2 contingency table
dp <- array(data = c(53,11,414,37,0,4,16,139),
            dim = c(2,2,2),
            dimnames = list("defendant.race" = c("white","black"),
                            "death.penalty" = c("yes","no"),
                            "victim.race" = c("white","black")))
dp
# extract each partial table
dp[,,1]
dp[,,2]

# Flatten table
ftable(dp)
ftable(dp, row.vars = c("victim.race","defendant.race"))

# convert to data frame with Freq column
# and save to csv file
dpFreq <- as.data.frame(as.table(dp))
write.csv(dpFreq, file = "death_penalty.csv", row.names = FALSE)

# turn into data frame with one row per observation
# and save to csv file
rep2 <- function(x){
  dpFreq[rep(x,dpFreq$Freq[x]),-4]
}
out <- lapply(1:8, rep2)
dp.df <- do.call(rbind,out)
dp.df <- dp.df[sample(nrow(dp.df)),]
write.csv(dp.df, file = "death_penalty2.csv", row.names = FALSE)


# import data frames
dp2 <- read.csv("death_penalty.csv")
dp2
dp3 <- read.csv("death_penalty2.csv")
head(dp3)

# create tables from data frames
# ~ row + column + layer
xtabs(Freq ~ defendant.race + death.penalty + victim.race, data = dp2)
xtabs(~ defendant.race + death.penalty + victim.race, data = dp3)

# To change order of rows and columns need to change order of levels
dp2$defendant.race <- factor(dp2$defendant.race, levels = c("white","black"))
dp2$victim.race <- factor(dp2$victim.race, levels = c("white","black"))
dp2$death.penalty <- factor(dp2$death.penalty, levels = c("yes","no"))

# now it matches what we created by hand
xtabs(Freq ~ defendant.race + death.penalty + victim.race, data = dp2)


# total number of subjects
sum(dp)

# study effect of defendant's race on death penalty verdict, treating victim's
# race as control variable

# each layer in the array is a partial table that display conditional
# associations.

prop.table(dp, margin = c(1,3))

# when victims were white, death penalty was imposed 22.9% - 11.3% = 11.6% more 
# often for black defendents.

# marginal table that does not control for victim's race; margin = c(1,2) means
# add the cells in the row and columns over each layer
dpMarginal <- margin.table(dp, margin = c(1,2))

prop.table(dpMarginal, margin = 1)

# In the marginal table, death penalty was imposed 11.0% - 7.9% = 3.1% more 
# often for white defendents.

# association reverses direction; this is known as Simpson's Paradox

# mosaicplot(dp2)
# mosaicplot(~ victim.race + death.penalty + defendant.race, data = dp)

library(epitools)
# odds ratio in first partial table
oddsratio(dp[,,1], rev = "both")

# sample odds for white defendants receiving the death penalty were 43% of the
# sample odds for black defendant

# odds ratio in second partial table
oddsratio(dp[,,2]) # error due to 0 count!
oddsratio(dp[,,2], method = "wald") # use method = "wald" to force calculation

# odds ratio for the marginal table
oddsratio(dpMarginal)
# sample odds of death penalty were 45% higher for white defendants than for
# black defendants


# homogeneous association

# when conditional odds ratio between X and Y is identical at each level of Z

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
# Table 3.3
ftable(lung.cancer, row.vars = c("city","smoking"))

# create csv file of lung cancer data
# lung.cancer.tab <- as.table(lung.cancer)
# lung.cancer.df <- as.data.frame(lung.cancer.tab)
# lung.cancer.df <- data.frame(sapply(lung.cancer.df[,-4], rep, times = lung.cancer.df$Freq))
# write.csv(lung.cancer.df, file = "lung_cancer.csv", row.names = FALSE)

lung.cancer.df <- read.csv("lung_cancer.csv")
sapply(lung.cancer.df, class)

xtabs(~ smoking + lung.cancer + city, data = lung.cancer.df)
# notice that default ordering is alphabetical

oddsratio(lung.cancer[,,1])$measure
oddsratio(lung.cancer[,,1], rev = "both")$measure

# conditional odds ratios
lapply(1:8, function(x)oddsratio(lung.cancer[,,x], rev = "both")$measure)

# Cochran-Mantel-Haenszel Test 

# Test that X and Y are conditionally independent given Z (ie, conditional odds
# ratio is 1 in each partial table)

# Common Odds Ratio

# Estimate strength of association when association seems stable across partial tables.

# The mantelhaen.test function does both
mantelhaen.test(lung.cancer)

# test is inappropriate when the association varies dramatically among the 
# partial tables. Sample odds ratios should fall in the same direction (all
# greater than 1, or all less than 1)

# Test that odds ratios are the same at each level of Z. Test of homogeneous
# association for 2 x 2 x K tables

# Breslow-Day Test

epiR::epi.2by2(lung.cancer)


# recoding continuous to categorical
# not often recommended; throws away information

# cut function; creates a factor
age <- round(rnorm(100,mean = 35,sd = 15))
cut(age, breaks = c(-Inf, 10, 20, 30, 40, 50, 60, 70, Inf))
# with custom labels
cut(age, breaks = c(-Inf, 10, 20, 30, 40, 50, 60, 70, Inf),
    labels = c("<10","11-20","21-30","31-40","41-50","51-60","61-70","71-80"))

# another way using division and ceiling function
ceiling(age/10)

agegpname <- c("<10","11-20","21-30","31-40","41-50","51-60","61-70","71-80")
agegpname[ceiling(age/10)] # not a factor



# glms --------------------------------------------------------------------

m1 <- glm(MI ~ group, data=aspirin.df, family = binomial)
summary(m1)
coef(m1)
coef(m1)[2]
exp(coef(m1)[2]) # odds ratio

# Those on aspirin 83% higher odds of NOT having a heart attack.

predict(m1, newdata = data.frame(group = c("Placebo","Aspirin")))
predict(m1, newdata = data.frame(group = c("Placebo","Aspirin")), type = "response")

# predicted probabilities of not having heart attack

# Let's make "No" the baseline so we get predicted probabilities for "Yes"
aspirin.df$MI <- relevel(aspirin.df$MI, ref = "No")
aspirin.df$group <- relevel(aspirin.df$group, ref = "Aspirin")
m1 <- glm(MI ~ group, data=aspirin.df, family = binomial)
summary(m1)
coef(m1)
coef(m1)[2]
exp(coef(m1)[2]) # odds ratio
# predicted probabilities
predict(m1, newdata = data.frame(group = c("Placebo","Aspirin")), type = "response")

# visualize model
library(effects)
plot(allEffects(m1))


m2 <- glm(lung.cancer ~ smoking + city, data = lung.cancer.df, family = binomial)
summary(m2)
coef(m2)
exp(coef(m2)[2]) # almost same as common odds ratio estimated by CMH Test
confint(m2, parm = "smokingsmokers")
exp(confint(m2, parm = "smokingsmokers")) # almost identical to CI given by CMH Test

# Alternative to Breslow-Day
# Test if interaction is significant
m3 <- glm(lung.cancer ~ smoking * city, data = lung.cancer.df, family = binomial)
summary(m3)
anova(m3)
car::Anova(m3) # last line almost identical to Breslow-Day Test

# visualize model
plot(allEffects(m3))


# Using data in Ch 1 of Applied Logistic Regression

# chd <- read.table("C:/users/jcf2d/Box Sync/_statistics/ALR/chdage.dat")
# names(chd) <- c("id","age","chd")
# str(chd)
# # write to csv
# write.csv(chd, file = "chd.csv", row.names = FALSE)

chd <- read.csv("chd.csv")

# fig 1.1
plot(chd ~ jitter(age), data=chd, ylim=c(0,1))

# add age group variable
chd$agrp <- cut(x = chd$age, breaks = c(0, 29, 34, 39, 44, 49, 54, 59, 100), 
                labels = c("<29", "30-34","35-39","40-44","45-49","50-54","55-59",">60"))
table(chd$agrp)
aggregate(chd ~ age, data = chd, mean)


# Table 1.2
library(dplyr)
chd2 <- chd %>% 
  group_by(agrp) %>% 
  summarise(n = n(), 
            absent = sum(chd==0), 
            present = sum(chd==1),
            proportion = round(present/n,2))
chd2
as.data.frame(chd2)

# fig 1.2
plot(seq(20,70, by=10),seq(0,1,by=0.2), type="n", xlab = "Age", ylab = "Percent with CHD")
with(chd2, lines(x = c(25, 32, 37, 42, 47, 52, 57, 65),
                  y = chd2$proportion, type = "b"))


mod1 <- glm(chd ~ age, data=chd, family=binomial)
summary(mod1)
predict(mod1)
predict(mod1, type="response")

prd <- predict(mod1, newdata = data.frame(age = seq(20,70,1)), type = "response")
lines(x = seq(20,70,1), y = prd, col = "blue")

library(effects)
plot(allEffects(mod1))
plot(allEffects(mod1), type="response")


# extract the log-likelihood
logLik(mod1)

mod0 <- glm(chd ~ 1, data=chd, family=binomial)
summary(mod0)
logLik(mod0)

# evaluation of G
anova(mod0, mod1)

# manually
G <- -2*log(exp(logLik(mod0))/exp(logLik(mod1)))
# or this way
G <- 2*(logLik(mod1) - logLik(mod0))

# confidence intervals
# Table 1.4
summary(mod1)$cov.unscaled

confint(mod1)
predict(object = mod1, newdata = data.frame(age=50), type="response", se.fit = TRUE)$se.fit

library(asbio)
data(crabs)
crabs$present <- ifelse(crabs$satell > 0, 1, 0)

mod1 <- glm(present ~ width, data=crabs, family = binomial)
summary(mod1)
exp(confint(mod1, parm = "width"))
# at least a 35% increase in the odds that a female has a satellite.

pred.out <- predict(mod1, type = "response")
pred.outC <- ifelse(pred.out > 0.5, 1, 0)
table(crabs$present, pred.outC)
(27 + 95)/nrow(crabs)

mod2 <- glm(present ~ width + color, data=crabs, family = binomial)
summary(mod2)
anova(mod1, mod2)
Anova(mod2)

# Figure 5.4
library(effects)
plot(Effect(focal.predictors = c("width", "color"), mod = mod2), type = "response", multiline = TRUE)

plot(allEffects(mod2), multiline=TRUE, type="response")



# ROC curves
library(ROCR)
pred <- prediction(predictions = pred.outC, labels = crabs$present)
perf <- performance(pred,measure = "tpr",x.measure="fpr")
plot(perf)
perf <- performance(pred,measure="sens", x.measure="spec")
plot(perf)
