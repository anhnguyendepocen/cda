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
# grp <- rep(c("placebo","aspirin"), margin.table(aspirin,margin = 1))
# mi <- c(rep(c("yes","no"), aspirin[1,]),rep(c("yes","no"), aspirin[2,]))
# aspirin.df <- data.frame(grp, mi = factor(mi, levels = c("yes","no")))
# head(aspirin.df)

xtabs(~ grp + mi, data=aspirin.df)
asp.table <- xtabs(~ grp + mi, data=aspirin.df)
summary(asp.table)

as.data.frame(aspirin)
as.data.frame(as.table(aspirin))

aspirin.tab <- as.table(aspirin)

library(vcd)
oddsratio(aspirin)
epitools::oddsratio(aspirin, rev = "both")


mosaicplot(aspirin)

aspirin.df2 <- as.data.frame(aspirinT)

library(ggplot2)
ggplot(aspirin.df, aes(x=group, fill=MI)) + geom_bar(width = 0.25) 
ggplot(aspirin.df, aes(x=group, fill=MI)) + geom_bar(width = 0.25, position = "dodge") 


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


# save the table and create a data frame with Freq column
lung.cancer2 <- xtabs(~ smoking + lung.cancer + city, data = lc.df)
lc.df2 <- as.data.frame(lung.cancer2)
lc.df2

# working with a three-way table
prop.table(lung.cancer2) # all cells sum to 1

# row-wise proportions for each city (ie, proportion of lung.cancer by smoking)
prop.table(lung.cancer2, margin = c(1,3))
# what does this do?
prop.table(lung.cancer2, margin = 1)


df <- as.data.frame(prop.table(lung.cancer2, margin = c(1,3)))
library(ggplot2)
ggplot(df, aes(x = smoking, color=lung.cancer, y = Freq, group=lung.cancer)) + 
  geom_point() + geom_line() +
  facet_wrap(~ city)

lung.cancer[,,1]
lung.cancer[,,"Shanghai"]
# show city name
lung.cancer[,,1, drop = FALSE]

# all smoking rows
lung.cancer[1,,]
# all smokers = yes
lung.cancer[1,1,]
lung.cancer["smokers","yes",]


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

library(faraway)
data(orings)
orings$damage <- ifelse(orings$damage > 0, 1, 0)
write.csv(orings, file = "orings.csv", row.names = FALSE)

# table 5.5
# logit models for qualitative predictors

azt <- array(data = c(14,32,93,81,
                      11,12,52,43),dim = c(2,2,2), 
             dimnames = list("azt" = c("yes","no"),
                             "symptoms" = c("yes","no"),
                             "race" = c("white", "black")))
azt.df <- DescTools::Untable(azt)
azt.df$symptoms <- relevel(azt.df$symptoms, ref = "no")
azt.df$race <- relevel(azt.df$race, ref = "black")
azt.df$azt <- relevel(azt.df$azt, ref = "no")


mod.azt <- glm(symptoms ~ azt + race, data = azt.df, family = binomial)
summary(mod.azt)
exp(coef(mod.azt)[2])
# for each race, the estimated odds of developing symptoms are half as high for
# those who took azt immediately

mod.azt2 <- glm(symptoms ~ race, data = azt.df, family = binomial)
# anova(smaller, bigger)
anova(mod.azt2, mod.azt)
pchisq(6.87, 1, lower.tail = F)

nd <- expand.grid(azt = c("yes","no"), race = c("white","black"))
yes <- predict(mod.azt, newdata = nd, type = "response")
no <- 1 - yes

# analyze goodness of fit
margin.table(azt, margin = c(1,3))
fitted <- c(margin.table(azt, margin = c(1,3)) * yes, 
            margin.table(azt, margin = c(1,3)) * no)
obs <- c(azt[,1,],azt[,2,])

csq <- sum((obs - fitted)^2 / fitted)
dev <- 2 * sum(obs * log(obs/fitted))

mantelhaen.test(azt)

# test of homogeneous odds ratios
mod.azt3 <- glm(symptoms ~ azt * race, data = azt.df, family = binomial)
car::Anova(mod.azt3)

# crabs data
install.packages("icda",repos="http://www.stat.ufl.edu/~presnell/R",type="source") 
library(icda)
data("horseshoecrabs")
horseshoecrabs$Satellite <- ifelse(horseshoecrabs$Satellites > 0, 1, 0)
horseshoecrabs$ColorC <- factor(horseshoecrabs$Color, labels = c("light medium","medium","dark medium","dark"))
horseshoecrabs$ColorC <- relevel(horseshoecrabs$ColorC, ref = "dark")
mod1 <- glm(Satellite ~ Width + ColorC, data=horseshoecrabs, family = binomial)
summary(mod1)

# Figure 5.4
library(effects)
plot(Effect(mod = mod1, focal.predictors = c("Width","ColorC")), type = "response", multiline = TRUE)

predict(object = mod1, 
        newdata = data.frame(Width = 26.3, ColorC = factor("light medium")),
        type = "response")
predict(object = mod1, 
        newdata = data.frame(Width = 26.3, ColorC = factor("dark")),
        type = "response")

# Keep color as a predictor
car::Anova(mod1)

# model with interaction
mod2 <- glm(Satellite ~ Width * ColorC, data=horseshoecrabs, family = binomial)
car::Anova(mod2)

# model with color as ordinal predictor
mod3 <- glm(Satellite ~ Width + Color, data=horseshoecrabs, family = binomial)
summary(mod3)
exp(coef(mod3)[3])
# At a given width, for every one-category increase in color darkness, the 
# estimated odds of a satellite multiply by 0.601. For example, the estimated 
# odds of a satellite for medium colored crabs are 60% of those for medium-light
# crabs.

car::Anova(mod3)
# test that simpler model holds; also a test that the color parameters follow a
# linear trend when plotted against color scores.
anova(mod3, mod1)
pchisq(1.66, df = 2, lower.tail = FALSE)

# another scoring model: {1,1,1,0}
horseshoecrabs$ColorB <- ifelse(horseshoecrabs$Color < 4, 1, 0)
mod4 <- glm(Satellite ~ Width + ColorB, data=horseshoecrabs, family = binomial)
summary(mod4)
anova(mod4, mod1)
pchisq(0.50085, df = 2, lower.tail = FALSE)

# model selection with several predictors
horseshoecrabs$Spine <- factor(horseshoecrabs$Spine)
horseshoecrabs$Spine <- relevel(horseshoecrabs$Spine, ref = 3)
mod5 <- glm(Satellite ~ ColorC + Spine + Weight + Width, data=horseshoecrabs, family = binomial)
summary(mod5)

cor(horseshoecrabs$Width, horseshoecrabs$Weight)
# redundant to use both weight and width in the model

mod6 <- glm(Satellite ~ Width * ColorC * Spine, data=horseshoecrabs, family = binomial, 
            control = list(maxit = 100, epsilon = 1e-6))
summary(mod6)
# This returns the first four rows of table 5.8
deviance(mod6)
drop1(mod6, test = "LRT")
car::Anova(mod6) # check interactions

# model 3c
mod7 <- glm(Satellite ~ Spine * ColorC + Width * ColorC, data=horseshoecrabs, family = binomial)
summary(mod7)
# model 4a
mod8 <- glm(Satellite ~ Spine + Width * ColorC, data=horseshoecrabs, family = binomial)
summary(mod8)
anova(mod8, mod7)
car::Anova(mod8)
# model 4b
mod9 <- glm(Satellite ~ Width + ColorC * Spine, data=horseshoecrabs, family = binomial)
summary(mod9)
anova(mod9, mod7)
car::Anova(mod9)

# main effects only
# model 5
mod10 <- glm(Satellite ~ Width + ColorC + Spine, data=horseshoecrabs, family = binomial)
summary(mod10)
car::Anova(mod10)
drop1(mod10, test = "LRT")

# model 6c
mod11 <- glm(Satellite ~ Width + ColorC, data=horseshoecrabs, family = binomial)
car::Anova(mod11)
drop1(mod11, test = "LRT")

mod12 <- glm(Satellite ~ Width + ColorB, data=horseshoecrabs, family = binomial)
car::Anova(mod12)
anova(mod12, mod11)
drop1(mod12, test = "LRT")
summary(mod12)


# correlation summary of predictive power
cor(horseshoecrabs$Satellite, predict(mod6, type = "response"))
cor(horseshoecrabs$Satellite, predict(mod11, type = "response"))
cor(horseshoecrabs$Satellite, predict(mod12, type = "response"))

# mod12 has 85% as high a correlation as the complex model 
cor(horseshoecrabs$Satellite, predict(mod12, type = "response")) / 
  cor(horseshoecrabs$Satellite, predict(mod6, type = "response"))


# sample size for comparing proportions
power.prop.test(p1 = 0.2, p2 = 0.3, power = 0.9)

# logistic regression sample size with quantitative predictor
alpha <- 0.05
beta <- 0.10
or <- 1.57
lambda <- log(or)
za <- qnorm(1 - alpha)
zb <- qnorm(1 - beta)
p.est <- 0.08
delta <- (1 + (1 + lambda^2) * exp((5 * lambda^2) / 4)) / (1 + exp((-lambda^2)/4))
N <- ((za + zb * exp((-lambda^2)/4))^2 * (1 + 2 * p.est * delta)) / (p.est * lambda^2)


# ch 6 - loglinear models
afterlife <- matrix(data = c(435, 375, 147, 134), ncol = 2, 
                    dimnames = list("gender" = c("female","male"),
                                    "believe" = c("yes","no")))
afterlife

addmargins(afterlife)
prop.table(afterlife, margin = 1)
pt <- prop.table(afterlife, margin = 1)
pt[1,1]/pt[1,2] # odds of belief in afterlife for females
pt[2,1]/pt[2,2] # odds of belief in afterlife for males
(pt[1,1]/pt[1,2]) / (pt[2,1]/pt[2,2]) # odds ratio


# test for independence
chisq.test(afterlife)
chisq.test(afterlife, correct = FALSE)

prop.test(afterlife)

# convert to data frame with a column for frequencies
al.df <- as.data.frame(as.table(afterlife))
al.df
# The independence model
mod1 <- glm(Freq ~ gender + believe, data = al.df, family = poisson)
summary(mod1)
predict(mod1, type = "response")
exp(coef(mod1))

# The saturated model - the most general model for 2-way contingency tables
mod2 <- glm(Freq ~ gender * believe, data = al.df, family = poisson)
summary(mod2) # interaction in Table 6.2
exp(coef(mod2)[4]) # odds ratio, same as
(pt[1,1]/pt[1,2]) / (pt[2,1]/pt[2,2]) 

# the interaction coefficient is not significant, indicating that belief in
# afterlife is independent of gender

# Table 6.3
seniors <- array(data = c(911, 44, 538, 456, 3, 2, 43, 279), dim = c(2,2,2), 
                 dimnames = list("cigarette" = c("yes","no"),
                                 "marijuana" = c("yes","no"),
                                 "alcohol" = c("yes","no")))
seniors
seniors.df <- as.data.frame(as.table(seniors))

# test null that X and Y (cigarette and marijuana) are conditionally independent
mantelhaen.test(seniors)

# loglinear models are primarily useful for modeling relationships among two or 
# more categorical response variables. Loglinear models for contingency tables
# do not distinguish between response and explanatory variables.

# Constructing table 6.5

mod1 <- glm(Freq ~ marijuana + cigarette + alcohol, data = seniors.df, family = poisson)
summary(mod1)
fitted(mod1)
deviance(mod1)
df.residual(mod1)
pchisq(deviance(mod1), df = df.residual(mod1), lower.tail = FALSE)
# combine fitted data with data (factor combinations)
fit1 <- cbind(mod1$data[,-4], Fit = fitted(mod1))
t.out <- xtabs(Fit ~ alcohol + cigarette, fit1)

xtabs(Fit ~ alcohol + marijuana + cigarette, fit1)

# function to calculate odds ratio
or <- function(t){
  (t[1,1]*t[2,2]) / (t[1,2]*t[2,1])
}
# function to calculate associations
# note: order matters in the xtabs functions
getAssoc <- function(mod){
  fit <- cbind(mod$data[,-4], Fit = fitted(mod))
  list(
    "conditional association" = c(or(xtabs(Fit ~ alcohol + cigarette + marijuana, fit)[,,1]),
    or(xtabs(Fit ~ alcohol + cigarette + marijuana, fit)[,,2]),
    or(xtabs(Fit ~ alcohol + marijuana + cigarette, fit)[,,1]),
    or(xtabs(Fit ~ alcohol + marijuana + cigarette, fit)[,,2]),
    or(xtabs(Fit ~ cigarette + marijuana + alcohol, fit)[,,1]),
    or(xtabs(Fit ~ cigarette + marijuana + alcohol, fit)[,,2])),

    "marginal association" = c(or(xtabs(Fit ~ alcohol + cigarette, fit)),
  or(xtabs(Fit ~ alcohol + marijuana, fit)),
  or(xtabs(Fit ~ cigarette + marijuana, fit)))
  )
}

getAssoc(mod1)

mod2 <- glm(Freq ~ cigarette * alcohol + marijuana, data = seniors.df, family = poisson)
summary(mod2)
fitted(mod2)
deviance(mod2)
df.residual(mod2)
pchisq(deviance(mod2), df = df.residual(mod2), lower.tail = FALSE)

getAssoc(mod2)

mod3 <- glm(Freq ~ alcohol * marijuana + cigarette * marijuana, data = seniors.df, family = poisson)
summary(mod3)
fitted(mod3)
deviance(mod3)
df.residual(mod3)
pchisq(deviance(mod3), df = df.residual(mod3), lower.tail = FALSE)
getAssoc(mod3)
# adjusted residuals
rstandard(mod3, type = "pearson")


# homogeneous association model
mod4 <- glm(Freq ~ (alcohol + marijuana + cigarette)^2, data = seniors.df, family = poisson)
summary(mod4)
fitted(mod4)
deviance(mod4)
df.residual(mod4)
pchisq(deviance(mod4), df = df.residual(mod4), lower.tail = FALSE)
coef(mod4)[5:7] # the highest-order terms
exp(coef(mod4)[5:7]) # exponentiate to get conditional associations
getAssoc(mod4)
# adjusted residuals
rstandard(mod4, type = "pearson")

mod5 <- glm(Freq ~ alcohol * marijuana * cigarette, data = seniors.df, family = poisson)
summary(mod5)
fitted(mod5) # perfect fit; saturated model
deviance(mod5)
df.residual(mod5)
pchisq(deviance(mod5), df = df.residual(mod5), lower.tail = FALSE)
getAssoc(mod5)

# Table 6.7
# rstandard(mod3, type = "pearson") gives "adjusted residuals";
# adjusted residuals larger than 3 indicate a lack of fit
cbind(mod4$data,
      fitted(mod3), 
      resid.3 = rstandard(mod3, type = "pearson"), 
      fitted(mod4),
      resid.4 = rstandard(mod4, type = "pearson"))

# 6.3.4 test about partial association
anova(mod3, mod4)
car::Anova(mod4)

# 6.3.5 CI for odds ratios
coef(mod4)
summary(mod4)
exp(confint(mod4)) # based on profile likelihood
exp(confint.default(mod4)) # based on asymptotic normality
round(exp(confint.default(mod4)),1)
exp(confint(mod4, parm = "alcoholno:cigaretteno"))
exp(confint(mod4, parm = "alcoholno:marijuanano"))
exp(confint(mod4, parm = "marijuanano:cigaretteno"))

# 6.4 loglinear models for higher dimensions
# Table 6.8

auto <- array(data = c(7287, 11587, 996, 759,
                       3246, 6134, 973, 757,
                       10381, 10969, 812, 380,
                       6123, 6693, 1084, 513), 
              dim = c(2,2,2,2),
              dimnames = list("seat belt" = c("no","yes"),
                              "injury" = c("no","yes"),
                              "location" = c("urban","rural"),
                              "gender" = c("female","male")))
ftable(auto, row.vars = c("gender","location","seat belt"))

prop.table(auto, margin = c(1,3,4))[,2,,]

auto.df <- as.data.frame(as.table(auto))

mod1 <- glm(Freq ~ seat.belt + injury + location + gender, data = auto.df, family = poisson)
summary(mod1)
pchisq(deviance(mod1), df = df.residual(mod1), lower.tail = FALSE)

# all two-factor terms
mod2 <- glm(Freq ~ (seat.belt + injury + location + gender)^2, data = auto.df, family = poisson)
summary(mod2)
pchisq(deviance(mod2), df = df.residual(mod2), lower.tail = FALSE)

# all three-factor terms
mod3 <- glm(Freq ~ (seat.belt + injury + location + gender)^3, data = auto.df, family = poisson)
summary(mod3)
pchisq(deviance(mod3), df = df.residual(mod3), lower.tail = FALSE)

# Right side of Table 6.8
cbind(mod2$data, fitted(mod2))

exp(coef(mod2))
matrix(exp(coef(mod2)),ncol=1, dimnames = list("coef" = names(coef(mod2))))

mod4 <- glm(Freq ~ gender*location*seat.belt + gender*injury + injury*location + injury*seat.belt, 
            data = auto.df, family = poisson)
matrix(exp(coef(mod4)),ncol=1, dimnames = list("coef" = names(coef(mod4))))
summary(mod4)
exp(coef(mod4))
fitted(mod4)
car::Anova(mod4)

# Table 6.10
fit.df <- cbind(mod4$data[,-5], fit = fitted(mod4))
or(xtabs(fit ~ gender + injury, data = fit.df, subset = seat.belt=="no" & location=="rural"))
or(xtabs(fit ~ gender + injury, data = fit.df, subset = seat.belt=="yes" & location=="rural"))
or(xtabs(fit ~ gender + injury, data = fit.df, subset = seat.belt=="no" & location=="urban"))
or(xtabs(fit ~ gender + injury, data = fit.df, subset = seat.belt=="yes" & location=="urban"))

or(xtabs(fit ~ location + injury, data = fit.df, subset = seat.belt=="yes" & gender=="female"))
or(xtabs(fit ~ location + injury, data = fit.df, subset = seat.belt=="no" & gender=="female"))
or(xtabs(fit ~ location + injury, data = fit.df, subset = seat.belt=="yes" & gender=="male"))
or(xtabs(fit ~ location + injury, data = fit.df, subset = seat.belt=="no" & gender=="male"))

or(xtabs(fit ~ seat.belt + injury, data = fit.df, location=="rural" & gender=="female"))
or(xtabs(fit ~ seat.belt + injury, data = fit.df, location=="urban" & gender=="female"))
or(xtabs(fit ~ seat.belt + injury, data = fit.df, location=="rural" & gender=="male"))
or(xtabs(fit ~ seat.belt + injury, data = fit.df, location=="urban" & gender=="male"))

or(xtabs(fit ~ gender + location, data = fit.df, subset = seat.belt=="no" & injury=="yes"))
or(xtabs(fit ~ gender + location, data = fit.df, subset = seat.belt=="no" & injury=="no"))
or(xtabs(fit ~ gender + location, data = fit.df, subset = seat.belt=="yes" & injury=="yes"))
or(xtabs(fit ~ gender + location, data = fit.df, subset = seat.belt=="yes" & injury=="no"))

or(xtabs(fit ~ gender + seat.belt, data = fit.df, subset = location=="urban" & injury=="yes"))
or(xtabs(fit ~ gender + seat.belt, data = fit.df, subset = location=="urban" & injury=="no"))
or(xtabs(fit ~ gender + seat.belt, data = fit.df, subset = location=="rural" & injury=="yes"))
or(xtabs(fit ~ gender + seat.belt, data = fit.df, subset = location=="rural" & injury=="no"))

or(xtabs(fit ~ location + seat.belt, data = fit.df, subset = gender=="female" & injury=="yes"))
or(xtabs(fit ~ location + seat.belt, data = fit.df, subset = gender=="female" & injury=="no"))
or(xtabs(fit ~ location + seat.belt, data = fit.df, subset = gender=="male" & injury=="yes"))
or(xtabs(fit ~ location + seat.belt, data = fit.df, subset = gender=="male" & injury=="no"))

# dissimilarity index
sum(abs(mod2$data$Freq - fitted(mod2))) / (2*sum(mod2$data$Freq))
sum(abs(mod4$data$Freq - fitted(mod4))) / (2*sum(mod4$data$Freq))

# problem 6.3
dp <- array(data = c(53,11,414,37,0,4,16,139),
            dim = c(2,2,2),
            dimnames = list("defendant.race" = c("white","black"),
                            "death.penalty" = c("yes","no"),
                            "victim.race" = c("white","black")))

dp

prop.table(dp, margin = c(1,3))
pt <- prop.table(dp, margin = c(1,3))

pt[1,1,1]/pt[1,2,1]
pt[2,1,1]/pt[2,2,1]
# odds ratio of death penalty for white versus black defendant when victim is white
(pt[1,1,1]/pt[1,2,1]) / (pt[2,1,1]/pt[2,2,1])
# odds of white defendent getting death penalty are 0.43 of black defendant

pt[1,1,2]/pt[1,2,2]
pt[2,1,2]/pt[2,2,2]
# odds ratio of death penalty for white versus black defendant when victim is black
(pt[1,1,2]/pt[1,2,2]) / (pt[2,1,2]/pt[2,2,2])
# 0: no white defendants received death penalty when victim was black

# If we don't take victim's race into consideratio,
# association reverses direction 
margin.table(dp, margin = c(1,2))
prop.table(margin.table(dp, margin = c(1,2)))
pt1 <- prop.table(margin.table(dp, margin = c(1,2)))
pt1[1,1]/pt1[1,2]
pt1[2,1]/pt1[2,2]
# odds ratio of death penalty for white versus black
(pt1[1,1]/pt1[1,2]) / (pt1[2,1]/pt1[2,2])
# odds of white defendant getting death penalty 1.44 times the odds of black
# defendant getting death penalty.


# homogenous association: odds ratios are indentical at each level of a third variable

dp.df <- as.data.frame(as.table(dp))
dp.df
dp.df$death.penalty <- relevel(dp.df$death.penalty, ref = "no")
# mutual independence model
mod0 <- glm(Freq ~ defendant.race + death.penalty + victim.race, data = dp.df, family = poisson)
summary(mod0)
# beware all the low p-values. This is actually a very poor model.
cbind(mod0$data, fit = round(fitted(mod0)))
pchisq(deviance(mod0), df = df.residual(mod0), lower.tail = FALSE)
rstandard(mod0, type = "pearson")
# homogenous association model

# all three pairs of variables are conditionally dependent. The conditional odds
# ratios between any two variables are identical at each level of the 3rd
# variable.

mod1 <- glm(Freq ~ (defendant.race + death.penalty + victim.race)^2, data = dp.df, family = poisson)
summary(mod1)
cbind(mod1$data, fit =fitted(mod1))
coef(mod1)
coef(mod1)[5]
exp(coef(mod1)[5])

anova(mod0, mod1)

mod2 <- glm(Freq ~ defendant.race * death.penalty * victim.race, data = dp.df, family = poisson)
summary(mod2)


# (a) conditional odds ratio
fit <- cbind(mod1$data[,-4], fit = fitted(mod1))
or(xtabs(fit ~ defendant.race + death.penalty, data = fit, subset = victim.race=="white"))
or(xtabs(fit ~ defendant.race + death.penalty, data = fit, subset = victim.race=="black"))
# (b) marginal odds ratio
or(xtabs(fit ~ defendant.race + death.penalty, data = fit))
or(xtabs(Freq ~ defendant.race + death.penalty, data = dp.df))
# (c) test goodness of fit
pchisq(deviance(mod1), df = df.residual(mod1), lower.tail = FALSE)
# (d) fit simpler model
mod2 <- glm(Freq ~ defendant.race * victim.race + death.penalty * victim.race, 
            data = dp.df, family = poisson)
summary(mod2)
pchisq(deviance(mod2), df = df.residual(mod2), lower.tail = FALSE)
# (e)
anova(mod2, mod1)
pchisq(5.0142, 1, lower.tail = F)
rstandard(mod2, type = "pearson")
cbind(mod2$data,fit = fitted(mod2),adj.r = rstandard(mod2, type = "pearson"))

# using loglin() function
ll.out <- loglin(dp, margin = list(c(1,2),c(1,3),c(2,3)), fit = TRUE, param = TRUE)


# ch 7
opinion <- matrix(data = c(81, 24, 18, 36,
                           68, 26, 41, 57,
                           60, 29, 74, 161, 
                           38, 14, 42, 157),
                  ncol = 4,
                  dimnames = list("premarital sex" = c("always wrong","almost always wrong", 
                                                       "wrong only sometimes","not wrong at all"),
                                  "birth control" = c("strongly disagree","disagree","agree","strongly agree")))
opinion.df <- as.data.frame(as.table(opinion))
mod1 <- glm(Freq ~ premarital.sex + birth.control, data=opinion.df, family = poisson)
deviance(mod1)
df.residual(mod1)
# Table 7.3
cbind(mod1$data, fit = fitted(mod1), adj.r = rstandard(mod1, type = "pearson"))

# 7.2.1 linear x linear association
# create scores and add to data frame
opinion.df <- cbind(opinion.df, expand.grid(ps.score = 1:4, bc.score = 1:4))

mod2 <- glm(Freq ~ premarital.sex + birth.control + I(ps.score * bc.score), data=opinion.df, family = poisson)
summary(mod2)
deviance(mod2)
df.residual(mod2)
cbind(mod2$data, fit = fitted(mod2), adj.r = rstandard(mod2, type = "pearson"))
# estimate of association parameter
coef(mod2)[8]
exp(coef(mod2)[8]) # estimated local odds ratio
# estimated odds ratio for the four corner cells
exp(coef(mod2)[8]*3*3)


anova(mod1, mod2)

# 7.3.2 - job satisfaction example
Satisfaction <-
  as.table(array(c(1, 2, 0, 0, 3, 3, 1, 2,
                   11, 17, 8, 4, 2, 3, 5, 2,
                   1, 0, 0, 0, 1, 3, 0, 1,
                   2, 5, 7, 9, 1, 1, 3, 6),
                 dim = c(4, 4, 2),
                 dimnames =
                   list(Income =
                          c("<5000", "5000-15000",
                            "15000-25000", ">25000"),
                        "Job Satisfaction" =
                          c("V_D", "L_S", "M_S", "V_S"),
                        Gender = c("Female", "Male"))))
Satisfaction
ftable(. ~ Gender + Income, Satisfaction)

sat.df <- as.data.frame(Satisfaction)
# conditional independence model
mod1 <- glm(Freq ~ Gender*Income + Gender*Job.Satisfaction, data = sat.df, family = poisson)
deviance(mod1)
df.residual(mod1)
mod2 <- glm(Freq ~ (Gender + Income + Job.Satisfaction)^2, data = sat.df, family = poisson)
deviance(mod2)
df.residual(mod2)
anova(mod1, mod2)
pchisq(12.275, df = 9, lower.tail = FALSE)

# 7.3.4 - detecting ordinal conditional association
# add scores to data frame
sat.df <- cbind(sat.df, expand.grid(inc.score = 1:4, js.score = 1:4))
mod1ll <- glm(Freq ~ Gender*Income + Gender*Job.Satisfaction + I(inc.score * js.score), 
            data = sat.df, family = poisson)
deviance(mod1ll)
df.residual(mod1ll)
anova(mod1,mod1ll)
summary(mod1ll)
# coefficient for association
coef(mod1ll)[9] 
# reveals tendency for jobsatisfaction to be greater at higher levels of income;
# estimated local odds ratio
exp(coef(mod1ll)[9])


# 7.3.7
mantelhaen.test(Satisfaction)

# 7.4.2 - clinical trials example
ct <- array(data = c(0,0,5,9,
                     1,0,12,10,
                     0,0,7,5,
                     6,2,3,6,
                     5,2,9,12), dim = c(2,2,5),
            dimnames = list(treatment = c("active drug","placebo"),
                            response = c("success","failure"),
                            center = 1:5))
ftable(ct, row.vars = c("center","treatment"))
# C - R marginal
t(margin.table(ct, margin = c(2,3)))
# T - R marginal
margin.table(ct, margin = c(1,2))

ct.df <- DescTools::Untable(ct)
mod1 <- glm(response ~ treatment * center, data = ct.df, family = binomial)
summary(mod1)

ct.df$center2 <- factor(ifelse(ct.df$center %in% c(1,2,3), 1, ct.df$center))
mod2 <- glm(response ~ treatment * center2, data = ct.df, family = binomial)
summary(mod2)

anova(mod1) # G^2 on last line
anova(mod2) # G^2 on last line
car::Anova(mod1)
car::Anova(mod2)

# ch 8
library(icda)
data(alligators1)
str(alligators1)
alligators1$Food <- relevel(alligators1$Food, ref = "O")
library(nnet)
mod0 <- multinom(Food ~ 1, data = alligators1)
mod1 <- multinom(Food ~ Length, data = alligators1)
summary(mod1)
anova(mod0,mod1)
car::Anova(mod1)

p.out <- predict(mod1, type = "probs", newdata = data.frame(Length = seq(1,4,0.1)))
p.out <- as.data.frame(cbind(p.out, Length = seq(1,4,0.1)))
library(reshape2)
p.outL <- melt(p.out, id.vars = "Length", variable.name = "Food", value.name = "Probability")
library(ggplot2)
ggplot(p.outL, aes(x = Length, y = Probability, color = Food)) + geom_line()

# Table 8.3
belief <- as.table(array(data = c(371, 250, 49, 45, 74, 71,
                                  64, 25, 9, 5, 15, 13),
                         dim = c(2,3,2),
                         dimnames = list("gender" = c("female","male"),
                                         "belief" = c("yes","undecided","no"),
                                         "race" = c("white","black"))))
belief.df <- DescTools::Untable(belief)
belief.df$gender <- relevel(belief.df$gender, ref = "male")
belief.df$belief <- relevel(belief.df$belief, ref = "no")
belief.df$race <- relevel(belief.df$race, ref = "black")
mod1 <- multinom(belief ~ gender + race, data = belief.df)
summary(mod1)
predict(mod1, type = "probs", newdata = expand.grid(gender = c("female","male"), race = c("white","black")))

# loglinear model
belief.df2 <- as.data.frame(belief)
belief.df2$belief <- relevel(belief.df2$belief, ref = "no")
belief.df2$gender <- relevel(belief.df2$gender, ref = "male")
belief.df2$race <- relevel(belief.df2$race, ref = "black")
mod2 <- glm(Freq ~ (belief + gender + race)^2, data = belief.df2, family = poisson)
summary(mod2)

(2.4036 + 0.8831 + 0.4259 + 0.4186) + (2.4036) - (2.4036 + 0.8831) - (2.4036 + 0.4259) 
exp((2.4036 + 0.8831 + 0.4259 + 0.4186) + (2.4036) - (2.4036 + 0.8831) - (2.4036 + 0.4259))

# estimated effect of gender on belief, controlling for race
# beliefyes:genderfemale         0.4186
exp(0.4186)
# estimated effect of race on belief, controlling for gender
# beliefyes:racewhite            0.3418
exp(0.3418)

# proportional odds models
# 8.2.2

polid <- matrix(data = c(80, 30, 81, 46, 171, 148, 41, 84, 55, 99), ncol = 5,
                dimnames = list("party" = c("dem","rep"), 
                                "ideology" = c("very liberal", "slightly liberal", "moderate", 
                                               "slightly conservative", "very conservative")))
polid

prop.table(polid, margin = 2)
prop.table(polid, margin = 1)
polid <- as.table(polid)
polid.df <- DescTools::Untable(polid)
polid.df$party <- relevel(polid.df$party, ref = "rep")
library(MASS)
po.mod1 <- polr(ideology ~ party, data = polid.df)
summary(po.mod1)
# fitted values in Table 8.6
# default for predict.polr is type = "class"
p.out <- predict(po.mod1, newdata = data.frame(party = c("rep","dem")), type = "probs")
margin.table(polid, margin = 1)
p.out[1,] * 407
p.out[2,] * 428

exp(coef(po.mod1))
# Ordinal test of independence
car::Anova(po.mod1)
# same as difference in residual deviances 
po.mod0 <- polr(ideology ~ 1, data = polid.df)
deviance(po.mod0) - deviance(po.mod1)


# ch 9 - models for matched pairs

# from mcnemar.test documentation

## Agresti (1990), p. 350.
## Presidential Approval Ratings.
##  Approval of the President's performance in office in two surveys,
##  one month apart, for a random sample of 1600 voting-age Americans.
Performance <-
  matrix(c(794, 86, 150, 570),
         nrow = 2,
         dimnames = list("1st Survey" = c("Approve", "Disapprove"),
                         "2nd Survey" = c("Approve", "Disapprove")))
Performance
mcnemar.test(Performance)
mcnemar.test(Performance, correct = FALSE)
nm.out <- mcnemar.test(Performance, correct = FALSE)
sqrt(nm.out$statistic) # Z statistic in book
## => significant change (in fact, drop) in approval ratings

# number of approve/disapprove at first survey
margin.table(Performance, margin = 1)
prop.table(margin.table(Performance, margin = 1))
# number of approve/disapprove at first survey
margin.table(Performance, margin = 2)
prop.table(margin.table(Performance, margin = 2))

diag(Performance)

x <- matrix(1:9, 3)
# extract the diagonal of a matrix
x[row(x) == col(x)]
diag(x)
# extract the off-diagonal of a matrix
x[row(x) != col(x)]

Performance[row(Performance) != col(Performance)]
# need to swap order to match the text; 150 "successes" and 86 "failures"
Performance[row(Performance) != col(Performance)][2:1]
binom.test(x = Performance[row(Performance) != col(Performance)][2:1])
binom.test(x = Performance[row(Performance) != col(Performance)][2:1], alternative = "greater")

dep.prop.test <- function(x, conf.level = 0.95){
  est <- prop.table(margin.table(x, margin = 1))[1] - prop.table(margin.table(x, margin = 2))[1]
  names(est) <- NULL
  num <- prod(prop.table(margin.table(x, margin = 1))) + 
    prod(prop.table(margin.table(x, margin = 2))) - 
    (2 * (((x[1,1]/sum(x)) * (x[2,2]/sum(x))) - ((x[1,2]/sum(x)) * (x[2,1]/sum(x)))))
  se <- sqrt(num/sum(x))
  conf.int <- est + (c(-1,1) * qnorm(p = (1 - 0.95)/2, lower.tail = FALSE) * se)
  conf.int
}
dep.prop.test(x = Performance)

# table 9.3

mi <- matrix(data = c(9,16,37,82),nrow = 2, 
             dimnames = list("MI Cases" = c("Diabetes","No Diabetes"),
                             "MI Controls" = c("Diabetes", "No Diabetes")))
mi <- as.table(mi)
mi.df <- DescTools::Untable(mi)

library(survival)
# clogit example
resp <- levels(logan$occupation)
n <- nrow(logan)
indx <- rep(1:n, length(resp))
logan2 <- data.frame(logan[indx,],
                     id = indx,
                     tocc = factor(rep(resp, each=n)))
logan2$case <- (logan2$occupation == logan2$tocc)
clogit(case ~ tocc + tocc:education + strata(id), logan2)

table.9.3 <- data.frame(pair=rep(1:144,each = 2), 
                        MI=rep(c(0,1),144), # 0 = control, 1 = case
                        # possible case-control pairs
                        diabetes=c(rep(c(1,1),9), # case and contol have diabetes
                                   rep(c(1,0),16), # control has diabetes, case does not
                                   rep(c(0,1),37), # case has diabetes, control does not
                                   rep(c(0,0),82))) # neither case not control have diabetes
head(table.9.3)
library(survival)
# formula: case.status ~ exposure + strata(matched.set)
fit.CLR <- clogit(MI ~ diabetes + strata(pair), method="exact", data=table.9.3)
summary(fit.CLR)

table.9.5 <- matrix(data = c(93, 9, 17, 6, 10,
                             17, 46, 11, 4, 4,
                             44, 11, 155, 9, 12,
                             7, 0, 9,15, 2,
                             10, 9, 12, 2, 27), nrow = 5)
dimnames(table.9.5) <- list("first purchase" = c("high point", "taster's choice", "sanka", "nescafe", "brim"),
                            "second purchase" = c("high point", "taster's choice", "sanka", "nescafe", "brim"))
table.9.5.df <- as.data.frame(as.table(table.9.5))

table.9.5.df$symm <- paste(pmin(as.numeric(table.9.5.df$first.purchase), 
                                as.numeric(table.9.5.df$second.purchase)),
                           pmax(as.numeric(table.9.5.df$first.purchase), 
                                as.numeric(table.9.5.df$second.purchase)),
                           sep = ":")

# symmetry model
mod1 <- glm(Freq ~ symm, data = table.9.5.df, family = poisson)
summary(mod1)

# Residual deviance:  22.473  on 10  degrees of freedom
# this is what is given on page 235 as G^2
cbind(mod1$data, 
      fit = fitted(mod1), 
      adj.res = round(rstandard(mod1, type = "pearson"),3))

# 9.3.3 Quasi Symmetry
# symmetry model is so simple it rarely fits well.
# quasi symmetry allows differing main effect terms
# quasi symmetry model
mod2 <- glm(Freq ~ symm + first.purchase, data = table.9.5.df, family = poisson)
summary(mod2)

table.10.5 <- data.frame(expand.grid(PreSex=factor(1:4),
                                     ExSex=factor(1:4)),
                         counts=c(144,33, 84, 126, 2, 4 ,14 ,
                                  29 ,0 ,2 ,6 ,25, 0, 0, 1, 5)) 

table.10.5$symm <- paste(pmin(as.numeric(table.10.5$PreSex),as.numeric(table.10.5$ExSex)),
                       pmax(as.numeric(table.10.5$PreSex),as.numeric(table.10.5$ExSex)),
                       sep=",")   

table.10.5$symm <- factor(table.10.5$symm, levels=unique(rev(table.10.5$symm)))
table.10.5$scores <- rep(1:4,each=4) 

fit.oqsymm <- glm(counts ~ symm + scores,data=table.10.5,family=poisson)
summary(fit.oqsymm)
