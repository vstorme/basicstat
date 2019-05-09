#' ---
#' title: "Factorial ANOVA"
#' author: "Veronique Storme"
#' date: "17/04/2019"
#' ---
#' 
#' R Code for Chapter 12 of:
#' 
#' Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#' 
#' (c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#' 
#' with adaptations from Veronique Storme
#' 
#' and additions of http://rcompanion.org/handbook/
#' 

## Set the working directory
## ------------------------------------------------------------------------

setwd("C:/Users/vesto/Documents/myCOURSES/basic-stat-R/april2019/data")

## Install and Load Packages

#install.packages("car")
#install.packages("ggplot2")
#install.packages("mosaic")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("lsmeans")

library(car)
library(ggplot2)
library(mosaic)
library(plyr)
library(dplyr)
library(emmeans)

###################
## two-way ANOVA ##
###################

## import the goggles data
##--------------------------

gogglesData <- read.csv("goggles.csv", header = TRUE)
head(gogglesData)

## data exploration
##--------------------------
### retrieve the levels of the factors
### Note: if categorical variables are coded numerically, you should set the categorical var as factors

levels(gogglesData$alcohol)
levels(gogglesData$gender)

### reset the order of the levels of alcohol and gender

gogglesData$alcohol <- factor(gogglesData$alcohol, levels = c("None", "2 Pints", "4 Pints"))
levels(gogglesData$alcohol)
gogglesData$gender <- factor(gogglesData$gender, levels = c("Male", "Female"))
levels(gogglesData$gender)

### frequency table

table(gogglesData$gender, gogglesData$alcohol)

### descriptives

summ <- ddply(gogglesData, .(gender, alcohol), summarise, 
      Nobs = sum(!is.na(attractiveness)),
      Nmiss = sum(is.na(attractiveness)),
      mean = mean(attractiveness, na.rm=TRUE), 
      sd = sd(attractiveness, na.rm=TRUE),
      se   = sd/sqrt(Nobs),
      t = qt(0.975, Nobs-1),
      lower = mean - t*se, 
      upper = mean + t*se)

summ

## graphical display
##------------------

### boxplots by gender and alcohol consumption

plot <- ggplot(gogglesData, aes(alcohol, attractiveness))

plot + geom_boxplot() + 
  facet_wrap(~gender) + 
  labs(x = "Alcohol Consumption", y = "Mean Attractiveness of Date (%)")

### bar charts by gender and alcohol consumption

plot + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + 
  stat_summary(fun.y = mean, geom = "bar", position="dodge") + 
  facet_wrap(~gender) +
  labs(x = "Alcohol Consumption", y = "Mean Attractiveness of Date (%)")

### interaction plot

ggplot(summ, aes(x=alcohol, y=mean, group=gender, color=gender)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1, 
                position=position_dodge(0.05)) +
  geom_line() + 
  geom_point() +
  labs(x = "Alcohol Consumption", y = "Mean Attractiveness of Date (%)") +
  scale_color_brewer(palette="Paired") 

### the lack of parallelism between lines reveals how one factor 
### changes the effect of the other one. 

## analysis with a linear model
##------------------------------

### to illustrate the analysis with a linear model, 
### we keep only the None and 4 Pints group

### subset the data

subset = filter(gogglesData, alcohol!="2 Pints")

### reset the factor levels

subset = droplevels(subset)
levels(subset$alcohol)

model.lm = lm(attractiveness ~ gender*alcohol,data=subset)
summary(model.lm)

Anova(model.lm, type="III")

### testing the assumptions

par(mfcol=c(1,2))
plot(model.lm,1:2)
par(mfcol=c(1,1))

### see slides for the interpretation

## For 2 factors with 2 levels each, the interaction effect is given by b3
## or:
### user defined contrasts in function of the lsmeans
lsm.lm <- emmeans (model.lm,  ~ gender * alcohol)
### get the order of the groups
lsm.lm
### the order is: 
### M0 F0 M4 F4

### c3: comparing differences (=interaction)
### example: compare Female-Male between 4 pints and None
### H0: (F4-F0 = M4-M0) or (F4-F0)-(M4-M0)=0 or F4-F0-M4+M0=0

contrast(lsm.lm, 
         list(c3=c(1, -1, -1, 1)), 
         adjust="none",
         by = NULL)


## two-way anova on complete data set
##------------------------------------

fit.int <- lm(attractiveness ~ gender + alcohol + gender:alcohol, data = gogglesData)

summary(fit.int)

Anova(fit.int, type="III")


### The F-test for the two-way interaction term tests for:
### H0: all group means are equal (there are 6 groups)
### HA: at least one of the group means is not equal

### The F-test indicates that there is enough evidence to reject H0 (p<0.05)

### Another way to test for significance is by performing an extra sum-of-squares F test to compare the interaction model with the main effect model (i.e. it tests whether reduction in the residual sum of squares are statistically significant or not). 
### Note that this makes sense only if the two models are nested models.
### Note also that the comparison between two or more models will only be valid if they are fitted to the same dataset. This may be a problem if there are missing values and R's default of na.action = na.omit is used.
### the function call is: anova(reduced model, full model)

fit.main = lm(attractiveness ~ gender + alcohol, data = gogglesData)
anova(fit.main,fit.int)

### testing the assumptions

par(mfcol=c(1,2))
plot(fit.int,1:2)
par(mfcol=c(1,1))

### getting the least-square means estimates 

### population average predicted values according to the model
### This equals the average values in a balanced situation

lsm <- emmeans (fit.int,  ~ gender * alcohol)
lsm 
plot(lsm)

lsmip(lsm, gender ~ alcohol, ylab = "Observed attractiveness")

### post-hoc tests

#### test all pairwise comparisons:

C1 = contrast(lsm, method="pairwise", adjust="tukey")

plot(C1)
confint(C1)

### simple tests of effects: 
### returns partial F-tests
### evaluate contrasts across the levels of one factor while the values 
### of the other interaction factor is kept fixed at certain levels
 
### example:
### H01: for gender=M: none=2pints=4pints
### H02: for gender=F: none=2pints=4pints

diffs.alc = lsmeans(fit.int, pairwise ~ alcohol | gender,glhargs=list())
diffs.alc[[2]]
confint(diffs.alc[[2]])
plot(diffs.alc[[2]])

diffs.g = lsmeans(fit.int, pairwise ~ gender | alcohol, glhargs=list())
diffs.g[[2]]
confint(diffs.g[[2]])
plot(diffs.g[[2]])

### user defined contrasts in function of the lsmeans

### c1: difference between alcohol=4pints and alcohol=2pints for gender=F
### c2: difference between alcohol=2pints and alcohol=placebo for gender=F
 
### H01: F4 - F2=0
### H02: F2 - F0 = 0

### get the order of the groups
lsm

### the order is: 
### M0 F0 M2 F2 M4 F4

contrast(lsm, 
    list(c1=c(0, 0, 0, -1, 0, 1), c2=c(0, -1, 0, 1, 0, 0)), 
    by = NULL)

### c3: comparing differences (=interaction)
### example: compare Female-Male between 4 pints and None
### H0: (F4-F0 = M4-M0) or (F4-F0)-(M4-M0)=0 or F4-F0-M4+M0=0

contrast(lsm, 
    list(c1=c(0, -1, 0, 0, 0, 1), 
         c2=c(-1, 0, 0, 0, 1, 0), 
         c3=c(1, -1, 0, 0, -1, 1)), 
    adjust="sidak",
    by = NULL)
