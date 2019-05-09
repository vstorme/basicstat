setwd("C:/Users/vesto/Documents/myCOURSES/basic-stat-R/april2019/data")
dir()

library(car)
library(ggplot2)
library(mosaic)
library(beanplot)
library(lmtest)
library(plyr)
library(dplyr)
library(MASS)

# Task 1
#############

## Ref: https://www.biology.lu.se/course-material-statistics-for-biologists

ancex <- read.csv("ANCEX.csv", header = TRUE, sep=";")
head(ancex)

## a. descriptive statistics
## ------------------------------------------------------------------------

summarise(ancex, Nobs = sum(!is.na(XVAR)),
          Nmiss = sum(is.na(XVAR)),
          mean = mean(XVAR, na.rm=TRUE), 
          sd = sd(XVAR, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

summarise(ancex, Nobs = sum(!is.na(YVAR)),
          Nmiss = sum(is.na(YVAR)),
          mean = mean(YVAR, na.rm=TRUE), 
          sd = sd(YVAR, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

## b. graphical display
## ------------------------------------------------------------------------

scatter <- ggplot(ancex, aes(XVAR, YVAR))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Blue", se = T, level=0.95)
  
## c - f/ run the simple linear regression model
## ------------------------------------------------------------------------

fit <- lm(YVAR ~ XVAR, data = ancex)
summary(fit)

confint(fit)

## g. Testing the assumptions:
##-----------------------------

## linearity, normality and homoscedasticity of the residuals

opar <- par(mfrow=c(2,2)) # make a copy of current settings
plot(fit)
par(opar) # restore original settings

# plot studentized residuals vs fitted values
plot(fit$fitted.values, studres(fit), ylim=c(-5,5))
abline(h=c(-2,2), col=c("blue", "blue"))
abline(h=c(-3,3), col=c("red", "red"))

## h. choose XVAR as the dep var
fit.rev <- lm(XVAR ~ YVAR, data = ancex)
summary(fit.rev)

summary(fit)

# Task 2
##############

## load the bodyfat2 data:
bodyfat <- read.delim("bodyfat2.txt", header = TRUE)
head(bodyfat)

## a/ descriptives
summarise(bodyfat, Nobs = sum(!is.na(PctBodyFat2)),
          Nmiss = sum(is.na(PctBodyFat2)),
          mean = mean(PctBodyFat2, na.rm=TRUE), 
          sd = sd(PctBodyFat2, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

summarise(bodyfat, Nobs = sum(!is.na(Weight)),
          Nmiss = sum(is.na(Weight)),
          mean = mean(Weight, na.rm=TRUE), 
          sd = sd(Weight, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

## b/ scatter plot
scatter <- ggplot(bodyfat, aes(Weight, PctBodyFat2))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Blue", se = T, level=0.95)

## c - h/ Regression model to predict PctBodyFat2 from weight:
fit.bf <-lm(PctBodyFat2 ~ Weight, data = bodyfat)
summary(fit.bf)

confint(fit.bf)

## i/ Testing the assumptions:
##-----------------------------

## linearity, normality and homoscedasticity of the residuals

opar <- par(mfrow=c(2,2)) # make a copy of current settings
plot(fit.bf)
par(opar) # restore original settings

# plot studentized residuals vs fitted values
plot(fit.bf$fitted.values, studres(fit.bf), ylim=c(-5,5))
abline(h=c(-2,2), col=c("blue", "blue"))
abline(h=c(-3,3), col=c("red", "red"))
