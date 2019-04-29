setwd("---")
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

summarise(---, Nobs = sum(!is.na(---)),
          Nmiss = sum(is.na(---)),
          mean = mean(---, na.rm=TRUE), 
          sd = sd(---, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

summarise(---, Nobs = sum(!is.na(---)),
          Nmiss = sum(is.na(---)),
          mean = mean(---, na.rm=TRUE), 
          sd = sd(---, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

## b. graphical display
## ------------------------------------------------------------------------

scatter <- ggplot(---, aes(---, ---))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Blue", se = T, level=0.95)
  
## c - f/ run the simple linear regression model
## ------------------------------------------------------------------------
---


## g. Testing the assumptions:
##-----------------------------

## linearity, normality and homoscedasticity of the residuals

opar <- par(mfrow=c(2,2)) # make a copy of current settings
plot(---)
par(opar) # restore original settings

# plot studentized residuals vs fitted values
plot(---)
abline(h=c(-2,2), col=c("blue", "blue"))
abline(h=c(-3,3), col=c("red", "red"))

## h. choose XVAR as the dep var
---

# Task 2
##############

