#' ---
#' title: "simple linear regression"
#' author: "Veronique Storme"
#' date: "16/04/2019"
#' ---
#' 
#' R Code for Chapter 7 of:
#' 
#' Field, A. P. & Miles, J. N. V. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. London Sage
#' 
#' (c) 2011 Andy P. Field & Jeremy N. V. Miles
#' 
#' with adaptations from Veronique Storme

#' Set the working directory
#' 
## ------------------------------------------------------------------------
setwd("C:/Users/vesto/Documents/myCOURSES/basic-stat-R/april2019/data")

#' 
#' Install and Load Packages
#' 
## ----libraries, results = "hide"-----------------------------------------
#install.packages("car")
#install.packages("ggplot2")
#install.packages("mosaic")
#install.packages("gridExtra")
#install.packages("lmtest")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("beanplot")
#install.packages("MASS")

library(car)
library(ggplot2)
library(mosaic)
library(beanplot)
library(gridExtra)
library(lmtest)
library(plyr)
library(dplyr)
library(MASS)


## simple linear regression with a continuous predictor

## > example : the album1 data

## colornames on http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
## ------------------------------------------------------------------------

album1 <- read.delim("AlbumSales1.dat", header = TRUE)
head(album1)

## descriptive statistics
## ------------------------------------------------------------------------

summarise(album1, Nobs = sum(!is.na(adverts)),
          Nmiss = sum(is.na(adverts)),
          mean = mean(adverts, na.rm=TRUE), 
          sd = sd(adverts, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

summarise(album1, Nobs = sum(!is.na(sales)),
          Nmiss = sum(is.na(sales)),
          mean = mean(sales, na.rm=TRUE), 
          sd = sd(sales, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

## graphical display
## ------------------------------------------------------------------------

scatter <- ggplot(album1, aes(adverts, sales))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Blue", se = T, level=0.95) +
  labs(x = "adverts", y = "sales (1000 BP)") 

## run the simple linear regression model
## ------------------------------------------------------------------------

albumSales.1 <- lm(sales ~ adverts, data = album1)
summary(albumSales.1)

## Testing the assumptions:
##-----------------------------

## linearity, normality and homoscedasticity of the residuals

opar <- par(mfrow=c(2,2)) # make a copy of current settings
plot(albumSales.1)
par(opar) # restore original settings

# plot studentized residuals vs fitted values
plot(albumSales.1$fitted.values, studres(albumSales.1), ylim=c(-3, 6))
abline(h=c(-2,2), col=c("blue", "blue"))
abline(h=c(-3,3), col=c("red", "red"))

## relation Pearson correlation coeff and R2
pcor = cor(album1$sales,album1$adverts)
pcor

pcor^2

## elements of the lm object
names(albumSales.1)
albumSales.1$coefficients

## elements of the summary of a lm object
names(summary(albumSales.1))
summary(albumSales.1)$fstatistic

# The F-statistics tells you if the model fits the data better than the mean.

## List the functions that can be applied on a certain class:
methods(class="lm")
rstudent(albumSales.1)[1:5]

## confidence limits for the mean
predict(albumSales.1, interval="conf")[1:5,]

# confidence limits for the mean response of observations with a given (observed) covariate values
predict(albumSales.1, list(adverts = 2000),interval="conf")

# prediction limits for a new observation
# prediction limits for new observations (be careful with extrapolation)
predict(albumSales.1, list(adverts = 1750),interval="pred")

# confidence interval for parameter estimates
confint(albumSales.1)

## advanced: outliers and influential observations (self-study)
##-------------------------------------------------------------

#### residual and influential plots with the stats library

# residual: difference between the predicted value and the observed value
#
# rstandard: standardised residuals
# rstudent: studentised residuals

rstandard(albumSales.1)[1:5]
plot(albumSales.1$fitted.values,rstandard(albumSales.1))
rstudent(albumSales.1)[1:5]
plot(albumSales.1$fitted.values,rstudent(albumSales.1))

#' 
#' dfbetas
#' 
## ------------------------------------------------------------------------
dfbetas = dfbetas(albumSales.1)
plot(dfbetas[,2],ylab = "adverts" )

#' 
#' dffit
#' 
## ------------------------------------------------------------------------
plot(dffits(albumSales.1))

#' 
#' covratio: change in determinant of the cov matrix of the estimates by deleting the ith observation
#' 
## ------------------------------------------------------------------------
plot(covratio(albumSales.1))

#' 
#' leverage : measure of how far an independent variable deviates from its mean.
#' High leverage observations can have (but not necessarily) a great amount of effect on the estimate of regr coeff
#' 
## ------------------------------------------------------------------------
plot(hatvalues(albumSales.1))

#' 
#' cook's distance: combines the information of leverage and residual
#' 
## ------------------------------------------------------------------------
plot(cooks.distance(albumSales.1))

#' 
#' plot(lmobject): for more info: type "?plot.lm"
#' 
## ------------------------------------------------------------------------
plot(albumSales.1, which=1)
plot(albumSales.1, which=2)
plot(albumSales.1, which=3)
plot(albumSales.1, which=4)
plot(albumSales.1, which=5)
plot(albumSales.1, which=6)

#' 
#' plot 1: raw residuals vs fitted
#' plot 2: qqplot of standardised residuals
#' plot 3: sqrt(standardised residuals) vs fitted
#' plot 4: Cook's distance
#' plot 5: standardised residuals vs leverage
#' plot 6: Cook's distance vs leverage
#' 
#' #### residual plots with the car library
#' 
#' residual plots 
#' performs also a curvature test, when residuals are plotted against fitted, 
#' this test is called Tukey Test for non-additivity
#'  

residualPlots(albumSales.1)

#' 
#' normality test (id.n ho many points should be labelled)
#' 
## ------------------------------------------------------------------------
qqPlot(albumSales.1,id.n = 3)

#' 
#' influential data 
#' 

influenceIndexPlot(albumSales.1, id.n=3)

#' 
#' "bubble" plot of Studentized residuals by hat values, with the areas of the circles representing the observations proportional to Cook's distances
#'  

influencePlot(albumSales.1, id.n=3)


## heteroscedasticity tests
##---------------------------

#' (score) test of the hypothesis of constant error variance against the alternative 
#' that the error variance changes with the level of the response (fitted values)

#library(car)
ncvTest(albumSales.1)

#' Breusch-Pagan test : regressing the squared residuals on the independent variables
#' test statistics BP = N*R^2 of this model follows a Chi-square distribution with degrees of freedom equal to the number of regressors

#library(lmtest)
bptest(albumSales.1, studentize = FALSE)

#' If one ignores heteroskedasticity and uses OLS estimators to estimate β’s,
#' the properties of unbiasedness and consistency still are not violated. But
#' OLS estimate is no more efficient. It is possible to find an alternative
#' unbiased linear estimate that has a lower variance than OLS estimate.

#### independence of residuals

#' Durban-Watson test (for time series data) with car library (bootstrapped p-values)

#library(car)
durbinWatsonTest(albumSales.1)

#' Durban-Watson test (for time series data) with lmtest library (p-values with "pan" algorithm, same value for test-statistic)

#library(lmtest)
dwtest(albumSales.1)

## residual plots using the gridExtra and mosaic library
##------------------------------------------------------

mplot(albumSales.1, which=1:7, system="lattice", multiplot=TRUE, ncol=2)

mplot(albumSales.1, which=1:7, system="ggplot2", multiplot=TRUE, ncol=2)

mplot(albumSales.1, which=7, system="lattice")
mplot(albumSales.1, which=7, system="ggplot2")


## R tends to give values to too many decimal places, you can round these values to 2 decimals.

round(rstandard(albumSales.1), 2)[1:10]