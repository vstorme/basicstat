#' ---
#' title: "one-way anova"
#' author: "Veronique Storme"
#' date: "17/04/2019"
#' ---
#' 
#' R Code for Chapter 10 of:
#' 
#' Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#' 
#' (c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#' 
#' and from introductory statistics with R (Peter Dalgaard)
#' 
#' with adaptations from Veronique Storme
#' 

## Set the working directory
## ------------------------------------------------------------------------

setwd("C:/Users/vesto/Documents/myCOURSES/basic-stat-R/april2019/data")

## Install and Load Packages

#install.packages("ggplot2")
#install.packages("granova")
#install.packages("car")
#install.packages("mosaic")
#install.packages("emmeans")
#install.packages("multcomp")

library(ggplot2)
library(granova)
library(car)
library(mosaic)
library(emmeans)
library(multcomp)
library(plyr)

## enter the viagra data
## ------------------------------------------------------------------------

id <- (1:15)
libido <- c(3,2,1,1,4,5,2,4,2,3,7,4,5,3,6)
dose <- gl(n=3,k=5, labels = c("Placebo", "Low Dose", "High Dose"))
viagraData <- data.frame(dose, libido)
head(viagraData)

## descriptives
## ------------------------------------------------------------------------

levels(viagraData$dose)

table(viagraData$dose)

by(viagraData$libido, viagraData$dose, favstats)

ddply(viagraData, .(dose), summarise, 
      Nobs = sum(!is.na(libido)),
      Nmiss = sum(is.na(libido)),
      mean = mean(libido, na.rm=TRUE), 
      sd = sd(libido, na.rm=TRUE),
      se   = sd/sqrt(Nobs),
      t = qt(0.975, Nobs-1),
      lower = mean - t*se, 
      upper = mean + t*se)

## graphical display
##------------------

### boxplots

plot <- ggplot(viagraData, aes(dose, libido))
plot + geom_boxplot(fill="slateblue") + 
  xlab("viagra dose") + ylab("libido") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  geom_jitter() 

### barcharts with 95% CI (requires ggplot2 and Hmisc library)

plot + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2) + 
  stat_summary(fun.y = mean, geom = "bar", fill = "slateblue", colour = "Black") +   
  labs(x = "viagra dose", y = "libido", title="libido + 95%CI ") 

### lineplot with 95% CL

plot +
  stat_summary(fun.y = mean, geom = "point", size = 3, colour = "#FF6633") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 0.75, colour = "#990000") + 
  stat_summary(fun.y = mean, geom = "line", size = 1, aes(group=1), colour = "#FF6633") + 
  labs(x = "Dose of Viagra", y = "Mean Libido", title="mean libido + 95%CI ")

## linear model
##---------------------

model2 = lm(libido ~ dose,data=viagraData)
summary(model2)

#### Note that R by default uses dummy coding also called treatment coding

contr.treatment(levels(viagraData$dose))

## one-way anova
##-----------------

### Brown-Forsythe test

leveneTest(viagraData$libido, viagraData$dose, center = median)

### anova

viagraModel.lm <- lm(libido ~ dose, data = viagraData)
summary(viagraModel.lm)
Anova(viagraModel.lm)

### verify the assumptions

opar <- par(mfrow=c(2,2)) # make a copy of current settings
plot(viagraModel.lm)
par(opar) # restore original settings

### get the least-square means estimates (= predicted group averages) and 95% CI

lsm = emmeans(viagraModel.lm, ~ dose)
lsm
plot(lsm)

### post-hoc tests

#### compare libido between each dose and placebo
#### p-value adjustment with Dunnett's method

C1 <- contrast(lsm, method="trt.vs.ctrl", ref=1, CIs=TRUE)
C1

plot(C1)
confint(C1)

#### all-pairwise comparisons
#### Tukey pvalue adjustment

C2 <- contrast(lsm, method="pairwise", adjust="tukey", CIs=TRUE)
C2

plot(C2)
confint(C2) 

#### this representation is better than a bar chart with stars
#### if 0 is not in the 95% CI, then the difference is statistically signifant 
#### at the 5% significance level

#### Test all pairwise comparisons by constructing linear hypotheses

L2 = rbind(c(0,1,0),c(0,0,1),c(0,-1, 1))
rownames(L2) = c("low-placebo","high-placebo", "high-low")
summary(glht(viagraModel.lm, linfct = L2))

### with the emmeans package, contrasts are set-up in function of the lsmeans
### get the order of the lsmeans:
lsm

# low-placebo: -1 1 0
# high - placebo: -1 0 1

C3 <- contrast(lsm, 
         list(c1=c(-1, 1, 0), c2=c(-1, 0, 1)), adjust="dunnett",
         by = NULL)

C3
confint(C3)

### Many vignettes available for the emmeans package on 
### https://cran.r-project.org/web/packages/emmeans/index.html
