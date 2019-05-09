setwd("C:/Users/vesto/Documents/myCOURSES/basic-stat-R/april2019/data")

library(ggplot2)
library(granova)
library(car)
library(multcomp)
library(mosaic)
library(lsmeans)


# Task 1
###########

## Load in the data:
superData = read.delim("Superhero.dat", header = TRUE)
head(superData)

### Set hero to be a factor:
superData$hero <- factor(superData$hero, levels = c(1:4), labels = c("Spiderman","Superman", "Hulk", "Ninja Turtle"))


############
## Task 2 ##
############

## Load in the data:
peas = read.csv("peas.csv", sep=";")
head(peas)













## Advanced:  mixed models allow different variances by group
##############################################################

library(nlme)
mod = gls(len ~ treat, data=peas, weights = varIdent(form = ~1|treat), 
          method="ML")

mod2 = gls(len ~ treat, data=peas, method="ML")
anova(mod,mod2)

summary(mod)
Anova(mod)

## least-squares means
lsm = emmeans(mod, ~ treat)
lsm

## Post Hoc tests with the lsmeans package:
con = contrast(lsm, method="trt.vs.ctrl", ref=5, CIs=TRUE)
con
confint(con)

## changing nr of digits, does not affect p-value however
getOption("digits")
options(digits=10)
emm_options(opt.digits = FALSE) # when FALSE, system settings are adopted

