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

## Descriptives:

### Set hero to be a factor:
superData$hero <- factor(superData$hero, levels = c(1:4), labels = c("Spiderman","Superman", "Hulk", "Ninja Turtle"))

levels(superData$hero)

table(superData$hero)

ddply(superData, .(hero), summarise, 
      Nobs = sum(!is.na(injury)),
      Nmiss = sum(is.na(injury)),
      mean = mean(injury, na.rm=TRUE), 
      sd = sd(injury, na.rm=TRUE),
      se   = sd/sqrt(Nobs),
      t = qt(0.975, Nobs-1),
      lower = mean - t*se, 
      upper = mean + t*se)

## graphical display
### bar chart
bar <- ggplot(superData, aes(hero, injury))

bar + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2) +
      stat_summary(fun.y = mean, geom = "bar")

### box plots
bar + geom_boxplot(fill="slateblue") + 
      xlab("hero") + ylab("injury") +
      stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
      geom_jitter() 


## Run the one-way ANOVA:
superModel <- lm(injury ~ hero, data = superData)
Anova(superModel,type="III")

#calculating corresponding p-value with pf function
#pf(8.317,df1=3, df=26,lower.tail=FALSE)

## diagnostics
opar = par()
par(mfrow=c(2,2))
plot(superModel)
par(opar)

### Test for normality
shapiro.test(superModel$resid)

## Test homogeneity of variance
leveneTest(superData$injury, superData$hero, center = median)

## least-squares means
lsm = emmeans(superModel, ~ hero)
lsm

## Post Hoc tests with the lsmeans package:
con = emmeans::contrast(lsm, method="pairwise", adjust="tukey")
con
confint(con)

############
## Task 2 ##
############

## Load in the data:
peas = read.csv("peas.csv", sep=";")
head(peas)

## Descriptives:

levels(peas$treat)

table(peas$treat)

ddply(peas, .(treat), summarise, 
      Nobs = sum(!is.na(len)),
      Nmiss = sum(is.na(len)),
      mean = mean(len, na.rm=TRUE), 
      sd = sd(len, na.rm=TRUE),
      se   = sd/sqrt(Nobs),
      t = qt(0.975, Nobs-1),
      lower = mean - t*se, 
      upper = mean + t*se)

## graphical display
### bar chart
bar <- ggplot(peas, aes(treat, len))

bar + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2) +
  stat_summary(fun.y = mean, geom = "bar")

### box plots
bar + geom_boxplot(fill="slateblue") + 
  xlab("treatment") + ylab("length") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  geom_jitter() 

## Run the one-way ANOVA:
mod.lm <- lm(len ~ treat, data = peas)
Anova(mod.lm,type="III")

## diagnostics
opar = par()
par(mfrow=c(2,2))
plot(mod.lm)
par(opar)

### Test for normality
shapiro.test(mod.lm$resid)

## Test homogeneity of variance
leveneTest(peas$len, peas$treat, center = median)

## least-squares means
lsm = emmeans(mod.lm, ~ treat)
lsm

## Post Hoc tests with the lsmeans package:
con.lm = contrast(lsm, method="trt.vs.ctrl", ref=5, CIs=TRUE)
con.lm
confint(con.lm)

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

