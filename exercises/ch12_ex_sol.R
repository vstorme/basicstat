setwd("C:/Users/vesto/Documents/myCOURSES/basic-stat-R/april2019/data")

library(car)
library(ggplot2)
library(mosaic)
library(plyr)
library(dplyr)
library(emmeans)

##########
# Task 1 #
##########

library(HSAUR3)

data(weightgain)
head(weightgain)

?weightgain

## descriptives

levels(weightgain$source)
levels(weightgain$type)

table(weightgain$source, weightgain$type)

summ <- ddply(weightgain, .(source, type), summarise, 
              Nobs = sum(!is.na(weightgain)),
              Nmiss = sum(is.na(weightgain)),
              mean = mean(weightgain, na.rm=TRUE), 
              sd = sd(weightgain, na.rm=TRUE),
              se   = sd/sqrt(Nobs),
              t = qt(0.975, Nobs-1),
              lower = mean - t*se, 
              upper = mean + t*se)

summ

## graphical analysis
### boxplots 

plot <- ggplot(weightgain, aes(source, weightgain))

plot + geom_boxplot() + 
  facet_wrap(~type) + 
  labs(x = "protein source", y = "amount of protein")

### bar charts 
plot + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + 
  stat_summary(fun.y = mean, geom = "bar", position="dodge") + 
  facet_wrap(~type) +
  labs(x = "protein source", y = "amount of protein")

### interaction plot
summ
ggplot(summ, aes(x=source, y=mean, group=type, color=type)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1, 
                position=position_dodge(0.05)) +
  geom_line() + 
  geom_point() +
  labs(x = "protein source", y = "amount of protein") +
  scale_color_brewer(palette="Paired") 

## analysis

fit.int <- lm(weightgain ~ source*type, data = weightgain)
summary(fit.int)
Anova(fit.int, type="III")


fit.main = lm(weightgain ~ source + type, data = weightgain)
Anova(fit.main, type="III")

anova(fit.main,fit.int)

# borderline significant

opar = par()
par(mfrow=c(2,2))
plot(fit.int)
par(opar)

lsm.int = emmeans(fit.int,  ~ type * source)
warnings()

lsm.int
# beefH beefL cerealH cerealL

# all-pairwise comparisons
C1 = contrast(lsm.int, method="pairwise", adjust="tukey")

plot(C1)
confint(C1)

# interaction effect
# H0: beefH-beefL = cerealH - cerealL
contrast(lsm.int, 
         list(c3=c(1, -1, -1, 1)), 
         adjust="none",
         by = NULL)


# Task 2
###############

# This example uses data from Kutner (1974, p. 98) to illustrate a two-way analysis of variance. The original data source is Afifi and Azen (1972, p. 166). 
# Afifi, A. A. and Azen, S. P. (1972), Statistical Analysis: A Computer-Oriented Approach, New York: Academic Press.
# Neter, J., Wasserman, W., and Kutner, M. H. (1990), Applied Linear Statistical Models, Third Edition, Homewood, IL: Irwin.
# 4 drugs were each tested on 3 diseases 

drugs2 <- read.delim("drugs2.txt", header = TRUE)
drugs2
drugs2$drug = factor(drugs2$drug)
drugs2$disease = factor(drugs2$disease)

## descriptives
replications(drugs2)

levels(drugs2$drug)
levels(drugs2$disease)

table(drugs2$drug, drugs2$disease)

summ <- ddply(drugs2, .(drug, disease), summarise, 
              Nobs = sum(!is.na(y)),
              Nmiss = sum(is.na(y)),
              mean = mean(y, na.rm=TRUE), 
              sd = sd(y, na.rm=TRUE),
              se   = sd/sqrt(Nobs),
              t = qt(0.975, Nobs-1),
              lower = mean - t*se, 
              upper = mean + t*se)

summ

## graphical analysis
# the interaction plot does not work when there are missing values
## graphical analysis
### boxplots 

plot <- ggplot(drugs2, aes(drug, y))

plot + geom_boxplot() + 
  facet_wrap(~disease) + 
  labs(x = "drug", y = "systolic blood pressure")

### bar charts 
plot + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + 
  stat_summary(fun.y = mean, geom = "bar", position="dodge") + 
  facet_wrap(~disease) +
  labs(x = "drug", y = "systolic blood pressure")

# solution to avoid lower limit
ggplot(summ) +
  geom_bar( aes(x=drug, y=mean), stat="identity", fill="skyblue", colour="black") +
  geom_errorbar( aes(x=drug, ymin=mean, ymax=upper), width=.1, 
                 position=position_dodge(0.05)) +
  facet_wrap(~disease) +
  labs(x = "drug", y = "systolic blood pressure")

### interaction plot
summ
ggplot(summ, aes(x=drug, y=mean, group=disease, color=disease)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1, 
                position=position_dodge(0.1)) +
  geom_line() + 
  geom_point() +
  labs(x = "drug", y = "systolic blood pressure") +
  scale_color_brewer(palette="Paired") 

## analysis of variance
fit.full = lm(y ~ drug*disease, data=drugs2)
Anova(fit.full, type="III")

fit.red = lm(y ~ drug + disease, data=drugs2)
anova(fit.red, fit.full)
Anova(fit.red, type="III")

fit.red2 = lm(y ~ drug , data=drugs2)
Anova(fit.red2, type="III")

# diagnostics
opar = par()
par(mfrow=c(2,2))
plot(fit.red2)
par(opar)

#Post Hoc tests with the lsmeans package:

lsm = emmeans(fit.red2, ~ drug)
lsm
C <- contrast(lsm, method="pairwise", adjust="tukey")
C
plot(C)
confint(C)



# interpretation
# There is a significant difference between the effect of drug 1 and drug 3. 
# In the abscence of a sign interaction, this difference is the same for all 4 disease types


# task 3
##############

consumption <- c(709,679,699,592,538,476,508,505,539, 657,594,677)
gender <- gl(2, 6, labels = c("M", "F"))
fat <- gl(n=2,k=3,length=12,labels=c("Fresh","Rancid"))
rats = data.frame(consumption, gender, fat)
head(rats)

## descriptives
levels(rats$gender)
levels(rats$fat)

table(rats$gender, rats$fat)

summ <- ddply(rats, .(gender, fat), summarise, 
              Nobs = sum(!is.na(consumption)),
              Nmiss = sum(is.na(consumption)),
              mean = mean(consumption, na.rm=TRUE), 
              sd = sd(consumption, na.rm=TRUE),
              se   = sd/sqrt(Nobs),
              t = qt(0.975, Nobs-1),
              lower = mean - t*se, 
              upper = mean + t*se)

summ

## graphical analysis
### boxplots 

plot <- ggplot(rats, aes(gender, consumption))

plot + geom_boxplot() + 
  facet_wrap(~fat) + 
  labs(x = "gender", y = "consumption (g)")

### bar charts 
plot + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + 
  stat_summary(fun.y = mean, geom = "bar", position="dodge") + 
  facet_wrap(~fat) +
  labs(x = "gender", y = "consumption (g)")

### interaction plot
summ
ggplot(summ, aes(x=gender, y=mean, group=fat, color=fat)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1, 
                position=position_dodge(0.05)) +
  geom_line() + 
  geom_point() +
  labs(x = "gender", y = "consumption (g)") +
  scale_color_brewer(palette="Paired") 

# anova
lm.full=lm(consumption ~ gender + fat + gender:fat, data=rats)
Anova(lm.full, type="III")
summary(lm.full)
confint(lm.full)

# There is a significant interaction indicating that the effect of fat depends on the gender
# the interaction effect follows directly from b3
# the average estimated difference in consumption between fresh fat and rancid fat between M and F
# is 286g [184g ; 387g]. Male rats prefer fresh fat.

# diagnostics
par(mfrow=c(2,2))
plot(lm.full)
par(opar)

#lsmeans

lsm.rats = emmeans(lm.full,  ~ gender * fat)
lsm.rats

# posthoc tests
C1 = contrast(lsm.rats, method="pairwise", adjust="tukey")
C1

plot(C1)
confint(C1)

# when the interest is in the different fat intake upon gender (ie the interaction) then we set up a user defined contrast:
# eg H0: (male, fresh - male, rancid) - (female, fresh - female, rancid) = 0
# (male, fresh - male, rancid - female, fresh + female, rancid = 0
lsm.rats
contrast(lsm.rats, 
         list(c1=c(1, -1, -1, 1)), 
         by = NULL)
# this is equal to the regr coeff b3




