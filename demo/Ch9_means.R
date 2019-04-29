#' ---
#' title: "comparing means"
#' author: "Veronique Storme"
#' date: "16/04/2019"
#' ---

#' R Code for Chapter 9 of:

#' Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage

#' (c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field

#' and from introductory statistics with R (Peter Dalgaard)

#' with small adaptations from Veronique Storme

## Set the working directory
## ------------------------------------------------------------------------

setwd("C:/Users/vesto/Documents/myCOURSES/basic-stat-R/april2019/data")

## Install and Load Packages

#install.packages("ggplot2")
#install.packages("car")
#install.packages("mosaic")
#install.packages("plyr")
#install.packages("dplyr")

library(car)
library(ggplot2)
library(mosaic)
library(plyr)
library(dplyr)

#####################
# one sample t-test #
#####################

## the data
##--------------
# This is an example concerning daily energy intake in kJ for 11 women (Altman, 1991, p183)
# we wish to investigate whether the women's energy intake deviates systematically 
# from a recommended vaue of 7725kJ. 
# We assume that the data comes from a normal distribution

daily.intake = c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)

## descriptives
## ------------------------------------------------------------------------

energy <- data.frame(daily.intake)

summarise(energy, Nobs = sum(!is.na(daily.intake)),
          Nmiss = sum(is.na(daily.intake)),
          mean = mean(daily.intake, na.rm=TRUE), 
          sd = sd(daily.intake, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

## graphical display
##---------------------

### boxplot

plot <- ggplot(energy, aes(x = "", y = daily.intake))

plot + geom_boxplot(fill="slateblue") + 
  labs(x= "female",y = "daily kJ intake") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme(legend.position="none")

### add observations to the boxplot

plot + geom_boxplot(fill="slateblue") + 
  labs(x= "female",y = "daily kJ intake") +
  geom_jitter()

#### bean plot as alternative to boxplot

beanplot(daily.intake, col="#CAB2D6")

### barcharts with 95% CI (requires ggplot2 and Hmisc library)

plot3 <- ggplot(energy, aes(1, daily.intake))

plot3 + stat_summary(fun.y = mean, geom = "bar", fill = "slateblue", colour = "Black") +   
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2) + 
  labs(x= "female",y = "daily kJ intake") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

### order matters:

plot3 + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2) + 
  stat_summary(fun.y = mean, geom = "bar", fill = "slateblue", colour = "Black") +   
  labs(x= "female",y = "daily kJ intake") +
  geom_jitter() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

## testing the assumptions
##---------------------------

### histogram

plot2 <- ggplot(energy, aes(daily.intake)) 

plot2 + geom_histogram(binwidth = 200) + 
  geom_rug(sides="t") +
  labs(x = "daily kJ intake", y = "Frequency")

### formal normality test

shapiro.test(daily.intake)

## one-sample t-test
##--------------------

t.test(daily.intake, mu=7725, alternative="two.sided")

############################################
# two-sample t-test or independent t-test #
###########################################

spider <- read.delim("SpiderLong.dat", header = TRUE)
head(spider)

levels(spider$Group)

## descriptive statistics
##-------------------------

table(spider$Group)

favstats(~ Anxiety | Group, data=spider)

ddply(spider, .(Group), summarise, 
      Nobs = sum(!is.na(Anxiety)),
      Nmiss = sum(is.na(Anxiety)),
      mean = mean(Anxiety, na.rm=TRUE), 
      sd = sd(Anxiety, na.rm=TRUE),
      se   = sd/sqrt(Nobs),
      t = qt(0.975, Nobs-1),
      lower = mean - t*se, 
      upper = mean + t*se)

## graphical display
##--------------------

### barcharts with 95% CI (requires ggplot2 and Hmisc library)

plot4 <- ggplot(spider, aes(Group, Anxiety))

plot4 + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2) + 
  stat_summary(fun.y = mean, geom = "bar", fill = "slateblue", colour = "Black") +   
  labs(x = "Group", y = "Anxiety") +
  geom_jitter() +
  theme(legend.position="none", axis.title.x=element_blank())

## testing normality assumption
##-----------------------------

### histogram

ggplot(spider,aes(x=Anxiety)) + 
  geom_histogram(data=subset(spider,Group == 'Picture'),fill = "red", alpha = 0.2, binwidth=4) +
  geom_histogram(data=subset(spider,Group == 'Real Spider'),fill = "blue", alpha = 0.2, binwidth=4)

### formal normality test

by(spider$Anxiety, spider$Group, shapiro.test)

### boxplots

plot4 <- ggplot(spider, aes(Group, Anxiety))

plot4 + geom_boxplot(fill="slateblue") + 
  labs(x = "Group", y = "Anxiety") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  geom_jitter() +
  theme(legend.position="none", axis.title.x=element_blank())

### bean plot

beanplot(Anxiety ~ Group, data=spider, col="#CAB2D6")

#### normality assumption is fulfilled

#### testing for equal variance

#### var.test for the special case of comparing variances in two samples from normal distributions {stats}
#### bartlett.test for testing homogeneity of variances in more than two samples from normal distributions {stats}
#### Levene's test (package car) for comparison of variances {car}
##### The Levene test is less sensitive than the Bartlett test to departures from normality

leveneTest(Anxiety ~ Group,data = spider)


## t.test 
##-------------------

#### t.test(x, y = NULL, alternative = c("two.sided", "less", "greater"),
####      mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

t.test(Anxiety ~ Group, data = spider, var.equal=TRUE)

## Testing for statistical significant difference in Anxiey between the 2 groups
##------------------------------------------------------------------------------

### simple linear regression with a binary predictor

model = lm(Anxiety ~ Group, data = spider)
summary(model)

Anova(model)

#### Note that the p-value for the F-test is the same as the p-value for the slope
#### Testing the assumptions:

opar <- par(mfrow=c(2,2)) # make a copy of current settings
plot(model)
par(opar) # restore original settings

##### Observe that the pvalue for the slope is  the same as the p-value 
# from the t-test

#####################################
# dependent t-test or paired t-test #
#####################################
 
## the same persons were asked to look at a real spider and to look at a picture

spiderWide <- read.delim("SpiderWide.dat", header = TRUE)
head(spiderWide)

# from wide to long

df.long = gather(data=spiderWide, key=Group, value=Anxiety, picture:real, -id)
df.long$id = c(1:12,1:12)
df.long

# summary statistics are the same as before

# graphs are the same as before

## testing assumptions 
## normality of differences

spiderWide$diff <- spiderWide$picture - spiderWide$real

shapiro.test(spiderWide$diff)

ggplot(spiderWide, aes(diff)) + 
  theme(legend.position = "none") + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=7) + 
  labs(x="differences", y = "Density")

## Dependent t test

t.test(Anxiety ~ Group, data = df.long, paired=TRUE)

#### More on the equivalence of linear models on
#### https://lindeloev.github.io/tests-as-linear/