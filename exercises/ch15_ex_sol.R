setwd("C:/Users/vesto/Documents/myCOURSES/basic-stat-R/april2019/data")

library(car)
library(clinfun)
library(ggplot2)
library(pgirmess)
library(mosaic)
library(tidyr)
library(gridExtra)
library(beanplot)

# Task 1
#############
# download from http://www.biomedcentral.com/1471-2105/7/85/additional
# additional file 9

qPCR = read.delim("1471-2105-7-85-S9.txt", header=FALSE)
colnames(qPCR) = c("treatment", "ddCt")
qPCR

## descriptive statistics
##-------------------------

levels(qPCR$treatment)

table(qPCR$treatment)

ddply(qPCR, .(treatment), summarise, 
      Nobs = sum(!is.na(ddCt)),
      Nmiss = sum(is.na(ddCt)),
      mean = mean(ddCt, na.rm=TRUE), 
      median = median(ddCt, na.rm=TRUE),
      sd = sd(ddCt, na.rm=TRUE),
      se   = sd/sqrt(Nobs),
      t = qt(0.975, Nobs-1),
      lower = mean - t*se, 
      upper = mean + t*se)


### histogram

ggplot(qPCR,aes(x=ddCt)) + 
  geom_histogram(data=subset(qPCR,treatment == 'Control'),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(qPCR,treatment == 'Treatment'),fill = "blue", alpha = 0.2)

### formal normality test

by(qPCR$ddCt, qPCR$treatment, shapiro.test)

### boxplots

plot <- ggplot(qPCR, aes(treatment, ddCt))

plot + geom_boxplot(fill="slateblue") + 
  labs(x = "treatment", y = "ddCt") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  geom_jitter() +
  theme(legend.position="none", axis.title.x=element_blank())

### test for equality of variance

leveneTest(ddCt ~ treatment, data=qPCR, center="median")

# Wilcoxon rank sum test

wilcox.test(ddCt ~ treatment, data = qPCR)


# Task 2
################

darkLord<-read.delim("DarkLord.dat", header = TRUE)
head(darkLord)
dim(darkLord)


## descriptive statistics
##-------------------------

summarise(darkLord, 
          Nobs = sum(!is.na(message)),
          Nmiss = sum(is.na(message)),
          mean = mean(message, na.rm=TRUE), 
          median = median(message, na.rm=TRUE), 
          sd = sd(message, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

summarise(darkLord, 
          Nobs = sum(!is.na(nomessag)),
          Nmiss = sum(is.na(nomessag)),
          mean = mean(nomessag, na.rm=TRUE), 
          median = median(nomessag, na.rm=TRUE), 
          sd = sd(message, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

## graphical display
##--------------------

### boxplots

plot1 <- ggplot(darkLord, aes(x = "", y = message)) +
  geom_boxplot(fill="slateblue") + 
  ylim(0, 25) +
  labs(x= "message",y = "nr of sacrificed goats") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme(legend.position="none")

plot2 <- ggplot(darkLord, aes(x = "", y = nomessag)) +
  geom_boxplot(fill="slateblue") + 
  ylim(0, 25) +
  labs(x= "nomessage",y = "nr of sacrificed goats") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme(legend.position="none")

grid.arrange(plot1, plot2, ncol=2)

### normality test on diffs

diff = darkLord$message - darkLord$nomessag
shapiro.test(diff)

beanplot(diff)

## Wilcoxon signed-rank test:
wilcox.test(darkLord$message, darkLord$nomessag,  paired = TRUE, correct= FALSE)


# Task 3
############

eastendersData <- read.delim("Eastenders.dat", header = TRUE)
eastendersData

## descriptives
summarise(eastendersData, 
          Nobs = sum(!is.na(eastend)),
          Nmiss = sum(is.na(eastend)),
          mean = mean(eastend, na.rm=TRUE), 
          median = median(eastend, na.rm=TRUE), 
          sd = sd(eastend, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

summarise(eastendersData, 
          Nobs = sum(!is.na(friends)),
          Nmiss = sum(is.na(friends)),
          mean = mean(friends, na.rm=TRUE), 
          median = median(friends, na.rm=TRUE), 
          sd = sd(friends, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

summarise(eastendersData, 
          Nobs = sum(!is.na(whales)),
          Nmiss = sum(is.na(whales)),
          mean = mean(whales, na.rm=TRUE), 
          median = median(whales, na.rm=TRUE), 
          sd = sd(whales, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)


## graphical display
##--------------------

### boxplots

plot1 <- ggplot(eastendersData, aes(x = "", y = eastend)) +
  geom_boxplot(fill="slateblue") + 
  ylim(0, 20) +
  labs(x= "eastend",y = "nr of arguments") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme(legend.position="none")

plot2 <- ggplot(eastendersData, aes(x = "", y = friends)) +
  geom_boxplot(fill="slateblue") + 
  ylim(0, 20) +
  labs(x= "friends",y = "nr of arguments") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme(legend.position="none")

plot3 <- ggplot(eastendersData, aes(x = "", y = whales)) +
  geom_boxplot(fill="slateblue") + 
  ylim(0, 20) +
  labs(x= "whales",y = "nr of arguments") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme(legend.position="none")

grid.arrange(plot1, plot2, plot3, ncol=3)

opar=par(mfrow=c(1,3))
beanplot(eastendersData$eastend, log="", ylim=c(-5,20))
beanplot(eastendersData$friends, log="", ylim=c(-5,20))
beanplot(eastendersData$whales, log="",ylim=c(-5,20))
par(opar)

## If y is a matrix, groups and blocks are obtained from the column and row indices, respectively
friedman.test(as.matrix(eastendersData))

## posthoc
friedmanmc(as.matrix(eastendersData))

#obs.dif is the difference in ranks

# Task 4
#################
poplar<-read.csv("poplar.csv", header = TRUE)
head(poplar)

levels(poplar$treatment)
table(poplar$treatment)

## Descriptives:
summ <- ddply(poplar, .(treatment), summarise, 
      Nobs = sum(!is.na(weight)),
      Nmiss = sum(is.na(weight)),
      mean = mean(weight, na.rm=TRUE), 
      sd = sd(weight, na.rm=TRUE),
      se   = sd/sqrt(Nobs),
      t = qt(0.975, Nobs-1),
      lower = mean - t*se, 
      upper = mean + t*se)


## graphical display
##------------------

### boxplots

plot <- ggplot(poplar, aes(treatment, weight))
plot + geom_boxplot(fill="slateblue") + 
  xlab("treatment") + ylab("weight") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  geom_jitter() 

### barcharts with 95% CI (requires ggplot2 and Hmisc library)

ggplot(summ) +
  geom_bar( aes(x=treatment, y=mean), stat="identity", fill="skyblue", colour="black") +
  geom_errorbar( aes(x=treatment, ymin=mean, ymax=upper), width=.1, 
                 position=position_dodge(0.05)) +
  labs(x = "treatment", y = "weight", title="weight + 95%CI ") 

### lineplot with 95% CL

plot +
  stat_summary(fun.y = mean, geom = "point", size = 3, colour = "#FF6633") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 0.75, colour = "#990000") + 
  stat_summary(fun.y = mean, geom = "line", size = 1, aes(group=1), colour = "#FF6633") + 
  labs(x = "treatment", y = "weight", title="weight + 95%CI ") 

## linear model
fit.lm <- lm(weight ~ treatment, data = poplar)
Anova(fit.lm, type="III")

opar=par(mfrow=c(2,2))
plot(fit.lm)
par(opar)

## Kruskal-Wallis test

kruskal.test(weight ~ treatment, data = poplar)

poplar$Ranks<-rank(poplar$weight)
by(poplar$Ranks, poplar$treatment, mean)

pairwise.wilcox.test(poplar$weight, poplar$treatment)

## library(pgirmess)
kruskalmc(weight ~ treatment, data = poplar)

# comparison versus a control
poplar$treatment = relevel(poplar$treatment, ref="no")

levels(poplar$treatment)
kruskalmc(weight ~ treatment, data = poplar, cont = 'two-tailed')

# Task 5
######################

winery <-read.csv("winery.csv", header = TRUE)
winery

## Descriptives:
summ <- ddply(winery, .(wine), summarise, 
              Nobs = sum(!is.na(score)),
              Nmiss = sum(is.na(score)),
              mean = mean(score, na.rm=TRUE), 
              median = median(score, na.rm=TRUE), 
              sd = sd(score, na.rm=TRUE),
              se   = sd/sqrt(Nobs),
              t = qt(0.975, Nobs-1),
              lower = mean - t*se, 
              upper = mean + t*se)

summ


# paired data in long format

## since the outcome variable is a score from 1 to 10, we do not expect normality

bar <- ggplot(winery, aes(wine, score))
bar + stat_summary(fun.y = mean, geom = "bar") + 
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange") 

beanplot(score ~ wine, data=winery)

## analysis

friedman.test(score ~ wine | subject, data=winery)

friedman.test(y=winery$score, groups= winery$wine , block=winery$subject)


