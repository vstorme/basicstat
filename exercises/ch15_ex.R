setwd("C:/MYCOURSES/FIELD_R/DATA")

library(car)
library(clinfun)
library(ggplot2)
library(pgirmess)
library(mosaic)
library(tidyr)

# Task 1
#############
# download from http://www.biomedcentral.com/1471-2105/7/85/additional
# additional file 9

setwd("C:/MYCOURSES/FIELD_R/DATA")

qPCR = read.delim("1471-2105-7-85-S9.txt", header=FALSE)
colnames(qPCR) = c("treatment", "ddCt")
qPCR

# exploratory analysis
by(qPCR$ddCt, qPCR$treatment, favstats)

boxplot(ddCt ~ treatment, data=qPCR)

histogram(~ ddCt | treatment, data=qPCR)

leveneTest(ddCt ~ treatment, data=qPCR, center=median)

# Wilcoxon rank sum test

wilcox.test(ddCt ~ treatment, data = qPCR)


# Task 2
################

darkLord<-read.delim("DarkLord.dat", header = TRUE)
head(darkLord)
dim(darkLord)

## exploratory analysis

diff = darkLord$message - darkLord$nomessag
favstats(diff)

histogram(~ diff, data=darkLord)

## Wilcoxon signed-rank test:
wilcox.test(darkLord$message, darkLord$nomessag,  paired = TRUE, correct= FALSE)


# Task 3
############

eastendersData <- read.delim("Eastenders.dat", header = TRUE)
eastendersData

## exploratory
dfapply(eastendersData, favstats, select = is.numeric)

## from wide to long
eastendersData$personID = row(eastendersData[1])
quarrels = gather(eastendersData, movie, value = quarrel, eastend:whales, -personID)
head(quarrels)

bwplot(~ quarrel | movie, data=quarrels)

bar <- ggplot(quarrels, aes(movie, quarrel))
bar + stat_summary(fun.y = mean, geom = "bar") + 
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange") 

histogram (~ quarrel | movie, data=quarrels)

## analysis
## friedman.test(y, groups, blocks)
friedman.test(quarrels$quarrel, quarrels$movie, quarrels$personID)

## friedman.test(y ~ groups | blocks, data=)
friedman.test(quarrel ~ movie | personID, data=quarrels)

## If y is a matrix, groups and blocks are obtained from the column and row indices, respectively
friedman.test(as.matrix(eastendersData[,-4]))

## posthoc
friedmanmc(as.matrix(eastendersData[,-4]))

#obs.dif is the difference in ranks

# Task 4
#################
poplar<-read.csv("poplar.csv", header = TRUE)
head(poplar)

levels(poplar$treatment)

## Descriptives:
by(poplar$weight, poplar$treatment, favstats)

bwplot(~ weight | treatment, data=poplar)

bar <- ggplot(poplar, aes(treatment, weight))
bar + stat_summary(fun.y = mean, geom = "bar") + 
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange") 

histogram (~ weight | treatment, data = poplar)

## Kruskal-Wallis test

kruskal.test(weight ~ treatment, data = poplar)

poplar$Ranks<-rank(poplar$weight)
by(poplar$Ranks, poplar$treatment, mean)

pairwise.wilcox.test(poplar$weight, poplar$treatment)

## library(pgirmess)
kruskalmc(weight ~ treatment, data = poplar)

# comparison versus a control
# poplar$treatment = relevel(poplar$treatment, ref="no")

levels(poplar$treatment)
kruskalmc(weight ~ treatment, data = poplar, cont = 'two-tailed')

# Task 5
######################

winery <-read.csv("winery.csv", header = TRUE)
winery

## Descriptives:
by(winery$score, winery$wine, favstats)

## since the outcome variable is a score from 1 to 10, we do not expect normality

bar <- ggplot(winery, aes(wine, score))
bar + stat_summary(fun.y = mean, geom = "bar") + 
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange") 

bwplot(~ score |wine, data=winery)

beanplot(score ~ wine, data=winery)

histogram(~ score | wine, data=winery)

## analysis

friedman.test(score ~ wine | subject, data=winery)

friedman.test(y=winery$score, groups= winery$wine , block=winery$subject)


