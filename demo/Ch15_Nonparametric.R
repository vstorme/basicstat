#' ---
#' title: "Nonparametric data analysis "
#' author: "Veronique Storme"
#' date: "16/04/2019"
#' ---
#' 
#' R Code for Chapter 15 of:
#'   
#' Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#' 
#' (c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#' 
#' with adaptations from Veronique Storme

## Set the working directory
## ------------------------------------------------------------------------

setwd("C:/Users/vesto/Documents/myCOURSES/basic-stat-R/april2019/data")

# Install and Load Packages

## install.packages("car")
## install.packages("clinfun")
## install.packages("ggplot2")
## install.packages("mosaic")
## install.packages("pgirmess")

library(car)
library(clinfun)
library(ggplot2)
library(pgirmess)
library(mosaic)

###############################
## Wilcoxon signed-rank test ##
###############################

## simulate some data:

sim <- runif(n=20,min=0, max=10)
hist(sim)
mean(sim)

## test H0: mu=3, HA: mu neq 3

## non parametric equivalent of the one-sample t-test

wilcox.test(sim,mu=3)

########################
## Wilcoxon Rank Sum ##
#######################
 
## non-parametric equivalent of the indep t-test

## load the data
##---------------

drugData<-read.delim("Drug.dat", header = TRUE)
head(drugData)
levels(drugData$drug)

## descriptive statistics
##-------------------------

levels(drugData$drug)

table(drugData$drug)

ddply(drugData, .(drug), summarise, 
      Nobs = sum(!is.na(sundayBDI)),
      Nmiss = sum(is.na(sundayBDI)),
      mean = mean(sundayBDI, na.rm=TRUE), 
      median = median(sundayBDI, na.rm=TRUE),
      sd = sd(sundayBDI, na.rm=TRUE),
      se   = sd/sqrt(Nobs),
      t = qt(0.975, Nobs-1),
      lower = mean - t*se, 
      upper = mean + t*se)

ddply(drugData, .(drug), summarise, 
      Nobs = sum(!is.na(wedsBDI)),
      Nmiss = sum(is.na(wedsBDI)),
      mean = mean(wedsBDI, na.rm=TRUE), 
      median = median(wedsBDI, na.rm=TRUE),
      sd = sd(wedsBDI, na.rm=TRUE),
      se   = sd/sqrt(Nobs),
      t = qt(0.975, Nobs-1),
      lower = mean - t*se, 
      upper = mean + t*se)

## graphical display
##--------------------

### histogram

ggplot(drugData,aes(x=sundayBDI)) + 
  geom_histogram(data=subset(drugData,drug == 'Alcohol'),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(drugData,drug == 'Ecstasy'),fill = "blue", alpha = 0.2)

ggplot(drugData,aes(x=wedsBDI)) + 
  geom_histogram(data=subset(drugData,drug == 'Alcohol'),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(drugData,drug == 'Ecstasy'),fill = "blue", alpha = 0.2)

### formal normality test

by(drugData$sundayBDI, drugData$drug, shapiro.test)
by(drugData$wedsBDI, drugData$drug, shapiro.test)

### boxplots

plot.sun <- ggplot(drugData, aes(drug, sundayBDI))

plot.sun + geom_boxplot(fill="slateblue") + 
  labs(x = "drug", y = "sundayBDI") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  geom_jitter() +
  theme(legend.position="none", axis.title.x=element_blank())

plot.wed <- ggplot(drugData, aes(drug, wedsBDI))

plot.wed + geom_boxplot(fill="slateblue") + 
  labs(x = "drug", y = "sundayBDI") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  geom_jitter() +
  theme(legend.position="none", axis.title.x=element_blank())

### test for normality and equality of variance

leveneTest(drugData$sundayBDI, drugData$drug, center = "median")
leveneTest(drugData$wedsBDI, drugData$drug, center = "median")

## the Wilcoxon rank sum test (=Mann-Whitney test) compares the 2 drug levels at each day
##--------------------------------------------------------------------------------------
wilcox.test(sundayBDI ~ drug, data = drugData, correct= FALSE)
wilcox.test(wedsBDI ~ drug, data = drugData, correct= FALSE)

### a continuity correction is an adjustment that is made when a discrete distribution is 
# approximated by a continuous distribution

###########################
## Wilcoxon Signed Rank ##
##########################

### non-parametric equivalent of paired t-test

### looking at the change in depression levels within people
### ie change between wednesday and sunday

drugData$BDIchange <- drugData$wedsBDI - drugData$sundayBDI

with(drugData, tapply(BDIchange, drug, shapiro.test))

ggplot(drugData,aes(x=BDIchange)) + 
  geom_histogram(data=subset(drugData, drug == 'Alcohol'),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(drugData, drug == 'Ecstasy'),fill = "blue", alpha = 0.2)

ggplot(drugData, aes(drug, BDIchange)) + geom_boxplot()

### perform test on both groups separately

alcoholData <- subset(drugData, drug == "Alcohol")

wilcox.test(alcoholData$wedsBDI, alcoholData$sundayBDI,  paired = TRUE, correct= FALSE)

median(alcoholData$wedsBDI)
median(alcoholData$sundayBDI)

ecstasyData<-subset(drugData, drug == "Ecstasy")

wilcox.test(ecstasyData$wedsBDI, ecstasyData$sundayBDI, paired = TRUE, correct= FALSE)

median(ecstasyData$wedsBDI)
median(ecstasyData$sundayBDI)

##########################
## Kruskal-Wallis test ##
#########################

### non-parametric counterpart of one-way anova

Sperm <- c(0.35, 0.58, 0.88, 0.92, 1.22, 1.51, 1.52, 1.57, 2.43, 2.79, 3.40, 4.52, 4.72, 6.90, 7.58, 7.78, 9.62, 10.05, 10.32, 21.08, 0.33, 0.36, 0.63, 0.64, 0.77, 1.53, 1.62, 1.71, 1.94, 2.48, 2.71, 4.12, 5.65, 6.76, 7.08, 7.26, 7.92, 8.04, 12.10, 18.47, 0.40, 0.60, 0.96, 1.20, 1.31, 1.35, 1.68, 1.83, 2.10, 2.93, 2.96, 3.00, 3.09, 3.36, 4.34, 5.81, 5.94, 10.16, 10.98, 18.21, 0.31, 0.32, 0.56, 0.57, 0.71, 0.81, 0.87, 1.18, 1.25, 1.33, 1.34, 1.49, 1.50, 2.09, 2.70, 2.75, 2.83, 3.07, 3.28, 4.11)
Soya <- gl(4, 20, labels = c("No Soya", "1 Soya Meal", "4 Soya Meals", "7 Soya Meals"))
soyaData <- data.frame(Sperm, Soya)
head(soyaData)

## descriptives
## ------------------------------------------------------------------------

levels(soyaData$Soya)
table(soyaData$Soya)

ddply(soyaData, .(Soya), summarise, 
      Nobs = sum(!is.na(Sperm)),
      Nmiss = sum(is.na(Sperm)),
      mean = mean(Sperm, na.rm=TRUE), 
      median = median(Sperm, na.rm=TRUE),
      sd = sd(Sperm, na.rm=TRUE),
      se   = sd/sqrt(Nobs),
      t = qt(0.975, Nobs-1),
      lower = mean - t*se, 
      upper = mean + t*se)

## Graphical Display
##----------------------

ggplot(soyaData,aes(x=Sperm)) + 
  geom_histogram(data=subset(soyaData, Soya == 'No Soya'),fill = "red", alpha = 0.2) 
ggplot(soyaData,aes(x=Sperm)) + 
  geom_histogram(data=subset(soyaData, Soya == '1 Soya Meal'),fill = "red", alpha = 0.2) 
ggplot(soyaData,aes(x=Sperm)) + 
  geom_histogram(data=subset(soyaData, Soya == '4 Soya Meals'),fill = "red", alpha = 0.2) 
ggplot(soyaData,aes(x=Sperm)) + 
  geom_histogram(data=subset(soyaData, Soya == '7 Soya Meals'),fill = "red", alpha = 0.2) 

ggplot(soyaData, aes(Soya, Sperm)) + geom_boxplot() +
  labs(y = "Sperm Count", x = "Number of Soya Meals Per Week")

plot <- ggplot(soyaData, aes(Soya, Sperm))
plot + geom_boxplot(fill="slateblue") + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme(legend.position="none", axis.title.x=element_blank())

### run a linear model

model.lm <- lm(Sperm ~ Soya, data = soyaData)

### verify the assumptions

opar <- par(mfrow=c(2,2)) # make a copy of current settings
plot(model.lm)
par(opar) # restore original settings

### linear model/anova  is thus not appropiate

leveneTest(soyaData$Sperm, soyaData$Soya)

### Kruskal-Wallis test

kruskal.test(Sperm ~ Soya, data = soyaData)

soyaData$Ranks <- rank(soyaData$Sperm)
by(soyaData$Ranks, soyaData$Soya, mean)

### post-hoc tests

pairwise.wilcox.test(soyaData$Sperm, soyaData$Soya)

### post-hoc tests with the pgirmess library
kruskalmc(Sperm ~ Soya, data = soyaData)

levels(soyaData$Soya)
kruskalmc(Sperm ~ Soya, data = soyaData, cont = 'two-tailed')

# possible alternative: log transform the outcome var

### trend test (library clinfun)

jonckheere.test(soyaData$Sperm, as.numeric(soyaData$Soya))

#######################
## Friedman's ANOVA ##
######################

### for repeated measures
### for analyzing unreplicated complete block designs (one obs in y for each level of group and block)

### enter the data

dietData <- read.delim("Diet.dat", header = TRUE)
head(dietData)

## descriptives
## ------------------------------------------------------------------------

dfapply(dietData, favstats, select = is.numeric)

## graphical display
##--------------------------

plot1 <- ggplot(dietData, aes(Start)) 
plot1 + geom_histogram() + geom_rug(sides="t") 

plot2 <- ggplot(dietData, aes(Month1)) 
plot2 + geom_histogram() + geom_rug(sides="t") 

plot3 <- ggplot(dietData, aes(Month2)) 
plot3 + geom_histogram() + geom_rug(sides="t") 
 
## perform the test
##-------------------

friedman.test(as.matrix(dietData))

## post-hoc tests with the pgirmess library

friedmanmc(as.matrix(dietData))

## in case of missing data:

dietCompleteCases=na.omit(dietData)

#### Note: The library coin has also most of the above mentioned tests