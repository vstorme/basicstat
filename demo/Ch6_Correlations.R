#' ---
#' title: "Correlations"
#' author: "Veronique Storme"
#' date: "16/04/2019"
#' ---
#' 
#' R Code for Chapter 6 of:
#' 
#' Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#' 
#' (c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#' 
#' with adaptations of Veronique Storme
#' 
#' 
#' Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter) 
#' 
## ------------------------------------------------------------------------
setwd("C:/Users/vesto/Documents/myCOURSES/basic-stat-R/april2019/data")

#' 
#' Install and Load Packages
#' 
-----
#install.packages("Hmisc")
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("corrplot")
#install.packages("ppcor")
#install.packages("qgraph")
#install.packages("dplyr")
#install.packages("MVN")  
#install.packages("DescTools") 

library(Hmisc)
library(ggplot2)
library(GGally)
library(corrplot)
library(ppcor)
library(qgraph)
library(plyr)
library(dplyr)
library(MVN)
library(mosaic)
library(DescTools)

## load the eggs dataset
#-------------------------------
# Ref: http://www.biostathandbook.com/linearregression.html

# data from McDonald (1989): 
# the number of eggs carried by the female amphipod crustacean Platorchestia platensis 
# vs their weight 

eggs = read.csv("eggs.csv",  header = TRUE)
eggs[1:6,]

## descriptive statistics
#------------------------

summary(eggs)

# mosaic library
favstats(~ weight,  data=eggs)

# use the apply-type function for dataframes to get summary of multiple variables
dfapply(eggs, favstats, select = is.numeric)
dfapply(eggs, favstats, select = c(TRUE, TRUE))

# using the Desc() function from the DescTools library
Desc(eggs$weight, main="", plotit=TRUE)
Desc(eggs$eggs, main="", plotit=TRUE)

# plyr package
summarise(eggs, 
          Nobs = sum(!is.na(weight)),
          Nmiss = sum(is.na(weight)),
          mean = mean(weight, na.rm=TRUE), 
          sd = sd(weight, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

summarise(eggs, 
          Nobs = sum(!is.na(eggs)),
          Nmiss = sum(is.na(eggs)),
          mean = mean(eggs, na.rm=TRUE), 
          sd = sd(eggs, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

## graphical display: scatterplots
##-----------------------------------

# ggplot2 library
scatter <- ggplot(eggs, aes(weight, eggs))
scatter + geom_point() + 
  geom_smooth(method = "lm", colour = "Blue", se = T, level=0.95) +
  labs(x = "weight(mg)", y = "egg count") 
ggsave("scatterplot.png")

## verifying asumptions
#----------------------

# formal normality test
shapiro.test(eggs$weight)
shapiro.test(eggs$eggs)

# univariate normality
ggplot(eggs, aes(weight)) + 
  theme(legend.position = "none") + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") + 
  labs(x="weight (mg)", y = "Density")

ggplot(eggs, aes(eggs)) + 
  theme(legend.position = "none") + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") + 
  labs(x="egg count", y = "Density")

#multivariate normality with the MVN library
result = mvn(data = eggs, 
             mvnTest = "hz",
             univariateTest = "AD", 
             univariatePlot = "histogram",
             multivariatePlot = "qq", multivariateOutlierMethod = "adj",
             showOutliers = TRUE, showNewData = FALSE)

result$multivariateNormality

## Pearson correlation
##----------------------

cor(eggs)

cor.test(eggs$weight, eggs$eggs)

# Hmisc library
rcorr(as.matrix(eggs))

# corrplot library
C = rcorr(as.matrix(eggs))
corrplot(C$r, method = "ellipse")
corrplot(C$r, method = "number")
corrplot(C$r, method = "ellipse", type="upper")
corrplot.mixed(C$r, lower = "number", upper = "ellipse")
corrplot(C$r, p.mat = C$P, sig.level = 0.001, method="ellipse")
corrplot(C$r, p.mat = C$P, insig = "p-value", sig.level = -1,  method="ellipse")


## load the Exam anxiety dataset
#-------------------------------
examData = read.delim("ExamAnxiety.dat",  header = TRUE)
examData[1:6,]

## descriptive statistics
#------------------------

# use the apply-type function for dataframes to get summary of multiple variables
dfapply(examData, favstats, select = is.numeric)
dfapply(examData, favstats, select = c(FALSE, TRUE, TRUE, TRUE, FALSE))

# using the Desc() function from the DescTools library
Desc(examData$Exam, main="", plotit=TRUE)

# plyr package
summarise(examData, 
          Nobs = sum(!is.na(Exam)),
          Nmiss = sum(is.na(Exam)),
          mean = mean(Exam, na.rm=TRUE), 
          sd = sd(Exam, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

summarise(examData, 
          Nobs = sum(!is.na(Anxiety)),
          Nmiss = sum(is.na(Anxiety)),
          mean = mean(Anxiety, na.rm=TRUE), 
          sd = sd(Anxiety, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

summarise(examData, 
          Nobs = sum(!is.na(Revise)),
          Nmiss = sum(is.na(Revise)),
          mean = mean(Revise, na.rm=TRUE), 
          sd = sd(Revise, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

## graphical display: scatterplots
##-----------------------------------

# ggplot2 and GGally
ggpairs(examData[, c("Exam", "Anxiety", "Revise")], 
        diag=list(continuous="densityDiag"), axisLabels='show')

## verifying asumptions
#----------------------

# formal normality test
shapiro.test(examData$Exam)
shapiro.test(examData$Revise)
shapiro.test(examData$Anxiety)

#multivariate normality with the MVN library
result = mvn(data = examData[,2:4], 
             mvnTest = "hz",
             univariateTest = "AD", 
             univariatePlot = "histogram",
             multivariatePlot = "qq", multivariateOutlierMethod = "adj",
             showOutliers = TRUE, showNewData = FALSE)

result$multivariateNormality

result$univariateNormality

## conclusion: Spearman correlation 
## measures the strength of a monotonic association
##---------------------------------------------------

cor.test(examData$Anxiety, examData$Exam, method="spearman")
cor.test(examData$Revise, examData$Exam, method="spearman")
cor.test(examData$Anxiety, examData$Revise, method="spearman")

cor(examData[, c("Exam", "Anxiety", "Revise")], method="spearman")

# in case of missing data, use:
# cor(examData[, c("Exam", "Anxiety", "Revise")], method="spearman", use = "complete.obs")
#' 
#' calculate correlations with p-values (rcorr() function from Hmisc package)
#' 
## ------------------------------------------------------------------------
rcorr(as.matrix(examData[, c("Exam", "Anxiety", "Revise")]), type="spearman")

# corrplot library
C = rcorr(as.matrix(examData[, c("Exam", "Anxiety", "Revise")]))
corrplot(C$r, method = "ellipse")


## mark example
## ------------------------------------------------------------------------
grade.english = c(56,75,45,71,62,64,58,80,76,61)
grade.math = c(66,70,40,60,65,56,59,77,67,63)
grade = data.frame(grade.english, grade.english)

scatter <- ggplot(grade, aes(grade.english, grade.math))
scatter + geom_point() + 
  geom_smooth(method = "lm", colour = "Blue", se = T, level=0.95) +
  labs(x = "grade English", y = "grade math") 

cor(grade.english, grade.math , method = "spearman")

## Note that the spearman's Rho corr is the same as the Pearson coor on ranks
Rx = c(9,3,10,4,6,5,8,1,2,7)
Ry= c(4,2,10,7,5,9,8,1,3,6)
cor(Rx, Ry , method = "pearson")

## Liar data
## ------------------------------------------------------------------------
liarData = read.delim("BiggestLiar.dat",  header = TRUE)
liarData[1:6,]

Desc(liarData$Creativity, main="", plotit=TRUE)
Desc(liarData$Position, main="", plotit=TRUE)

scatter <- ggplot(liarData, aes(Creativity, Position))
scatter + geom_point() + 
  geom_smooth(method = "lm", colour = "Blue", se = T, level=0.95) 

#multivariate normality with the MVN library
result = mvn(data = liarData[,1:2], 
             mvnTest = "hz",
             univariateTest = "AD", 
             univariatePlot = "histogram",
             multivariatePlot = "qq", multivariateOutlierMethod = "adj",
             showOutliers = TRUE, showNewData = FALSE)

result$multivariateNormality

## Kendall's Tau
## quantifies the difference between the % of concordant and discordant pairs among all possible pairwise events

cor.test(liarData$Position, liarData$Creativity, method = "kendall")
cor.test(liarData$Position, liarData$Creativity, method = "spearman")
