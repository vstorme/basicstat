setwd("C:/Users/vesto/Documents/myCOURSES/basic-stat-R/april2019/data")

library(ggplot2)
library(mosaic)
library(Hmisc)
library(qgraph)
library(gridExtra)
library(car)

# Task 1
##########

## Load the data:

essayData = read.delim("EssayMarks.dat",  header = TRUE)
head(essayData)

## get descriptives
summarise(essayData, 
          Nobs = sum(!is.na(essay)),
          Nmiss = sum(is.na(essay)),
          mean = mean(essay, na.rm=TRUE), 
          sd = sd(essay, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

summarise(essayData, 
          Nobs = sum(!is.na(hours)),
          Nmiss = sum(is.na(hours)),
          mean = mean(hours, na.rm=TRUE), 
          sd = sd(hours, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)


## Association essay score and time spent

### scatter plot {mosaic}
xyplot(essay ~ hours, essayData)

### scatter plot {ggplot2}
scatter <- ggplot(essayData, aes(hours, essay))
scatter + geom_point() + 
  geom_smooth(method = "lm", colour = "Blue", se = T, level=0.95) +
  labs(x = "time spent (h)", y = "essay score") 

### correlation

#### assess (bivariate) normality

result = mvn(data = essayData[,1:2], 
             mvnTest = "hz",
             univariateTest = "AD", 
             univariatePlot = "histogram",
             multivariatePlot = "qq", multivariateOutlierMethod = "adj",
             showOutliers = TRUE, showNewData = FALSE)

result$multivariateNormality

#### Pearson correlation
cor.test(essayData$essay, essayData$hours, method = "pearson")

## Association essay grade and hours

### grade is not numerically coded, recode
levels(essayData$grade)
table(essayData$grade)

essayData$grade2[essayData$grade=="First Class"]=1
essayData$grade2[essayData$grade=="Upper Second Class"]=2
essayData$grade2[essayData$grade=="Lower Second Class"]=3
essayData$grade2[essayData$grade=="Third Class"]=4

### scatter plot {mosaic}
xyplot(grade2 ~ hours, essayData)

### scatter plot {ggplot2}
scatter <- ggplot(essayData, aes(hours, grade2))
scatter + geom_point() + 
  geom_smooth(method = "lm", colour = "Blue", se = T, level=0.95) +
  labs(x = "time spent (h)", y = "grade") 

#### assess (bivariate) normality

result = mvn(data = essayData[,c(2,4)], 
             mvnTest = "hz",
             univariateTest = "AD", 
             univariatePlot = "histogram",
             multivariatePlot = "qq", multivariateOutlierMethod = "adj",
             showOutliers = TRUE, showNewData = FALSE)

result$multivariateNormality

## no evidence to reject mvn, however grade2 is clearly not continuous, but ordinal 
## with only 4 levels

### correlation
cor.test(essayData$hours, essayData$grade2, method = "kendall")
cor.test(essayData$hours, essayData$grade2, method = "spearman")

# Task 2
########################

## load the data
gradesData = read.csv("grades.csv", header = TRUE)
head(gradesData)

## descriptives ordinal data
levels(as.factor(gradesData$stats))
levels(as.factor(gradesData$gcse))

table(gradesData)

## scatter plot {mosaic}
xyplot(stats ~ gcse, gradesData)

### scatter plot {ggplot2}
scatter <- ggplot(gradesData, aes(gcse, stats))
scatter + geom_point() + 
  labs(x = "gcse score", y = "stats score") 

## data not continuous
## Conduct a Spearman correlation:
cor.test(gradesData$gcse, gradesData$stats, method = "spearman")

####conduct a Kendall correlation:
cor.test(gradesData$gcse, gradesData$stats, method = "kendall")




