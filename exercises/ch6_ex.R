setwd("---")

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

## Association essay score and time spent
##---------------------------------------

## get descriptives
summarise(---, 
          Nobs = sum(!is.na(---)),
          Nmiss = sum(is.na(---)),
          mean = mean(---, na.rm=TRUE), 
          sd = sd(---, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

summarise(---, 
          Nobs = sum(!is.na(---)),
          Nmiss = sum(is.na(---)),
          mean = mean(---, na.rm=TRUE), 
          sd = sd(---, na.rm=TRUE),
          se   = sd/sqrt(Nobs),
          t = qt(0.975, Nobs-1),
          lower = mean - t*se, 
          upper = mean + t*se)

### scatter plot {mosaic}
xyplot(---)

### scatter plot {ggplot2}
scatter <- ggplot(---
scatter + geom_point() + 
  geom_smooth(method = "lm", colour = "Blue", se = T, level=0.95) +
  labs(x = "---", y = "---") 

### correlation

#### assess (bivariate) normality

result = mvn(data = ---, 
             mvnTest = "hz",
             univariateTest = "AD", 
             univariatePlot = "histogram",
             multivariatePlot = "qq", multivariateOutlierMethod = "adj",
             showOutliers = TRUE, showNewData = FALSE)

result$multivariateNormality

#### Pearson correlation
---

## Association essay grade and hours
  ##-----------------------------------

### grade is not numerically coded, recode
levels(essayData$grade)
table(essayData$grade)

essayData$grade2[essayData$grade=="First Class"]=1
essayData$grade2[essayData$grade=="Upper Second Class"]=2
essayData$grade2[essayData$grade=="Lower Second Class"]=3
essayData$grade2[essayData$grade=="Third Class"]=4

### scatter plot {mosaic}
xyplot(---)

### scatter plot {ggplot2}
scatter <- ggplot(---)
scatter + geom_point() + 
  geom_smooth(method = "lm", colour = "Blue", se = T, level=0.95) +
  labs(x = "---", y = "---") 

#### assess (bivariate) normality
--- 
  
### correlation
---

# Task 2
########################




