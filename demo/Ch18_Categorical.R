#' ---
#' title: "Analysis of categorical data"
#' author: "Veronique Storme"
#' date: "17/04/2019"
#' ---
#' 
#' R Code for Chapter 18 of:
#'   
#' Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#' 
#' (c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#' 

## Set the working directory
## ------------------------------------------------------------------------

setwd("C:/Users/vesto/Documents/myCOURSES/basic-stat-R/april2019/data")

#' Install and Load Packages

#install.packages("gmodels")
#install.packages("MASS")
#install.packages("janitor")
#install.packages("epitools")

library(gmodels)
library(MASS)
library(janitor)
library(epitools)
library(DescTools)

#' 
#' ### one sample tests
#' 
#' > binomial test
#' 
## ------------------------------------------------------------------------
binom.test( x=17,n=25,p=8/20)

#' 
#' > One categorical variable with more than two possible outcomes
#' 
## ------------------------------------------------------------------------
drosophila=c(2,21,16,52)
n=sum(drosophila)
p.expected=c(1/16,3/16,3/16,9/16)

chisq.test(drosophila, p = p.expected,rescale.p=TRUE,correct=FALSE)  

#' 
#' ### two-way contingency tables
#' 
#' For 2x2 tables it is preferred to put first the ref level (possibly labeled as 0)
#' 
#' load the data
#' 
## ------------------------------------------------------------------------
catsData<-read.delim("cats.dat", header = TRUE)
head(catsData)

#' 
#' get contingency table from a dataframe
#' tabyl{janitor}, a tidyverse-oriented replacement for table()
#' CrossTable{gmodels}
#' 
## ------------------------------------------------------------------------
table(catsData$Training, catsData$Dance)

xtabs(~ Training + Dance, data=catsData)

CrossTable(catsData$Training, catsData$Dance)

cats.tabyl = catsData %>% tabyl(Training, Dance)
cats.tabyl

#' 
#' create a matrix and add the names of the variables as column and row labels
#' 
## ------------------------------------------------------------------------
cats.matrix=matrix(c(28,10,48,114),ncol=2,byrow=TRUE,
                   dimnames=list(training=c("food reward","affection reward"),
                                 dancing=c("yes","no")))
cats.matrix

# row proportions
#----------------
stats::prop.test(cats.matrix,correct=F)

# riskratio or relative risk
#---------------
# DescTools
RelRisk(cats.matrix, method="wald")
# 0.7368/0.2963

#library(epitools)
riskratio(catsData$Training, catsData$Dance, method = "wald")

## odds ratio
##-------------
# DescTools
OddsRatio(cats.matrix, method="wald")

#{epitools} 
oddsratio(catsData$Training, catsData$Dance, method="wald")

# affection as Reward is the reference level
# the odds of dancing when the reward was food is 6.5 times the odds of dancing when the cats were rewarded with food. This effect is sign at the 5% sign level since the value 1 is not in the 95% CI.


#' ### chi-square test of independence
#' 
## ------------------------------------------------------------------------
chisq.test(cats.matrix,correct=FALSE)

#' 
#' small sample method for IxJ tables
#' 
## ------------------------------------------------------------------------
chisq.test(cats.matrix,simulate.p.value=TRUE)

#' 
#' or with the function CrossTable from the gmodels package
#' 
## ------------------------------------------------------------------------
CrossTable(catsData$Training, catsData$Dance, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SAS")

CrossTable(cats.matrix, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SAS")

#' 
#' ### Fisher's exact test
#' 
## ------------------------------------------------------------------------
tea <- matrix(c(3, 1, 1, 3), ncol= 2, 
              dimnames = list(Truth = c("Tea","Milk" ),
                              Lady_says = c("Tea first","Milk first")))

tea

#### one-sided Fisher's exact test

fisher.test(tea, alternative = "greater")

#' 
#' 
#' 
#' ## the analysis of paired dichotomous, categorical variables
#' 
#' eg effects pre and post-treatment
#' eg case-control study
#' eg testing disagreement between 2 measurement methods/ between 2 raters
#' 
#' The McNemar test tests the null hypothesis that the proportions are equal across matched pairs
#' 
#' example data
#' Consider paired binary response data. For example, suppose you have twins randomized to
#' two treatment groups (Test and Control) then tested on a binary outcome (pass or fail).
#' There are 4 possible outcomes for each pair: 
#' - both twins fail
#' - the twin in the control group fails and the one in the test group passes
#' - the twin on the test group fails and the one in the control group passes
#' - both twins pass
#' 
## ------------------------------------------------------------------------
x = matrix(c(21,9,2,12),ncol=2,byrow=T,
           dimnames=list(treat.ctrl=c("fail","pass"),
                         treat.test=c("fail","pass")))
x
mcnemar.test(x, correct=F)

CrossTable(x, mcnemar=TRUE, expected = TRUE, sresid = TRUE, format = "SAS")

#' 
#' ### Cochran-Mantel-Haenszel test
#' 
#' This test gives an assessment of the relationship between X1, X2, stratified by (or controlling for) X3. 
#' 
## ------------------------------------------------------------------------
Rabbits <-
array(c(0, 0, 6, 5,
        3, 0, 3, 6,
        6, 2, 0, 4,
        5, 6, 1, 0,
        2, 5, 0, 0),
      dim = c(2, 2, 5),
      dimnames = list(
          Delay = c("None", "1.5h"),
          Response = c("Cured", "Died"),
          Penicillin.Level = c("1/8", "1/4", "1/2", "1", "4")))
Rabbits
mantelhaen.test(Rabbits)

#' 
#' After adjusting for the level of penicillin, there is some evidence for a higher cure rate after immediate injection
#' 
