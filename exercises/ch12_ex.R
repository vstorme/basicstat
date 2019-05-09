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

## graphical analysis

## analysis

## diagnostics

## post-hoc


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

## graphical analysis

## analysis

## diagnostics

## post-hoc

# task 3
##############

consumption <- c(709,679,699,592,538,476,508,505,539, 657,594,677)
gender <- gl(2, 6, labels = c("M", "F"))
fat <- gl(n=2,k=3,length=12,labels=c("Fresh","Rancid"))
rats = data.frame(consumption, gender, fat)
head(rats)

