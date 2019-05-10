setwd("C:/Users/vesto/Documents/myCOURSES/basic-stat-R/april2019/data")

library(gmodels)
library(MASS)

# Task 1
###################
n=682+243

binom.test(x=682,n=n,p=0.75)

prop.test(x=682,n=n,p=0.75)

# Task 2:
#####################

#wt (wings and red eyes) = 108
#apterous (and red eyes) = 40, 
#sepia (and wings) = 35
#apterous sepia = 17

#thus:
#normal wings: 108+35=143
#apterous: 40+17=57
#red eye: 108+40=148
#sepia eye: 35+17=52

fly.array=array(c(108,40,35,17),dim=c(2,2),
                dimnames=list(wings=c("yes","no"),
                              eyes=c("red","sepia")))
fly.array

fly.matrix = matrix(c(108,40,35,17),byrow=FALSE,nrow=2,
             dimnames=list(wings=c("yes","no"),
                           eyes=c("red","sepia")))
fly.matrix

# Chi-square test of independence:
CrossTable(fly.matrix, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, 
           format = "SAS")

# H0: genes are unlinked, no association
# indeed ap on chrom 2 and se on chrom3

# Task 3
####################
X <- matrix(c(7,77-7,350-7,NA), byrow=FALSE,nrow=2,
            dimnames=list(DE=c("yes","no"),
                          GO=c("immune","rest")))
X
X[2,2] = 638118-7-70-343
X

fisher.test(X,alternative="greater")

#usage of the hypergeometric distribution
n11 = X[1,1]
n.1=sum(X[,1])
n.2=sum(X[,2])
n1. = sum(X[1,])
phyper(n11-1,n.1,n.2,n1.,lower.tail=FALSE)
#Prob that X greater than or equal to x
#This is the correct test for over-representation

# Task 4
##########

# GWAS application

gwas = matrix(c(180,20,40,10),byrow=FALSE,nrow=2,
                    dimnames=list(disease=c("control","cases"),
                                  genotype=c("AA/Aa","aa")))
gwas

CrossTable(gwas, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SAS")
# the odds of having the A allele compared to aa  is 2.2 times higher in the control group 
# than in the cases group 
