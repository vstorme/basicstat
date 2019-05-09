# ---
# title: "Randomised complete block designs RCBD"
# author: "Veronique Storme"
# date: "09/05/2019"
# output: html_document
# editor_options: 
#   chunk_output_type: console
# ---
# 

library(car)
library(emmeans)

# Set the working directory
## ------------------------------------------------------------------------
setwd("C:/Users/vesto/Documents/myCOURSES/basic-stat-R/april2019/data")

# load the data
## ------------------------------------------------------------------------
emerge = read.table("seedemergence.txt", header=T)
head(emerge)

# make sure blocks is treated as categorical
emerge$block = factor(emerge$block)

# descriptives
#-----------------

levels(emerge$treatment)
emerge$treatment = relevel(emerge$treatment, ref="Control")
levels(emerge$treatment)
levels(emerge$block)

table(emerge$treatment, emerge$block)

summ <- ddply(emerge, .(treatment), summarise, 
              Nobs = sum(!is.na(emergence)),
              Nmiss = sum(is.na(emergence)),
              mean = mean(emergence, na.rm=TRUE), 
              sd = sd(emergence, na.rm=TRUE),
              se   = sd/sqrt(Nobs),
              t = qt(0.975, Nobs-1),
              lower = mean - t*se, 
              upper = mean + t*se)

summ

# graphical analysis
plot <- ggplot(emerge, aes(treatment, emergence))
plot + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + 
  stat_summary(fun.y = mean, geom = "bar", position="dodge") + 
  geom_jitter(aes(x=treatment, y=emergence, group=block, color=block)) +
  labs(x = "treatment", y = "emergence (nr)")

# anova
## ------------------------------------------------------------------------
fit.rcbd = lm(emergence ~ treatment + block, data=emerge)
summary(fit.rcbd)
Anova(fit.rcbd, type="III")

# keep block in the model because of the design, even if it is not sign

# diagnostics
opar = par(mfrow=c(2,2))
plot(fit.rcbd)
par(opar)

#' 
#' additivity assumption
#' 
## ------------------------------------------------------------------------
ggplot(emerge, aes(x=treatment, y=emergence, group=block, color=block)) + 
  geom_line() + 
  labs(x = "treatment", y = "nr of emerged seeds") +
  scale_color_brewer(palette="Paired") 
#' 
#' > lsmeans
#' 
## ------------------------------------------------------------------------
lsm = emmmeans(fit.rcbd, ~ treatment)
lsm

#' 
#' > post hoc: treatment effect vs control
#' 
## ------------------------------------------------------------------------
# ! use order in lsm object, and not order from levels()
contrast(lsm, method="trt.vs.ctrl", ref=2)

#' 
#' ### advanced: mixed effects model (self study)
#' 
#' 
#' Load Packages
#' 
## ------------------------------------------------------------------------
library(nlme)
library(car)
library(gmodels)
#' 
#' > fit a random effects model
#' 
## ------------------------------------------------------------------------
fit.random <- lme(emergence ~ treatment, data = emerge, random = ~ 1 | block, method="REML")
summary( fit.random )

#' 
#' the overall significance of the Type term, is assessed with the Anova function.
#' 
## ------------------------------------------------------------------------
Anova(fit.random, type="III" )

# diagnostics
scaled = residuals(fit.random, scaled=TRUE)

plot(fit.random$fitted[,1],scaled)
abline(h=0, lty=2)

# Post-hoc comparison

lsm.r = emmeans(fit.random,  ~ treatment)
lsm.r

contrast(lsm.r, method="trt.vs.ctrl", ref=1, CIs=TRUE)
