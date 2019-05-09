library(nlme)
library(multcomp)
library(mosaic)
library(beanplot)


# fictive data set
# Mycelial growth in terms of diameter of the colony (mm) of R. solani isolates on PDA medium after 14 hours of incubation.

isolates = rep(c("RS1", "RS2","RS3", "RS4", "RS5"),3)
repl = c(rep(1,5),rep(2,5),rep(3,5))
growth = c(29.0,33.5,26.5,48.5,34.5,28.0,31.5,30.0,46.5,31.0,29.0,29.0,28.0,49.0,32.0)
mgrowth = data.frame(isolates, repl,growth)
head(mgrowth)

mgrowth$repl = factor(mgrowth$repl)

















## fit a mixed model (advanced)
#############################

# isolates is a fixed effect, repl is a random effect
fit.r <- lme(growth ~ isolates,  random = ~1|repl, data = mgrowth)
summary(fit.r)

Anova(fit.r, type="III")

names(fit.r)
methods(lme)

# diagnostics
scaled = residuals(fit.r, scaled=TRUE)

plot(fit.r$fitted[,1],scaled)
abline(h=0, lty=2)

# Post-hoc comparison

lsm.r = emmeans(fit.r,  ~ isolates)
lsm.r

contrast(lsm.r, method="pairwise", adjust="tukey")


