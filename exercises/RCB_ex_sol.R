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

## Descriptives:

levels(mgrowth$isolates)
levels(mgrowth$repl)

table(mgrowth$isolates, mgrowth$repl)

summ <- ddply(mgrowth, .(isolates), summarise, 
              Nobs = sum(!is.na(growth)),
              Nmiss = sum(is.na(growth)),
              mean = mean(growth, na.rm=TRUE), 
              sd = sd(growth, na.rm=TRUE),
              se   = sd/sqrt(Nobs),
              t = qt(0.975, Nobs-1),
              lower = mean - t*se, 
              upper = mean + t*se)

summ

## looking at the data
plot <- ggplot(mgrowth, aes(isolates, growth))
plot + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + 
  stat_summary(fun.y = mean, geom = "bar", position="dodge") + 
  geom_jitter(aes(x=isolates, y=growth, group=repl, color=repl)) +
  labs(x = "isolates", y = "mean diameter colony (mm)")

## fit a fixed model
####################
fit = lm(growth ~ isolates + repl, data=mgrowth)

Anova(fit, type="III")

## diagnostics
par(mfrow=c(2,2))
plot(fit)
par(opar)

## least-squares means
lsm = emmeans(fit, ~ isolates)
lsm

## Post Hoc tests with the lsmeans package:
contrast(lsm, method="pairwise", adjust="tukey")

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


