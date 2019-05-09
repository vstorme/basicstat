library(car)
library(ggplot2)
library(mosaic)

# Task 1
###############################
try(data(package="datasets"))
data()

## load the data

data(sleep)
head(sleep)
?sleep

## descriptives

table(sleep$group)

favstats(~ extra | group, data=sleep)

ddply(sleep, .(group), summarise, 
      Nobs = sum(!is.na(extra)),
      Nmiss = sum(is.na(extra)),
      mean = mean(extra, na.rm=TRUE), 
      sd = sd(extra, na.rm=TRUE),
      se   = sd/sqrt(Nobs),
      t = qt(0.975, Nobs-1),
      lower = mean - t*se, 
      upper = mean + t*se)

## barchart
plot4 <- ggplot(sleep, aes(group, extra))

plot4 + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2) + 
  stat_summary(fun.y = mean, geom = "bar", fill = "slateblue", colour = "Black") +   
  labs(x = "group", y = "extra sleep (h)") +
  geom_jitter() +
  theme(legend.position="none", axis.title.x=element_blank())

## paired T-test
### verify assumptions
group1=subset(sleep, group==1)
group2=subset(sleep, group==2)
diff = group1$extra - group2$extra

df.diff = as.data.frame(diff)

ggplot(df.diff, aes(diff)) + 
  theme(legend.position = "none") + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=1) + 
  labs(x="differences", y = "Density")

shapiro.test(diff)

## The assumptions are not met. A non-parametric test should be used

### test
# t.test(extra ~ group, data = sleep, paired=TRUE)

# Task 2
###########

Group<-gl(2, 12, labels = c("SBPbefore", "SBPafter"))
pressure=c(120,140,126,124,128,130,130,140,126,118,135,127,128,132,118,131,125,132,131,141,129,127,137,135)
pressureLong=data.frame(Group,pressure)
pressureLong

## descriptives
table(pressureLong$Group)

favstats(~ pressure | Group, data=pressureLong)

ddply(pressureLong, .(Group), summarise, 
      Nobs = sum(!is.na(pressure)),
      Nmiss = sum(is.na(pressure)),
      mean = mean(pressure, na.rm=TRUE), 
      sd = sd(pressure, na.rm=TRUE),
      se   = sd/sqrt(Nobs),
      t = qt(0.975, Nobs-1),
      lower = mean - t*se, 
      upper = mean + t*se)

## barchart
bar <- ggplot(pressureLong, aes(Group, pressure))
bar +    
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2) +
  stat_summary(fun.y = mean, geom = "bar") 

## paired T-test
### testing assumption :normality of differences
SBPbefore=pressureLong[pressureLong$Group=="SBPbefore",2]
SBPafter=pressureLong[pressureLong$Group=="SBPafter",2]
diff<-SBPafter-SBPbefore

df.diff = as.data.frame(diff)

ggplot(df.diff, aes(diff)) + 
  theme(legend.position = "none") + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=1) + 
  labs(x="differences", y = "Density")

shapiro.test(diff)

library(beanplot)
beanplot(diff)

### test
t.test(pressure ~ Group, data=pressureLong, paired = TRUE)

# Note: analyzes SBPbefore-SBPafter because it is read in in that order with Group
levels(pressureLong$Group)






