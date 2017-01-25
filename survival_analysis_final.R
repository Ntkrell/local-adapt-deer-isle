# Survival analysis
require(survival)
#install.packages("coxme")
library(coxme)

# download data from dropbox
dat.db <- repmis::source_DropboxData("Processed_survival.csv",
                                          "2uyoy4ysb1qegg5",
                                          sep = ",",
                                          header = TRUE) 
# or grab data from desktop
setwd("~/Dropbox/LocalAdaptation_DeerIsle/Earlier Folders/Data")
dat = read.csv("Processed_survival.csv", header=T, sep=",")

# remove NAs where status==2 (these are replicates that never germinated, went missing from the study, or had some other issue such as the wrong species germinated in the soil)
dat <- na.omit(dat)

# remove potting soil values
dat<-new.dat[!(dat$Soil=="P"), ]
dat$Soil <- as.factor(as.character(dat$Soil))


# Subset out species for analysis
am.dat<-dat[(dat$Species=="AM"), ] 
hp.dat<-dat[(dat$Species=="HP"), ]

###### Cox mixed model survival analysis for AM
am.dat$Mother <- as.character(am.dat$Mother)
am.dat$Mother <- as.numeric(am.dat$Mother)
am.dat$Mother[c(18,19)] <- c(9,9)
am.dat$Mother <- factor(am.dat$Mother)
am.dat$Soil <- factor(am.dat$Soil)

### Even simpler versions of this model won't fit, very likely due to the very low variation in survival
### i.e., most are 365+
fit1 <- coxme(Surv(time, status) ~ Source + Soil + Source*Soil + (1|Source/Mother), data=am.dat)
summary(fit1)

# Kaplan-Meier Estimator
# fit.byspecies <- survfit(Surv(time, status) ~ Species, data = new.dat)
# summary(fit.byspecies)
# plot(fit.byspecies, lty=c(1:2), col=c("darkblue", "orange"),lwd=3,  main='Kaplan-Meier estimate with 95% confidence bounds',
#      xlab='Time', ylab='Survival probability', conf.int=T)
# legend(20, 0.2, col=c("darkblue", "red"),  c("Achillea millefolium", "Hypericum perforatum"), lty=c(1:2)) 
# 
# fit.bysource <- survfit(Surv(time, status) ~ Source, data = new.dat)
# fit.bysource
# plot(fit.bysource, lty=c(2,1), col=c("darkblue", "green"), lwd=3, main='Kaplan-Meier estimate with 95% confidence bounds',
#      xlab='Time', ylab='Survival probability', conf.int=T)
# legend(20, 0.2, col=c("darkblue", "green"),  c("Pine Hill", "Settlement Quarry"), lty=c(1:2)) 
# 
# fit.bysoil <- survfit(Surv(time, status) ~ Soil, data = new.dat)
# fit.bysoil
# plot(fit.bysoil, lty=c(2,1,3), col=c("darkblue", "green", "orange"), lwd=3, main='Kaplan-Meier estimate with 95% confidence bounds',
#      xlab='Time', ylab='Survival probability', conf.int=F)
# legend(20, 0.2, col=c("darkblue", "green", "orange"),  lwd=3, c("G", "P", "S"), lty=c(2, 1,3)) 

# Analyze by soil
# fit.ambysoil <- survfit(Surv(time, status) ~ Soil, data = am.dat)
# fit.ambysoil
# plot(fit.ambysoil, lty=c(2,1,3), col=c("darkblue", "green", "orange"), lwd=3, main='Kaplan-Meier estimate with 95% confidence bounds',
#      xlab='Time', ylab='Survival probability', conf.int=F)
# legend(20, 0.2, col=c("darkblue", "green", "orange"),  lwd=3, c("G", "P", "S"), lty=c(2, 1,3)) 
# 
# fit.hpbysoil <- survfit(Surv(time, status) ~ Soil, data = hp.dat)
# fit.hpbysoil
# plot(fit.hpbysoil, lty=c(2,1,3), col=c("darkblue", "green", "orange"), lwd=3, main='Kaplan-Meier estimate with 95% confidence bounds',
#      xlab='Time', ylab='Survival probability', conf.int=F)
# legend(20, 0.2, col=c("darkblue", "green", "orange"),  lwd=3, c("G", "P", "S"), lty=c(2, 1,3)) 

# Remove control
am.dat<-am.dat[!(am.dat$Soil=="P"), ]
hp.dat <- hp.dat[!hp.dat$Soil=="P",]

# Analyze by home v. away
fit.ambytag <- survfit(Surv(time, status) ~ tag, data = am.dat)
fit.ambytag
plot(fit.ambytag, lty=c(2,1,3), col=c("darkblue", "green", "orange"), lwd=3, main='Kaplan-Meier estimate with 95% confidence bounds for AM',
     xlab='Time', ylab='Survival probability', conf.int=F)
legend(20, 0.2, col=c("darkblue", "green", "orange"),  lwd=3, c("away", "home"), lty=c(2, 1,3)) 

fit.hpbytag <- survfit(Surv(time, status) ~ tag, data = hp.dat)
fit.hpbytag
plot(fit.hpbytag, lty=c(2,1,3), col=c("darkblue", "green", "orange"), lwd=3, main='Kaplan-Meier estimate with 95% confidence bounds for HP',
     xlab='Time', ylab='Survival probability', conf.int=F)
legend(20, 0.2, col=c("darkblue", "green", "orange"),  lwd=3, c("away", "home"), lty=c(2, 1,3)) 

# comparing survival curves
# survdiff(Surv(time, status==1) ~ Soil, data=new.dat)
# survdiff(Surv(time, status) ~ Soil, data = new.dat)
# survdiff(Surv(time, status) ~ Soil, data = hp.dat) #significant
# survdiff(Surv(time, status) ~ Soil, data = am.dat)
# survdiff(Surv(time, status) ~ Source, data = hp.dat)
# survdiff(Surv(time, status) ~ Source, data = am.dat)
survdiff(Surv(time, status) ~ tag, data = hp.dat) #not significant 
survdiff(Surv(time, status) ~ tag, data = am.dat) #not significant 
