# Date of First Flower analysis
# Author: Natasha T. Krell & Luka Negoita
# Date: 20 November 2016

require(survival)
#install.packages("coxme")
library(coxme)
library(lmerTest)
library(sciplot)

# download data from dropbox
dat.db <- repmis::source_DropboxData("Processed_survival.csv",
                                          "2uyoy4ysb1qegg5",
                                          sep = ",",
                                          header = TRUE) 
# or grab data from desktop
setwd("~/Dropbox/LocalAdaptation_DeerIsle/Earlier Folders/Data")
dat.surv = read.csv("Processed_survival.csv", header=T, sep=",")

# change missing & NaN to NA
dat.surv$time_dff[dat.surv$time_dff == "missing"] <- NA
dat.surv$time_dff[dat.surv$time_dff == "NaN"] <- NA

# remove all NAs 
dat.surv <- na.omit(dat.surv)

# remove controls 
dat.surv <- dat.surv[dat.surv$Soil!="P",]

# Can only do analysis for HP because AM only has 2 non-control soils data points 
dat.surv<-dat.surv[(dat.surv$Species=="HP"), ]

# Cox mixed model survival analysis for HP only
dat.surv$Mother <- as.character(dat.surv$Mother)
dat.surv$Mother <- as.numeric(dat.surv$Mother)
dat.surv$Mother <- factor(dat.surv$Mother)
dat.surv$Soil <- factor(dat.surv$Soil)
dat.surv$time_dff <- as.character(dat.surv$time_dff) 
dat.surv$time_dff <- as.numeric(dat.surv$time_dff) 

### I think this was the problem... Now, each event is a 1 (but instead of a "death" it is a first flower)
dat.surv$status <- rep(1,19)

fit1 <- coxme(Surv(time_dff, status) ~ Source + Soil + Source*Soil + (1|Source/Mother), data=dat.surv)
fit2 <- coxme(Surv(time_dff, status) ~ Source + Soil + (1|Source/Mother), data=dat.surv)
summary(fit1)
anova(fit1,fit2) #almost, but not quite significant...

quartz(w=6,h=4)
lineplot.CI(Soil,time_dff, group = dat.surv$Source, legend=F, data = dat.surv, 
            cex = 1.5, xlab = "Soil Treatment", ylab = "Days to first flower", 
            cex.lab = 1.2, pch = c(16,16), lwd=2, cex.leg=1.2, 
            main="HP Days to First Flower Reaction norm")
text(x=2,y=210, adj=c(-.3,.5),"PH", cex=1.5)
text(x=2,y=191, adj=c(-.3,.5),"SQ", cex=1.5)
text(x=1,y=210, adj=c(.4,.5),"P = 0.077", cex=1.4)
