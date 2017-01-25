# Mixed model for seed mass
# Author: Natasha T. Krell & Luka Negoita
# Date: 20 November 2016

#install.packages("lme4")
library(lme4)
library(car)

# get data 
setwd("~/Dropbox/LocalAdaptation_DeerIsle/Earlier Folders/Data")
dat = read.csv("Processed_final_2.csv", header=T, sep=",")

# remove NAs where status==2 (these are replicates that never germinated, went missing from the study, or had some other issue such as the wrong species germinated in the soil)
new.dat <- na.omit(dat)

# remove potting soil values
new.dat<-new.dat[!(new.dat$Soil=="P"), ]
new.dat$Soil <- as.factor(as.character(new.dat$Soil))


# Divide up by species 
hp.dat<-new.dat[(new.dat$Species=="HP"), ]
hp.dat

# Analyze Seed_mass with the mixed effects model and the simple "home or away" effect.

hp.dat$Mother <- factor(hp.dat$Mother) # drop unused levels on mother after excluding controls

hp.dat$Seed_mass <- as.character(hp.dat$Seed_mass) # drop unused levels on mother after excluding controls

hp.dat$Seed_mass <- as.numeric(hp.dat$Seed_mass) # drop unused levels on mother after excluding controls

# Remove missing value in row 92 in HP data
hp.dat<- hp.dat[-17,]

# What do the data look like?
hist(log(hp.dat$Seed_mass))

# replace 0s with NAs
hp.dat$Seed_mass[hp.dat$Seed_mass==0] <- NA


#### Now do the analysis:
mod_1 <- lmer(log(Seed_mass) ~ Soil + Source + Source*Soil + (1|Source/Mother), data=hp.dat)
mod_null <- lmer(log(Seed_mass) ~ Soil + Source + (1|Source/Mother), data=hp.dat)

hist(log(hp.dat$Seed_mass))

summary(mod_1)
anova(mod_1,mod_null)

lineplot.CI(Source,log(Seed_mass), group = Soil, legend=T, data = hp.dat, 
            cex = 1.5, xlab = "Source Population", ylab = "Seed_mass (log, ± SEM)", 
            cex.lab = 1.2, pch = c(16,16), lwd=2, cex.leg=1.2, main="HP Seed Mass Reaction norm (n.s.)")

quartz(w=6,h=4)
lineplot.CI(Soil,log(Seed_mass), group = Source, legend=F, data = hp.dat, 
            cex = 1.5, xlab = "Soil Treatment", ylab = "Seed_mass (log, ± SEM)", 
            cex.lab = 1.2, pch = c(16,16), lwd=2, cex.leg=1.2, 
            main="HP Seed Mass Reaction norm")
text(x=1.1,y=-1.2, adj=c(-.3,.5),"PH", cex=1.5)
text(x=1.1,y=-1.8, adj=c(-.3,.5),"SQ", cex=1.5)
text(x=1.7,y=-1.2, adj=c(.4,.5),"P = 0.46", cex=1.4)
