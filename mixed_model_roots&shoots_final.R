# Mixed model for Roots & Shoots data
# Author: Natasha T. Krell & Luka Negoita
# Date: 20 November 2016

#install.packages("lme4")
library(lme4)
#install.packages("lmerTest") 
library(lmerTest)
#install.packages("sciplot") 
library(sciplot)

# download data from dropbox
dat.db <- repmis::source_DropboxData("Processed_final_2.csv",
                                     "laojjl98lwpzfys",
                                     sep = ",",
                                     header = TRUE)

# or grab data from desktop
setwd("~/Dropbox/LocalAdaptation_DeerIsle/Earlier Folders/Data")
dat = read.csv("Processed_final_2.csv", header=T, sep=",")

# remove NAs where status==2 (these are replicates that never germinated, went missing from the study, or had some other issue such as the wrong species germinated in the soil)
new.dat <- na.omit(dat)

# remove potting soil values
new.dat<-new.dat[!(new.dat$Soil=="P"), ]
new.dat$Soil <- factor(new.dat$Soil)

# Divide up by species 
am.dat<-new.dat[(new.dat$Species=="AM"), ]
am.dat
hp.dat<-new.dat[(new.dat$Species=="HP"), ]
hp.dat

###### Hypericum
### Shoots_biomass

#replace 0s with NAs
hp.dat$Shoots_biomass[hp.dat$Shoots_biomass == 0] <- NA

#remove potting soil treatment
hp.dat <- hp.dat[hp.dat$Soil!="P",]
hp.dat$Soil <- as.factor(as.character(hp.dat$Soil))

hist(log(hp.dat$Shoots_biomass)) 

m4 <- lmer(log(Shoots_biomass) ~ Soil + Source + Source*Soil + (1|Source/Mother), data = hp.dat, REML=F)
m5 <- lmer(log(Shoots_biomass) ~ 1 + Soil + Source + (1|Source/Mother), data = hp.dat, REML=F)
summary(m4)
anova(m4, m5, test="Chisq") # there is no significant difference between the slopes
boxplot(log(Shoots_biomass) ~ Source*Soil, data= hp.dat) # indeed, no apparent difference.

quartz(w=6,h=4)
lineplot.CI(Soil,log(Shoots_biomass), group = Source, legend=F, data = hp.dat, 
            cex = 1.5, xlab = "Soil Treatment", ylab = "Shoot Biomass (log, ± SEM)", 
            cex.lab = 1.2, pch = c(16,16), lwd=2, cex.leg=1.2, 
            main="HP Shoot Biomass Reaction norm")
text(x=1.1,y=-1.4, adj=c(-.3,.5),"PH", cex=1.5)
text(x=1.1,y=-1, adj=c(-.3,.5),"SQ", cex=1.5)
text(x=1.7,y=-1, adj=c(.4,.5),"P = 0.77", cex=1.4)



### Belowground_biomass? 
hp.dat$Bg_biomass <- as.numeric(as.character(hp.dat$Bg_biomass)) # make data numeric (was a factor)
hp.dat$Bg_biomass[hp.dat$Bg_biomass == 0] <- NA

hist(log(hp.dat$Bg_biomass))
mod1 <- lmer(log(Bg_biomass) ~ Soil + Source + Source*Soil + (1|Source/Mother), data = hp.dat, REML=F)
mod2 <- lmer(log(Bg_biomass) ~ Soil + Source + (1|Source/Mother), data = hp.dat, REML=F)
summary(mod1)
anova(mod1, mod2, test="Chisq") #not a normal ANOVA, this is the likelihood ratio test

boxplot(log(Bg_biomass) ~ Source*Soil, data= hp.dat) ### difference is noticable

quartz(w=6,h=4)
lineplot.CI(Soil,log(Bg_biomass), group = Source, legend=F, data = hp.dat, 
            cex = 1.5, xlab = "Soil Treatment", ylab = "Belowground Biomass (log, ± SEM)", 
            cex.lab = 1.2, pch = c(16,16), lwd=2, cex.leg=1.2, 
            main="HP Belowground Biomass Reaction norm *")
text(x=2,y=.5, adj=c(-.3,.5),"PH", cex=1.5)
text(x=2,y=-.9, adj=c(-.3,.5),"SQ", cex=1.5)
text(x=1.2,y=0.7, adj=c(.4,.5),"P = 0.049", cex=1.4)



##### Achillea 
### Shoots_biomass
am.dat$Shoots_biomass[am.dat$Shoots_biomass == 0] <- NA
hist(log(am.dat$Shoots_biomass))

m4 <- lmer(log(Shoots_biomass) ~ Soil + Source + Source*Soil + (1|Source/Mother), data = am.dat, REML=F)
m5 <- lmer(log(Shoots_biomass) ~ Soil + Source + (1|Source/Mother), data = am.dat, REML=F)
summary(m4)

anova(m4, m5, test="Chisq") # not a normal ANOVA, this is the likelihood ratio test


quartz(w=6,h=4)
lineplot.CI(Soil,log(Shoots_biomass), group = Source, legend=F, data = am.dat, 
            cex = 1.5, xlab = "Soil Treatment", ylab = "Shoot Biomass (log, ± SEM)", 
            cex.lab = 1.2, pch = c(16,16), lwd=2, cex.leg=1.2, 
            main="AM Shoot Biomass Reaction norm")
text(x=1,y=-1.9, adj=c(-.3,.5),"PH", cex=1.5)
text(x=1.2,y=-1.4, adj=c(-.3,.5),"SQ", cex=1.5)
text(x=1.7,y=-1.4, adj=c(.4,.5),"P = 0.41", cex=1.4)


#### Test for Nishi 1, In AM, does biomass differ on G soil?
dat.temp <- hp.dat[am.dat$Source=="SQ",]
summary(lm(dat.temp$Shoots_biomass~dat.temp$Soil))

plot(log(dat.temp$Shoots_biomass))

### Bg_biomass
am.dat$Bg_biomass <- as.numeric(as.character(am.dat$Bg_biomass)) # make data numeric (was a factor)
am.dat$Bg_biomass[am.dat$Bg_biomass == 0] <- NA

hist(log(am.dat$Bg_biomass))

m4 <- lmer(log(Bg_biomass) ~ Soil + Source + Source*Soil + (1|Source/Mother), data = am.dat, REML=F)
m5 <- lmer(log(Bg_biomass) ~ Soil + Source + (1|Source/Mother), data = am.dat, REML=F)
summary(m4)

anova(m4, m5, test="Chisq") # not a normal ANOVA, this is the likelihood ratio test

lineplot.CI(Source,log(Bg_biomass), group = Soil, legend=T, data = am.dat, 
            cex = 1.5, xlab = "Source Population", ylab = "Belowground Biomass (log, ± SEM)", 
            cex.lab = 1.2, pch = c(16,16), lwd=2, cex.leg=1.2, main="AM Belowground Biomass Reaction norm (n.s.)")

quartz(w=6,h=4)
lineplot.CI(Soil,log(Bg_biomass), group = Source, legend=F, data = am.dat, 
            cex = 1.5, xlab = "Soil Treatment", ylab = "Belowground Biomass (log, ± SEM)", 
            cex.lab = 1.2, pch = c(16,16), lwd=2, cex.leg=1.2, 
            main="AM Belowground Biomass Reaction norm")
text(x=2,y=1, adj=c(-.3,.5),"PH", cex=1.5)
text(x=2,y=1.6, adj=c(-.3,.5),"SQ", cex=1.5)
text(x=1.3,y=1.8, adj=c(.4,.5),"P = 0.42", cex=1.4)



###### Calculate Root & Shoot ratio noted as rsr 
rsr_am <- am.dat$Bg_biomass / am.dat$Shoots_biomass
rsr_hp <- hp.dat$Bg_biomass / hp.dat$Shoots_biomass

hist(log(rsr_am))
hist(log(rsr_hp))

#### Achillea 
mod1 <- lmer(log(rsr_am) ~ Soil + Source + Source*Soil + (1|Source/Mother), data = am.dat, REML=F)
mod2 <- lmer(log(rsr_am) ~ Soil + Source + (1|Source/Mother), data = am.dat, REML=F)
summary(mod1)

anova(mod1, mod2)

lineplot.CI(Source,log(rsr_am), group = Soil, legend=T, data = am.dat, 
            cex = 1.5, xlab = "Source Population", ylab = "Root:Shoot (log, ± SEM)", 
            cex.lab = 1.2, pch = c(16,16), lwd=2, cex.leg=1.2, main="AM Root:Shoot Reaction norm (n.s.)")

quartz(w=6,h=4)
lineplot.CI(Soil,log(rsr_am), group = Source, legend=F, data = am.dat, 
            cex = 1.5, xlab = "Soil Treatment", ylab = "Root:Shoot (log, ± SEM)", 
            cex.lab = 1.2, pch = c(16,16), lwd=2, cex.leg=1.2, 
            main="AM Root:Shoot Reaction norm")
text(x=1,y=3, adj=c(-.3,.5),"PH", cex=1.5)
text(x=1.1,y=2.3, adj=c(-.3,.5),"SQ", cex=1.5)
text(x=1.2,y=4, adj=c(.4,.5),"P = 0.30", cex=1.4)


#### Hypericum
mod1 <- lmer(log(rsr_hp) ~ Soil + Source + Source*Soil + (1|Source/Mother), data = hp.dat, REML=F)
mod2 <- lmer(log(rsr_hp) ~ Soil + Source + (1|Source/Mother), data = hp.dat, REML=F)
summary(mod1)

anova(mod1, mod2)

lineplot.CI(Source,log(rsr_hp), group = Soil, legend=T, data = hp.dat, 
            cex = 1.5, xlab = "Source Population", ylab = "Root:Shoot (log, ± SEM)", 
            cex.lab = 1.2, pch = c(16,16), lwd=2, cex.leg=1.2, main="HP Root:Shoot Reaction norm (n.s.)")

quartz(w=6,h=4)
lineplot.CI(Soil,log(rsr_hp), group = Source, legend=F, data = hp.dat, 
            cex = 1.5, xlab = "Soil Treatment", ylab = "Root:Shoot (log, ± SEM)", 
            cex.lab = 1.2, pch = c(16,16), lwd=2, cex.leg=1.2, 
            main="HP Root:Shoot Reaction norm")
text(x=2,y=2.2, adj=c(-.3,.5),"PH", cex=1.5)
text(x=2,y=1, adj=c(-.3,.5),"SQ", cex=1.5)
text(x=1.2,y=2.2, adj=c(.4,.5),"P = 0.08", cex=1.4)

