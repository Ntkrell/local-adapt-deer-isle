# Mixed model fpr number of flowers
# Author: Natasha T. Krell & Luka Negoita
# Date: 20 November 2016

# Zero-inflated negative binomial model for number of flowers 
#install.packages("pscl")
library(pscl)
require(MASS)
require(boot)

setwd("~/Dropbox/LocalAdaptation_DeerIsle/Earlier Folders/Data")

# Load data
dat = read.csv("Processed_final_2.csv", header=T, sep=",")

# remove NAs where status==2 (these are replicates that never germinated, went missing from the study, or had some other issue such as the wrong species germinated in the soil)
new.dat <- na.omit(dat)

# Divide up by species (just HP)
hp.dat<-new.dat[(new.dat$Species=="HP"), ] #length = 18
hp.dat

# Remove controls:
hp.dat <- hp.dat[hp.dat$Tag!="control",]

flower_num <- hp.dat$No_flowers #there are a good amount of zeroes, need to zero-inflate the model

##################################
### To do the zero inflated neg binom model with random effects, we have to do the models separately

#### First model is to see whether the plant flowered or not:

### Make our first data set (does it have flowers: 0 or 1):
flwr_0_1 <- flower_num/flower_num 
flwr_0_1[is.na(flwr_0_1)] <- 0
flwr_0_1 <- as.factor(flwr_0_1)

### So, because this first model is flwr_0_1 ~ Home_or_away, the appropriate test is actually a Chi-square test
Source_x_Soil <- paste(hp.dat$Source,".",hp.dat$Soil, sep="")

data.frame(flwr_0_1,Source_x_Soil)

### Look at a contingency table (how many 0s or 1s do home and away have):
table(flwr_0_1,Source_x_Soil)

### Here is the actual Chi squared test
tbl <- table(flwr_0_1,Source_x_Soil)
chisq.test(tbl)
### Not significant

##### Now on to the negative binomial model

#replace zeros with NAs
flower_num[flower_num==0] <- NA

#remove potting soil treatment
hp.dat <- hp.dat[hp.dat$Soil!="P",]
hp.dat$Soil <- as.factor(as.character(hp.dat$Soil))

### Some issues fitting these models...
mod_1 <- glmer.nb(flower_num ~ Soil + Source + Source*Soil + (1|Source/Mother), data=hp.dat)
mod_null <- glmer.nb(flower_num ~ Soil + Source + (1|Source/Mother), data=hp.dat)

# ### This continues running the model optimization till it fits.
# ss <- getME(mod_1,c("theta","fixef"))
# mod_1_re <- update(mod_1,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
# ss <- getME(mod_null,c("theta","fixef"))
# mod_null_re <- update(mod_null,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))

summary(mod_1)
anova(mod_1,mod_null)

quartz(w=6,h=4)
lineplot.CI(Soil,flower_num, group = Source, legend=F, data = hp.dat, 
            cex = 1.5, xlab = "Soil Treatment", ylab = "Flower Number", 
            cex.lab = 1.2, pch = c(16,16), lwd=2, cex.leg=1.2, 
            main="HP Flower Number Reaction norm")
text(x=1.1,y=70, adj=c(-.3,.5),"PH", cex=1.5)
text(x=1.1,y=40, adj=c(-.3,.5),"SQ", cex=1.5)
text(x=1.7,y=60, adj=c(.4,.5),"P = 0.46", cex=1.4)
