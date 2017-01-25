# Descriptive statistics + Mixed model comparing flowering phenology at two Deer Isle sites
# Author: Natasha T. Krell & Luka Negoita
# Date: 20 November 2016

# pull data from local machine
dat.DI <- read.csv("/Volumes/LACIE SHARE/Desktop_LocalAdaptationPaper_DeerIsle/Data/DI_Phenology/Processed/FloweringPhenology_DeerIsle_Summer2015.csv") # Deer Isle
dat.GC <- read.csv("/Volumes/LACIE SHARE/Desktop_LocalAdaptationPaper_DeerIsle/Data/GC_Phenology/processed/Processed_GrowthChamber_Phenology.csv") # Growth Chamber 

### Deer Isle Data

# only use Hypericum data - subset data by species
y <- subset(dat.DI, Species=="HP", select=c(Location, First.Bloom,  Mid.Bloom,	Full.Bloom))

# remove NAs
y <- y[complete.cases(y[,1:2]),]

# compare date of first flower 
y1 <- y[(y$Location=="PH"), ]
y2 <- y[(y$Location=="SQ"), ]

mean(y1$First.Bloom) # mean date of first flower for PH is 190.3
sd(y1$First.Bloom) #s.dev is 2.89
mean(y2$First.Bloom) # mean date of first flower for SQ is 183.5
sd(y2$First.Bloom) #s. dev is 3.67

### Growth Chamber Data 

# remove NaN and missing values 
z <- dat.GC[dat.GC$DOY!="missing",]
z1 <- z[z$DOY!="NaN",]

# Mixed model?

install.packages("pscl")
install.packages("lme4")
install.packages("sciplot")
library(sciplot)
library(lme4)
library(pscl)
require(MASS)
require(boot)

# Divide up by species 
am.dat<-z1[(z1$Species=="AM"), ] 
hp.dat<-z1[(z1$Species=="HP"), ] 

# Remove controls:
am.dat <- am.dat[hp.dat$Soil!="P",]
hp.dat <- hp.dat[hp.dat$Soil!="P",]

# Hypericum:
mod_1 <- lmer(DOY ~ Soil + Source + Source*Soil + (1|Source/Mother), data=hp.dat)
mod_null <- lmer(DOY ~ Soil + Source + (1|Source/Mother), data=hp.dat)

summary(mod_1)
anova(mod_1,mod_null) #p-value = 0.05962 

quartz(w=6,h=4)
lineplot.CI(Soil,DOY, group = Source, legend=F, data = hp.dat, 
            cex = 1.5, xlab = "Soil Treatment", ylab = "DOY of First Flower", 
            cex.lab = 1.2, pch = c(16,16), lwd=2, cex.leg=1.2, 
            main="HP DFF Reaction norm")
text(x=1.1,y=70, adj=c(-.3,.5),"PH", cex=1.5)
text(x=1.1,y=40, adj=c(-.3,.5),"SQ", cex=1.5)
text(x=1.7,y=60, adj=c(.4,.5),"P = 0.46", cex=1.4)


# Achillea?
mod_1 <- lmer(DOY ~ Soil + Source + Source*Soil + (1|Source/Mother), data=am.dat)
mod_null <- lmer(DOY ~ Soil + Source + (1|Source/Mother), data=am.dat)
