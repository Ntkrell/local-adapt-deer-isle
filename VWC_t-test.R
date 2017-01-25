# T-test comparing Volumetric Water Content (VWC) at two Deer Isle sites
# Author: Natasha T. Krell and Luka Negoita
# Date: 20 November 2016

# install pckgs
install.packages('repmis', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(repmis)

# download data from dropbox
dat.db.DI <- source_DropboxData("Processed_Field_TDR.csv",
                                     "sh427j0qqbmgbq0",
                                     sep = ",",
                                     header = TRUE) # Deer Isle data

dat.db.GC <- source_DropboxData("Processed_GrowthChamber_TDR.csv",
                                        "dmv1s1qb3vr7qab",
                                        sep = ",",
                                        header = TRUE) # Growth Chamber data 

# or pull from local machine
dat.DI <- read.csv("/Volumes/LACIE SHARE/Desktop_LocalAdaptationPaper_DeerIsle/Data/VWC/processed/Processed_TDR_Field.csv") # Deer Isle
dat.GC <- read.csv("/Volumes/LACIE SHARE/Desktop_LocalAdaptationPaper_DeerIsle/Data/VWC/processed/Processed_TDR_GrowthChamber.csv") # Growth Chamber 

# independent 2-group t-test to analyze Deer Isle VWC

# Deer Isle 
y <- subset(dat.DI, select=c("Location.1", "Reading"))
y1 <- y[(dat.DI$Location.1=="PH"), ]
y2 <- y[(dat.DI$Location.1=="SQ"), ]

t.test(y1$Reading,y2$Reading) #p-value = 0.5245

# Growth Chamber
z <- subset(dat.GC, select=c("Source", "Measurement"))
z1 <- z[(dat.GC$Source=="PH"), ]
z2 <- z[(dat.GC$Source=="SQ"), ]

t.test(z1$Measurement,z2$Measurement) #p-value = 0.3825
