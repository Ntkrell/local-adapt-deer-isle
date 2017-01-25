# T-test comparing Temperature (degC) at two Deer Isle sites using iButtons 
# Author: Natasha T. Krell & Luka Negoita
# Date: 20 November 2016

# download from dropbox
dat.db.DI <- repmis::source_DropboxData("Processed_DeerIsle_Temps.csv",
                                          "z3czk6gllhxn1a5",
                                          sep = ",",
                                          header = TRUE)

# or pull from local machine
dat.DI <- read.csv("/Volumes/LACIE SHARE/Desktop_LocalAdaptationPaper_DeerIsle/Data/EnvironmentalConditions/processed/Processed_DeerIsle_Temps.csv") # Deer Isle

# independent 2-group t-test to analyze Deer Isle VWC

# First compare iButton temps within site - should be no significant difference
y1 <- dat.DI$"PH1_TEMP"
y2 <- dat.DI$"PH2_TEMP"
t.test(y1,y2) #p-value =  0.9637

z1 <- dat.DI$"SQ1_TEMP"
z2 <- dat.DI$"SQ2_TEMP"
t.test(z1,z2) #p-value =  0.5148

# Compare sites using PH1 & SQ1
t.test(y1,z1) #p-value = 0.00263
