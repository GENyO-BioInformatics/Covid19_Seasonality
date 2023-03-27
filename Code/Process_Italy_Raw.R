###################################
## Procesing Europe Raw data
###################################
#R packages
library(dlnm)
library(mgcv)
library(visreg)
library(corrplot)
library(mctest)
library(rlist)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(oddsratio)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(forestplot)
library(metafor)

######################
# FULL PERIOD
######################
setwd("RawData")
load("ItalyRawData.RData")
setwd("..")



period <- seq.Date(from = as.Date("2020-06-01"), to = as.Date("2021-12-31"), 
    by = "1 day")
dates <- as.character(period)

#Dependent Variable
Re <- ItalyRawData$Rt[, dates]

#Remove Bolzano y Valle d'costa
Re <- Re[-c(18, 20),]
Regions <- rownames(Re)
#Independent variable
newCases <- ItalyRawData$newCasesTotal[Regions, dates]
newDeaths <- ItalyRawData$newDeaths[Regions, dates]

#Independent variables
Temperature <- ItalyRawData$Temperature[Regions,dates]
SH <- ItalyRawData$SpecificHumidity[Regions, dates]
SH <- SH * 1000 # Pass to g/kg
StringencyIndex <- ItalyRawData$StringencyIndex[Regions, dates]
Vac <- ItalyRawData$PercentagedCompletedVaccinated[Regions, dates]



#Main COVID-19 Variant
variants <- rep(0, length(colnames(ItalyRawData$CasesTotal)))
names(variants) <- colnames(ItalyRawData$CasesTotal)
##### Lag of 7 days between Italy and Spain


#which(names(variants)=="2020-12-28")
#which(names(variants)=="2021-07-05")
#which(names(variants)=="2021-12-20")
#alpha
variants[which(names(variants)=="2020-12-28"):which(names(variants)=="2021-07-05")] <- 1
#delta
variants[which(names(variants)=="2021-07-05"):which(names(variants)=="2021-12-26")] <- 2
#omicron
variants[which(names(variants)=="2021-12-20"):length(variants)] <- 3
variants <- factor(variants, levels = c("0","1","2","3"), 
    labels=c("Original", "Alpha", "Delta", "Omicron"))
variants <- variants[dates]


setwd("Data")
save(file = "FullPeriod_Italy.RData", StringencyIndex, newCases, newDeaths,
    Re, SH, Temperature, Vac, variants)
setwd("..")
rm(list = ls())



####################
### INITIAL PERIOD
####################
setwd("RawData")
load("ItalyRawData.RData")
setwd("..")

period <- seq.Date(from = as.Date("2020-06-01"), to = as.Date("2020-12-31"), 
    by = "1 day")
dates <- as.character(period)
datesLag <- as.character(period-14)


#Dependent variables
Re <- ItalyRawData$Rt[, dates]
#Remove  Bolzano y Valle d'costa
Re <- Re[-c(18, 20),]
Regions <- rownames(Re)

#Independent variables
newCases <- ItalyRawData$newCasesTotal[Regions, dates]
Temperature <- ItalyRawData$Temperature[Regions,datesLag]
SH <- ItalyRawData$SpecificHumidity[Regions, datesLag]
SH <- SH * 1000 #pass to g/kg
StringencyIndex <- ItalyRawData$StringencyIndex[Regions, datesLag]
Vac <- ItalyRawData$PercentagedCompletedVaccinated[Regions,dates]


#Main COVID-19 Variant
variants <- rep(0, length(colnames(ItalyRawData$CasesTotal)))
names(variants) <- colnames(ItalyRawData$CasesTotal)
##### Lag of 7 days between Italy and Spain
#which(names(variants)=="2020-12-28")
#which(names(variants)=="2021-07-05")
#which(names(variants)=="2021-12-20")

#alpha
variants[which(names(variants)=="2020-12-28"):which(names(variants)=="2021-07-05")] <- 1
#delta
variants[which(names(variants)=="2021-07-05"):which(names(variants)=="2021-12-26")] <- 2
#omicron
variants[which(names(variants)=="2021-12-20"):length(variants)] <- 3
variants <- factor(variants, levels = c("0","1","2","3"), 
    labels=c("Original", "Alpha", "Delta", "Omicron"))
variants <- variants[dates]


setwd("Data")
save(file = "P1_Italy.RData", StringencyIndex, newCases,
    Re, SH, Temperature, Vac, variants)
setwd("..")
rm(list = ls())


####################
### FINAL PERIOD
####################
setwd("RawData")
load("ItalyRawData.RData")
setwd("..")

period <- seq.Date(from = as.Date("2021-06-01"), to = as.Date("2021-12-31"), 
    by = "1 day")
dates <- as.character(period)
datesLag <- as.character(period-7)


#Dependent variable
Re <- ItalyRawData$Rt[, dates]
#Remove Bolzano y Valle d'costa
Re <- Re[-c(18, 20),]
Regions <- rownames(Re)

#Independent variables
Temperature <- ItalyRawData$Temperature[Regions,datesLag]
newCases <- ItalyRawData$newCasesTotal[Regions, dates]
SH <- ItalyRawData$SpecificHumidity[Regions, datesLag]
SH <- SH * 1000 #pass to g/kg
StringencyIndex <- ItalyRawData$StringencyIndex[Regions, datesLag]
Vac <- ItalyRawData$PercentagedCompletedVaccinated[Regions,datesLag]

#Main COVID-19 Variant
variants <- rep(0, length(colnames(ItalyRawData$CasesTotal)))
names(variants) <- colnames(ItalyRawData$CasesTotal)
##### Lag of 7 days between Italy and Spain
#which(names(variants)=="2020-12-28")
#which(names(variants)=="2021-07-05")
#which(names(variants)=="2021-12-20")


#alpha
variants[which(names(variants)=="2020-12-28"):which(names(variants)=="2021-07-05")] <- 1
#delta
variants[which(names(variants)=="2021-07-05"):which(names(variants)=="2021-12-26")] <- 2
#omicron
variants[which(names(variants)=="2021-12-20"):length(variants)] <- 3
variants <- factor(variants, levels = c("0","1","2","3"), labels=c("Original", "Alpha", "Delta", "Omicron"))
variants <- variants[dates]



setwd("Data")
save(file = "P2_Italy.RData", StringencyIndex, newCases,
    Re, SH, Temperature, Vac, variants)
setwd("..")
rm(list = ls())




