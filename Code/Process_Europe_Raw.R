###################################
## Procesing Europe Raw data
###################################
#R packages
require(dlnm)
require(mgcv)
require(visreg)
require(mctest)
require(rlist)
require(ggplot2)
require(RColorBrewer)
require(gridExtra)
require(dplyr)
require(patchwork)



######################
# FULL PERIOD
######################
setwd("Data")
load("EuropeRawData.RData")
setwd("..")

period <- seq.Date(from = as.Date("2020-06-01"), to = as.Date("2021-12-31"), 
    by = "1 day")
dates <- as.character(period)


#Variables
Rt <- EuropeRawData$Rt[, dates]
newCases <- EuropeRawData$newCasesTotal[, dates]
newDeaths <- EuropeRawData$newDeaths[, dates]

#Independent variables
Temperature <- EuropeRawData$Temperature[,dates]
SH <- EuropeRawData$SpecificHumidity[, dates]
SH <- SH * 1000 #pass to g/kg
Vac <- EuropeRawData$PercentagedCompletedVaccinated[,dates]
Vac[is.na(Vac)] <- 0
StringencyIndex <- EuropeRawData$StringencyIndex[, dates]
Retail <- EuropeRawData$mobility_retail_and_recreation[, dates]
Work <- EuropeRawData$mobility_workplaces[, dates]
Residential <- EuropeRawData$mobility_residential[, dates]

#Main COVID-19 Variant
variants <- rep(0, length(colnames(EuropeRawData$CasesTotal)))
names(variants) <- colnames(EuropeRawData$CasesTotal)
#which(names(variants)=="2020-12-28")
#which(names(variants)=="2021-07-05")
#which(names(variants)=="2021-12-20")


#Europe 7 days lag with Spain
#alpha
variants[which(names(variants)=="2020-12-28"):which(names(variants)=="2021-07-05")] <- 1
#delta
variants[which(names(variants)=="2021-07-05"):which(names(variants)=="2021-12-26")] <- 2
#omicron
variants[which(names(variants)=="2021-12-20"):length(variants)] <- 3
variants <- factor(variants, levels = c("0","1","2","3"), labels=c("Original", "Alpha", "Delta", "Omicron"))
variants <- variants[dates]


setwd("Data")
save(file = "FullPeriod_Europe.RData", StringencyIndex, newCases, newDeaths,
     Rt, SH, Temperature, Vac, variants)
setwd("..")
rm(list = ls())


####################
### INITIAL PERIOD
####################
setwd("Data")
load("EuropeRawData.RData")
setwd("..")

period <- seq.Date(from = as.Date("2020-06-01"), to = as.Date("2020-12-31"), 
    by = "1 day")
dates <- as.character(period)
datesLag <- as.character(period-14)



#Dependent variable
Re <- EuropeRawData$Rt[, dates]

#Independent variables
Temperature <- EuropeRawData$Temperature[,datesLag]
SH <- EuropeRawData$SpecificHumidity[, datesLag]
SH <- SH * 1000 #pass to g/kg
Vac <- EuropeRawData$PercentagedCompletedVaccinated[,datesLag]
Vac[is.na(Vac)] <- 0
StringencyIndex <- EuropeRawData$StringencyIndex[, datesLag]
Retail <- EuropeRawData$mobility_retail_and_recreation[, datesLag]
Work <- EuropeRawData$mobility_workplaces[, datesLag]
Residential <- EuropeRawData$mobility_residential[, datesLag]


#Main COVID-19 Variant
variants <- rep(0, length(colnames(EuropeRawData$CasesTotal)))
names(variants) <- colnames(EuropeRawData$CasesTotal)
#which(names(variants)=="2020-12-28")
#which(names(variants)=="2021-07-05")
#which(names(variants)=="2021-12-20")
#Europe 7 days lag with Spain
#alpha
variants[which(names(variants)=="2020-12-28"):which(names(variants)=="2021-07-05")] <- 1
#delta
variants[which(names(variants)=="2021-07-05"):which(names(variants)=="2021-12-26")] <- 2
#omicron
variants[which(names(variants)=="2021-12-20"):length(variants)] <- 3
variants <- factor(variants, levels = c("0","1","2","3"), labels=c("Original", "Alpha", "Delta", "Omicron"))
variants <- variants[dates]


setwd("Data")
save(file = "P1_Europe.RData", StringencyIndex,
    Re, SH, Temperature, Vac, variants)
setwd("..")
rm(list = ls())


####################
### FINAL PERIOD
####################
setwd("Data")
load("EuropeRawData.RData")
setwd("..")

period <- seq.Date(from = as.Date("2021-06-01"), to = as.Date("2021-12-31"), 
    by = "1 day")
dates <- as.character(period)
datesLag <- as.character(period-7)

#Dependent variables
Re <- EuropeRawData$Rt[, dates]


#Independent variables
Temperature <- EuropeRawData$Temperature[,datesLag]
SH <- EuropeRawData$SpecificHumidity[, datesLag]
SH <- SH * 1000 # pass to g/kg
Vac <- EuropeRawData$PercentagedCompletedVaccinated[,datesLag]
Vac[is.na(Vac)] <- 0
StringencyIndex <- EuropeRawData$StringencyIndex[, datesLag]

#Main COVID-19 Variant
variants <- rep(0, length(colnames(EuropeRawData$CasesTotal)))
names(variants) <- colnames(EuropeRawData$CasesTotal)
#which(names(variants)=="2020-12-28")
#which(names(variants)=="2021-07-05")
#which(names(variants)=="2021-12-20")


#Europe 7 days lag with Spain
#alpha
variants[which(names(variants)=="2020-12-28"):which(names(variants)=="2021-07-05")] <- 1
#delta
variants[which(names(variants)=="2021-07-05"):which(names(variants)=="2021-12-26")] <- 2
#omicron
variants[which(names(variants)=="2021-12-20"):length(variants)] <- 3
variants <- factor(variants, levels = c("0","1","2","3"), labels=c("Original", "Alpha", "Delta", "Omicron"))
variants <- variants[dates]


setwd("Data")
save(file = "P2_Europe.RData", StringencyIndex,
     Re, SH, Temperature, Vac, variants)
setwd("..")
rm(list = ls())

################################################################################
##################### Prepare data for dlnm
################################################################################
setwd("Data")
load("FullPeriod_Europe.RData")
setwd("..")

Re <- Rt
days <- 1:length(colnames(Re))
names(days) <- colnames(Re)


#Select only period 1 and period 2
p1 <- which(colnames(Re)=="2020-06-01"):which(colnames(Re)=="2020-12-31")
p2 <- which(colnames(Re)=="2021-06-01"):which(colnames(Re)=="2021-12-31")

Re <- Re[,c(p1,p2)]
Temperature <- Temperature[,c(p1,p2)]
SH <- SH[, c(p1,p2)]
StringencyIndex <- StringencyIndex[, c(p1,p2)]
Vac <- Vac[, c(p1,p2)]
days <- days[c(p1,p2)]
dates <- colnames(Re)
variants <- variants[c(p1,p2)]

regions <- unique(rownames(Re))

list_reg <- list()

for(region in regions){
    df_reg <- data.frame(
        dates,
        days,
        Re = Re[rownames(Re) == region,],
        Temperature = Temperature[rownames(Temperature) == region,],
        SH = SH[rownames(Re) == region,],
        StringencyIndex = StringencyIndex[rownames(StringencyIndex) == region,],
        Vac = Vac[rownames(Vac) == region,],
        variants
    )
    list_reg[[region]] <- df_reg
}

list_reg[[8]] <- NULL

setwd("Data")
save(list_reg, file = "Europe_dlnm.RData")
setwd("..")

rm(list = ls())

