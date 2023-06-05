###################################
## Procesing Spanish Raw data
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
load("SpainRawData.RData")
setwd("..")


period <- seq.Date(from = as.Date("2020-06-01"), to = as.Date("2021-12-31"), 
    by = "1 day")
dates <- as.character(period)

#Variables
Re <- SpainData$Rt[,dates]
newCases <- SpainData$newCasesTotal[,dates]
newDeaths <- SpainData$newDeaths[,dates]
Temperature <- SpainData$Temperature[,dates]

SH <- SpainData$SpecificHumidity
rownames(SH) <- c("Andalucía", "Aragón", "Principado de Asturias", 
    "Illes Balears", "Comunitat Valenciana", "Canarias", "Cantabria",
    "Castilla-La Mancha", "Castilla y León", "Cataluña", "Extremadura",
    "Galicia", "La Rioja", "Comunidad de Madrid", "Región de Murcia",
    "Comunidad Foral de Navarra", "País Vasco", "Ciudad de Ceuta", 
    "Ciudad de Melilla")
SH <- SH[rownames(Re),dates]


#Vaccination
Vac <- SpainData$CompletedVaccinated
rownames(Vac) <- c("Andalucía", "Aragón", "Principado de Asturias", 
    "Illes Balears", "Comunitat Valenciana", "Canarias", "Cantabria",
    "Castilla-La Mancha", "Castilla y León", "Cataluña", "Extremadura",
    "Galicia", "La Rioja", "Comunidad de Madrid", "Región de Murcia",
    "Comunidad Foral de Navarra", "País Vasco", "Ciudad de Ceuta", 
    "Ciudad de Melilla")
Vac <- Vac[rownames(Re),dates]
Vac2 <- SpainData$PercentagedCompletedVaccinated[,
    as.character(seq.Date(from = as.Date("2021-01-04"), 
        to = as.Date("2021-12-31"), by = "1 day"))]
Vac[,colnames(Vac2)] <- Vac2
Vac[is.na(Vac)] <- 0

#Control of the goverment StringencyIndex
StringencyIndex <- SpainData$StringencyIndex
rownames(StringencyIndex) <- c("Andalucía", "Aragón", "Principado de Asturias", 
    "Illes Balears", "Comunitat Valenciana", "Canarias", "Cantabria",
    "Castilla-La Mancha", "Castilla y León", "Cataluña", "Extremadura",
    "Galicia", "La Rioja", "Comunidad de Madrid", "Región de Murcia",
    "Comunidad Foral de Navarra", "País Vasco", "Ciudad de Ceuta", 
    "Ciudad de Melilla")
StringencyIndex <- StringencyIndex[rownames(Re),dates]



#Main COVID-19 Variant
variants <- rep(0, length(colnames(SpainData$CasesTotal)))
names(variants) <- colnames(SpainData$CasesTotal)
#which(names(variants)=="2020-12-21")
#which(names(variants)=="2021-06-28")
#which(names(variants)=="2021-12-13")


#alpha
variants[which(names(variants)=="2020-12-21"):which(names(variants)=="2021-06-28")] <- 1
#delta
variants[which(names(variants)=="2021-06-28"):which(names(variants)=="2021-12-13")] <- 2
#omicron
variants[which(names(variants)=="2021-12-13"):length(variants)] <- 3
variants <- factor(variants, levels = c("0","1","2","3"), labels=c("Original", "Alpha", "Delta", "Omicron"))
variants <- variants[dates]

#Quitar Ceuta y Melilla y Canarias
Re <- Re[-c(3,8,9),]
newCases <- newCases[-c(3,8,9),]
newDeaths <- newDeaths[-c(3,8,9),]
Vac <- Vac[-c(3,8,9),]
StringencyIndex <- StringencyIndex[-c(3,8,9),]
SH <- SH[-c(3,8,9),] 
SH <- SH * 1000 # pass SH to g/kg
Temperature <- Temperature[-c(3,8,9),]


setwd("Data")
save(Re, newCases, newDeaths, Temperature, SH, StringencyIndex,
    Vac, variants,file = "FullPeriod_Spain.RData")
setwd("..")
rm(list = ls())


#############################################################
### Initial Period
#############################################################
setwd("Data")
load("SpainRawData.RData")
setwd("..")

period <- seq.Date(from = as.Date("2020-06-01"), to = as.Date("2020-12-31"), 
    by = "1 day")
dates <- as.character(period)
datesLag <- as.character(period-14) #Lag of 14 days

################# Depedent variable
Re <- SpainData$Rt[,dates]

################# Independent variables
#Temperature
Temperature <- SpainData$Temperature[,datesLag]

#Specific humidity
SH <- SpainData$SpecificHumidity
rownames(SH) <- c("Andalucía", "Aragón", "Principado de Asturias", 
    "Illes Balears", "Comunitat Valenciana", "Canarias", "Cantabria",
    "Castilla-La Mancha", "Castilla y León", "Cataluña", "Extremadura",
    "Galicia", "La Rioja", "Comunidad de Madrid", "Región de Murcia",
    "Comunidad Foral de Navarra", "País Vasco", "Ciudad de Ceuta", 
    "Ciudad de Melilla")
SH <- SH[rownames(Re),datesLag]


#Vaccination
Vac <- SpainData$CompletedVaccinated
rownames(Vac) <- c("Andalucía", "Aragón", "Principado de Asturias", 
    "Illes Balears", "Comunitat Valenciana", "Canarias", "Cantabria",
    "Castilla-La Mancha", "Castilla y León", "Cataluña", "Extremadura",
    "Galicia", "La Rioja", "Comunidad de Madrid", "Región de Murcia",
    "Comunidad Foral de Navarra", "País Vasco", "Ciudad de Ceuta", 
    "Ciudad de Melilla")
Vac <- Vac[rownames(Re),datesLag]
Vac[is.na(Vac)] <- 0

#Control of the goverment StringencyIndex
StringencyIndex <- SpainData$StringencyIndex
rownames(StringencyIndex) <- c("Andalucía", "Aragón", "Principado de Asturias", 
    "Illes Balears", "Comunitat Valenciana", "Canarias", "Cantabria",
    "Castilla-La Mancha", "Castilla y León", "Cataluña", "Extremadura",
    "Galicia", "La Rioja", "Comunidad de Madrid", "Región de Murcia",
    "Comunidad Foral de Navarra", "País Vasco", "Ciudad de Ceuta", 
    "Ciudad de Melilla")
StringencyIndex <- StringencyIndex[rownames(Re),datesLag]

#Main COVID-19 Variant
variants <- rep(0, length(colnames(SpainData$CasesTotal)))
names(variants) <- colnames(SpainData$CasesTotal)
#which(names(variants)=="2020-12-21")
#which(names(variants)=="2021-06-28")
#which(names(variants)=="2021-12-13")

#alpha
variants[which(names(variants)=="2020-12-21"):which(names(variants)=="2021-06-28")] <- 1
#delta
variants[which(names(variants)=="2021-06-28"):which(names(variants)=="2021-12-13")] <- 2
#omicron
variants[which(names(variants)=="2021-12-13"):length(variants)] <- 3
variants <- factor(variants, levels = c("0","1","2","3"), labels=c("Original", "Alpha", "Delta", "Omicron"))
variants <- variants[dates]

#Quitar Ceuta y Melilla
Re <- Re[-c(3,8,9),]
#newCases <- newCases[-c(8,9),]
Vac <- Vac[-c(3,8,9),]
StringencyIndex <- StringencyIndex[-c(3,8,9),]
SH <- SH[-c(3,8,9),]
SH <- SH * 1000 #pass SH to g/Kg
Temperature <- Temperature[-c(3,8,9),]


setwd("Data")
save(file = "P1_Spain.RData", StringencyIndex, Re, SH, 
    Temperature, Vac, variants)
setwd("..")
rm(list = ls())

###############################################################################
############### Final Period P2
##############################################################################
setwd("Data")
load("SpainRawData.RData")
setwd("..")

period <- seq.Date(from = as.Date("2021-06-01"), to = as.Date("2021-12-31"), 
    by = "1 day")
dates <- as.character(period)
datesLag <- as.character(period-14) #Lag of 14 days

################# Depedent variables
Re <- SpainData$Rt[,dates]

################# Independent variables
#Temperature
Temperature <- SpainData$Temperature[,datesLag]

#Specific humidity
SH <- SpainData$SpecificHumidity
rownames(SH) <- c("Andalucía", "Aragón", "Principado de Asturias", 
    "Illes Balears", "Comunitat Valenciana", "Canarias", "Cantabria",
    "Castilla-La Mancha", "Castilla y León", "Cataluña", "Extremadura",
    "Galicia", "La Rioja", "Comunidad de Madrid", "Región de Murcia",
    "Comunidad Foral de Navarra", "País Vasco", "Ciudad de Ceuta", 
    "Ciudad de Melilla")
SH <- SH[rownames(Re),datesLag]


#Vaccination
Vac <- SpainData$CompletedVaccinated
rownames(Vac) <- c("Andalucía", "Aragón", "Principado de Asturias", 
    "Illes Balears", "Comunitat Valenciana", "Canarias", "Cantabria",
    "Castilla-La Mancha", "Castilla y León", "Cataluña", "Extremadura",
    "Galicia", "La Rioja", "Comunidad de Madrid", "Región de Murcia",
    "Comunidad Foral de Navarra", "País Vasco", "Ciudad de Ceuta", 
    "Ciudad de Melilla")
Vac <- Vac[rownames(Re),datesLag]
Vac2 <- SpainData$PercentagedCompletedVaccinated[,
    as.character(seq.Date(from = as.Date("2021-05-18"), 
        to = as.Date("2021-12-17"), by = "1 day"))]
Vac[,colnames(Vac2)] <- Vac2
Vac[is.na(Vac)] <- 0

#Control of the goverment StringencyIndex
StringencyIndex <- SpainData$StringencyIndex
rownames(StringencyIndex) <- c("Andalucía", "Aragón", "Principado de Asturias", 
    "Illes Balears", "Comunitat Valenciana", "Canarias", "Cantabria",
    "Castilla-La Mancha", "Castilla y León", "Cataluña", "Extremadura",
    "Galicia", "La Rioja", "Comunidad de Madrid", "Región de Murcia",
    "Comunidad Foral de Navarra", "País Vasco", "Ciudad de Ceuta", 
    "Ciudad de Melilla")
StringencyIndex <- StringencyIndex[rownames(Re),datesLag]


#Main COVID-19 Variant
variants <- rep(0, length(colnames(SpainData$CasesTotal)))
names(variants) <- colnames(SpainData$CasesTotal)
#which(names(variants)=="2020-12-21")
#which(names(variants)=="2021-06-28")
#which(names(variants)=="2021-12-13")

#alpha
variants[which(names(variants)=="2020-12-21"):which(names(variants)=="2021-06-28")] <- 1
#delta
variants[which(names(variants)=="2021-06-28"):which(names(variants)=="2021-12-13")] <- 2
#omicron
variants[which(names(variants)=="2021-12-13"):length(variants)] <- 3
variants <- factor(variants, levels = c("0","1","2","3"), labels=c("Original", "Alpha", "Delta", "Omicron"))
variants <- variants[dates]

#Quitar Ceuta y Melilla
Re <- Re[-c(3,8,9),]
#newCases <- newCases[-c(8,9),]
Vac <- Vac[-c(3,8,9),]
StringencyIndex <- StringencyIndex[-c(3,8,9),]
SH <- SH[-c(3,8,9),]
SH <- SH * 1000 #pass to g/kg
Temperature <- Temperature[-c(3,8,9),]

setwd("Data")
save(file = "P2_Spain.RData", StringencyIndex,
    Re, SH, Temperature, Vac, variants)
setwd("..")
rm(list = ls())

################################################################################
##################### Prepare data for dlnm
################################################################################
setwd("Data")
load("FullPeriod_Spain.RData")
setwd("..")

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

names(list_reg)[c(1,2,4,5,6,7,8,9,12,14,15,16)] <- c("Andalusia", "Aragon",
    "Castilla y Leon ","Castille-La Mancha",
    "Catalonia", "Madrid", "Navarre", "C. Valenciana", "The Balearic Islands",
    "Basque Country", "Asturias", "Murcia")


setwd("Data")
save(list_reg, file = "Spain_dlnm.RData")
setwd("..")

rm(list = ls())











