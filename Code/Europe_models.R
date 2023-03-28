##########################
#### Europe Code
##########################

#Neccessary packages
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


####################
## Initial Period
####################

####################
## Period P1
####################

#Load data and model functions
load("Data/P1_Europe.RData")
source("Code/GAM_Function.R")

#########################
# Apply GAM models
#########################

#Relation between transmission and temperature
res_P1_Temp_Europe <- GamModel_Temp(Re, Temperature, StringencyIndex, 
    Vac, variants)


#Relation between transmission and relative humidity
res_P1_SH_Europe <- GamModel_SH(Re, SH, StringencyIndex, Vac, variants)

#Save models results P1 adn data
setwd("Data")
save(res_P1_Temp_Europe, res_P1_SH_Europe,
    file = "GAM_Europe_P1.RData")
setwd("..")

#Save tables models results P1
dir.create("Tables", showWarnings = FALSE)
setwd("Tables")
#Calculate adjust p-values
res_P1_Temp_Europe$Gam[,3:6] <- apply(res_P1_Temp_Europe$Gam[,3:6], 2, function(x){
    results <- p.adjust(x, method = "fdr")  
})
write.csv(res_P1_Temp_Europe$Gam, file = "Results_P1_Temperature_Europe.csv")

#Calculate adjust p-values
res_P1_SH_Europe$Gam[,3:6] <- apply(res_P1_SH_Europe$Gam[,3:6], 2, function(x){
    results <- p.adjust(x, method = "fdr")  
})
write.csv(res_P1_SH_Europe$Gam, file = "Results_P1_SH_Europe.csv")
setwd("..")
rm(list = ls())


#############################
### Final Period Analysis
#############################
#Load data and model functions
load("Data/P2_Europe.RData")
source("Code/GAM_Function.R")


#########################
# Apply GAM models
#########################
variants <- relevel(variants, ref = "Delta")

#Relation between transmission and temperature
res_P2_Temp_Europe <- GamModel_Temp(Re, Temperature, StringencyIndex, 
    Vac, variants)


#Relation between transmission and relative humidity
res_P2_SH_Europe <- GamModel_SH(Re, SH, StringencyIndex, Vac, variants)

#Save models results P2 and data
setwd("Data")
save(res_P2_Temp_Europe, res_P2_SH_Europe,
    file = "GAM_Europe_P2.RData")
setwd("..")

#Save tables models results P2
setwd("Tables")
#Calculate adjust p-values
res_P2_Temp_Europe$Gam[,3:6] <- apply(res_P2_Temp_Europe$Gam[,3:6], 2, function(x){
    results <- p.adjust(x, method = "fdr")  
})
write.csv(res_P2_Temp_Europe$Gam, file = "Results_P2_Temperature_Europe.csv")

#Calculate adjust p-values
res_P2_SH_Europe$Gam[,3:6] <- apply(res_P2_SH_Europe$Gam[,3:6], 2, function(x){
    results <- p.adjust(x, method = "fdr")  
})
write.csv(res_P2_SH_Europe$Gam, file = "Results_P2_SH_Europe.csv")
setwd("..")
rm(list = ls())


