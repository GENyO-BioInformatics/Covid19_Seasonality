##########################
#### USA Code
##########################

#Neccessary packages
require(dlnm)
require(mgcv)
require(visreg)
require(corrplot)
require(mctest)
require(rlist)
require(ggplot2)
require(RColorBrewer)
require(gridExtra)
require(oddsratio)
require(dplyr)
require(patchwork)
require(hrbrthemes)
require(forestplot)
require(metafor)

####################
## Initial Period
####################

####################
## Period P1
####################

#Load data and model functions
load("Data/P1_USA.RData")
source("Code/GAM_Function.R")

#########################
# Apply GAM models
#########################

#Relation between transmission and temperature
res_P1_Temp_USA <- GamModel_Temp(Re, Temperature, StringencyIndex, 
    Vac, variants)


#Relation between transmission and relative humidity
res_P1_SH_USA <- GamModel_SH(Re, SH, StringencyIndex, Vac, variants)

#Save models results P1 adn data
setwd("Data")
save(res_P1_Temp_USA, res_P1_SH_USA,
    file = "GAM_USA_P1.RData")
setwd("..")

#Save tables models results P1
setwd("Tables")
#Calculate adjust p-values
res_P1_Temp_USA$Gam[,3:6] <- apply(res_P1_Temp_USA$Gam[,3:6], 2, function(x){
    results <- p.adjust(x, method = "fdr")  
})
write.csv(round(res_P1_Temp_USA$Gam,3), file = "Results_P1_Temp_USA.csv")

#Calculate adjust p-values
res_P1_SH_USA$Gam[,3:6] <- apply(res_P1_SH_USA$Gam[,3:6], 2, function(x){
    results <- p.adjust(x, method = "fdr")  
})
write.csv(round(res_P1_SH_USA$Gam,3), file = "Results_P1_SH_USA.csv")
setwd("..")
rm(list = ls())


#############################
### Final Period Analysis
#############################
#Load data and model functions
load("Data/P2_USA.RData")
source("Code/GAM_Function.R")


#########################
# Apply GAM models
#########################
variants <- relevel(variants, ref = "Delta")

#Relation between transmission and temperature
res_P2_Temp_USA <- GamModel_Temp(Re, Temperature, StringencyIndex, 
    Vac, variants)


#Relation between transmission and relative humidity
res_P2_SH_USA <- GamModel_SH(Re, SH, StringencyIndex, Vac, variants)

#Save models results P2 and data
setwd("Data")
save(res_P2_Temp_USA, res_P2_SH_USA,
    file = "GAM_USA_P2.RData")
setwd("..")

#Save tables models results P2
setwd("Tables")
#Calculate adjust p-values
res_P2_Temp_USA$Gam[,3:6] <- apply(res_P2_Temp_USA$Gam[,3:6], 2, function(x){
    results <- p.adjust(x, method = "fdr")  
})
write.csv(round(res_P2_Temp_USA$Gam,3), file = "Results_P2_Temp_USA.csv")

#Calculate adjust p-values
res_P2_SH_USA$Gam[,3:6] <- apply(res_P2_SH_USA$Gam[,3:6], 2, function(x){
    results <- p.adjust(x, method = "fdr")  
})
write.csv(round(res_P2_SH_USA$Gam,3), file = "Results_P2_SH_USA.csv")
setwd("..")
rm(list = ls())