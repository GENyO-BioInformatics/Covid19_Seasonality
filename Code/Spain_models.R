##########################
#### Spain Code
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
## Period P1
####################

#Load data and model functions
load("Data/P1_Spain.RData")
source("Code/GAM_Function.R")


#########################
# Apply GAM models
#########################

#Relation between transmission and temperature
res_P1_Temp_Spain <- GamModel_Temp(Re, Temperature, StringencyIndex, 
    Vac, variants)

#Relation between transmission and SH
res_P1_SH_Spain <- GamModel_SH(Re, SH, StringencyIndex, 
    Vac, variants)

#Save models results P1 and data
setwd("Data")
save(res_P1_Temp_Spain, res_P1_SH_Spain, file = "GAM_Spain_P1.RData")
setwd("..")

#Save tables models results P1
setwd("Tables")
#Calculate adjust p-values
res_P1_Temp_Spain$Gam[,3:6] <- apply(res_P1_Temp_Spain$Gam[,3:6], 2, function(x){
    results <- p.adjust(x, method = "fdr")  
})

res_P1_SH_Spain$Gam[,3:6] <- apply(res_P1_SH_Spain$Gam[,3:6], 2, function(x){
    results <- p.adjust(x, method = "fdr")  
})

write.csv(res_P1_Temp_Spain$Gam, file = "Results_P1_Temperature_Spain.csv")
write.csv(res_P1_SH_Spain$Gam, file = "Results_P1_SH_Spain.csv")

setwd("..")
rm(list = ls())
#############################
### Final Period Analysis
#############################
#Load data and model functions
load("Data/P2_Spain.RData")
source("Code/GAM_Function.R")


#########################
# Apply GAM models
#########################

variants <- relevel(variants, ref = "Delta")
#Relation between transmission and temperature
res_P2_Temp_Spain <- GamModel_Temp(Re, Temperature, StringencyIndex, 
    Vac, variants)

#Relation between transmission and temperature
res_P2_SH_Spain <- GamModel_SH(Re, SH, StringencyIndex, 
    Vac, variants)




#Save models results P2 and data
setwd("Data")
save(res_P2_Temp_Spain, res_P2_SH_Spain, file = "GAM_Spain_P2.RData")
setwd("..")

#Save tables models results P2
setwd("Tables")
#Calculate adjust p-values
res_P2_Temp_Spain$Gam[,3:6] <- apply(res_P2_Temp_Spain$Gam[,3:6], 2, function(x){
    results <- p.adjust(x, method = "fdr")  
})
write.csv(res_P2_Temp_Spain$Gam, file = "Results_P2_Temperature_Spain.csv")

res_P2_SH_Spain$Gam[,3:6] <- apply(res_P2_SH_Spain$Gam[,3:6], 2, function(x){
    results <- p.adjust(x, method = "fdr")  
})
write.csv(res_P2_SH_Spain$Gam, file = "Results_P2_SH_Spain.csv")
setwd("..")
rm(list = ls())



