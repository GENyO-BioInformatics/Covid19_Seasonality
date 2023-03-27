#################################
## GAM and OR Plots - Europe
#################################
#Abbreviation
#Com -> Communities
#Ini -> Intial
#Fin -> P2
#Res/r -> Results

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


#Load GAM results
load("Data/GAM_Europe_P1.RData")
load("Data/GAM_Europe_P2.RData")


#####################################
# Prediction of Re from Temperature
#####################################

############################
#Prepare Data for plots
############################
r_P1 <- res_P1_Temp_Europe$Data
r_P2 <- res_P2_Temp_Europe$Data

for (i in names(r_P1)){
    r_P1[[i]]$Country = i
    r_P2[[i]]$Country = i
}

for (i in 1:length(r_P1)){
    long <- length(r_P1[[i]]$Temp)
    Temp <- r_P1[[i]]$Temp
    StringencyIndex <- rep(mean(r_P1[[i]]$StringencyIndex), long)
    Vac <- rep(0, long)
    Variants <- rep("Alpha", long)
    newData <- data.frame(Temp,StringencyIndex, Vac, Variants)
    r_P1[[i]]$pred <- predict.gam(res_P1_Temp_Europe$Models[[i]], newdata = newData)
    r_P1[[i]]$se <- predict.gam(res_P1_Temp_Europe$Models[[i]], 
        newdata = newData, se.fit = TRUE)$se
    long <-  length(r_P2[[i]]$Temp)
    Temp <-  r_P2[[i]]$Temp
    StringencyIndex <- rep(mean(r_P1[[i]]$StringencyIndex), long)
    Vac <- rep(0, long)
    Variants <-  rep("Alpha", long)
    newData <- data.frame(Temp,StringencyIndex, Vac, Variants)
    r_P2[[i]]$pred <- predict.gam(res_P2_Temp_Europe$Models[[i]], newdata = newData)
    r_P2[[i]]$se <- predict.gam(res_P2_Temp_Europe$Models[[i]], 
        newdata = newData, se.fit = TRUE)$se
}

r_P1 = do.call("rbind",r_P1)
r_P2 = do.call("rbind",r_P2)

r_P1$Period <- rep("P1", nrow(r_P1))
r_P2$Period <- rep("P2", nrow(r_P2))

res_total <- rbind(r_P1, r_P2)

#Create and Save the plot
setwd("Figures")

vars <- c("P1"="#0070bb", "P2" ="#f1c037")
Gam_Temp_Europe <- ggplot() +
    facet_wrap(~Country,scales = "free") +
    geom_smooth(data = res_total, aes(x = Temp, y = pred, 
        colour = Period), method = "gam", 
        formula = y ~ s(x, bs = "cs"), linewidth = 0.2, se = FALSE) +
    scale_color_manual(name = "Period:", values = vars) +
    geom_ribbon(data = res_total, aes(x = Temp, y = pred, 
        ymin = pred - se, ymax = pred + se, group = Period, fill = Period), 
        alpha = 0.2) +
    scale_fill_manual(name = "Period:", values = vars) +
    labs(x = "Temperature ºC",y = bquote(R[e])) +
    theme(legend.position = "bottom", 
        legend.text = element_text(size = 4),
        text = element_text(size = 4),
        strip.text.x = element_text(size = 3, margin = margin(2,2,2,2)),
        legend.key.size = unit(0.2, "cm"),
        axis.ticks = element_line(linewidth = 0.1),
        axis.ticks.length = unit(0.05, "cm"),
        axis.line = element_line(linewidth = 0.1),
        panel.spacing = unit(0.1, "lines"),
        axis.text.x = element_text(margin = margin(t = 0.5, b = 0.5), vjust = 0.5),
        axis.text.y = element_text(margin = margin(r = 0.5, l = 0.5),hjust = 1),
        panel.grid = element_line(linewidth = 0.1,color = "#f2f2f2"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(t = -6)
        )
ggsave(
    filename = "Supplementary_Figure_7.pdf",
    plot = Gam_Temp_Europe,
    scale = 1,
    width = 1000,
    height  = 800,
    units = "px",
    dpi = 300
)

ggsave(
    filename = "Supplementary_Figure_7.jpeg",
    plot = Gam_Temp_Europe,
    scale = 1,
    width = 1400,
    height  = 1200,
    units = "px",
    dpi = 300
)


setwd("..")




#############################
# Specific Humidity
#############################

#Load GAM results
load("Data/GAM_Europe_P1.RData")
load("Data/GAM_Europe_P2.RData")

############################
#Prepare Data for plots
############################
r_P1 <- res_P1_SH_Europe$Data
r_P2 <- res_P2_SH_Europe$Data


for (i in 1:length(r_P1)){
    long <- length(r_P1[[i]]$SH)
    SH <- r_P1[[i]]$SH
    StringencyIndex <- rep(mean(r_P1[[i]]$StringencyIndex), long)
    Vac <- rep(0, long)
    Variants <- rep("Alpha", long)
    newData <- data.frame(SH,StringencyIndex, Vac, Variants)
    r_P1[[i]]$pred <- predict.gam(res_P1_SH_Europe$Models[[i]], newdata = newData)
    r_P1[[i]]$se <- predict.gam(res_P1_SH_Europe$Models[[i]], newdata = newData, 
        se.fit = TRUE)$se
    long <-  length(r_P2[[i]]$SH)
    SH <-  r_P2[[i]]$SH
    StringencyIndex <- rep(mean(r_P1[[i]]$StringencyIndex), long)
    Vac <- rep(0, long)
    Variants <-  rep("Alpha", long)
    newData <- data.frame(SH,StringencyIndex, Vac, Variants)
    r_P2[[i]]$pred <- predict.gam(res_P2_SH_Europe$Models[[i]], newdata = newData)
    r_P2[[i]]$se <- predict.gam(res_P2_SH_Europe$Models[[i]], newdata = newData, 
        se.fit = TRUE)$se
}


for (i in names(r_P1)){
    r_P1[[i]]$Country = i
    r_P2[[i]]$Country = i
}

r_P1 = do.call("rbind",r_P1)
r_P2 = do.call("rbind",r_P2)

r_P1$Period <- rep("P1", nrow(r_P1))
r_P2$Period <- rep("P2", nrow(r_P2))

res_total <- rbind(r_P1, r_P2)

#Create and Save the plot
setwd("Figures")

vars <- c("P1"="#0070bb", "P2" ="#f1c037")
Gam_SH_Europe <- ggplot() +
    facet_wrap(~Country,scales = "free") +
    geom_smooth(data = res_total, aes(x = SH, y = pred, 
        colour = Period), method = "gam", 
        formula = y ~ s(x, bs = "cs"), linewidth = 0.2, se = FALSE) +
    scale_color_manual(name = "Period:", values = vars) +
    geom_ribbon(data = res_total, aes(x = SH, y = pred, 
        ymin = pred - se, ymax = pred + se, group = Period, fill = Period), 
        alpha = 0.2) +
    scale_fill_manual(name = "Period:", values = vars) +
    labs(x = "Specific humidity (g/kg)",y = bquote(R[e])) +
    theme(legend.position = "bottom", 
        legend.text = element_text(size = 4),
        text = element_text(size = 4),
        strip.text.x = element_text(size = 3, margin = margin(2,2,2,2)),
        legend.key.size = unit(0.2, "cm"),
        axis.ticks = element_line(linewidth = 0.1),
        axis.ticks.length = unit(0.05, "cm"),
        axis.line = element_line(linewidth = 0.1),
        panel.spacing = unit(0.1, "lines"),
        axis.text.x = element_text(margin = margin(t = 0.5, b = 0.5), vjust = 0.5),
        axis.text.y = element_text(margin = margin(r = 0.5, l = 0.5),hjust = 1),
        panel.grid = element_line(linewidth = 0.1,color = "#f2f2f2"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(t = -6)
    )
ggsave(
    filename = "Supplementary_Figure_8.pdf",
    plot = Gam_SH_Europe,
    scale = 1,
    width = 1000,
    height  = 800,
    units = "px",
    dpi = 300
)

ggsave(
    filename = "Supplementary_Figure_8.jpeg",
    plot = Gam_SH_Europe,
    scale = 1,
    width = 1400,
    height  = 1200,
    units = "px",
    dpi = 300
)

setwd("..")





######################
## FULL PERIOD PLOTS
######################

load("Data/FullPeriod_Europe.RData")



##################################
#Evolution of vaccination in Europe
##################################

load("Data/FullPeriod_Spain.RData")
vacSpain <- colMeans(Vac)
for(i in 1:(length(vacSpain)-1)){
    if(vacSpain[i] > vacSpain[i+1]){
        vacSpain[i+1] <- vacSpain[i]}
}

load("Data/FullPeriod_Europe.RData")



#Prepare the data for the plot
Vac2 <- Vac
fechas <- colnames(Vac2)
fechas <- fechas[which(fechas =="2021-01-01"):which(fechas =="2021-12-31")]
num <- length(fechas)
paises <- c(rownames(Vac2), "Spain")
vacSpain <- vacSpain[fechas]
Vac2 <- Vac2[,fechas]
VacRate <- c(Vac2[1,] ,Vac2[2,], Vac2[3,], Vac2[4,], Vac2[5,], Vac2[6,], Vac2[7,], Vac2[8,], vacSpain)
dates <- rep(fechas,9)
dates <- as.Date(dates)
VacEurope <- data.frame(VacRate, dates)
paisesVac <- c(
    rep(paises[1], num), rep(paises[2], num), rep(paises[3], num), rep(paises[4], num),
    rep(paises[5], num), rep(paises[6], num), rep(paises[7], num), rep(paises[8], num),
    rep(paises[9], num))

paises <- paisesVac
VacEurope$Country <- paises





#Create and save the plor
setwd("Figures")
my_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", 
    "#b2df8a", "#fb9a99", "#fdbf6f")
Vaccination_Europe <- ggplot(VacEurope, aes(dates)) +
    geom_line(aes(y = VacRate, color = Country), linewidth = 0.3) +
    xlab("Date") + 
    ylab("Vaccination rate %") +
    geom_hline(yintercept = 50, color = "gray", linetype = 2, linewidth = 0.3) +
    #scale_colour_brewer(palette="Set3") +
    scale_color_manual(values = my_colors) +
    theme(legend.position = "right", 
        legend.text = element_text(size = 4),
        legend.key  = element_rect(fill = "white"),
        text = element_text(size = 4),
        strip.text.x = element_text(size = 3, margin = margin(2,2,2,2)),
        legend.key.size = unit(0.2, "cm"),
        axis.ticks = element_line(linewidth = 0.1),
        axis.ticks.length = unit(0.05, "cm"),
        axis.line = element_line(linewidth = 0.1),
        panel.spacing = unit(0.1, "lines"),
        axis.text.x = element_text(margin = margin(t = 0.5, b = 0.5), vjust = 0.5),
        axis.text.y = element_text(margin = margin(r = 0.5, l = 0.5),hjust = 1),
        panel.grid = element_line(linewidth = 0.1,color = "#f2f2f2"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white")
    )
ggsave(
    filename = "Supplementary_Figure_9.pdf",
    plot = Vaccination_Europe,
    scale = 1,
    width = 1000,
    height  = 800,
    units = "px",
    dpi = 300
)

ggsave(
    filename = "Supplementary_Figure_9.jpeg",
    plot = Vaccination_Europe,
    scale = 1,
    width = 1400,
    height  = 1200,
    units = "px",
    dpi = 300
)



setwd("..")

rm(list = ls())




