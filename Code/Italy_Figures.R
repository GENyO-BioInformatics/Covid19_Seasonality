#################################
## GAM and OR Plots - Italy
#################################
#Abbreviation
#Com -> Communities
#Ini -> Intial
#Fin -> P2
#Res/r -> Results

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



#Create directory
dir.create("Figures", showWarnings = FALSE)

#Load GAM results
load("Data/GAM_Italy_P1.RData")
load("Data/GAM_Italy_P2.RData")


#####################################
# Prediction of Re from Temperature
#####################################

############################
#Prepare Data for plots
############################
r_P1 <- res_P1_Temp_Italy$Data
r_P2 <- res_P2_Temp_Italy$Data


for (i in 1:length(r_P1)){
    long <- length(r_P1[[i]]$Temp)
    Temp <- r_P1[[i]]$Temp
    StringencyIndex <- rep(mean(r_P1[[i]]$StringencyIndex), long)
    Vac <- rep(0, long)
    Variants <- rep("Alpha", long)
    newData <- data.frame(Temp,StringencyIndex, Vac, Variants)
    r_P1[[i]]$pred <- predict.gam(res_P1_Temp_Italy$Models[[i]], newdata = newData)
    r_P1[[i]]$se <- predict.gam(res_P1_Temp_Italy$Models[[i]], 
        newdata = newData, se.fit = TRUE)$se.fit
    long <-  length(r_P2[[i]]$Temp)
    Temp <-  r_P2[[i]]$Temp
    StringencyIndex <- rep(mean(r_P1[[i]]$StringencyIndex), long)
    Vac <- rep(0, long)
    Variants <-  rep("Alpha", long)
    newData <- data.frame(Temp,StringencyIndex, Vac, Variants)
    r_P2[[i]]$pred <- predict.gam(res_P2_Temp_Italy$Models[[i]], newdata = newData)
    r_P2[[i]]$se <- predict.gam(res_P2_Temp_Italy$Models[[i]], 
        newdata = newData, se.fit = TRUE)$se.fit
}

Inames <- c("Piemonte", "Toscana", "Abruzzo", "Basilicata", "Calabria",   "Campania",             
    "Emilia-Romagna",  "Friuli Venezia Giulia", "Lazio",   "Liguria",             
    "Lombardia" ,    "Marche"  , "Molise"  ,                           
    "Puglia",    "Sardegna" , "Sicilia" ,              
    "Umbria"  , "Trento", "Veneto")

r_P1 <- r_P1[Inames]
r_P2 <- r_P2[Inames]

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
#dev.off()


vars <- c("P1"="#0070bb", "P2" ="#f1c037")
Gam_Temp_Italy <- ggplot() +
    geom_smooth(data = res_total %>% mutate(Country = factor(Country, levels = Inames)), aes(x = Temp, y = pred, 
        colour = Period), method = "gam", 
        formula = y ~ s(x, bs = "cs"), fill = "white", linewidth = 0.2) +
    scale_color_manual(name = "Period:", values = vars) +
    geom_ribbon(data = res_total %>% mutate(Country = factor(Country, levels = Inames)), aes(x = Temp, y = pred, 
        ymin = pred - se, ymax = pred + se, group = Period, fill = Period),alpha = 0.2) +
    scale_fill_manual(name = "Period:", values = vars) +
    facet_wrap(~Country,scales = "free") +
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
    filename = "Supplementary_Figure_S15.pdf",
    plot = Gam_Temp_Italy,
    scale = 1,
    width = 1000,
    height  = 800,
    units = "px",
    dpi = 300
)

ggsave(
    filename = "Supplementary_Figure_S15.jpeg",
    plot = Gam_Temp_Italy,
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
load("Data/GAM_Italy_P1.RData")
load("Data/GAM_Italy_P2.RData")

############################
#Prepare Data for plots
############################
r_P1 <- res_P1_SH_Italy$Data
r_P2 <- res_P2_SH_Italy$Data


for (i in 1:length(r_P1)){
    long <- length(r_P1[[i]]$SH)
    SH <- r_P1[[i]]$SH
    StringencyIndex <- rep(mean(r_P1[[i]]$StringencyIndex), long)
    Vac <- rep(0, long)
    Variants <- rep("Alpha", long)
    newData <- data.frame(SH,StringencyIndex, Vac, Variants)
    r_P1[[i]]$pred <- predict.gam(res_P1_SH_Italy$Models[[i]], newdata = newData)
    r_P1[[i]]$se <- predict.gam(res_P1_SH_Italy$Models[[i]], newdata = newData, 
        se.fit = TRUE)$se
    long <-  length(r_P2[[i]]$SH)
    SH <-  r_P2[[i]]$SH
    StringencyIndex <- rep(mean(r_P1[[i]]$StringencyIndex), long)
    Vac <- rep(0, long)
    Variants <-  rep("Alpha", long)
    newData <- data.frame(SH,StringencyIndex, Vac, Variants)
    r_P2[[i]]$pred <- predict.gam(res_P2_SH_Italy$Models[[i]], newdata = newData)
    r_P2[[i]]$se <- predict.gam(res_P2_SH_Italy$Models[[i]], newdata = newData, 
        se.fit = TRUE)$se
    
}


Inames <- c("Piemonte", "Toscana", "Abruzzo", "Basilicata", "Calabria",   "Campania",             
    "Emilia-Romagna",  "Friuli Venezia Giulia", "Lazio",   "Liguria",             
    "Lombardia" ,    "Marche"  , "Molise"  ,                           
    "Puglia",    "Sardegna" , "Sicilia" ,              
    "Umbria"  , "Trento", "Veneto")

r_P1 <- r_P1[Inames]
r_P2 <- r_P2[Inames]

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

#Create and Save the plot
setwd("Figures")
#dev.off()


vars <- c("P1"="#0070bb", "P2" ="#f1c037")
Gam_SH_Italy <- ggplot() +
    geom_smooth(data = res_total %>% mutate(Country = factor(Country, levels = Inames)), aes(x = SH, y = pred, 
        colour = Period), method = "gam", 
        formula = y ~ s(x, bs = "cs"), fill = "white", linewidth = 0.2) +
    scale_color_manual(name = "Period:", values = vars) +
    geom_ribbon(data = res_total %>% mutate(Country = factor(Country, levels = Inames)), aes(x = SH, y = pred, 
        ymin = pred - se, ymax = pred + se, group = Period, fill = Period),alpha = 0.2) +
    scale_fill_manual(name = "Period:", values = vars) +
    facet_wrap(~Country,scales = "free") +
    labs(x = "Specific Humidity (g/kg)",y = bquote(R[e])) +
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
    filename = "Supplementary_Figure_S17.pdf",
    plot = Gam_SH_Italy,
    scale = 1,
    width = 1000,
    height  = 800,
    units = "px",
    dpi = 300
)

ggsave(
    filename = "Supplementary_Figure_S17.jpeg",
    plot = Gam_SH_Italy,
    scale = 1,
    width = 1400,
    height  = 1200,
    units = "px",
    dpi = 300
)

setwd("..")

rm(list = ls())

