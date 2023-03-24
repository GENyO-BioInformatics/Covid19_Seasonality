#################################
## GAM and OR Plots - Spain
#################################
#Abbreviation
#Com -> Communities
#Ini -> Intial
#Fin -> Final
#Res/r -> Results

#Neccesary packages
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


################################################################################
########### GAM PLOTS
################################################################################

####################3
#TEMPERATURE
#####################

#Load GAM results
load("Data/GAM_Spain_P1.RData")
load("Data/GAM_Spain_P2.RData")


#####################################
# Prediction of Re from Temperature
#####################################

############################
#Prepare Data for plots
############################
r_P1 <- res_P1_Temp_Spain$Data
r_P2 <- res_P2_Temp_Spain$Data


for (i in 1:length(r_P1)){
    long <- length(r_P1[[i]]$Temp)
    Temp <- r_P1[[i]]$Temp
    StringencyIndex <- rep(mean(r_P1[[i]]$StringencyIndex), long)
    Vac <- rep(0, long)
    Variants <- rep("Alpha", long)
    newData <- data.frame(Temp,StringencyIndex, Vac, Variants)
    r_P1[[i]]$pred <- predict.gam(res_P1_Temp_Spain$Models[[i]], newdata = newData)
    r_P1[[i]]$se <- predict.gam(res_P1_Temp_Spain$Models[[i]], 
        newdata = newData, se.fit = TRUE)$se.fit
    long <-  length(r_P2[[i]]$Temp)
    Temp <-  r_P2[[i]]$Temp
    StringencyIndex <- rep(mean(r_P1[[i]]$StringencyIndex), long)
    Vac <- rep(0, long)
    Variants <-  rep("Alpha", long)
    newData <- data.frame(Temp,StringencyIndex, Vac, Variants)
    r_P2[[i]]$pred <- predict.gam(res_P2_Temp_Spain$Models[[i]], newdata = newData)
    r_P2[[i]]$se <- predict.gam(res_P2_Temp_Spain$Models[[i]], newdata = newData,
        se.fit = TRUE)$se.fit
}



names_com <- names(r_P1)
names_com[c(1,2,4,5,6,7,8,9,12,14,15,16)] <- c("Andalusia", "Aragon",
    "Castilla y Leon ","Castille-La Mancha",
    "Catalonia", "Madrid", "Navarre", "C. Valenciana", "The Balearic Islands",
    "Basque Country", "Asturias", "Murcia")
names(r_P1) <- names_com
names(r_P2) <- names_com


com_P1 = r_P1
com_P2 <- r_P2
for (i in names(com_P1)){
    com_P1[[i]]$Country = i
    com_P2[[i]]$Country = i
}

com_P1 = do.call("rbind",com_P1)
com_P2 = do.call("rbind",com_P2)

com_P1$Period <- rep("P1", nrow(com_P1))
com_P2$Period <- rep("P2", nrow(com_P2))

res_total <- rbind(com_P1, com_P2)

#############################
#Create and Save the plot
############################
setwd("Figures")

vars <- c("P1"="#0070bb", "P2" ="#f1c037")
Gam_Temp_Spain  <- ggplot() +
    facet_wrap(~Country,scales = "free") +
    geom_smooth(data = res_total, aes(x = Temp, y = pred, 
        colour = Period), method = "gam", 
        formula = y ~ s(x, bs = "cs"), fill = "white", linewidth = 0.2) +
    scale_color_manual(name = "Period:", values = vars) +
    geom_ribbon(data = res_total, aes(x = Temp, y = pred, 
        ymin = pred - se, ymax = pred + se, group = Period, fill = Period),alpha = 0.2) +
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
    filename = "Gam_Temp_Spain.pdf",
    plot = Gam_Temp_Spain,
    scale = 1,
    width = 1000,
    height  = 800,
    units = "px",
    dpi = 300
)

ggsave(
    filename = "Gam_Temp_Spain.jpeg",
    plot = Gam_Temp_Spain,
    scale = 1,
    width = 1400,
    height  = 1200,
    units = "px",
    dpi = 300
)



setwd("..")




####################################################
# Comparison of the fitted model with the real data
####################################################
for (i in 1:length(r_P1)){
    long <- length(r_P1[[i]]$Temp)
    Temp <- r_P1[[i]]$Temp
    StringencyIndex <- r_P1[[i]]$StringencyIndex
    Vac <- r_P1[[i]]$Vac
    Variants <- r_P1[[i]]$Variants
    newData <- data.frame(Temp,StringencyIndex, Vac, Variants)
    r_P1[[i]]$pred <- predict.gam(res_P1_Temp_Spain$Models[[i]], newdata = newData)
    long <-  length(r_P2[[i]]$Temp)
    Temp <-  r_P2[[i]]$Temp
    StringencyIndex <- r_P2[[i]]$StringencyIndex
    Vac <- r_P2[[i]]$Vac
    Variants <- r_P2[[i]]$Variants
    newData <- data.frame(Temp,StringencyIndex, Vac, Variants)
    r_P2[[i]]$pred <- predict.gam(res_P2_Temp_Spain$Models[[i]], newdata = newData)
}


names_com <- names(r_P1)
names_com[c(1,2,4,5,6,7,8,9,12,14,15,16)] <- c("Andalusia", "Aragon",
    "Castilla y Leon ","Castille-La Mancha",
    "Catalonia", "Madrid", "Navarre", "C. Valenciana", "The Balearic Islands",
    "Basque Country", "Asturias", "Murcia")
names(r_P1) <- names_com
names(r_P2) <- names_com


com_P1 = r_P1
com_P2 <- r_P2
for (i in names(com_P1)){
    com_P1[[i]]$Country = i
    com_P2[[i]]$Country = i
}

com_P1 = do.call("rbind",com_P1)
com_P2 = do.call("rbind",com_P2)

com_P1$Period <- rep("P1", nrow(com_P1))
com_P2$Period <- rep("P2", nrow(com_P2))

res_total <- rbind(com_P1, com_P2)

#P1
setwd("Figures")

vars <- c("P1"="#0070bb", "P2" ="#f1c037")
Gam_P1_Temp_Spain <- ggplot() +
    facet_wrap(~Country,scales = "free") +
    geom_smooth(data = res_total[res_total$Period=="P1",], aes(x = Temp, y = pred), 
        method = "gam", 
        formula = y ~ s(x, bs = "cs"), fill = "#0070bb", colour = "#0070bb",
        linewidth = 0.3, se = FALSE) +
    geom_point(data = res_total[res_total$Period=="P1",], 
        aes(x = Temp, y = vd), alpha = 0.1, size = 0.001, color = "black") +
    theme(legend.position = "bottom", text = element_text(size = 20)) + 
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
    filename = "Gam_P1_Temp_Spain.pdf",
    plot = Gam_P1_Temp_Spain,
    scale = 1,
    width = 1000,
    height  = 800,
    units = "px",
    dpi = 300
)

ggsave(
    filename = "Gam_P1_Temp_Spain.jpeg",
    plot = Gam_P1_Temp_Spain,
    scale = 1,
    width = 1400,
    height  = 1200,
    units = "px",
    dpi = 300
)


setwd("..")

#P2
setwd("Figures")

vars <- c("P1"="#0070bb", "P2" ="#f1c037")
Gam_P2_Temp_Spain <- ggplot() +
    facet_wrap(~Country,scales = "free") +
    geom_smooth(data = res_total[res_total$Period=="P2",], aes(x = Temp, y = pred), 
        method = "gam", 
        formula = y ~ s(x, bs = "cs"), fill = "#f1c037", colour = "#f1c037",
        linewidth = 0.3, se = FALSE) +
    geom_point(data = res_total[res_total$Period=="P2",], 
        aes(x = Temp, y = vd,), alpha = 0.1, size = 0.001, color = "black") +
    theme(legend.position = "bottom", text = element_text(size = 20)) + 
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
    filename = "Gam_P2_Temp_Spain.pdf",
    plot = Gam_P2_Temp_Spain,
    scale = 1,
    width = 1000,
    height  = 800,
    units = "px",
    dpi = 300
)

ggsave(
    filename = "Gam_P2_Temp_Spain.jpeg",
    plot = Gam_P2_Temp_Spain,
    scale = 1,
    width = 1400,
    height  = 1200,
    units = "px",
    dpi = 300
)

setwd("..")

rm(list = ls())

########################################
#SPECIFIC HUMIDITY
########################################

#Load GAM results
load("Data/GAM_Spain_P1.RData")
load("Data/GAM_Spain_P2.RData")

#####################################
# Prediction of Re from SH
#####################################

############################
#Prepare Data for plots
############################
r_P1 <- res_P1_SH_Spain$Data
r_P2 <- res_P2_SH_Spain$Data

for (i in 1:length(r_P1)){
    long <- length(r_P1[[i]]$SH)
    SH <- r_P1[[i]]$SH
    StringencyIndex <- rep(mean(r_P1[[i]]$StringencyIndex), long)
    Vac <- rep(0, long)
    Variants <- rep("Alpha", long)
    newData <- data.frame(SH,StringencyIndex, Vac, Variants)
    r_P1[[i]]$pred <- predict.gam(res_P1_SH_Spain$Models[[i]], newdata = newData)
    r_P1[[i]]$se <- predict.gam(res_P1_SH_Spain$Models[[i]], 
        newdata = newData, se.fit = TRUE)$se.fit
    long <-  length(r_P2[[i]]$SH)
    SH <-  r_P2[[i]]$SH
    StringencyIndex <- rep(mean(r_P1[[i]]$StringencyIndex), long)
    Vac <- rep(0, long)
    Variants <-  rep("Alpha", long)
    newData <- data.frame(SH,StringencyIndex, Vac, Variants)
    r_P2[[i]]$pred <- predict.gam(res_P2_SH_Spain$Models[[i]], newdata = newData)
    r_P2[[i]]$se <- predict.gam(res_P2_SH_Spain$Models[[i]], 
        newdata = newData, se.fit = TRUE)$se.fit
}

names_com <- names(r_P1)
names_com[c(1,2,4,5,6,7,8,9,12,14,15,16)] <- c("Andalusia", "Aragon",
    "Castilla y Leon ","Castille-La Mancha",
    "Catalonia", "Madrid", "Navarre", "C. Valenciana", "The Balearic Islands",
    "Basque Country", "Asturias", "Murcia")
names(r_P1) <- names_com
names(r_P2) <- names_com

com_P1 = r_P1
com_P2 <- r_P2
for (i in names(com_P1)){
    com_P1[[i]]$Country = i
    com_P2[[i]]$Country = i
}

com_P1 = do.call("rbind",com_P1)
com_P2 = do.call("rbind",com_P2)

com_P1$Period <- rep("P1", nrow(com_P1))
com_P2$Period <- rep("P2", nrow(com_P2))

res_total <- rbind(com_P1, com_P2)


#############################
#Create and Save the plot
############################
setwd("Figures")


vars <- c("P1"="#0070bb", "P2" ="#f1c037")
Gam_SH_Spain <- ggplot() +
    facet_wrap(~Country,scales = "free") +
    geom_smooth(data = res_total, aes(x = SH, y = pred, 
        colour = Period), method = "gam", 
        formula = y ~ s(x, bs = "cs"), fill = "white", linewidth = 0.2) +
    scale_color_manual(name = "Period:", values = vars) +
    geom_ribbon(data = res_total, aes(x = SH, y = pred, 
        ymin = pred - se, ymax = pred + se, group = Period, 
        fill = Period),alpha = 0.2) +
    scale_fill_manual(name = "Period:", values = vars) +
    theme(legend.position = "bottom", text = element_text(size = 20)) + 
    #geom_hline(yintercept = 1, color = "red", linetype = 2) +
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
    filename = "Gam_SH_Spain.pdf",
    plot = Gam_SH_Spain,
    scale = 1,
    width = 1000,
    height  = 800,
    units = "px",
    dpi = 300
)

ggsave(
    filename = "Gam_SH_Spain.jpeg",
    plot = Gam_SH_Spain,
    scale = 1,
    width = 1400,
    height  = 1200,
    units = "px",
    dpi = 300
)
    
    
    
    
setwd("..")



##################################################
#Differences between prediction and real data
##################################################
for (i in 1:length(r_P1)){
    long <- length(r_P1[[i]]$SH)
    SH <- r_P1[[i]]$SH
    StringencyIndex <- r_P1[[i]]$StringencyIndex
    Vac <- r_P1[[i]]$Vac
    Variants <- r_P1[[i]]$Variants
    newData <- data.frame(SH,StringencyIndex, Vac, Variants)
    r_P1[[i]]$pred <- predict.gam(res_P1_SH_Spain$Models[[i]], newdata = newData)
    long <-  length(r_P2[[i]]$SH)
    SH <-  r_P2[[i]]$SH
    StringencyIndex <- r_P2[[i]]$StringencyIndex
    Vac <- r_P2[[i]]$Vac
    Variants <- r_P2[[i]]$Variants
    newData <- data.frame(SH,StringencyIndex, Vac, Variants)
    r_P2[[i]]$pred <- predict.gam(res_P2_SH_Spain$Models[[i]], newdata = newData)
}

names_com <- names(r_P1)
names_com[c(1,2,4,5,6,7,8,9,12,14,15,16)] <- c("Andalusia", "Aragon",
    "Castilla y Leon ","Castille-La Mancha",
    "Catalonia", "Madrid", "Navarre", "C. Valenciana", "The Balearic Islands",
    "Basque Country", "Asturias", "Murcia")
names(r_P1) <- names_com
names(r_P2) <- names_com


com_P1 = r_P1
com_P2 <- r_P2
for (i in names(com_P1)){
    com_P1[[i]]$Country = i
    com_P2[[i]]$Country = i
}

com_P1 = do.call("rbind",com_P1)
com_P2 = do.call("rbind",com_P2)

com_P1$Period <- rep("P1", nrow(com_P1))
com_P2$Period <- rep("P2", nrow(com_P2))

res_total <- rbind(com_P1, com_P2)



#P1
setwd("Figures")

vars <- c("P1"="#0070bb", "P2" ="#f1c037")
Gam_P1_SH_Spain <- ggplot() +
    facet_wrap(~Country,scales = "free") +
    geom_smooth(data = res_total[res_total$Period=="P1",], aes(x = SH, y = pred, 
        colour = "blue"), method = "gam", 
        formula = y ~ s(x, bs = "cs"), fill = "#0070bb", colour = "#0070bb",
        linewidth = 0.3, se = FALSE) +
    geom_point(data = res_total[res_total$Period=="P1",], 
        aes(x = SH, y = vd,), alpha = 0.1, size = 0.005)+ 
    theme(legend.position = "bottom", text = element_text(size = 20)) + 
    labs(x = "Specific humidity (g/kg)", y = bquote(R[e])) +
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
    filename = "Gam_P1_SH_Spain.pdf",
    plot = Gam_P1_SH_Spain,
    scale = 1,
    width = 1000,
    height  = 800,
    units = "px",
    dpi = 300
)

ggsave(
    filename = "Gam_P1_SH_Spain.jpeg",
    plot = Gam_P1_SH_Spain,
    scale = 1,
    width = 1400,
    height  = 1200,
    units = "px",
    dpi = 300
)



setwd("..")

#P2
setwd("Figures")

vars <- c("P1"="#0070bb", "P2" ="#f1c037")
Gam_P2_SH_Spain <- ggplot() +
    facet_wrap(~Country,scales = "free") +
    geom_smooth(data = res_total[res_total$Period=="P2",], aes(x = SH, y = pred, 
        colour = "blue"), method = "gam", 
        formula = y ~ s(x, bs = "cs"), fill = "#f1c037", colour = "#f1c037",
        linewidth = 0.3, se = FALSE) +
    geom_point(data = res_total[res_total$Period=="P2",], 
        aes(x = SH, y = vd,), alpha = 0.1, size = 0.005)+ 
    theme(legend.position = "bottom", text = element_text(size = 20)) + 
    labs(x = "Specific humidity (g/kg)", y = bquote(R[e])) +
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
    filename = "Gam_P2_SH_Spain.pdf",
    plot = Gam_P2_SH_Spain,
    scale = 1,
    width = 1000,
    height  = 800,
    units = "px",
    dpi = 300
)

ggsave(
    filename = "Gam_P2_SH_Spain.jpeg",
    plot = Gam_P2_SH_Spain,
    scale = 1,
    width = 1400,
    height  = 1200,
    units = "px",
    dpi = 300
)



setwd("..")



#######################################
###### FULL PERIOD SPAIN PLOTS
#######################################

#Load data
load("Data/FullPeriod_Spain.RData")


#################################
# Evolution of pandemic graphics
#################################



##########################################
### Evolution of Re and period
#########################################
#Load data
load("Data/FullPeriod_Spain.RData")

dat <- list()
for(i in 1:nrow(Re)){
    datos <- data.frame(Re[i,], SH[i,], colnames(Re), variants)
    datos$Period <- NA
    datos[which(rownames(datos)=="2020-06-01"): which(rownames(datos)=="2020-12-31"),]$Period <- "P1"
    datos[which(rownames(datos)=="2021-06-01"): which(rownames(datos)=="2021-12-31"),]$Period <- "P2"
    names(datos) <- c("Re", "SH", "date", "variants", "Period")
    datos$date <- as.Date(datos$date)
    dat <- list.append(dat, datos)
}


names(dat) <- rownames(Re)
names(dat)[c(1,2,4,5,6,7,8,9,12,14,15,16)] <- c("Andalusia", "Aragon",
    "Castilla y Leon","Castille-La Mancha",
    "Catalonia", "Madrid", "Navarre", "C. Valenciana", "The Balearic Islands",
    "Basque Country", "Asturias", "Murcia")
data2 <- list(dat, nom = names(dat))
names(data2) <- c("dat", "nom")


for(i in names(dat)){
    dat[[i]]$Country = i
}
com = do.call("rbind",dat)
coeff <- 10

setwd("Figures")

Evolution_Re_Period <- ggplot(com, aes(x = date)) +
    facet_wrap(~Country,scales = "free") +
    #P1
    annotate("rect",
        xmin = as.Date("2020-06-01"),
        xmax = as.Date("2020-12-31"),
        ymin = -Inf,
        ymax = Inf, fill = "#0070bb", alpha = 0.2) +
    #P2
    annotate("rect",
        xmin = as.Date("2021-06-01"),
        xmax = as.Date("2021-12-31"),
        ymin = -Inf,
        ymax = Inf, fill = "#f1c037", alpha = 0.2) +
    geom_line(aes(y = Re, color = Period), alpha = 0.4, linewidth = 0.2) +
    scale_color_manual(name='Period:',
        values=c('P1'='#0070bb', 'P2'='#f1c037')) +
    labs(x = "Date", y = bquote(R[e])) +
    geom_line(aes(y = Re), linewidth = 0.2) +
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
        strip.background = element_rect(fill = "white")
        )

ggsave(
    filename = "Evolution_Re_Period.pdf",
    plot = Evolution_Re_Period,
    scale = 1,
    width = 1000,
    height  = 800,
    units = "px",
    dpi = 300
)

ggsave(
    filename = "Evolution_Re_Period.jpeg",
    plot = Evolution_Re_Period,
    scale = 1,
    width = 1400,
    height  = 1200,
    units = "px",
    dpi = 300
)

rm(list = ls())
setwd("..")



#######################
# Re and temperature
#######################
#Load data
load("Data/FullPeriod_Spain.RData")
dat <- list()
for(i in 1:nrow(Re)){
    datos <- data.frame(Re[i,], Temperature[i,], colnames(Re), variants)
    names(datos) <- c("Re", "Temperature", "date", "variants")
    datos$date <- as.Date(datos$date)
    dat <- list.append(dat, datos)
}
names(dat) <- rownames(Re)
names(dat)[c(1,2,4,5,6,7,8,9,12,14,15,16)] <- c("Andalusia", "Aragon",
    "Castilla y Leon","Castille-La Mancha",
    "Catalonia", "Madrid", "Navarre", "C. Valenciana", "The Balearic Islands",
    "Basque Country", "Asturias", "Murcia")
data2 <- list(dat, nom = names(dat))
names(data2) <- c("dat", "nom")


for(i in names(dat)){
    dat[[i]]$Country = i
}
com = do.call("rbind",dat)
ReColor <- "black"
TemperatureColor <- "#e8282d"
coeff <- 12



#Save the plot
setwd("Figures")


fechas_variante <- unique(com[,c("date","variants")])


Re_vs_Temperature <- ggplot(data = com, aes(x = date)) +
    facet_wrap(~Country,scales = "free") +
    geom_smooth(aes(y = Re), color = ReColor, method = "gam", se = FALSE, 
        linewidth=0.2) +
    geom_smooth( aes(y=Temperature / coeff), linewidth=0.2, 
        color=TemperatureColor, method ="gam", se = FALSE) +
    annotate("rect",
        xmin = as.Date("2020-06-01"),
        xmax = as.Date("2020-12-31"),
        ymin = -Inf,
        ymax = Inf, fill = "#0070bb", alpha = 0.2) +
    #P2
    annotate("rect",
        xmin = as.Date("2021-06-01"),
        xmax = as.Date("2021-12-31"),
        ymin = -Inf,
        ymax = Inf, fill = "#f1c037", alpha = 0.2) +
    scale_y_continuous(
        # Features of the first axis
        name = bquote(R[e]),
        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name="Temperature Cº")
    ) + 
    xlab("Date") +
    theme(
        axis.title.y = element_text(color = ReColor, size=4),
        axis.title.y.right = element_text(color = TemperatureColor, size=4),
        legend.position = "bottom",
        legend.text = element_text(size = 4),
        text = element_text(size = 4),
        strip.text.x = element_text(size = 3, margin = margin(2,2,2,2)),
        legend.key.size = unit(0.2, "cm"),
        axis.ticks = element_line(linewidth = 0.1),
        axis.ticks.length = unit(0.05, "cm"),
        axis.line = element_line(linewidth = 0.1),
        panel.spacing = unit(0.1, "lines"),
        axis.text.x = element_text(margin = margin(t = 0.5, b = 0.5), vjust = 0.5),
        axis.text.y.left = element_text(margin = margin(r = 0.5, l = 0.5),hjust = 1, color ="black"),
        axis.text.y.right = element_text(margin = margin(r = 0.5, l = 0.5),hjust = 1, color = "red"),
        panel.grid = element_line(linewidth = 0.1,color = "#f2f2f2"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white")
    )

    
    
ggsave(
    filename = "Re_vs_Temperature.pdf",
    plot = Re_vs_Temperature,
    scale = 1,
    width = 1200,
    height  = 1000,
    units = "px",
    dpi = 300
    )


ggsave(
    filename = "Re_vs_Temperature.jpeg",
    plot = Re_vs_Temperature,
    scale = 1,
    width = 1400,
    height  = 1200,
    units = "px",
    dpi = 300
)

setwd("..")



#######################
# Re and SH
#######################
dat <- list()
for(i in 1:nrow(Re)){
    datos <- data.frame(Re[i,], SH[i,], colnames(Re), variants)
    names(datos) <- c("Re", "SH", "date", "variants")
    datos$date <- as.Date(datos$date)
    dat <- list.append(dat, datos)
}
names(dat) <- rownames(Re)
names(dat)[c(1,2,4,5,6,7,8,9,12,14,15,16)] <- c("Andalusia", "Aragon",
    "Castilla y Leon","Castille-La Mancha",
    "Catalonia", "Madrid", "Navarre", "C. Valenciana", "The Balearic Islands",
    "Basque Country", "Asturias", "Murcia")
data2 <- list(dat, nom = names(dat))
names(data2) <- c("dat", "nom")


for(i in names(dat)){
    dat[[i]]$Country = i
}
com = do.call("rbind",dat)
ReColor <- "black"
SHColor <- "#e8282d"
coeff <- 8



#Save the plot
setwd("Figures")

fechas_variante <- unique(com[,c("date","variants")])

Re_vs_SH <- ggplot(data = com, aes(x = date)) +
    facet_wrap(~Country,scales = "free") +
    geom_smooth(aes(y = Re), color = ReColor, method = "gam", se = FALSE, 
        linewidth=0.2) +
    geom_smooth( aes(y=SH / coeff), linewidth=0.2, 
        color=TemperatureColor, method ="gam", se = FALSE) +
    annotate("rect",
        xmin = as.Date("2020-06-01"),
        xmax = as.Date("2020-12-31"),
        ymin = -Inf,
        ymax = Inf, fill = "#0070bb", alpha = 0.2) +
    #P2
    annotate("rect",
        xmin = as.Date("2021-06-01"),
        xmax = as.Date("2021-12-31"),
        ymin = -Inf,
        ymax = Inf, fill = "#f1c037", alpha = 0.2) +
    scale_y_continuous(
        # Features of the first axis
        name = bquote(R[e]),
        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name="SH (g/kg)")
    ) + 
    xlab("Date") +
    theme(
        axis.title.y = element_text(color = ReColor, size=4),
        axis.title.y.right = element_text(color = TemperatureColor, size=4),
        legend.position = "bottom",
        legend.text = element_text(size = 4),
        text = element_text(size = 4),
        strip.text.x = element_text(size = 3, margin = margin(2,2,2,2)),
        legend.key.size = unit(0.2, "cm"),
        axis.ticks = element_line(linewidth = 0.1),
        axis.ticks.length = unit(0.05, "cm"),
        axis.line = element_line(linewidth = 0.1),
        panel.spacing = unit(0.1, "lines"),
        axis.text.x = element_text(margin = margin(t = 0.5, b = 0.5), vjust = 0.5),
        axis.text.y.left = element_text(margin = margin(r = 0.5, l = 0.5),hjust = 1, color ="black"),
        axis.text.y.right = element_text(margin = margin(r = 0.5, l = 0.5),hjust = 1, color = "red"),
        panel.grid = element_line(linewidth = 0.1,color = "#f2f2f2"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white")
    )



ggsave(
    filename = "Re_vs_SH.pdf",
    plot = Re_vs_SH,
    scale = 1,
    width = 1200,
    height  = 1000,
    units = "px",
    dpi = 300
)


ggsave(
    filename = "Re_vs_SH.jpeg",
    plot = Re_vs_SH,
    scale = 1,
    width = 1400,
    height  = 1200,
    units = "px",
    dpi = 300
)


setwd("..")



##################################
# Vaccination and StringencyIndex
####################################
dat <- list()
for(i in 1:nrow(Re)){
    datos <- data.frame(Vac[i,], StringencyIndex[i,], colnames(Re), variants)
    names(datos) <- c("Vac", "SI", "date", "variants")
    datos$date <- as.Date(datos$date)
    dat <- list.append(dat, datos)
}
names(dat) <- rownames(Re)
names(dat)[c(1,2,4,5,6,7,8,9,12,14,15,16)] <- c("Andalusia", "Aragon",
    "Castilla y Leon","Castille-La Mancha",
    "Catalonia", "Madrid", "Navarre", "C. Valenciana", "The Balearic Islands",
    "Basque Country", "Asturias", "Murcia")
data2 <- list(dat, nom = names(dat))
names(data2) <- c("dat", "nom")


for(i in names(dat)){
    dat[[i]]$Country = i
}
com = do.call("rbind",dat)
VacColor <- "#00bb4b"
SIColor <- "#bb0070"
coeff <- 0.92



#Save the plot
setwd("Figures")

fechas_variante <- unique(com[,c("date","variants")])

Vac_vs_SI <- ggplot(data = com, aes(x = date)) +
    facet_wrap(~Country,) +
    geom_smooth(aes(y = Vac), color = VacColor, method = "gam", se = FALSE, 
        linewidth=0.2) +
    geom_smooth( aes(y=SI / coeff), linewidth=0.2, 
        color=SIColor, method ="gam", se = FALSE) +
    annotate("rect",
        xmin = as.Date("2020-06-01"),
        xmax = as.Date("2020-12-31"),
        ymin = -Inf,
        ymax = Inf, fill = "#0070bb", alpha = 0.2) +
    #P2
    annotate("rect",
        xmin = as.Date("2021-06-01"),
        xmax = as.Date("2021-12-31"),
        ymin = -Inf,
        ymax = Inf, fill = "#f1c037", alpha = 0.2) +
    scale_y_continuous(
        # Features of the first axis
        name = "Vaccination Rate (%)",
        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name="Stringency Index (SI)")
    ) + 
    xlab("Date") +
    theme(
        axis.title.y = element_text(color = VacColor, size=4),
        axis.title.y.right = element_text(color = SIColor, size=4),
        legend.position = "bottom",
        legend.text = element_text(size = 4),
        text = element_text(size = 4),
        strip.text.x = element_text(size = 3, margin = margin(2,2,2,2)),
        legend.key.size = unit(0.2, "cm"),
        axis.ticks = element_line(linewidth = 0.1),
        axis.ticks.length = unit(0.05, "cm"),
        axis.line = element_line(linewidth = 0.1),
        panel.spacing = unit(0.4, "lines"),
        axis.text.x = element_text(margin = margin(t = 0.5, b = 0.5), vjust = 0.5),
        axis.text.y.left = element_text(margin = margin(r = 0.5, l = 0.5),hjust = 1, color =VacColor),
        axis.text.y.right = element_text(margin = margin(r = 0.5, l = 0.5),hjust = 1, color = SIColor),
        panel.grid = element_line(linewidth = 0.1,color = "#f2f2f2"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white")
    )



ggsave(
    filename = "Vac_vs_SI.pdf",
    plot = Vac_vs_SI,
    scale = 1,
    width = 1200,
    height  = 1000,
    units = "px",
    dpi = 300
)

ggsave(
    filename = "Vac_vs_SI.jpeg",
    plot = Vac_vs_SI,
    scale = 1,
    width = 1400,
    height  = 1200,
    units = "px",
    dpi = 300
)


setwd("..")

rm(list = ls())





##################################
#Evolution of vaccination in Spain
##################################

#Load data
#load("Data/FullPeriod_Spain.RData")
#setwd("Figures")
#vacSpain <- colMeans(Vac)
#for(i in 1:(length(vacSpain)-1)){
#    if(vacSpain[i] > vacSpain[i+1]){
#        vacSpain[i+1] <- vacSpain[i]}
#}
#dates <- as.Date(names(vacSpain))
#Variants <- factor(variants, labels = c("Original", "Alpha", "Delta", "Omicron"))
#datVac <- data.frame(vacSpain, dates, Variants)
#
##Create the plot
#jpeg(file = "Evolution_Vacciantion_Spain.jpeg", height = 800, width = 800)
#ggplot(datVac, aes(x = dates)) +
#    theme_bw() +
#    geom_area(aes(y = vacSpain, color = Variants, fill = Variants), alpha = 0.6) +
#    xlab("Date") + 
#    ylab("Vaccination rate %") + 
#    scale_color_manual("COVID-19 Variant", 
#        values=c("#009e73", "#999999", "#E69F00", "#56B4E9")) +
#    scale_fill_manual("COVID-19 Variant", 
#        values=c("#009e73","#999999", "#E69F00", "#56B4E9")) +
#    theme(text = element_text(size = 20))
#dev.off()
#rm(list = ls())
#setwd("..")





