#################################
## GAM and OR Plots - USA
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
load("Data/GAM_USA_P1.RData")
load("Data/GAM_USA_P2.RData")


#####################################
# Prediction of Re from Temperature
#####################################

############################
#Prepare Data for plots
############################
r_P1 <- res_P1_Temp_USA$Data
r_P2 <- res_P2_Temp_USA$Data

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
    r_P1[[i]]$vd <- predict.gam(res_P1_Temp_USA$Models[[i]], newdata = newData)
    long <-  length(r_P2[[i]]$Temp)
    Temp <-  r_P2[[i]]$Temp
    StringencyIndex <- rep(mean(r_P1[[i]]$StringencyIndex), long)
    Vac <- rep(0, long)
    Variants <-  rep("Alpha", long)
    newData <- data.frame(Temp,StringencyIndex, Vac, Variants)
    r_P2[[i]]$vd <- predict.gam(res_P2_Temp_USA$Models[[i]], newdata = newData)
}

r_P1 = do.call("rbind",r_P1)
r_P2 = do.call("rbind",r_P2)

r_P1$Period <- rep("P1", nrow(r_P1))
r_P2$Period <- rep("P2", nrow(r_P2))

res_total <- rbind(r_P1, r_P2)

#Create and Save the plot
setwd("Supplementary_Figures")
#dev.off()
jpeg(filename = "Gam_Temp_USA.jpeg", 
    height = 800, width = 1000)
vars <- c("P1"="#0070bb", "P2" ="#f1c037")
ggplot() +
    theme_bw() + 
    facet_wrap(~Country,scales = "free") +
    geom_smooth(data = res_total, aes(x = Temp, y = vd, 
        colour = Period), method = "gam", 
        formula = y ~ s(x, bs = "cs"), fill = "#b4b1b1") +
    scale_color_manual(name = "Period:", values = vars) +
    theme(legend.position = "bottom", text = element_text(size = 20))+ 
    #geom_hline(yintercept = 1, color = "red", linetype = 2) +
    labs(x = "Temperature ºC",y = bquote(R[e]))
dev.off()
setwd("..")


####################################################
# Comparison of the fitted model with the real data
####################################################
r_P1 <- res_P1_Temp_USA$Data
r_P2 <- res_P2_Temp_USA$Data

for (i in 1:length(r_P1)){
    long <- length(r_P1[[i]]$Temp)
    Temp <- r_P1[[i]]$Temp
    StringencyIndex <- r_P1[[i]]$StringencyIndex
    Vac <- r_P1[[i]]$Vac
    Variants <- r_P1[[i]]$Variants
    newData <- data.frame(Temp,StringencyIndex, Vac, Variants)
    r_P1[[i]]$pred <- predict.gam(res_P1_Temp_USA$Models[[i]], newdata = newData)
    long <-  length(r_P2[[i]]$Temp)
    Temp <-  r_P2[[i]]$Temp
    StringencyIndex <- r_P2[[i]]$StringencyIndex
    Vac <- r_P2[[i]]$Vac
    Variants <- r_P2[[i]]$Variants
    newData <- data.frame(Temp,StringencyIndex, Vac, Variants)
    r_P2[[i]]$pred <- predict.gam(res_P2_Temp_USA$Models[[i]], newdata = newData)
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

#P1
setwd("Supplementary_Figures")
jpeg(filename = "Gam_P1_Temp_USA.jpeg", 
    height = 800, width = 1000)
vars <- c("P1"="#0070bb", "P2" ="#f1c037")
ggplot() +
    theme_bw() + 
    facet_wrap(~Country,scales = "free") +
    geom_smooth(data = res_total[res_total$Period=="P1",], aes(x = Temp, y = pred, 
        colour = "blue"), method = "gam", 
        formula = y ~ s(x, bs = "cs"), fill = "#0070bb", colour = "#0070bb") +
    geom_point(data = res_total[res_total$Period=="P1",], 
        aes(x = Temp, y = vd,), alpha = 0.1) +
    theme(legend.position = "bottom", text = element_text(size = 20)) + 
    labs(x = "Temperature ºC",y = bquote(R[e]))
dev.off()
setwd("..")

#P2
setwd("Supplementary_Figures")
jpeg(filename = "Gam_P2_Temp_USA.jpeg", 
    height = 800, width = 1000)
vars <- c("P1"="#0070bb", "P2" ="#f1c037")
ggplot() +
    theme_bw() + 
    facet_wrap(~Country,scales = "free") +
    geom_smooth(data = res_total[res_total$Period=="P2",], aes(x = Temp, y = pred), 
        method = "gam", 
        formula = y ~ s(x, bs = "cs"), fill = "#f1c037", colour = "#f1c037") +
    geom_point(data = res_total[res_total$Period=="P2",], 
        aes(x = Temp, y = vd,), alpha = 0.2) +
    theme(legend.position = "bottom", text = element_text(size = 20)) + 
    labs(x = "Temperature ºC",y = bquote(R[e]))
dev.off()
setwd("..")




#############################
# Specific Humidity
#############################

#Load GAM results
load("Data/GAM_USA_P1.RData")
load("Data/GAM_USA_P2.RData")

############################
#Prepare Data for plots
############################
r_P1 <- res_P1_SH_USA$Data
r_P2 <- res_P2_SH_USA$Data


for (i in 1:length(r_P1)){
    long <- length(r_P1[[i]]$SH)
    SH <- r_P1[[i]]$SH
    StringencyIndex <- rep(mean(r_P1[[i]]$StringencyIndex), long)
    Vac <- rep(0, long)
    Variants <- rep("Alpha", long)
    newData <- data.frame(SH,StringencyIndex, Vac, Variants)
    r_P1[[i]]$vd <- predict.gam(res_P1_SH_USA$Models[[i]], newdata = newData)
    long <-  length(r_P2[[i]]$SH)
    SH <-  r_P2[[i]]$SH
    StringencyIndex <- rep(mean(r_P1[[i]]$StringencyIndex), long)
    Vac <- rep(0, long)
    Variants <-  rep("Alpha", long)
    newData <- data.frame(SH,StringencyIndex, Vac, Variants)
    r_P2[[i]]$vd <- predict.gam(res_P2_SH_USA$Models[[i]], newdata = newData)
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
setwd("Supplementary_Figures")
jpeg(filename = "Gam_SH_USA.jpeg", 
    height = 800, width = 1000)
vars <- c("P1"="#0070bb", "P2" ="#f1c037")
ggplot() +
    theme_bw() + 
    facet_wrap(~Country,scales = "free") +
    geom_smooth(data = res_total, aes(x = SH, y = vd, 
        colour = Period), method = "gam", 
        formula = y ~ s(x, bs = "cs"), fill = "#b4b1b1") +
    scale_color_manual(name = "Period:", values = vars) +
    theme(legend.position = "bottom", text = element_text(size = 20))+ 
    #geom_hline(yintercept = 1, color = "red", linetype = 2) +
    labs(x = "Specific Humidity (g/kg)",y = bquote(R[e]))
dev.off()
setwd("..")



####################################################
# Comparison of the fitted model with the real data
####################################################
r_P1 <- res_P1_SH_USA$Data
r_P2 <- res_P2_SH_USA$Data

for (i in 1:length(r_P1)){
    long <- length(r_P1[[i]]$SH)
    SH <- r_P1[[i]]$SH
    StringencyIndex <- r_P1[[i]]$StringencyIndex
    Vac <- r_P1[[i]]$Vac
    Variants <- r_P1[[i]]$Variants
    newData <- data.frame(SH,StringencyIndex, Vac, Variants)
    r_P1[[i]]$pred <- predict.gam(res_P1_SH_USA$Models[[i]], newdata = newData)
    long <-  length(r_P2[[i]]$SH)
    SH <-  r_P2[[i]]$SH
    StringencyIndex <- r_P2[[i]]$StringencyIndex
    Vac <- r_P2[[i]]$Vac
    Variants <- r_P2[[i]]$Variants
    newData <- data.frame(SH,StringencyIndex, Vac, Variants)
    r_P2[[i]]$pred <- predict.gam(res_P2_SH_USA$Models[[i]], newdata = newData)
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

#P1
setwd("Supplementary_Figures")
jpeg(filename = "Gam_P1_SH_USA.jpeg", 
    height = 800, width = 1000)
vars <- c("P1"="#0070bb", "P2" ="#f1c037")
ggplot() +
    theme_bw() + 
    facet_wrap(~Country,scales = "free") +
    geom_smooth(data = res_total[res_total$Period=="P1",], aes(x = SH, y = pred, 
        colour = "blue"), method = "gam", 
        formula = y ~ s(x, bs = "cs"), fill = "#0070bb", colour = "#0070bb") +
    geom_point(data = res_total[res_total$Period=="P1",], 
        aes(x = SH, y = vd,), alpha = 0.1) +
    theme(legend.position = "bottom", text = element_text(size = 20)) + 
    labs(x = "Specific Humidity (g/kg)",y = bquote(R[e]))
dev.off()
setwd("..")

#P2
setwd("Supplementary_Figures")
jpeg(filename = "Gam_P2_SH_USA.jpeg", 
    height = 800, width = 1000)
vars <- c("P1"="#0070bb", "P2" ="#f1c037")
ggplot() +
    theme_bw() + 
    facet_wrap(~Country,scales = "free") +
    geom_smooth(data = res_total[res_total$Period=="P2",], aes(x = SH, y = pred), 
        method = "gam", 
        formula = y ~ s(x, bs = "cs"), fill = "#f1c037", colour = "#f1c037") +
    geom_point(data = res_total[res_total$Period=="P2",], 
        aes(x = SH, y = vd,), alpha = 0.2) +
    theme(legend.position = "bottom", text = element_text(size = 20)) + 
    labs(x = "Specific Humidity (g/kg)",y = bquote(R[e]))
dev.off()
setwd("..")



######################
## FULL PERIOD PLOTS
######################

load("Data/FullPeriod_USA.RData")

#################################
# Evolution of pandemic graphics
#################################

#Prepare the data for the plot
dat <- list()
for(i in 1:nrow(newCases)){
    datos <- data.frame(newCases[i,], newDeaths[i,], colnames(newCases))
    names(datos) <- c("Cases", "Deaths", "date")
    datos$date <- as.Date(datos$date)
    dat <- list.append(dat, datos)
}
names(dat) <- rownames(newCases)

for(i in names(dat)){
    dat[[i]]$Country = i
}
com = do.call("rbind",dat)
caseColor <- "#1376b0"
deathColor <- "#e8282d"
coeff <- 0.01

setwd("Supplementary_Figures")
jpeg(file = "Deaths_vs_cases_USA.jpeg", height = 800, width = 1200)
ggplot(data = com, aes(x = date)) +
    theme_bw() + 
    facet_wrap(~Country,scales = "free") +
    geom_bar(aes(y=Cases), stat="identity", linewidth =.1, 
        fill=caseColor, color=caseColor, alpha=.4) + 
    geom_line( aes(y=Deaths / coeff), linewidth=0.5, color=deathColor) +
    scale_y_continuous(
        # Features of the first axis
        name = "Nº Cases",
        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name="Nº Deaths")
    ) + 
    theme(
        text = element_text(size = 13),
        axis.title.y = element_text(color = caseColor, size=13),
        axis.title.y.right = element_text(color = deathColor, size=13)
    ) +
    xlab("Date")
dev.off()

##################################
#Evolution of vaccination in USA
##################################

#Prepare the data for the plot
Vac2 <- Vac
fechas <- colnames(Vac2)
fechas <- fechas[which(fechas =="2021-01-01"):which(fechas =="2021-12-31")]
num <- length(fechas)
paises <- rownames(Vac2)
Vac2 <- Vac2[,fechas]
VacRate <- c(Vac2[1,],Vac2[2,],Vac2[3,],Vac2[4,],Vac2[5,],Vac2[6,])
dates <- rep(fechas,6)
dates <- as.Date(dates)
VacUSA <- data.frame(VacRate, dates)
paisesVac <- c(
    rep(paises[1], num), rep(paises[2], num), rep(paises[3], num), rep(paises[4], num),
    rep(paises[5], num), rep(paises[6], num))

paises <- paisesVac
VacUSA$Country <- paises

#Create and save the plor
jpeg(file = "Vaccination_USA.jpeg", height = 800, width = 1200)
ggplot(VacUSA, aes(dates)) +
    theme_bw() +
    geom_line(aes(y = VacRate, color = Country), linewidth = 2) +
    xlab("Date") + 
    ylab("Vaccination rate %") +
    scale_colour_brewer(palette="Set2") +
    theme(legend.position = "right", text = element_text(size = 20))
dev.off()


rm(list = ls())
setwd("..")