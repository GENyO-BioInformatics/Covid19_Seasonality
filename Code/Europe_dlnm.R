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
library(splines)
library(mvmeta)

#Load data
setwd("Data")
load("Europe_dlnm.RData")
setwd("..")


list_regPre <- list()
list_model <- list()
metar_data <- list()

coef1 <- coef2 <- matrix(NA, nrow = length(list_reg), 1, dimnames = list(names(list_reg)))
vcov1 <- vcov2 <- vector("list",length(list_reg))
names(vcov1) <- names(vcov2) <- names(list_reg)


#################
#Temperature
################

#Obtain variables
for(j in names(list_reg)){
    datReg <- list_reg[[j]]
    #Tranform vacccination in a factor
    vacI <- datReg$Vac
    vacI[datReg$Vac > 0] <- 1
    vacI <- factor(vacI, levels = c(0,1), labels = c("No", "Yes"))
    #Crossbasiscs
    cb_temp <- crossbasis(x = datReg$Temperature, lag = c(7,14), group = vacI)
    cb_SI <- crossbasis(x = datReg$StringencyIndex, lag = c(7,14), group = vacI)
    #Interation terms
    datenum <- as.numeric(as.Date(datReg$dates))
    day1 <- as.numeric(as.Date("2020-06-01"))
    day2 <- as.numeric(as.Date("2021-06-01"))
    int1 <- ((datenum-day1)/(day2-day1))*cb_temp
    int2 <- ((datenum-day2)/(day2-day1))*cb_temp
    #Create models
    modelp1 <- gam(datReg$Re ~ cb_temp + cb_SI + datReg$variants + vacI + int1 + ns(datReg$days, df = 2), 
        family = quasipoisson())
    modelp2 <- gam(datReg$Re ~ cb_temp + cb_SI + datReg$variants + vacI + int2 + ns(datReg$days, df = 2), 
        family = quasipoisson())
    #Predictions
    pred.p1 <- crosspred(cb_temp, modelp1, cen = mean(datReg$Temperature,na.rm = TRUE), 
        at = min(datReg$Temperature, na.rm = TRUE):max(datReg$Temperature,na.rm = TRUE))
    pred.p2 <- crosspred(cb_temp, modelp2, cen = mean(datReg$Temperature,na.rm = TRUE), 
        at = min(datReg$Temperature, na.rm = TRUE):max(datReg$Temperature, na.rm = TRUE))
    #New dataframe
    RR <- c(pred.p1$allRRfit,pred.p2$allRRfit)
    RRlow <- c(pred.p1$allRRlow, pred.p2$allRRlow)
    RRhigh <- c(pred.p1$allRRhigh, pred.p2$allRRhigh)
    Temp <- as.numeric(names(RR))
    Period <- c(rep("P1",length(RR)/2), rep("P2",length(RR)/2))
    newdata <- data.frame(RR, RRlow,RRhigh, Temp, Period)
    #Save models and data
    list_regPre[[j]] <- newdata
    list_model[[j]][[1]] <- modelp1
    list_model[[j]][[2]] <- modelp2
    names(list_model[[j]]) <- c("modelp1", "modelp2")
    #Data for the meta-regresion
    cen = mean(datReg$Temperature, na.rm = TRUE)
    red1 <- crossreduce(cb_temp, modelp1, cen = cen)
    red2 <- crossreduce(cb_temp, modelp2, cen = cen)
    coef1[j,] <- coef(red1)
    coef2[j,] <- coef(red2)
    vcov1[[j]] <- vcov(red1)
    vcov2[[j]] <- vcov(red2)
}



for (i in names(list_regPre)){
    list_regPre[[i]]$Country = i
}

datReg = do.call("rbind",list_regPre)

vars <- c("P1"="#0070bb", "P2" ="#f1c037")
dlnm_Europe_fig <- ggplot() +
    facet_wrap(~Country,scales = "free") +
    geom_path(data = datReg, aes(x = Temp, y = RR, 
        colour = Period), linewidth = 0.2) +
    scale_color_manual(name = "Period:", values = vars) +
    geom_ribbon(data = datReg, aes(x = Temp, y = RR,
        ymin = RRlow, ymax = RRhigh, fill = Period), alpha = 0.2) +
    scale_fill_manual(name = "Period:", values = vars) +
    geom_hline(yintercept = 1, color = "red", linetype = "dashed", linewidth = 0.2) +
    labs(x = "Temperature ºC",y = "RR") +
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


setwd("Figures")
ggsave(
    filename = "Supplementary_Figure_S13.jpeg",
    plot = dlnm_Europe_fig,
    scale = 1,
    width = 1400,
    height  = 1200,
    units = "px",
    dpi = 300
)

setwd("..")



###################
#Meta-regresion
###################
avgtemp <- sapply(list_reg, function(x) mean(x$Temperature,na.rm=T))
rangetmean <- sapply(list_reg,function(x) diff(range(x$Temperature,na.rm=T)))
regions <- data.frame(names(list_reg))
colnames(regions) <- "region"


mv1 <- mvmeta(coef1~ avgtemp + rangetmean, vcov1, regions)
mv2 <- mvmeta(coef2 ~ avgtemp + rangetmean, vcov2, regions)


#Pruebas
datanew <- data.frame(avgtemp=mean(tapply(avgtemp,regions$region,mean)),
    rangetmean=mean(tapply(rangetmean,regions$region,mean)))


mvpred1 <- predict(mv1,datanew,vcov=T,format="list")
mvpred2 <- predict(mv2,datanew,vcov=T,format="list")

library(dplyr)

Temperatures <- matrix(NA, nrow = length(list_reg$Portugal$Temperature), length(list_reg))
colnames(Temperatures) <- names(list_reg)
for (reg in names(list_reg)){
    Temperatures[,reg] <- list_reg[[reg]]$Temperature
}

Temperatures <- na.omit(Temperatures)
tmean <- rowMeans(Temperatures, na.rm = TRUE)

bvar <- onebasis(tmean)
cp1 <- crosspred(bvar, coef = mvpred1$fit, vcov = mvpred1$vcov, model.link = "log",
    at = tmean, cen = mean(tmean))
cp2 <- crosspred(bvar, coef = mvpred2$fit, vcov = mvpred2$vcov, model.link = "log",
    at = tmean, cen = mean(tmean))


#New dataframe
RR <- c(cp1$allRRfit, cp2$allRRfit)
RRlow <- c(cp1$allRRlow, cp2$allRRlow)
RRhigh <- c(cp1$allRRhigh, cp2$allRRhigh)
Temp <- as.numeric(names(RR))
Period <- c(rep("P1",length(RR)/2), rep("P2",length(RR)/2))
newMetadata <- data.frame(RR, RRlow,RRhigh, Temp, Period)

#ggplot2_meta-regresion

dlnm_meta_Europe_fig <- ggplot() +
    geom_path(data = newMetadata, aes(x = Temp, y = RR, 
        colour = Period), linewidth = 0.2) +
    scale_color_manual(name = "Period:", values = vars) +
    geom_ribbon(data = newMetadata, aes(x = Temp, y = RR,
        ymin = RRlow, ymax = RRhigh, fill = Period), alpha = 0.2) +
    scale_fill_manual(name = "Period:", values = vars) +
    geom_hline(yintercept = 1, color = "red", linetype = "dashed", linewidth = 0.2) +
    labs(x = "Temperature ºC",y = "RR") +
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

setwd("Figures")
ggsave(
    filename = "dlnm_meta_Europe_Temperature.jpeg",
    plot = dlnm_meta_Europe_fig,
    scale = 1,
    width = 1400,
    height  = 1200,
    units = "px",
    dpi = 300
)

setwd("..")

rm(list = ls())


#####################
## SH
#####################


#Load data
setwd("Data")
load("Europe_dlnm.RData")
setwd("..")


list_regPre <- list()
list_model <- list()
metar_data <- list()

coef1 <- coef2 <- matrix(NA, nrow = length(list_reg), 1, dimnames = list(names(list_reg)))
vcov1 <- vcov2 <- vector("list",length(list_reg))
names(vcov1) <- names(vcov2) <- names(list_reg)


#################
#SH
################

#Obtain variables
for(j in names(list_reg)){
    datReg <- list_reg[[j]]
    #Tranform vacccination in a factor
    vacI <- datReg$Vac
    vacI[datReg$Vac > 0] <- 1
    vacI <- factor(vacI, levels = c(0,1), labels = c("No", "Yes"))
    #Crossbasiscs
    cb_SH <- crossbasis(x = datReg$SH, lag = c(7,14), group = vacI)
    cb_SI <- crossbasis(x = datReg$StringencyIndex, lag = c(7,14), group = vacI)
    #Interation terms
    datenum <- as.numeric(as.Date(datReg$dates))
    day1 <- as.numeric(as.Date("2020-06-01"))
    day2 <- as.numeric(as.Date("2021-06-01"))
    int1 <- ((datenum-day1)/(day2-day1))*cb_SH
    int2 <- ((datenum-day2)/(day2-day1))*cb_SH
    #Create models
    modelp1 <- gam(datReg$Re ~ cb_SH + cb_SI + datReg$variants + vacI + int1 + ns(datReg$days, df = 2), 
        family = quasipoisson())
    modelp2 <- gam(datReg$Re ~ cb_SH + cb_SI + datReg$variants + vacI + int2 + ns(datReg$days, df = 2), 
        family = quasipoisson())
    #Predictions
    pred.p1 <- crosspred(cb_SH, modelp1, cen = mean(datReg$SH,na.rm = TRUE), 
        at = min(datReg$SH, na.rm = TRUE):max(datReg$SH,na.rm = TRUE))
    pred.p2 <- crosspred(cb_SH, modelp2, cen = mean(datReg$SH,na.rm = TRUE), 
        at = min(datReg$SH, na.rm = TRUE):max(datReg$SH, na.rm = TRUE))
    #New dataframe
    RR <- c(pred.p1$allRRfit,pred.p2$allRRfit)
    RRlow <- c(pred.p1$allRRlow, pred.p2$allRRlow)
    RRhigh <- c(pred.p1$allRRhigh, pred.p2$allRRhigh)
    SH <- as.numeric(names(RR))
    Period <- c(rep("P1",length(RR)/2), rep("P2",length(RR)/2))
    newdata <- data.frame(RR, RRlow,RRhigh, SH, Period)
    #Save models and data
    list_regPre[[j]] <- newdata
    list_model[[j]][[1]] <- modelp1
    list_model[[j]][[2]] <- modelp2
    names(list_model[[j]]) <- c("modelp1", "modelp2")
    #Data for the meta-regresion
    cen = mean(datReg$SH, na.rm = TRUE)
    red1 <- crossreduce(cb_SH, modelp1, cen = cen)
    red2 <- crossreduce(cb_SH, modelp2, cen = cen)
    coef1[j,] <- coef(red1)
    coef2[j,] <- coef(red2)
    vcov1[[j]] <- vcov(red1)
    vcov2[[j]] <- vcov(red2)
}



for (i in names(list_regPre)){
    list_regPre[[i]]$Country = i
}

datReg = do.call("rbind",list_regPre)

vars <- c("P1"="#0070bb", "P2" ="#f1c037")
dlnm_Europe_fig <- ggplot() +
    facet_wrap(~Country,scales = "free") +
    geom_path(data = datReg, aes(x = SH, y = RR, 
        colour = Period), linewidth = 0.2) +
    scale_color_manual(name = "Period:", values = vars) +
    geom_ribbon(data = datReg, aes(x = SH, y = RR,
        ymin = RRlow, ymax = RRhigh, fill = Period), alpha = 0.2) +
    scale_fill_manual(name = "Period:", values = vars) +
    geom_hline(yintercept = 1, color = "red", linetype = "dashed", linewidth = 0.2) +
    labs(x = "Specific humidity (g/kg)",y = "RR") +
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


setwd("Figures")
ggsave(
    filename = "Supplementary_Figure_S14.jpeg",
    plot = dlnm_Europe_fig,
    scale = 1,
    width = 1400,
    height  = 1200,
    units = "px",
    dpi = 300
)

setwd("..")



###################
#Meta-regresion
###################
avgSH <- sapply(list_reg, function(x) mean(x$SH,na.rm=T))
rangetmean <- sapply(list_reg,function(x) diff(range(x$SH,na.rm=T)))
regions <- data.frame(names(list_reg))
colnames(regions) <- "region"


mv1 <- mvmeta(coef1~ avgSH + rangetmean, vcov1, regions)
mv2 <- mvmeta(coef2 ~ avgSH + rangetmean, vcov2, regions)


#Pruebas
datanew <- data.frame(avgSH=mean(tapply(avgSH,regions$region,mean)),
    rangetmean=mean(tapply(rangetmean,regions$region,mean)))


mvpred1 <- predict(mv1,datanew,vcov=T,format="list")
mvpred2 <- predict(mv2,datanew,vcov=T,format="list")

library(dplyr)

SHs <- matrix(NA, nrow = length(list_reg$Portugal$SH), length(list_reg))
colnames(SHs) <- names(list_reg)
for (reg in names(list_reg)){
    SHs[,reg] <- list_reg[[reg]]$SH
}

SHs <- na.omit(SHs)
tmean <- rowMeans(SHs, na.rm = TRUE)

bvar <- onebasis(tmean)
cp1 <- crosspred(bvar, coef = mvpred1$fit, vcov = mvpred1$vcov, model.link = "log",
    at = tmean, cen = mean(tmean))
cp2 <- crosspred(bvar, coef = mvpred2$fit, vcov = mvpred2$vcov, model.link = "log",
    at = tmean, cen = mean(tmean))


#New dataframe
RR <- c(cp1$allRRfit, cp2$allRRfit)
RRlow <- c(cp1$allRRlow, cp2$allRRlow)
RRhigh <- c(cp1$allRRhigh, cp2$allRRhigh)
SH <- as.numeric(names(RR))
Period <- c(rep("P1",length(RR)/2), rep("P2",length(RR)/2))
newMetadata <- data.frame(RR, RRlow,RRhigh, SH, Period)

#ggplot2_meta-regresion

dlnm_meta_Europe_fig <- ggplot() +
    geom_path(data = newMetadata, aes(x = SH, y = RR, 
        colour = Period), linewidth = 0.2) +
    scale_color_manual(name = "Period:", values = vars) +
    geom_ribbon(data = newMetadata, aes(x = SH, y = RR,
        ymin = RRlow, ymax = RRhigh, fill = Period), alpha = 0.2) +
    scale_fill_manual(name = "Period:", values = vars) +
    geom_hline(yintercept = 1, color = "red", linetype = "dashed", linewidth = 0.2) +
    labs(x = "Specific humidity (g/kg)",y = "RR") +
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

setwd("Figures")
ggsave(
    filename = "dlnm_meta_Europe_SH.jpeg",
    plot = dlnm_meta_Europe_fig,
    scale = 1,
    width = 1400,
    height  = 1200,
    units = "px",
    dpi = 300
)

setwd("..")

rm(list = ls())
