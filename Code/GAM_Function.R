#################################
#FUNCTIONS TO APPLY GAM MODELS
#################################

#TEMPERATURE
GamModel_Temp <- function(vd,
    Temp, StringencyIndex, Vac, variants){
    require(rlist)
    resultados <- matrix(NA, nrow = nrow(vd), ncol = 6)
    colnames(resultados) <- c("DevianceExplained", "R2Adjusted",
        "Temp", "StringencyIndex", "Vac", "Variants")
    rownames(resultados) <- rownames(vd)
    list.models <- list()
    data.models <- list()
    for(i in 1:nrow(vd)){
        datos_mod <- data.frame(vd[i,], Temp[i,], StringencyIndex[i,], 
            Vac[i,], variants)
        colnames(datos_mod) <- c("vd","Temp", "StringencyIndex",
            "Vac", "Variants")
        modeloGAM <- modeloGAM <- gam(vd ~ s(Temp) + StringencyIndex + 
                Vac + Variants, data = datos_mod, method = "REML")
        list.models <- list.append(list.models, modeloGAM)
        data.models <- list.append(data.models, datos_mod)
        modeloGAM <- summary(modeloGAM)
        resultados[i,1] <- modeloGAM$dev.expl * 100
        resultados[i,2] <- modeloGAM$r.sq
        resultados[i,3] <- modeloGAM$s.pv[1] #Temperature
        resultados[i,4] <- modeloGAM$p.pv[2] #StringencyIndex
        resultados[i,5] <- modeloGAM$p.pv[3] #Vac
        resultados[i,6] <- modeloGAM$p.pv[4] #Variants
    }
    names(list.models) <- rownames(vd)
    names(data.models) <- rownames(vd)
    models.results <- list(resultados, list.models, data.models)
    names(models.results) <- c("Gam", "Models", "Data")
    return(models.results)
}

#Specific Humidity
GamModel_SH <- function(vd,
    SH, StringencyIndex, Vac, variants){
    require(rlist)
    resultados <- matrix(NA, nrow = nrow(vd), ncol = 6)
    colnames(resultados) <- c("DevianceExplained", "R2Adjusted",
        "SH", "StringencyIndex", "Vac", "Variants")
    rownames(resultados) <- rownames(vd)
    list.models <- list()
    data.models <- list()
    for(i in 1:nrow(vd)){
        datos_mod <- data.frame(vd[i,], SH[i,], StringencyIndex[i,], 
            Vac[i,], variants)
        colnames(datos_mod) <- c("vd","SH", "StringencyIndex",
            "Vac", "Variants")
        modeloGAM <- modeloGAM <- gam(vd ~ s(SH) + StringencyIndex + 
                Vac + Variants, data = datos_mod, method = "REML")
        list.models <- list.append(list.models, modeloGAM)
        data.models <- list.append(data.models, datos_mod)
        modeloGAM <- summary(modeloGAM)
        resultados[i,1] <- modeloGAM$dev.expl * 100
        resultados[i,2] <- modeloGAM$r.sq
        resultados[i,3] <- modeloGAM$s.pv[1] #SH
        resultados[i,4] <- modeloGAM$p.pv[2] #StringencyIndex
        resultados[i,5] <- modeloGAM$p.pv[3] #Vac
        resultados[i,6] <- modeloGAM$p.pv[4] #Variants
    }
    names(list.models) <- rownames(vd)
    names(data.models) <- rownames(vd)
    models.results <- list(resultados, list.models, data.models)
    names(models.results) <- c("Gam", "Models", "Data")
    return(models.results)
}





#GAM FUNCTION WITHOUT VARIANTS
#TEMPERATURE
GamModel_variants <- function(vd,
    Temp, StringencyIndex, Vac){
    require(rlist)
    resultados <- matrix(NA, nrow = nrow(vd), ncol = 5)
    colnames(resultados) <- c("DevianceExplained", "R2Adjusted",
        "Temp", "StringencyIndex", "Vac")
    rownames(resultados) <- rownames(vd)
    list.models <- list()
    data.models <- list()
    for(i in 1:nrow(vd)){
        datos_mod <- data.frame(vd[i,], Temp[i,], StringencyIndex[i,], 
            Vac[i,])
        colnames(datos_mod) <- c("vd","Temp", "StringencyIndex",
            "Vac")
        modeloGAM <- modeloGAM <- gam(vd ~ s(Temp) + StringencyIndex + 
                Vac, data = datos_mod, method = "REML")
        list.models <- list.append(list.models, modeloGAM)
        data.models <- list.append(data.models, datos_mod)
        modeloGAM <- summary(modeloGAM)
        resultados[i,1] <- modeloGAM$dev.expl * 100
        resultados[i,2] <- modeloGAM$r.sq
        resultados[i,3] <- modeloGAM$s.pv[1] #Temperature
        resultados[i,4] <- modeloGAM$p.pv[2] #StringencyIndex
        resultados[i,5] <- modeloGAM$p.pv[3] #Vac
    }
    names(list.models) <- rownames(vd)
    names(data.models) <- rownames(vd)
    models.results <- list(resultados, list.models, data.models)
    names(models.results) <- c("Gam", "Models", "Data")
    return(models.results)
}




#GAM FUNCTION FOR MOBILITY VARIABLES AND Stringency Index 
#AS CROSSBASICS TO AVOID COLINEARITY

GamModelCrossbasics <- function(vd,
    Temp, StringencyIndex, Retail, Work, Residential, Vac, variants){
    require(rlist)
    require(dlnm)
    require(mgcv)
    resultados <- matrix(NA, nrow = nrow(vd), ncol = 9)
    colnames(resultados) <- c("DevianceExplained", "R2Adjusted",
        "Temp", "StringencyIndex", "Residential",
        "Retail", "Work", "Vac", "Variants")
    rownames(resultados) <- rownames(vd)
    list.models <- list()
    data.models <- list()
    for(i in 1:nrow(vd)){
        datos_mod <- data.frame(vd[i,], Temp[i,], 
            as.numeric(crossbasis(StringencyIndex[i,])), 
            as.numeric(crossbasis(Retail[i,])), as.numeric(crossbasis(Work[i,])), 
            as.numeric(crossbasis(Residential[i,])), Vac[i,], variants)
        colnames(datos_mod) <- c("vd","Temp", "StringencyIndex", "Residential",
            "Retail", "Work", "Vac", "Variants")
        modeloGAM <- modeloGAM <- gam(vd ~ s(Temp) + StringencyIndex + Retail + Work + 
                Residential + Vac + Variants, data = datos_mod, method = "REML")
        list.models <- list.append(list.models, modeloGAM)
        data.models <- list.append(data.models, datos_mod)
        modeloGAM <- summary(modeloGAM)
        resultados[i,1] <- modeloGAM$dev.expl * 100
        resultados[i,2] <- modeloGAM$r.sq
        resultados[i,3] <- modeloGAM$s.pv[1] #Temperature
        resultados[i,4] <- modeloGAM$p.pv[2] #StringencyIndex
        resultados[i,5] <- modeloGAM$p.pv[3] #Retail
        resultados[i,6] <- modeloGAM$p.pv[4] #Work
        resultados[i,7] <- modeloGAM$p.pv[5] #Residential
        resultados[i,8] <- modeloGAM$p.pv[6] #Vac
        resultados[i,9] <- modeloGAM$p.pv[7] #Variants
    }
    names(list.models) <- rownames(vd)
    names(data.models) <- rownames(vd)
    models.results <- list(resultados, list.models, data.models)
    names(models.results) <- c("Gam", "Models", "Data")
    return(models.results)
}
