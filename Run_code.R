##################
#Launch all code
##################
#Install the necessary packages

packages <- c("ggplot2", "dlnm", "mgcv", "visreg", "mctest", "rlist", 
    "EpiEstim", "RColorBrewer", "gridExtra", "dplyr", "patchwork")

for (p in packages) {
    if (!require(p, character.only = TRUE)) {
        install.packages(p)
    }
}


#Process Raw Data
system("Rscript Code/Process_Spain_Raw.R")
system("Rscript Code/Process_Italy_Raw.R")
system("Rscript Code/Process_Europe_Raw.R")


#Models
system("Rscript Code/Spain_models.R")
system("Rscript Code/Europe_models.R")
system("Rscript Code/Italy_models.R")

#Figures
system("Rscript Code/Spain_Figures.R")
system("Rscript Code/Europe_Figures.R")
system("Rscript Code/Italy_Figures.R")


