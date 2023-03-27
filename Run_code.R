##################
#Launch all code
##################
#Install the neccessary packages


#Create the necessary folders
dir.create("Data")
dir.create("Figures")
dir.create("Tables")

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


