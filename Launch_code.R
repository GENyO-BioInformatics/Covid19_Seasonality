##################
#Launch all code
##################

#Create the necessary folders
dir.create("Figures")
dir.create("Supplementary_Figures")
dir.create("Tables")




#Spain code
system("Rscript Code/Spain_models.R")
system("Rscript Code/Spain_Figures.R")


#Europe code
system("Rscript Code/Europe_models.R")
system("Rscript Code/Europe_Figures.R")

#Italy code
system("Rscript Code/Italy_models.R")
system("Rscript Code/Italy_Figures.R")


