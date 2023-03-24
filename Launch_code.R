##################
#Launch all code
##################

#Create the necessary folders
dir.create("Figures")
dir.create("Supplementary_Figures")
dir.create("Tables")




#Spain code
system("Rscript RawData/Process_Spain_Raw.R")
system("Rscript Code/Spain_models.R")
system("Rscript Code/Spain_Figures.R")

#Italy code
system("Rscript RawData/Process_Italy_Raw.R")
system("Rscript Code/Italy_models.R")
system("Rscript Code/Italy_Figures.R")


#Europe code
system("Rscript RawData/Process_Europe_Raw.R")
system("Rscript Code/Europe_models.R")
system("Rscript Code/Europe_Figures.R")



#USA code
system("Rscript RawData/Process_USA_Raw.R")
system("Rscript Code/USA_models.R")
system("Rscript Code/USA_Figures.R")
