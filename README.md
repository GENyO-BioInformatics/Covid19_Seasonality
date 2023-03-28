# Covid19_Seasonality
This repository contains the functions and data to reproduce results of the article: "Examining the Impact of Population Immunity on the Relationship Between Climate Variables and SARS-CoV-2 Transmission"

## Data

The "Data" folder consists of three files, namely SpainRawData.RData, ItaliaRawData.RData, and EuropeRawData.RData. These files contain the various variables that were downloaded and analyzed for each of the European countries, as well as the regions of Spain and Italy.

## Code, results and figures
Using the Run_code.R file, all the results and figures that appear in the article are generated. Neverthless, each of the files that contains the different functions can also be executed individually.

### Process data

 * The files Process_Spain_Raw.R, Process_Italy_Raw.R and Process_Europe_Raw.R process the data files SpainRawData.RData, ItaliaRawData.RData and EuropeRawData.RData to prepare the data for analysis.

### Apply models and results

* The file GAM_Function.R contains the neccessary functions to apply the GAM models.

* The files Spain_models.R, Europe_models.R and Italy_models.R apply the GAM models to the processed data and obtains the tables included in the study.

### Figures

* The files Spain_Figures.R, Europe_Figures.R and Italy_Figure.R create from the processed data and the results of the GAM models the figures contained in the article

