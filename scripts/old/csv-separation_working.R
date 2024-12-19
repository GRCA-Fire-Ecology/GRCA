# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2024-09-30
# Last Edited: 2024-10-02
#
# Take mass FFI CSV export, separate by monitoring type

################################################################################
# BEFORE STARTING
################################################################################

#install packages
install.packages("tidyverse")
#load packages
library(tidyverse)

################################################################################
# MAKE SURE FILE PATHS ARE CORRECT
################################################################################

#identify working directory (specifically user name)
getwd()

#load in data and name them based on file path
#change file path based on user name!
my_path_data <- "C:/Users/alalor.NPS/OneDrive - DOI/FireFX2.0/FFI Data Management/Queries/R/2024/"
#my_path_data <- "X:/Exports from FFI - PLEASE USE VERSIONING/OriginalExports_01OCT2024/2024_10SEP_GRCA_AllFMH_NotSCPN/"
my_path_csv <- "C:/Users/alalor.NPS/Desktop/R/GRCA/test/data_qaqc/2024/"


################################################################################
# LOAD DATA
################################################################################

#read csvs
FuelsFWD <- read.csv(paste0(my_path_data, "Surface Fuels - Fine_XPT.csv"))
FuelsCWD <- read.csv(paste0(my_path_data, "Surface Fuels - 1000Hr_XPT.csv"))
FuelsDuffLitt <- read.csv(paste0(my_path_data, "Surface Fuels - Duff_Litter_XPT.csv"))
HerbsPoints <- read.csv(paste0(my_path_data, "Cover - Points (metric)_XPT.csv"))
HerbsObs <- read.csv(paste0(my_path_data, "Cover - Species Composition (metric)_XPT.csv"))
Shrubs <- read.csv(paste0(my_path_data, "Density - Belts (metric)_XPT.csv"))
Seedlings <- read.csv(paste0(my_path_data, "Density - Quadrats (metric)_XPT.csv"))
Trees <- read.csv(paste0(my_path_data, "Trees - Individuals (metric)_XPT.csv"))
PostBurn <- read.csv(paste0(my_path_data, "Post Burn Severity_XPT.csv"))

#create Monitoring Type column
FuelsFWD <- FuelsFWD %>%
  separate(MacroPlot.Name, sep = " ", into = c("MonitoringType", "Plot"), remove = FALSE)
FuelsCWD <- FuelsCWD %>%
  separate(MacroPlot.Name, sep = " ", into = c("MonitoringType", "Plot"), remove = FALSE)
FuelsDuffLitt <- FuelsDuffLitt %>%
  separate(MacroPlot.Name, sep = " ", into = c("MonitoringType", "Plot"), remove = FALSE)
HerbsPoints <- HerbsPoints %>%
  separate(MacroPlot.Name, sep = " ", into = c("MonitoringType", "Plot"), remove = FALSE)
HerbsObs <- HerbsObs %>%
  separate(MacroPlot.Name, sep = " ", into = c("MonitoringType", "Plot"), remove = FALSE)
Shrubs <- Shrubs %>%
  separate(MacroPlot.Name, sep = " ", into = c("MonitoringType", "Plot"), remove = FALSE)
Seedlings <- Seedlings %>%
  separate(MacroPlot.Name, sep = " ", into = c("MonitoringType", "Plot"), remove = FALSE)
Trees <- Trees %>%
  separate(MacroPlot.Name, sep = " ", into = c("MonitoringType", "Plot"), remove = FALSE)
PostBurn <- PostBurn %>%
  separate(MacroPlot.Name, sep = " ", into = c("MonitoringType", "Plot"), remove = FALSE)

#check unique Monitoring Types
unique(FuelsCWD$MonitoringType)


################################################################################
#separate by monitoring type and save as csv
################################################################################

#FuelsFWD
FuelsFWD_PIPN <- FuelsFWD %>%
  filter(MonitoringType == "PIPN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIPN_Surface Fuels - Fine_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
FuelsFWD_PIEN <- FuelsFWD %>%
  filter(MonitoringType == "PIEN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIEN_Surface Fuels - Fine_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
FuelsFWD_PIAB <- FuelsFWD %>%
  filter(MonitoringType == "PIAB") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIAB_Surface Fuels - Fine_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
FuelsFWD_PIPO <- FuelsFWD %>%
  filter(MonitoringType == "PIPO") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIPO_Surface Fuels - Fine_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
FuelsFWD_PIED <- FuelsFWD %>%
  filter(MonitoringType == "PIED") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIED_Surface Fuels - Fine_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
FuelsFWD_SHOS <- FuelsFWD %>%
  filter(MonitoringType == "SHOS") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "SHOS_Surface Fuels - Fine_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")

#FuelsCWD
FuelsCWD_PIPN <- FuelsCWD %>%
  filter(MonitoringType == "PIPN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIPN_Surface Fuels - 1000Hr_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
FuelsCWD_PIEN <- FuelsCWD %>%
  filter(MonitoringType == "PIEN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIEN_Surface Fuels - 1000Hr_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
FuelsCWD_PIAB <- FuelsCWD %>%
  filter(MonitoringType == "PIAB") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIAB_Surface Fuels - 1000Hr_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
FuelsCWD_PIPO <- FuelsCWD %>%
  filter(MonitoringType == "PIPO") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIPO_Surface Fuels - 1000Hr_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
FuelsCWD_PIED <- FuelsCWD %>%
  filter(MonitoringType == "PIED") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIED_Surface Fuels - 1000Hr_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
FuelsCWD_SHOS <- FuelsCWD %>%
  filter(MonitoringType == "SHOS") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "SHOS_Surface Fuels - 1000Hr_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")

#FuelsDuffLitt
FuelsDuffLitt_PIPN <- FuelsDuffLitt %>%
  filter(MonitoringType == "PIPN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIPN_Surface Fuels - Duff_Litter_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
FuelsDuffLitt_PIEN <- FuelsDuffLitt %>%
  filter(MonitoringType == "PIEN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIEN_Surface Fuels - Duff_Litter_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
FuelsDuffLitt_PIAB <- FuelsDuffLitt %>%
  filter(MonitoringType == "PIAB") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIAB_Surface Fuels - Duff_Litter_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
FuelsDuffLitt_PIPO <- FuelsDuffLitt %>%
  filter(MonitoringType == "PIPO") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIPO_Surface Fuels - Duff_Litter_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
FuelsDuffLitt_PIED <- FuelsDuffLitt %>%
  filter(MonitoringType == "PIED") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIED_Surface Fuels - Duff_Litter_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
FuelsDuffLitt_SHOS <- FuelsDuffLitt %>%
  filter(MonitoringType == "SHOS") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "SHOS_Surface Fuels - Duff_Litter_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")

#HerbsPoints
HerbsPoints_PIPN <- HerbsPoints %>%
  filter(MonitoringType == "PIPN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIPN_Cover - Points (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
HerbsPoints_PIEN <- HerbsPoints %>%
  filter(MonitoringType == "PIEN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIEN_Cover - Points (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
HerbsPoints_PIAB <- HerbsPoints %>%
  filter(MonitoringType == "PIAB") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIAB_Cover - Points (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
HerbsPoints_PIPO <- HerbsPoints %>%
  filter(MonitoringType == "PIPO") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIPO_Cover - Points (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
HerbsPoints_PIED <- HerbsPoints %>%
  filter(MonitoringType == "PIED") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIED_Cover - Points (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
HerbsPoints_SHOS <- HerbsPoints %>%
  filter(MonitoringType == "SHOS") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "SHOS_Cover - Points (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")

#HerbsObs
HerbsObs_PIPN <- HerbsObs %>%
  filter(MonitoringType == "PIPN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIPN_Cover - Species Composition (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
HerbsObs_PIEN <- HerbsObs %>%
  filter(MonitoringType == "PIEN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIEN_Cover - Species Composition (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
HerbsObs_PIAB <- HerbsObs %>%
  filter(MonitoringType == "PIAB") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIAB_Cover - Species Composition (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
HerbsObs_PIPO <- HerbsObs %>%
  filter(MonitoringType == "PIPO") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIPO_Cover - Species Composition (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
HerbsObs_PIED <- HerbsObs %>%
  filter(MonitoringType == "PIED") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIED_Cover - Species Composition (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
HerbsObs_SHOS <- HerbsObs %>%
  filter(MonitoringType == "SHOS") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "SHOS_Cover - Species Composition (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")

#Shrubs
Shrubs_PIPN <- Shrubs %>%
  filter(MonitoringType == "PIPN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIPN_Density - Belts (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
Shrubs_PIEN <- Shrubs %>%
  filter(MonitoringType == "PIEN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIEN_Density - Belts (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
Shrubs_PIAB <- Shrubs %>%
  filter(MonitoringType == "PIAB") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIAB_Density - Belts (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
Shrubs_PIPO <- Shrubs %>%
  filter(MonitoringType == "PIPO") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIPO_Density - Belts (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
Shrubs_PIED <- Shrubs %>%
  filter(MonitoringType == "PIED") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIED_Density - Belts (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
Shrubs_SHOS <- Shrubs %>%
  filter(MonitoringType == "SHOS") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "SHOS_Density - Belts (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")

#Seedlings
Seedlings_PIPN <- Seedlings %>%
  filter(MonitoringType == "PIPN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIPN_Density - Quadrats (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
Seedlings_PIEN <- Seedlings %>%
  filter(MonitoringType == "PIEN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIEN_Density - Quadrats (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
Seedlings_PIAB <- Seedlings %>%
  filter(MonitoringType == "PIAB") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIAB_Density - Quadrats (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
Seedlings_PIPO <- Seedlings %>%
  filter(MonitoringType == "PIPO") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIPO_Density - Quadrats (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
Seedlings_PIED <- Seedlings %>%
  filter(MonitoringType == "PIED") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIED_Density - Quadrats (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
Seedlings_SHOS <- Seedlings %>%
  filter(MonitoringType == "SHOS") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "SHOS_Density - Quadrats (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")

#Trees
Trees_PIPN <- Trees %>%
  filter(MonitoringType == "PIPN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIPN_Trees - Individuals (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
Trees_PIEN <- Trees %>%
  filter(MonitoringType == "PIEN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIEN_Trees - Individuals (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
Trees_PIAB <- Trees %>%
  filter(MonitoringType == "PIAB") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIAB_Trees - Individuals (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
Trees_PIPO <- Trees %>%
  filter(MonitoringType == "PIPO") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIPO_Trees - Individuals (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
Trees_PIED <- Trees %>%
  filter(MonitoringType == "PIED") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIED_Trees - Individuals (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
Trees_SHOS <- Trees %>%
  filter(MonitoringType == "SHOS") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "SHOS_Trees - Individuals (metric)_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")

#PostBurn
PostBurn_PIPN <- PostBurn %>%
  filter(MonitoringType == "PIPN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIPN_Post Burn Severity_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
PostBurn_PIEN <- PostBurn %>%
  filter(MonitoringType == "PIEN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIEN_Post Burn Severity_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
PostBurn_PIAB <- PostBurn %>%
  filter(MonitoringType == "PIAB") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIAB_Post Burn Severity_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
PostBurn_PIPO <- PostBurn %>%
  filter(MonitoringType == "PIPO") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIPO_Post Burn Severity_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
PostBurn_PIED <- PostBurn %>%
  filter(MonitoringType == "PIED") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "PIED_Post Burn Severity_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")
PostBurn_SHOS <- PostBurn %>%
  filter(MonitoringType == "SHOS") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_csv, "SHOS_Post Burn Severity_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")




