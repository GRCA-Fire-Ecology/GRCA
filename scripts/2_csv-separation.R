# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2024-09-30
# Last Edited: 2024-09-30
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
my_path_data <- "C:/Users/alalor.NPS/OneDrive - DOI/FireFX2.0/FFI Data Management/Queries/R/"


################################################################################
# LOAD DATA
################################################################################

CWD <- read.csv(paste0(my_path_data, "Surface Fuels - 1000Hr_XPT.csv"))

glimpse(CWD)

CWD <- CWD %>%
  separate(MacroPlot.Name, sep = " ", into = c("MonitoringType", "Plot"), remove = FALSE)


CWD_PIPO <- CWD %>%
  filter(MonitoringType == "PIPO") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_data, "PIPO_Surface Fuels - 1000Hr_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")

CWD_PIED <- CWD %>%
  filter(MonitoringType == "PIED") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_data, "PIED_Surface Fuels - 1000Hr_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")

CWD_SHOS <- CWD %>%
  filter(MonitoringType == "SHOS") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_data, "SHOS_Surface Fuels - 1000Hr_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")

CWD_PIPN <- CWD %>%
  filter(MonitoringType == "PIPN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_data, "PIPN_Surface Fuels - 1000Hr_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")

CWD_PIEN <- CWD %>%
  filter(MonitoringType == "PIEN") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_data, "PIEN_Surface Fuels - 1000Hr_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")

CWD_PSME <- CWD %>%
  filter(MonitoringType == "PSME") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_data, "PSME_Surface Fuels - 1000Hr_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")

CWD_PIAB <- CWD %>%
  filter(MonitoringType == "PIAB") %>%
  select(-c(MonitoringType, Plot)) %>%
  write.csv(paste0(my_path_data, "PIAB_Surface Fuels - 1000Hr_XPT.csv"), quote=FALSE, row.names = FALSE, na = "")







