# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2024-05-01
# Last Edited: 2024-06-08
#
# To take data from excel files and save individual protocols/tabs as CSVs,
# and name them appropriately

# Folder Setup:
# Navigate to where you store FX excel data files (e.g. data/GRCA/FMH/2023/)
# In this folder, create 2 subfolders. One called "Collected", and the other called "_CSV_Import to FFI"
# Put all your completed data collection excel spreadsheets into the "Collected" folder.
#   Make sure excel files are named as follows: MonitoringType_Plot#_Status (e.g. PIPN_08_02Year05.xlsx)
# The "_CSV_Import to FFI" folder should be empty. This is where CSVs will be stored after running this code.


################################################################################
# BEFORE STARTING
################################################################################

#install packages
install.packages("tidyverse")
install.packages("googledrive")
#load packages
library(tidyverse)
library(readxl)
library(googledrive)

#identify working directory
#setwd("/Users/alalor.NPS/Desktop/FX_Lalor/R/GRCA/test")
getwd()

#connect with grandcanyonfirefx@gmail.com google account
#not working an not sure how to make it work
drive_auth(
  email = "grandcanyonfirefx@gmail.com",
  path = NULL,
  scopes = "https://www.googleapis.com/auth/drive",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)
