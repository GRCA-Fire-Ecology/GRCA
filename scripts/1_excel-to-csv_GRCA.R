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


################################################################################
# MAKE SURE FILE PATHS ARE CORRECT
################################################################################

#load in data and name them based on file path
#Allie personal computer: "C:/Users/allie/OneDrive/Desktop/R Projects/GRCA/test/data_raw/SAGU/2023/Collected/"
#Allie personal computer: "C:/Users/allie/OneDrive/Desktop/R Projects/GRCA/test/data_raw/SAGU/2023/_CSV_Import to FFI/"

#my_path_data <- "X:/Data Collection/GRCA/2024/Collected/"
#my_path_csv <- "X:/Data Collection/CRCA/2024/_CSV_Import to FFI/"

my_path_data <- "C:/Users/alalor.NPS/OneDrive - DOI/FireFX2.0/Data Collection/GRCA - FMH/2024/Collected/"
my_path_csv <- "C:/Users/alalor.NPS/OneDrive - DOI/FireFX2.0/Data Collection/GRCA - FMH/2024/_CSV_Import to FFI/"

# my_path_data <- "C:/Users/alalor.NPS/Desktop/FX_Lalor/R/GRCA/test/data_raw/WACA/2020/Collected/"
# my_path_csv <- "C:/Users/alalor.NPS/Desktop/FX_Lalor/R/GRCA/test/data_raw/WACA/2020/_CSV_Import to FFI/"


################################################################################
# CREATE LIST OF DATA NEEDED
################################################################################

#create list of file names
file_names_list <- list.files(my_path_data)

#specify file path each excel sheet
file_path <- paste0(my_path_data, file_names_list)

#add file paths and names to a dataframe
file_names_df <- data.frame(FilePath = file_path, text = file_names_list) %>%
  separate(text, sep = ".xlsx", into = ("Plot_Status"))


################################################################################
# MAIN CODE / DO THE THING!
################################################################################


#separate excel files into tabs, save as CSVs, and name them appropriately
for(i in 1:nrow(file_names_df)) {
  path <- file_names_df[i,1]
  name <- file_names_df[i,2]

  FuelsFWD <- read_excel(path, sheet = "Fuels FWD")
  FuelsCWD <- read_excel(path, sheet = "Fuels CWD")
  FuelsDuffLitt <- read_excel(path, sheet = "Fuels Duff-Litt")
  HerbsPoints <- read_excel(path, sheet = "Herbs (Points)")
  HerbsObs <- read_excel(path, sheet = "Herbs-Ob (Sp Comp)")
  Shrubs <- read_excel(path, sheet = "Shrubs (Belt)")
  Seedlings <- read_excel(path, sheet = "Seedlings (Quad)")
  Trees <- read_excel(path, sheet = "Trees")
  #Collected <- read_excel(path, sheet = "Collected By")

  my_path_csv_FuelsFWD <- paste0(my_path_csv, name, "_FuelsFWD.csv")
  my_path_csv_FuelsCWD <- paste0(my_path_csv, name, "_FuelsCWD.csv")
  my_path_csv_FuelsDuffLitt <- paste0(my_path_csv, name, "_FuelsDuffLitt.csv")
  my_path_csv_HerbsPoints <- paste0(my_path_csv, name, "_HerbsPoints.csv")
  my_path_csv_HerbsObs<- paste0(my_path_csv, name, "_HerbsObs.csv")
  my_path_csv_Shrubs<- paste0(my_path_csv, name, "_Shrubs.csv")
  my_path_csv_Seedlings <- paste0(my_path_csv, name, "_Seedlings.csv")
  my_path_csv_Trees <- paste0(my_path_csv, name, "_Trees.csv")
  #my_path_csv_Collected <- paste0(my_path_csv, name, "_Collected.csv")

  write.csv(FuelsFWD, my_path_csv_FuelsFWD, quote = FALSE, row.names = FALSE)
  write.csv(FuelsCWD, my_path_csv_FuelsCWD, quote = FALSE, row.names = FALSE)
  write.csv(FuelsDuffLitt, my_path_csv_FuelsDuffLitt, quote = FALSE, row.names = FALSE)
  write.csv(HerbsPoints, my_path_csv_HerbsPoints, quote = FALSE, row.names = FALSE)
  write.csv(HerbsObs, my_path_csv_HerbsObs, quote = FALSE, row.names = FALSE)
  write.csv(Shrubs, my_path_csv_Shrubs, quote = FALSE, row.names = FALSE)
  write.csv(Seedlings, my_path_csv_Seedlings, quote = FALSE, row.names = FALSE)
  write.csv(Trees, my_path_csv_Trees, quote = FALSE, row.names = FALSE)
  #write.csv(Collected, my_path_csv_Collected, quote=FALSE, row.names = FALSE)
}


################################################################################
# IDEAS FOR DATA CHECKING WITHIN R
################################################################################


# All protocols, minus Trees
# Delete empty rows
# Change numbers in index column into ascending order
FuelsFWD <- subset(FuelsFWD, Transect != "") %>%
  mutate(Index = row_number())
FuelsCWD <- subset(FuelsCWD, Transect != "") %>%
  mutate(Index = row_number())
FuelsDuffLitt <- subset(FuelsDuffLitt, Transect != "") %>%
  mutate(Index = row_number())
HerbsPoints <- subset(HerbsPoints, Species != "") %>%
  mutate(Index = row_number())
HerbsObs <- subset(HerbsObs, Species != "") %>%
  mutate(Index = row_number())
Seedlings <- subset(Seedlings, Species != "") %>%
  mutate(Index = row_number())
Shrubs <- subset(Shrubs, Species != "") %>%
  mutate(Index = row_number())

# Trees
# Ensure tree order is triple sorted by “SubFrac”, “QTR”, and “TagNo” (smallest to largest)
# Check that index is in ascending order from top to bottom (1, 2, 3, …). Trees are commonly unsorted
# “IsVerified” column is TRUE
Trees <- subset(Trees, Species != "") %>%
  arrange(SubFrac, QTR, TagNo) %>%
  mutate(Index = row_number()) %>%
  mutate(IsVerified = "TRUE")







