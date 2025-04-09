# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2024-08-01
# Last Edited: 2024-08-02
#
# recognize duplicates between herb points and herbs observed

################################################################################
# BEFORE STARTING
################################################################################

#install packages
install.packages("tidyverse")
#load packages
library(tidyverse)
library(readxl)

#identify working directory
#setwd("/Users/alalor.NPS/Desktop/FX_Lalor/R/GRCA/test")
getwd()

################################################################################
# MAKE SURE FILE PATHS ARE CORRECT
################################################################################

#load in data and name them based on file path

my_path_data <- "C:/Users/alalor.NPS/OneDrive - DOI/FireFX2.0/Data Collection/GRCA - FMH/2024/Collected/test/"
my_path_csv <- "C:/Users/alalor.NPS/OneDrive - DOI/FireFX2.0/Data Collection/GRCA - FMH/2024/_CSV_Import to FFI/test"

################################################################################
# CREATE LIST OF DATA NEEDED
################################################################################

#create list of file names
file_names_list <- list.files(my_path_data)

#specify file path each excel sheet
file_path <- paste0(my_path_data, file_names_list)

#add file paths and names to a dataframe
file_names_df <- data.frame(FilePath = file_path, text = file_names_list) %>%
  separate(text, sep = ".xlsx", into = ("Plot_Status")) %>%
  separate("Plot_Status", sep = "_", into = c("MonitoringType", "Plot", "Read", "Tablet"), remove = FALSE)



################################################################################
# MAIN CODE / DO THE THING!
################################################################################


#separate excel files into tabs, save as CSVs, and name them appropriately
# for(i in 1:nrow(file_names_df)) {
#   path <- file_names_df[i,1]
#   name <- file_names_df[i,2]
#
#   #read tabs of excel files, bring them into R
#   HerbsPoints <- read_excel(path, sheet = "Herbs (Points)")
#   HerbsObs <- read_excel(path, sheet = "Herbs-Ob (Sp Comp)")
#
#   #create csv paths
#   my_path_csv_HerbsPoints <- paste0(my_path_csv, name, "_HerbsPoints.csv")
#   my_path_csv_HerbsObs<- paste0(my_path_csv, name, "_HerbsObs.csv")
#
#   # QAQC all protocols, minus Trees, Delete empty rows, Change numbers in index column into ascending order
#   HerbsPointsCount <- sum(!is.na(HerbsPoints$Height))
#   HerbsPoints <-
#     mutate(HerbsPoints, Count = HerbsPointsCount) %>%
#     subset(Count != "0") %>%
#     mutate(Index = row_number()) %>%
#     map_df(str_replace_all, pattern = ",", replacement = ";")
#   HerbsObs <- subset(HerbsObs, Species != "") %>%
#     mutate(Index = row_number()) %>%
#     map_df(str_replace_all, pattern = ",", replacement = ";")
#
#   #create CSVs, exclude blank data frames
#   if(dim(HerbsPoints)[1] == 0) {print(paste0(name," ","Herbs Points is empty"))}
#   else{write.csv(HerbsPoints, my_path_csv_HerbsPoints, quote=FALSE, row.names = FALSE, na = "")}
#   if(dim(HerbsObs)[1] == 0) {print(paste0(name," ","Herbs Obs is empty"))}
#   else{write.csv(HerbsObs, my_path_csv_HerbsObs, quote=FALSE, row.names = FALSE, na = "")}
# }



path <- file_names_df[1,1]
name <- file_names_df[1,2]

#read tabs of excel files, bring them into R
HerbsPoints <- read_excel(path, sheet = "Herbs (Points)")
HerbsObs <- read_excel(path, sheet = "Herbs-Ob (Sp Comp)")
Shrubs <- read_excel(path, sheet = "Shrubs (Belt)")
Seedlings <- read_excel(path, sheet = "Seedlings (Quad)")
Trees <- read_excel(path, sheet = "Trees")

#create csv paths
my_path_csv_HerbsPoints <- paste0(my_path_csv, name, "_HerbsPoints.csv")
my_path_csv_HerbsObs<- paste0(my_path_csv, name, "_HerbsObs.csv")
my_path_csv_Shrubs<- paste0(my_path_csv, name, "_Shrubs.csv")
my_path_csv_Seedlings <- paste0(my_path_csv, name, "_Seedlings.csv")
my_path_csv_Trees <- paste0(my_path_csv, name, "_Trees.csv")

# QAQC all protocols, minus Trees, Delete empty rows, Change numbers in index column into ascending order
#make sure there are hits on the herb line, add these up
HerbsPointsCount <- sum(!is.na(HerbsPoints$Height))
#add herbs count, remove data if = 0, update index, replace , with ;
HerbsPoints <-
  mutate(HerbsPoints, Count = HerbsPointsCount) %>%
  subset(Count != "0") %>%
  mutate(Index = row_number()) %>%
  map_df(str_replace_all, pattern = ",", replacement = ";")
Seedlings <- subset(Seedlings, Species != "") %>%
  mutate(Index = row_number()) %>%
  map_df(str_replace_all, pattern = ",", replacement = ";")
Shrubs <- subset(Shrubs, Species != "") %>%
  mutate(Index = row_number()) %>%
  map_df(str_replace_all, pattern = ",", replacement = ";")
Trees <- subset(Trees, Status != "X") %>%
  arrange(SubFrac, QTR, TagNo) %>%
  mutate(Index = row_number()) %>%
  mutate(IsVerified = "TRUE") %>%
  map_df(str_replace_all, pattern = ",", replacement = ";")



#identify duplicate species in Herbs Obs
DuplicateHerbs <-
  merge(HerbsPoints, HerbsObs, by = "Species") %>%
  select(Species) %>%
  distinct(Species)

DuplicateShrubs <-
  merge(Shrubs, HerbsObs, by = "Species") %>%
  select(Species) %>%
  distinct(Species)

DuplicateSeedlings <-
  merge(Seedlings, HerbsObs, by = "Species") %>%
  select(Species) %>%
  distinct(Species)

DuplicateTrees <-
  merge(Trees, HerbsObs, by = "Species") %>%
  select(Species) %>%
  distinct(Species)

DuplicateSpecies <- rbind(DuplicateHerbs, DuplicateShrubs, DuplicateSeedlings, DuplicateTrees)

#remove duplicate species from Herbs Obs
HerbsObs <- anti_join(HerbsObs, DuplicateSpecies, by = "Species")


#remove data if = 0, update index, replace , with ;
HerbsObs <- subset(HerbsObs, Species != "") %>%
  mutate(Index = row_number()) %>%
  map_df(str_replace_all, pattern = ",", replacement = ";")



#create CSVs, exclude blank data frames
if(dim(HerbsPoints)[1] == 0) {print(paste0(name," ","Herbs Points is empty"))}else{write.csv(HerbsPoints, my_path_csv_HerbsPoints, quote=FALSE, row.names = FALSE, na = "")}
if(dim(HerbsObs)[1] == 0) {print(paste0(name," ","Herbs Obs is empty"))}else{write.csv(HerbsObs, my_path_csv_HerbsObs, quote=FALSE, row.names = FALSE,na = "")}

