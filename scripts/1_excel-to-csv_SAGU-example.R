## Created by: Alexandra Lalor
## Email: alexandra_lalor@nps.gov, allielalor@gmail.com
## Date Created: 2024-05-01
## Last Edited: 2024-05-22
##
## To take data from excel files and save individual protocols/tabs as CSVs, and name them appropriately

## Folder Setup:
## Navigate to where you store FX excel data files (e.g. data/GRCA/FMH/2023/)
## In this folder, create 2 subfolders. One called "Collected", and the other called "_CSV_Import to FFI"
## Put all your completed data collection excel spreadsheets into the "Collected" folder.
#### Make sure excel files are named as follows: MonitoringType_Plot#_Status (e.g. PIPN_08_02Year05.xlsx)
## The "_CSV_Import to FFI" folder should be empty. This is where CSVs will be stored after running this code.


################################################################################
################################################################################
# USE THIS CODE IF FILE STRUCTURE DOES NOT INCLUDE MONITORING TYPE SUB FOLDERS


################################################################################
# BEFORE STARTING
################################################################################

#install packages
install.packages("tidyverse")
#load packages
library(tidyverse)
library(readxl)


################################################################################
# MAKE SURE FILE PATHS ARE CORRECT
################################################################################

#identify working directory (specifically user name)
getwd()

#my_path_data <- "X:/Data Collection/WACA/2020/Collected/"
#my_path_csv <- "X:/Data Collection/WACA/2020/_CSV_Import to FFI/"

#load in data and name them based on file path
#change file path based on user name!
my_path_data <- "C:/Users/alalor.NPS/Desktop/FX_Lalor/R/GRCA/test/data_raw/SAGU/2023/Collected/"
my_path_csv <- "C:/Users/alalor.NPS/Desktop/FX_Lalor/R/GRCA/test/data_raw/SAGU/2023/_CSV_Import to FFI/"


FX_rocks <- 1+5
FX_rocks <- "1+5"
FX_rox <- 1+5

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


path <- file_names_df[1,1]
name <- file_names_df[1,2]

FuelsFWD <- read_excel(path, sheet = "Fuels FWD")
FuelsCWD <- read_excel(path, sheet = "Fuels CWD")
FuelsDuffLitt <- read_excel(path, sheet = "Fuels Duff-Litt")
HerbsCover <- read_excel(path, sheet = "Cover")
Seedlings <- read_excel(path, sheet = "Seedlings")
Trees <- read_excel(path, sheet = "Trees")

colnames(FuelsFWD) <- c("I", NA, NA, NA, "1", "10", "100", "C", "Fuel Constant")
colnames(FuelsFWD)[colnames(FuelsFWD) == "1 Hour"] <- "OneHr"
colnames(FuelsFWD)[colnames(FuelsFWD) == "HunHr"] <- "100 Hour"
colnames(FuelsFWD)<-c(colnames(FuelsFWD)[1:4],"1 Hour", "10 Hour", "100 Hr", colnames(FuelsFWD)[8:9])
colnames(FuelsFWD)[5:7]<-c("1 Hour", "10 Hour", "100 Hr")

my_path_csv_FuelsFWD <- paste0(my_path_csv, name, "_FuelsFWD.csv")
my_path_csv_FuelsCWD <- paste0(my_path_csv, name, "_FuelsCWD.csv")
my_path_csv_FuelsDuffLitt <- paste0(my_path_csv, name, "_FuelsDuffLitt.csv")
my_path_csv_HerbsCover <- paste0(my_path_csv, name, "_HerbsCover.csv")
my_path_csv_Seedlings <- paste0(my_path_csv, name, "_Seedlings.csv")
my_path_csv_Trees <- paste0(my_path_csv, name, "_Trees.csv")

write.csv(FuelsFWD, my_path_csv_FuelsFWD, quote=FALSE, row.names = FALSE)
write.csv(FuelsCWD, my_path_csv_FuelsCWD, quote=FALSE, row.names = FALSE)
write.csv(FuelsDuffLitt, my_path_csv_FuelsDuffLitt, quote=FALSE, row.names = FALSE)
write.csv(HerbsCover, my_path_csv_HerbsCover, quote=FALSE, row.names = FALSE)
write.csv(Seedlings, my_path_csv_Seedlings, quote=FALSE, row.names = FALSE)
write.csv(Trees, my_path_csv_Trees, quote=FALSE, row.names = FALSE)




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
  HerbsCover <- read_excel(path, sheet = "Cover")
  Seedlings <- read_excel(path, sheet = "Seedlings")
  Trees <- read_excel(path, sheet = "Trees")

  my_path_csv_FuelsFWD <- paste0(my_path_csv, name, "_FuelsFWD.csv")
  my_path_csv_FuelsCWD <- paste0(my_path_csv, name, "_FuelsCWD.csv")
  my_path_csv_FuelsDuffLitt <- paste0(my_path_csv, name, "_FuelsDuffLitt.csv")
  my_path_csv_HerbsCover <- paste0(my_path_csv, name, "_HerbsCover.csv")
  my_path_csv_Seedlings <- paste0(my_path_csv, name, "_Seedlings.csv")
  my_path_csv_Trees <- paste0(my_path_csv, name, "_Trees.csv")

  write.csv(FuelsFWD, my_path_csv_FuelsFWD, quote=FALSE, row.names = FALSE)
  write.csv(FuelsCWD, my_path_csv_FuelsCWD, quote=FALSE, row.names = FALSE)
  write.csv(FuelsDuffLitt, my_path_csv_FuelsDuffLitt, quote=FALSE, row.names = FALSE)
  write.csv(HerbsCover, my_path_csv_HerbsCover, quote=FALSE, row.names = FALSE)
  write.csv(Seedlings, my_path_csv_Seedlings, quote=FALSE, row.names = FALSE)
  write.csv(Trees, my_path_csv_Trees, quote=FALSE, row.names = FALSE)
}
