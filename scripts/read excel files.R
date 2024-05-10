## Created by: Alexandra Lalor
## Email: alexandra_lalor@nps.gov, allielalor@gmail.com
## Date Created: 2024-05-01
## Last Edited: 2024-05-10
##
## To take data from excel files and save individual protocols/tabs as CSVs,
## and name them appropriately



################################################################################
# BEFORE STARTING
################################################################################

#install packages
install.packages("tidyverse")
#load packages
library(tidyverse)

#identify working directory
#setwd("/Users/alalor.NPS/Desktop/FX_Lalor/R/GRCA/test")
getwd()


################################################################################
# MAKE SURE FILE PATHS ARE CORRECT
################################################################################

#load in data and name them based on file path
my_path_data <- "C:/Users/allie/OneDrive/Desktop/R Projects/GRCA/test/data_raw/SAGU/2023/Collected/"
my_path_csv <- "C:/Users/allie/OneDrive/Desktop/R Projects/GRCA/test/data_raw/SAGU/2023/_CSV_Import to FFI/"
folder_names_list <- list.files(my_path_data)


################################################################################
# CREATE LIST OF DATA NEEDED
################################################################################


#make empty file names data frame
file_names_df <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(file_names_df) <- c("FilePath", "Plot_Status")


#create list of excel file paths and names, add to empty data frame
for(i in 1:length(folder_names_list)) {
  folder_names <- folder_names_list[i]
  folder_path <- paste0(my_path_data, folder_names, "/")
  file_names <- list.files(folder_path)
  file_path <- paste0(folder_path, file_names)
  file_names_df_1 <- data.frame(FilePath = file_path, text = file_names) %>%
    separate(text, sep = ".xlsx", into = ("Plot_Status"))
  file_names_df <- rbind(file_names_df, file_names_df_1)
}


################################################################################
# MAIN CODE / DO THE THING!
################################################################################


#separate excel files into tabs, save as CSVs, and name them appropriately
for(i in 1:nrow(file_names_df)) {
  path <- file_names_df[i,1]
  name <- file_names_df[i,2]
  my_path_csv_FuelsFWD <- paste0(my_path_csv, name, "_FuelsFWD.csv")
  my_path_csv_FuelsCWD <- paste0(my_path_csv, name, "_FuelsCWD.csv")
  my_path_csv_FuelsDuffLitt <- paste0(my_path_csv, name, "_FuelsDuffLitt.csv")
  my_path_csv_HerbsCover <- paste0(my_path_csv, name, "_HerbsCover.csv")
  my_path_csv_Seedlings <- paste0(my_path_csv, name, "_Seedlings.csv")
  my_path_csv_Trees <- paste0(my_path_csv, name, "_Trees.csv")

  FuelsFWD <- read_excel(path, sheet = "Fuels FWD")
  FuelsFWD <- read_excel(path, sheet = "Fuels FWD")
  FuelsCWD <- read_excel(path, sheet = "Fuels CWD")
  FuelsDuffLitt <- read_excel(path, sheet = "Fuels Duff-Litt")
  HerbsCover <- read_excel(path, sheet = "Cover")
  Seedlings <- read_excel(path, sheet = "Seedlings")
  Trees <- read_excel(path, sheet = "Trees")

  write.csv(FuelsFWD, my_path_csv_FuelsFWD, quote=FALSE, row.names = FALSE)
  write.csv(FuelsCWD, my_path_csv_FuelsCWD, quote=FALSE, row.names = FALSE)
  write.csv(FuelsDuffLitt, my_path_csv_FuelsDuffLitt, quote=FALSE, row.names = FALSE)
  write.csv(HerbsCover, my_path_csv_HerbsCover, quote=FALSE, row.names = FALSE)
  write.csv(Seedlings, my_path_csv_Seedlings, quote=FALSE, row.names = FALSE)
  write.csv(Trees, my_path_csv_Trees, quote=FALSE, row.names = FALSE)
}




