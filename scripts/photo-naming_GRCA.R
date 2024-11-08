# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2024-08-02
# Last Edited: 2024-09-03
#
# test out photo naming efficiency


################################################################################
# BEFORE STARTING
################################################################################

#install packages
install.packages("tidyverse")
install.packages("diffr")
#load packages
library(tidyverse)
library(diffr)

#identify working directory
#setwd("/Users/alalor.NPS/Desktop/FX_Lalor/R/GRCA/test")
getwd()


################################################################################
# MAKE SURE FILE PATHS ARE CORRECT
################################################################################

#load in data and name them based on file path
#change file path based on user name!
my_path <- "C:/Users/alalor.NPS/Desktop/R/GRCA/test/photos/PIED/"


################################################################################
# CREATE LIST OF DATA NEEDED
################################################################################


#create list of folders
folder_names_list <- list.files(my_path, pattern = "^[0-9]")



#create list of excel file paths and names, add to empty data frame

file_names_df <- data.frame(FilePath = file_path_old, OldNames = file_names_old)

folder_names <- folder_names_list[2]
folder_path <- paste0(my_path, folder_names, "/")
file_names <- list.files(folder_path)
file_path <- paste0(folder_path, file_names)
file_names_df_1 <- data.frame(FilePath = file_path, text = file_names) %>%
  separate(text, sep = ".xlsx", into = ("Plot_Status"))
file_names_df <- rbind(file_names_df, file_names_df_1)



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
# MAKE SURE FILE PATHS ARE CORRECT
################################################################################

#load in data and name them based on file path
#change file path based on user name!
my_path <- "C:/Users/alalor.NPS/Desktop/R/GRCA/test/photos/PIPN/11_new/"


################################################################################
# CREATE LIST OF DATA NEEDED
################################################################################

#create list of file names
file_names_old <- list.files(my_path)

#specify file path each excel sheet
file_path_old <- paste0(my_path, file_names_old)

#add file paths and names to a dataframe
file_names_df <- data.frame(FilePath = file_path_old, OldNames = file_names_old) %>%
  separate(FilePath, sep = "/", into = c("Drive","Users","User","Desktop","R","GRCA","test","photos",
                                         "MonitoringType","PlotDelete","JPG"), remove = FALSE) %>%
  mutate(OldNames = toupper(OldNames)) %>%
  separate(OldNames, sep = ".JPG", into = ("Plot_Status"), remove = FALSE) %>%
  separate(Plot_Status, sep = "_", into = c("Plot", "Read", "PhotoPoint")) %>%
  select(c("FilePath", "MonitoringType", "Plot", "Read", "PhotoPoint", "OldNames"))


################################################################################
# ENSURE CONSISTENT FORMATTING
################################################################################

#data check with 01 reads and formatting
file_names_df <- file_names_df %>%
  mutate(Update = ifelse(grepl("^[0-9]", file_names_df$Read), "Yes", "No")) %>%
  mutate(Update = ifelse(Read == "PRE", "Yes", Update)) %>%
  mutate(Read = case_when((Update == "No" ~ paste0("01", Read)),
                                (Update == "Yes" ~ Read))) %>%
  mutate(Read = tolower(Read)) %>%
  mutate(PhotoPoint = toupper(PhotoPoint)) %>%
  mutate(NewNames = paste0(MonitoringType,"_",Plot,"_",Read,"_",PhotoPoint,".JPG"))


################################################################################
# RENAME FILES!
################################################################################

file_names_new <- file_names_df$NewNames
file_path_new <- paste0(my_path, file_names_new)
file.rename(file_path_old, file_path_new)


################################################################################
#compare with external drive

path_network <- "X:/Plots/GRCA - FMH/Photos/PIAB/01/"
path_external <- "D:/Firefx_Recovered/Plots/GRCA - FMH/Photos/PIAB/01/"


files_network <- list.files(path_network)
files_external <- list.files(path_external)

files_external ==  files_network
