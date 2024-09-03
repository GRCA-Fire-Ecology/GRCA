# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2024-08-02
# Last Edited: 2024-08-02
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
  separate(OldNames, sep = ".JPG", into = ("Plot_Status"), remove = FALSE) %>%
  separate(Plot_Status, sep = "_", into = c("Plot", "Read", "PhotoPoint")) %>%
  select(c("FilePath", "MonitoringType", "Plot", "Read", "PhotoPoint", "OldNames")) %>%
  mutate(NewNames = paste0(MonitoringType,"_",Plot,"_",Read,"_",PhotoPoint,".JPG"))

#data check with 01 reads and formating
file_names_df <- file_names_df %>%
  mutate(Read = tolower(Read))


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
