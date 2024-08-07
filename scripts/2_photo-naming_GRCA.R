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

path_network <- "X:/Plots/GRCA - FMH/Photos/PIAB/01/"
path_external <- "D:/Firefx_Recovered/Plots/GRCA - FMH/Photos/PIAB/01/"


files_network <- list.files(path_network)
files_external <- list.files(path_external)

files_external ==  files_network
