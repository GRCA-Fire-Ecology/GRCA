
# Load packages

# tidyverse and dplyr have lots of useful functions for data cleaning
library(tidyverse)
# here is useful for easily finding the working directory of any project
library(here)


# Adjust File Paths

path_data <- paste0(here(), "/data/MB-data-export/")


# Load Data

Fuels1000_all <- read.csv(paste0(path_data, "surface_fuels_1000hr_Alaska.csv"))
FuelsDuffLittAK_all <- read.csv(paste0(path_data, "surface_fuels_alaska_duff_litter_Alaska.csv"))
FuelsDuffLitt_all <- read.csv(paste0(path_data, "surface_fuels_duff_litter_Alaska.csv"))
FuelsFine_all <- read.csv(paste0(path_data, "surface_fuels_fine_Alaska.csv"))
#PostBurn_all <- read.csv(paste0(path_data, "Post Burn Severity_XPT.csv"), quote = "")
Trees_all <- read.csv(paste0(path_data, "tree_individuals_Alaska.csv"))
Saplings_all <- read.csv(paste0(path_data, "tree_saplings_Alaska.csv"))
Seedlings_all <- read.csv(paste0(path_data, "tree_seedlings_Alaska.csv"))
#Shrubs_all <- read.csv(paste0(path_data, "Density - Belts (metric)_XPT.csv"), quote = "")
HerbsObs_all <- read.csv(paste0(path_data, "cover_species_composition_Alaska.csv"))
HerbsPoints_all <- read.csv(paste0(path_data, "cover_frequency_Alaska.csv"))

