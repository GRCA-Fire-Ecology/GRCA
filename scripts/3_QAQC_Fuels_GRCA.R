# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2024-11-04
# Last Edited: 2024-11-04

# Based off of Eva's SAGU data-cleaning-functions script
# Adapting to GRCA needs, and breaking down into more digestable chunks

# Folder Setup:
# within R project, create a "data" folder
# within the data folder, create a "data_qaqc" folder
# create a folder with "All Years" and the current year ("2024")


################################################################################
# BEFORE STARTING
################################################################################

# Install packages (if needed)
install.packages("tidyverse")

# Load packages
library(tidyverse)

# Identify working directory (specifically user name)
getwd()

# Load in data and name them based on file path
# Change file path based on user name!
data_path <- "C:/Users/alalor.NPS/Desktop/R/GRCA/data/data_qaqc/2024/"


################################################################################
# SURFACE FUELS - 1000HR CWD
################################################################################

# Load in data
FuelsCWD <- read.csv(paste0(data_path, "Surface Fuels - 1000Hr_XPT.csv"))

# Fix date column
FuelsCWD <- FuelsCWD %>%
  separate(Date, sep = " ", into = c("Date", "Time"))


#### Fuel 1000 DecayClass ####

# Set parameters of the QAQC test (what is the query, what is the error being targeted, what are the valid values)
saved_query <- "Fuel 1000 DecayClass"
error_message <- "Invalid decay class value"
valid_values <- c(3,4)

# Extract non-missing values from the specified column, filter out NA columns
FuelsCWD_DecayCl <- FuelsCWD %>%
  filter(!is.na(DecayCl))

# Find indices of values that are not in the list of valid_values
errors_DecayCl <- FuelsCWD_DecayCl %>%
  filter(!DecayCl %in% valid_values) %>%
  mutate("Saved Query" = saved_query) %>%
  select(c("Saved Query", "Date", "MacroPlot.Name", "DecayCl")) %>%
  mutate("Error" = paste(error_message, DecayCl)) %>%
  mutate("Fixed?" = "no","Explanation" = "","Queryer" = "") %>%
  select(!"DecayCl")



################################################################################
# SAVE ALL ERRORS
################################################################################

# Make empty data frame to store errors
errors <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(errors) <- c("Saved Query", "Date of Read", "Macro Plot ID", "Error", "Fixed?", "Explanation", "Queryer")

# Save to master error list
errors <- rbind(errors, errors_DecayCl)


