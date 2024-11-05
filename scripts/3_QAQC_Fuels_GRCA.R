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
Fuels1000_all <- read.csv(paste0(data_path, "Surface Fuels - 1000Hr_XPT.csv"))

# Fix date column and filter out header info
Fuels1000 <- Fuels1000_all %>%
  separate(Date, sep = " ", into = c("Date", "Time")) %>%
  separate(MacroPlot.Name, sep = " ", into = c("MonitoringType", "Plot"), remove = FALSE) %>%
  filter(is.na(Visited))

Fuels1000_header <- Fuels1000_all %>%
  separate(Date, sep = " ", into = c("Date", "Time")) %>%
  separate(MacroPlot.Name, sep = " ", into = c("MonitoringType", "Plot"), remove = FALSE) %>%
  filter(!is.na(Visited))

#### Fuel 1000 DecayClass ####

# Set parameters of the QAQC test (what is the query, what is the error being targeted, what are the valid values)
saved_query <- "Fuel 1000 DecayClass"
error_message <- "Invalid decay class value"
valid_values <- c(3,4)

# Identify errors
errors_DecayCl <- Fuels1000 %>%
  filter(!DecayCl %in% valid_values) %>%
  mutate("Saved Query" = saved_query) %>%
  select(c("Saved Query", "Date", "MacroPlot.Name", "DecayCl")) %>%
  mutate("Error" = paste(error_message, DecayCl)) %>%
  mutate("Fixed?" = "no","Explanation" = "","Queryer" = "") %>%
  select(!"DecayCl")


#### Fuel 1000 FuelConst (PIAB or PIED or PIEN or PIPN or PIPO) ####

# Set parameters of the QAQC test (what is the query, what is the error being targeted, what are the valid values)
saved_query <- "Fuel 1000 FuelConst"
error_message <- "Incorrect fuel constant entered"
valid_values <- c("PIAB", "PIED", "PIEN", "PIPN", "PIPO")

# Identify errors
# Check that monitoring type matches with Fuel Constant
errors_FuelConst_1 <- Fuels1000 %>%
  filter(!MonitoringType == CWDFuConSt) %>%
  filter(!MonitoringType == "SHOS") %>%
  mutate("Saved Query" = saved_query) %>%
  select(c("Saved Query", "Date", "MacroPlot.Name", "CWDFuConSt")) %>%
  mutate("Error" = paste(error_message, CWDFuConSt)) %>%
  mutate("Fixed?" = "no","Explanation" = "","Queryer" = "") %>%
  select(!"CWDFuConSt")
# Check that fuel constant is among the list of valid values
errors_FuelConst_2 <- Fuels1000 %>%
  filter(!CWDFuConSt %in% valid_values) %>%
  mutate("Saved Query" = saved_query) %>%
  select(c("Saved Query", "Date", "MacroPlot.Name", "CWDFuConSt")) %>%
  mutate("Error" = paste(error_message, CWDFuConSt)) %>%
  mutate("Fixed?" = "no","Explanation" = "","Queryer" = "") %>%
  select(!"CWDFuConSt")

# Merge errors
errors_FuelConst <- rbind(errors_FuelConst_1, errors_FuelConst_2)



################################################################################
# SAVE ALL ERRORS
################################################################################

# Make empty data frame to store errors
errors <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(errors) <- c("Saved Query", "Date of Read", "Macro Plot ID", "Error", "Fixed?", "Explanation", "Queryer")

# Save to master error list
errors <- rbind(errors, errors_DecayCl, errors_FuelConst)


