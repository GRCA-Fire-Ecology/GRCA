# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2024-12-02
# Last Edited: 2024-12-02
#
# data wrangling GRCA FFI exports


################################################################################
# BEFORE STARTING
################################################################################

#install packages
#install.packages("tidyverse")
#load packages
library(tidyverse)


################################################################################
# MAKE SURE FILE PATHS ARE CORRECT
################################################################################

#identify working directory (specifically user name)
getwd()

#load in data and name them based on file path
#change file path based on user name!
path_data <- "C:/Users/alalor.NPS/OneDrive - DOI/FireFX2.0/FFI Data Management/Queries/R Exports/GRCA_FMH/All Years All Fields/"


################################################################################
# LOAD IN DATA
################################################################################

Trees_all <- read.csv(paste0(path_data, "Trees - Individuals (metric)_XPT.csv"))


# Clean up data frame
Trees_all <- Trees_all %>%
  # Separate Date column into Date and Time columns
  separate(Date, sep = " ", into = c("Date", "Time")) %>%   separate(Date, sep = "/", into = c("Month", "Day", "Year"), remove = FALSE) %>%
  select(!c("Month", "Day")) %>%
  # Separate MacroPlotName to get Monitoring type (or export data with Monitoring Type already included)
  separate(MacroPlot.Name, sep = " ", into = c("MonitoringType", "Plot"), remove = FALSE)

# Ensure blanks in Visited column are NA
Trees_all$Visited[Trees_all$Visited==""] <- NA


################################################################################
# FIGURE OUT DATA
################################################################################

str(Trees_all)
