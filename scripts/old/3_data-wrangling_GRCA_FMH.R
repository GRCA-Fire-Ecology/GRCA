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

# Sort out columns

columns_all <- c("MacroPlot.Purpose",
                 "MacroPlot.Type",
                 "MacroPlot.Latitude",
                 "MacroPlot.Longitude",
                 "MacroPlot.UTM.X",
                 "MacroPlot.UTM.Y",
                 "MacroPlot.UTM.Zone",
                 "MacroPlot.PDOP",
                 "MacroPlot.Error",
                 "MacroPlot.Date.In",
                 "MacroPlot.Elevation",
                 "MacroPlot.Azimuth",
                 "MacroPlot.Aspect",
                 "MacroPlot.Hill.Slope",
                 "MacroPlot.Transect.Slope",
                 "MacroPlot.Comment",
                 "MacroPlot.UV1",
                 "MacroPlot.UV2",
                 "MacroPlot.UV3",
                 "MacroPlot.UV4",
                 "MacroPlot.UV5",
                 "MacroPlot.UV6",
                 "MacroPlot.UV7",
                 "MacroPlot.UV8",
                 "MacroPlot.Name",
                 "Monitoring.Status",
                 "Status.Prefix",
                 "Status.Base",
                 "Status.Suffix",
                 "Status.Order",
                 "Status.Comment",
                 "Status.UV1",
                 "Status.UV2",
                 "Status.UV3",
                 "Legacy.Monitoring.Status",
                 "SampleEvent_TreatmentUnit",
                 "Team",
                 "SampleEvent.Comment",
                 "SampleEvent.UV1",
                 "SampleEvent.UV2",
                 "SampleEvent.UV3",
                 "Date")
columns_keep <- c("MacroPlot.Purpose",
                  "MacroPlot.Type",
                  "MacroPlot.UV1",
                  "MacroPlot.Name",
                  "Monitoring.Status",
                  "Date")
columns_remove <- c("MacroPlot.Latitude",
                    "MacroPlot.Longitude",
                    "MacroPlot.UTM.X",
                    "MacroPlot.UTM.Y",
                    "MacroPlot.UTM.Zone",
                    "MacroPlot.PDOP",
                    "MacroPlot.Error",
                    "MacroPlot.Date.In",
                    "MacroPlot.Elevation",
                    "MacroPlot.Azimuth",
                    "MacroPlot.Aspect",
                    "MacroPlot.Hill.Slope",
                    "MacroPlot.Transect.Slope",
                    "MacroPlot.Comment",
                    "MacroPlot.UV2",
                    "MacroPlot.UV3",
                    "MacroPlot.UV4",
                    "MacroPlot.UV5",
                    "MacroPlot.UV6",
                    "MacroPlot.UV7",
                    "MacroPlot.UV8",
                    "Status.Prefix",
                    "Status.Base",
                    "Status.Suffix",
                    "Status.Order",
                    "Status.Comment",
                    "Status.UV1",
                    "Status.UV2",
                    "Status.UV3",
                    "Legacy.Monitoring.Status",
                    "SampleEvent_TreatmentUnit",
                    "Team",
                    "SampleEvent.Comment",
                    "SampleEvent.UV1",
                    "SampleEvent.UV2",
                    "SampleEvent.UV3")

# Clean up data frame
Trees_all <- Trees_all %>%
  # Separate Date column into Date and Time columns
  separate(Date, sep = " ", into = c("Date", "Time")) %>%   separate(Date, sep = "/", into = c("Month", "Day", "Year"), remove = FALSE) %>%
  select(!c("Month", "Day", "Time"))

# Ensure blanks in Visited column are NA
Trees_all$Visited[Trees_all$Visited==""] <- NA



################################################################################
# FIGURE OUT DATA
################################################################################

str(Trees_all)
