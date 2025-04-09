# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2024-11-04
# Last Edited: 2024-11-04

# Based off of Eva's SAGU data-cleaning-functions script
# Adapting to GRCA needs, and breaking down into more digestible chunks

# Folder Setup:
# within R project, create a "data" folder
# within the data folder, create a "data_qaqc" folder and a "data_errors"
# create a folder with "All Years" and the current year ("2024")


################################################################################
# BEFORE STARTING
################################################################################

# Install packages (if needed)
install.packages("tidyverse")

# Load packages
library(tidyverse)
library(writexl)

# Identify working directory (specifically user name)
getwd()

# Load in data and name them based on file path
# Change file path based on user name!
data_path <- "C:/Users/alalor.NPS/Desktop/R/GRCA/data/data_qaqc/2024/ORIGINALS/"
errors_path <- "C:/Users/alalor.NPS/Desktop/R/GRCA/data/data_errors/2024/"


################################################################################
# SURFACE FUELS - 1000HR CWD ####
################################################################################
#### Load in data ####
Fuels1000_all <- read.csv(paste0(data_path, "Surface Fuels - 1000Hr_XPT.csv"))

## NEED TO MAKE SURE COLUMNS HAVE SIMILAR OUTPUT!! I'VE GOTTEN INCONSISTENT RESULTS
## EG BLANKS VS NA, CAPITALIZED VS NOT, ETC...
Fuels1000_all$Visited[Fuels1000_all$Visited==""] <- NA

Fuels1000_all <- Fuels1000_all %>%
  mutate(Visited = toupper(Visited)) %>%
  separate(Date, sep = " ", into = c("Date", "Time")) %>%
  separate(MacroPlot.Name, sep = " ", into = c("MonitoringType", "Plot"), remove = FALSE)

# Filter for only data
Fuels1000_data <- Fuels1000_all %>%
  filter(is.na(Visited))

# Filter for only headers
Fuels1000_header <- Fuels1000_all %>%
  filter(!is.na(Visited))

# Blank data frame for no errors
no_errors <- data.frame("SavedQuery" = "","MacroPlot.Name" = "","Date" = "","Error" = "No Error","Fixed?" = "","Explanation" = "","Person" = "")


#### Fuel 1000 DecayClass ####
# Decay class values are correct

# Set parameters
saved_query <- "Fuel 1000 DecayClass"
error_message <- "Decay Class"
column_data <- Fuels1000_data$DecayCl
valid_values <- c(3,4)

# Identify errors
errors_DecayCl <- Fuels1000_data %>%
  mutate("SavedQuery" = saved_query,
         "Error" = paste(error_message, "=", column_data),
         "Fixed?" = "",
         "Explanation" = "",
         "Person" = "") %>%
  filter(!column_data %in% valid_values) %>%
  select(c("SavedQuery", "MacroPlot.Name", "Date", "Error", "Fixed?","Explanation","Person"))

# If no errors, add "No Errors" to existing data frame
if (nrow(errors_DecayCl) == 0) {
  errors_DecayCl <- no_errors %>%
    mutate(SavedQuery = saved_query)
} else {
  errors_DecayCl <- errors_DecayCl
}

#### Fuel 1000 FuelConst (PIAB or PIED or PIEN or PIPN or PIPO) ####
# Monitoring type matches with fuel constant, and fuel constant values are correct

# Set parameters
saved_query <- "Fuel 1000 FuelConst"
error_message <- "Fuel Constant"
column_data <- Fuels1000_data$CWDFuConSt
valid_values <- Fuels1000_data$MonitoringType

# Identify errors
errors_FuelConst <- Fuels1000_data %>%
  mutate("SavedQuery" = saved_query,
         "Error" = paste(error_message, "=", column_data),
         "Fixed?" = "",
         "Explanation" = "",
         "Person" = "") %>%
  filter(!column_data == valid_values) %>%
  select(c("SavedQuery", "MacroPlot.Name", "Date", "Error", "Fixed?","Explanation","Person"))

# If no errors, add "No Errors" to existing data frame
if (nrow(errors_FuelConst) == 0) {
  errors_FuelConst <- no_errors %>%
    mutate(SavedQuery = saved_query)
} else {
  errors_FuelConst <- errors_FuelConst
}



################################################################################
#### SAVE ALL ERRORS ####
################################################################################

# # Make empty data frame to store errors
# errors <- data.frame(matrix(ncol = 7, nrow = 0))
# colnames(errors) <- c("SavedQuery", "Date of Read", "Macro Plot ID", "Error", "Fixed?", "Explanation", "Queryer")

# Save to master error list
errors <- rbind(errors_DecayCl, errors_FuelConst)


errors_path_Fuels1000_csv <- paste0(errors_path, "errors_Fuels1000.csv")
errors_path_Fuels1000_xlsx <- paste0(errors_path, "errors_Fuels1000.xlsx")
write.csv(errors, errors_path_Fuels1000_csv, quote=FALSE, row.names = FALSE, na = "")
write_xlsx(errors, errors_path_Fuels1000_xlsx)


