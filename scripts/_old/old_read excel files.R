## Created by: Alexandra Lalor
## Email: alexandra_lalor@nps.gov, allielalor@gmail.com
## Date Created: 2024-05-01
## Last Edited: 2024-05-10
## To take data from excel files and save individual protocols/tabs as CSVs, and name them appropriately


###################################################################################

#make empty file names data frame
file_names_df <- data.frame(matrix(ncol = 18, nrow = 0))
colnames(file_names_df) <- c("Drive", "Users", "Person", "OneDrive", "Desktop", "Folder",
                             "GRCA", "Project", "Data_folder", "Park", "Year", "Collected", "Type",
                             "Monitoring_Type", "Plot", "PlotID", "Status","Fieldcopy", "FileType")

#list file names in each folder and add to date frame
for(i in 1:length(folder_names_list)) {
  folder_names <- folder_names_list[i]
  folder_path <- paste0(my_path_data, folder_names, "/")
  file_names <- list.files(folder_path)
  file_path <- paste0(folder_path, file_names)
  file_names_df_1 <- data.frame(text = file_path) %>%
    separate(text, sep = "/", into = c("Drive", "Users", "Person", "OneDrive", "Desktop", "Folder",
                                       "GRCA", "Project", "Data_folder", "Park", "Year", "Collected",
                                       "Monitoring_Type", "Event")) %>%
    separate(Event, sep = "_", into = c("Plot", "PlotID", "Status","Suffix")) %>%
    separate(Suffix, into = c("Fieldcopy", "FileType")) %>%
    mutate(Plot = paste(Plot, PlotID, sep=""))
  file_names_df <- rbind(file_names_df, file_names_df_1)
}


#create condensed version to add to data, we don't need all this other stuff
file_add <- file_names_df %>%
  select(c("Plot", "Status"))



#create list of excel file paths and names
for(i in 1:length(folder_names_list)) {
  folder_names <- folder_names_list[i]
  folder_path <- paste0(my_path_data, folder_names, "/")
  file_names <- list.files(folder_path)
  file_path <- paste0(folder_path, file_names)
  file_names_df_1 <- data.frame(FilePath = file_path, text = file_names) %>%
    separate(text, sep = "_", into = c("MonitoringType", "PlotID", "Status","Suffix")) %>%
    mutate(Plot = paste(MonitoringType, PlotID, sep="_")) %>%
    mutate(Plot_Status = paste(Plot, Status, sep="_")) %>%
    select(c("FilePath", "Plot_Status"))
  file_names_df <- rbind(file_names_df, file_names_df_1)
}



## read excel files
FilePath <- "/Users/alalor.NPS/Desktop/FX_Lalor/R/GRCA/test/data/collected/2023/PIPN_03_04Year5.xlsx"

FuelsFWD <- read_excel(file_path, sheet = "Fuels FWD")
FuelsCWD <- read_excel(FilePath, sheet = "Fuels CWD")
FuelsDuffLitt <- read_excel(FilePath, sheet = "Fuels Duff-Litt")
HerbsPoints <- read_excel(FilePath, sheet = "Herbs (Points)")
HerbsSpComp <- read_excel(FilePath, sheet = "Herbs-Ob (Sp Comp)")
Shrubs <- read_excel(FilePath, sheet = "Shrubs (Belt)")
Seedlings <- read_excel(FilePath, sheet = "Seedlings (Quad)")
Trees <- read_excel(FilePath, sheet = "Trees")

file_path



# path <- file_names_df[1,1]
# name <- file_names_df[1,2]
# my_path_csv_FuelsFWD <- paste0(my_path_csv, "/", name, "_FuelsFWD.csv")
# my_path_csv_FuelsCWD <- paste0(my_path_csv, "/", name, "_FuelsCWD.csv")
# my_path_csv_FuelsDuffLitt <- paste0(my_path_csv, "/", name, "_FuelsDuffLitt.csv")
# my_path_csv_HerbsCover <- paste0(my_path_csv, "/", name, "_HerbsCover.csv")
# my_path_csv_Seedlings <- paste0(my_path_csv, "/", name, "_Seedlings.csv")
# my_path_csv_Trees <- paste0(my_path_csv, "/", name, "_Trees.csv")
# FuelsFWD <- read_excel(path, sheet = "Fuels FWD")
# FuelsCWD <- read_excel(path, sheet = "Fuels CWD")
# FuelsDuffLitt <- read_excel(path, sheet = "Fuels Duff-Litt")
# HerbsCover <- read_excel(path, sheet = "Cover")
# Seedlings <- read_excel(path, sheet = "Seedlings")
# Trees <- read_excel(path, sheet = "Trees")
# write.csv(FuelsFWD, my_path_csv_FuelsFWD, quote=FALSE, row.names = FALSE)
# write.csv(FuelsCWD, my_path_csv_FuelsCWD, quote=FALSE, row.names = FALSE)
# write.csv(FuelsDuffLitt, my_path_csv_FuelsDuffLitt, quote=FALSE, row.names = FALSE)
# write.csv(HerbsCover, my_path_csv_HerbsCover, quote=FALSE, row.names = FALSE)
# write.csv(Seedlings, my_path_csv_Seedlings, quote=FALSE, row.names = FALSE)
# write.csv(Trees, my_path_csv_Trees, quote=FALSE, row.names = FALSE)

