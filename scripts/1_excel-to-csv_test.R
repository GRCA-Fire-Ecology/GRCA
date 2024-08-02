# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2024-02-02
# Last Edited: 2024-08-02
#
# testing excel-to-csv outside of for loop



################################################################################
# IDEAS FOR DATA CHECKING WITHIN R
################################################################################

path <- file_names_df[1,1]
name <- file_names_df[1,2]

#read tabs of excel files, bring them into R
FuelsFWD <- read_excel(path, sheet = "Fuels FWD")
FuelsCWD <- read_excel(path, sheet = "Fuels CWD")
FuelsDuffLitt <- read_excel(path, sheet = "Fuels Duff-Litt")
HerbsPoints <- read_excel(path, sheet = "Herbs (Points)")
HerbsObs <- read_excel(path, sheet = "Herbs-Ob (Sp Comp)")
Shrubs <- read_excel(path, sheet = "Shrubs (Belt)")
Seedlings <- read_excel(path, sheet = "Seedlings (Quad)")
Trees <- read_excel(path, sheet = "Trees")
PostBurn <- read_excel(path, sheet = "Post Burn")

#create csv paths
my_path_csv_FuelsFWD <- paste0(my_path_csv, name, "_FuelsFWD.csv")
my_path_csv_FuelsCWD <- paste0(my_path_csv, name, "_FuelsCWD.csv")
my_path_csv_FuelsDuffLitt <- paste0(my_path_csv, name, "_FuelsDuffLitt.csv")
my_path_csv_HerbsPoints <- paste0(my_path_csv, name, "_HerbsPoints.csv")
my_path_csv_HerbsObs<- paste0(my_path_csv, name, "_HerbsObs.csv")
my_path_csv_Shrubs<- paste0(my_path_csv, name, "_Shrubs.csv")
my_path_csv_Seedlings <- paste0(my_path_csv, name, "_Seedlings.csv")
my_path_csv_Trees <- paste0(my_path_csv, name, "_Trees.csv")
my_path_csv_PostBurn <- paste0(my_path_csv, name, "_PostBurn.csv")

# QAQC all protocols, minus Trees, Delete empty rows, Change numbers in index column into ascending order
FuelsFWD <- subset(FuelsFWD, OneHr != "") %>%
  mutate(Index = row_number()) %>%
  map_df(str_replace_all, pattern = ",", replacement = ";")
FuelsCWD <- subset(FuelsCWD, Dia != "") %>%
  mutate(Index = row_number())
FuelsDuffLitt <- subset(FuelsDuffLitt, LittDep != "") %>%
  mutate(Index = row_number())
HerbsPointsCount <- sum(!is.na(HerbsPoints$Height))
HerbsPoints <-
  mutate(HerbsPoints, Count = HerbsPointsCount) %>%
  subset(Count != "0") %>%
  mutate(Index = row_number()) %>%
  map_df(str_replace_all, pattern = ",", replacement = ";")
# HerbsObs <- subset(HerbsObs, Species != "") %>%
#   mutate(Index = row_number()) %>%
#   map_df(str_replace_all, pattern = ",", replacement = ";")
Seedlings <- subset(Seedlings, Species != "") %>%
  mutate(Index = row_number()) %>%
  map_df(str_replace_all, pattern = ",", replacement = ";")
Shrubs <- subset(Shrubs, Species != "") %>%
  mutate(Index = row_number()) %>%
  map_df(str_replace_all, pattern = ",", replacement = ";")
PostBurn <- subset(PostBurn, Sub != "") %>%
  mutate(Index = row_number())
# Trees, Ensure tree order is triple sorted by “SubFrac”, “QTR”, and “TagNo” (smallest to largest), Check that index is in ascending order from top to bottom (1, 2, 3, …). Trees are commonly unsorted, “IsVerified” column is TRUE
Trees <- subset(Trees, Status != "X") %>%
  arrange(SubFrac, QTR, TagNo) %>%
  mutate(Index = row_number()) %>%
  mutate(IsVerified = "TRUE") %>%
  map_df(str_replace_all, pattern = ",", replacement = ";")

#identify duplicate species in Herbs Obs
DuplicateHerbs <-
  merge(HerbsPoints, HerbsObs, by = "Species") %>%
  select(Species) %>%
  distinct(Species)
DuplicateShrubs <-
  merge(Shrubs, HerbsObs, by = "Species") %>%
  select(Species) %>%
  distinct(Species)
DuplicateSeedlings <-
  merge(Seedlings, HerbsObs, by = "Species") %>%
  select(Species) %>%
  distinct(Species)
DuplicateTrees <-
  merge(Trees, HerbsObs, by = "Species") %>%
  select(Species) %>%
  distinct(Species)
#create master list of duplicate species
DuplicateSpecies <- rbind(DuplicateHerbs, DuplicateShrubs, DuplicateSeedlings, DuplicateTrees)
#remove duplicate species from Herbs Obs
HerbsObs <- anti_join(HerbsObs, DuplicateSpecies, by = "Species")
#remove data if = 0, update index, replace , with ;
HerbsObs <- subset(HerbsObs, Species != "") %>%
  mutate(Index = row_number()) %>%
  map_df(str_replace_all, pattern = ",", replacement = ";")

#save CSVs
if(dim(FuelsFWD)[1] == 0) {print(paste0(name," ","Fuels FWD is empty"))}else{write.csv(FuelsFWD, my_path_csv_FuelsFWD, quote=FALSE, row.names = FALSE, na = "")}
if(dim(FuelsCWD)[1] == 0) {print(paste0(name," ","Fuels CWD is empty"))}else{write.csv(FuelsCWD, my_path_csv_FuelsCWD, quote=FALSE, row.names = FALSE, na = "")}
if(dim(FuelsDuffLitt)[1] == 0) {print(paste0(name," ","Fuels Duff-Litt is empty"))}else{write.csv(FuelsDuffLitt, my_path_csv_FuelsDuffLitt, quote=FALSE, row.names = FALSE, na = "")}
if(dim(HerbsPoints)[1] == 0) {print(paste0(name," ","Herbs Points is empty"))}else{write.csv(HerbsPoints, my_path_csv_HerbsPoints, quote=FALSE, row.names = FALSE, na = "")}
if(dim(HerbsObs)[1] == 0) {print(paste0(name," ","Herbs Obs is empty"))}else{write.csv(HerbsObs, my_path_csv_HerbsObs, quote=FALSE, row.names = FALSE,na = "")}
if(dim(Shrubs)[1] == 0) {print(paste0(name," ","Shrubs is empty"))}else{write.csv(Shrubs, my_path_csv_Shrubs, quote=FALSE, row.names = FALSE, na = "")}
if(dim(Seedlings)[1] == 0) {print(paste0(name," ","Seedlings is empty"))}else{write.csv(Seedlings, my_path_csv_Seedlings, quote=FALSE, row.names = FALSE,na = "")}
if(dim(Trees)[1] == 0) {print(paste0(name," ","Trees is empty"))}else{write.csv(Trees, my_path_csv_Trees, quote=FALSE, row.names = FALSE, na = "")}
if(dim(PostBurn)[1] == 0) {print(paste0(name," ","Post Burn is empty"))}else{write.csv(PostBurn, my_path_csv_PostBurn, quote = FALSE, row.names = FALSE, na = "")}

