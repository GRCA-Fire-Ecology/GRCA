## Created by: Alexandra Lalor
## Email: alexandra_lalor@nps.gov
## Date Created: 2024-05-01
## Last Edited: 2024-05-01

## install packages
install.packages("tidyverse")

## load packages
library(tidyverse)

## set working directory
setwd("/Users/alalor.NPS/Desktop/FX_Lalor/R/GRCA/test")
getwd()

## read excel files
FilePath <- "/Users/alalor.NPS/Desktop/FX_Lalor/R/GRCA/test/data/collected/2023/PIPN_03_04Year5.xlsx"

PIPN_03_04Year05_FuelsFWD <- read_excel(FilePath, sheet = "Fuels FWD")
PIPN_03_04Year05_FuelsCWD <- read_excel(FilePath, sheet = "Fuels CWD")
PIPN_03_04Year05_FuelsDuffLitt <- read_excel(FilePath, sheet = "Fuels Duff-Litt")
PIPN_03_04Year05_HerbsPoints <- read_excel(FilePath, sheet = "Herbs (Points)")
PIPN_03_04Year05_HerbsSpComp <- read_excel(FilePath, sheet = "Herbs-Ob (Sp Comp)")
PIPN_03_04Year05_Shrubs <- read_excel(FilePath, sheet = "Shrubs (Belt)")
PIPN_03_04Year05_Seedlings <- read_excel(FilePath, sheet = "Seedlings (Quad)")
PIPN_03_04Year05_Trees <- read_excel(FilePath, sheet = "Trees")

