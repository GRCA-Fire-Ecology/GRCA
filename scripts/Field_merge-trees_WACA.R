# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2025-04-29


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
path_data_IM <- "X:/Data Collection/WACA/2024/Collected/Trees CSVs from SCPN/"
path_data_FX <- "X:/Data Collection/WACA/2024/Collected/CSV_Import to FFI/"
path_data_FX_old <- "X:/Data Collection/WACA/2024/Prepped/CSV_Old data/"
path_csv <- "X:/Data Collection/WACA/2024/Collected/CSV_Merged trees from R/"

################################################################################
# LOAD DATA
################################################################################

# I&M overstory tree data
PIPO_01_IM <- read.csv(paste0(path_data_IM, "WACA_P01.csv"))
PIPO_02_IM <- read.csv(paste0(path_data_IM, "WACA_P02.csv"))
PIPO_03_IM <- read.csv(paste0(path_data_IM, "WACA_P03.csv"))
PIPO_04_IM <- read.csv(paste0(path_data_IM, "WACA_P04.csv"))
#PIPO_05_IM <- read.csv(paste0(path_data_IM, "WACA_P05.csv"))
PIPO_06_IM <- read.csv(paste0(path_data_IM, "WACA_P06.csv"))
PIPO_07_IM <- read.csv(paste0(path_data_IM, "WACA_P07.csv"))
PIPO_08_IM <- read.csv(paste0(path_data_IM, "WACA_P08.csv"))
PIPO_09_IM <- read.csv(paste0(path_data_IM, "WACA_P09.csv"))
PIPO_10_IM <- read.csv(paste0(path_data_IM, "WACA_P10.csv"))
PIPO_11_IM <- read.csv(paste0(path_data_IM, "WACA_P11.csv"))
PIPO_13_IM <- read.csv(paste0(path_data_IM, "WACA_P13.csv"))
PIPO_14_IM <- read.csv(paste0(path_data_IM, "WACA_P14.csv"))

# FX pole tree data
PIPO_01_FX <- read.csv(paste0(path_data_FX, "PIPO_01_02Pre_Trees.csv"), quote = "")
PIPO_02_FX <- read.csv(paste0(path_data_FX, "PIPO_02_02Pre_Trees.csv"), quote = "")
PIPO_03_FX <- read.csv(paste0(path_data_FX, "PIPO_03_02Pre_Trees.csv"), quote = "")
PIPO_04_FX <- read.csv(paste0(path_data_FX, "PIPO_04_02Pre_Trees.csv"), quote = "")
PIPO_05_FX <- read.csv(paste0(path_data_FX, "PIPO_05_02Pre_Trees.csv"), quote = "")
PIPO_06_FX <- read.csv(paste0(path_data_FX, "PIPO_06_02Pre_Trees.csv"), quote = "")
PIPO_07_FX <- read.csv(paste0(path_data_FX, "PIPO_07_02Pre_Trees.csv"), quote = "")
PIPO_08_FX <- read.csv(paste0(path_data_FX, "PIPO_08_02Pre_Trees.csv"), quote = "")
PIPO_09_FX <- read.csv(paste0(path_data_FX, "PIPO_09_02Pre_Trees.csv"), quote = "")
PIPO_10_FX <- read.csv(paste0(path_data_FX, "PIPO_10_Pre_Trees.csv"), quote = "")
PIPO_11_FX <- read.csv(paste0(path_data_FX, "PIPO_11_Pre_Trees.csv"), quote = "")
PIPO_13_FX <- read.csv(paste0(path_data_FX, "PIPO_13_Pre_Trees.csv"), quote = "")
PIPO_14_FX <- read.csv(paste0(path_data_FX, "PIPO_14_Pre_Trees.csv"), quote = "")

# FX old tree data
PIPO_01_FX_old <- read.csv(paste0(path_data_FX_old, "PIPO_01_trees.csv"), quote = "")
PIPO_02_FX_old <- read.csv(paste0(path_data_FX_old, "PIPO_02_trees.csv"), quote = "")
PIPO_03_FX_old <- read.csv(paste0(path_data_FX_old, "PIPO_03_trees.csv"), quote = "")
PIPO_04_FX_old <- read.csv(paste0(path_data_FX_old, "PIPO_04_trees.csv"), quote = "")
PIPO_05_FX_old <- read.csv(paste0(path_data_FX_old, "PIPO_05_trees.csv"), quote = "")
PIPO_06_FX_old <- read.csv(paste0(path_data_FX_old, "PIPO_06_trees.csv"), quote = "")
PIPO_07_FX_old <- read.csv(paste0(path_data_FX_old, "PIPO_07_trees.csv"), quote = "")
PIPO_08_FX_old <- read.csv(paste0(path_data_FX_old, "PIPO_08_trees.csv"), quote = "")
PIPO_09_FX_old <- read.csv(paste0(path_data_FX_old, "PIPO_09_trees.csv"), quote = "")
PIPO_10_FX_old <- read.csv(paste0(path_data_FX_old, "PIPO_10_trees.csv"), quote = "")
PIPO_11_FX_old <- read.csv(paste0(path_data_FX_old, "PIPO_11_trees.csv"), quote = "")
PIPO_13_FX_old <- read.csv(paste0(path_data_FX_old, "PIPO_13_trees.csv"), quote = "")
PIPO_14_FX_old <- read.csv(paste0(path_data_FX_old, "PIPO_14_trees.csv"), quote = "")


################################################################################
# CLEAN DATA
################################################################################

PIPO_01_FX_old <- PIPO_01_FX_old %>%
  select(SubFrac, QTR, TagNo, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct) %>%
  mutate(CrwnCl = ifelse(CrwnCl == "", NA, CrwnCl),
         DamCd1 = ifelse(DamCd1 == "", NA, DamCd1),
         DamCd2 = ifelse(DamCd2 == "", NA, DamCd2),
         DamCd3 = ifelse(DamCd3 == "", NA, DamCd3),
         DamCd4 = ifelse(DamCd4 == "", NA, DamCd4),
         DamCd5 = ifelse(DamCd5 == "", NA, DamCd5)) %>%
  filter(CrwnCl != "DD")

PIPO_02_FX_old <- PIPO_02_FX_old %>%
  select(SubFrac, QTR, TagNo, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct) %>%
  mutate(CrwnCl = ifelse(CrwnCl == "", NA, CrwnCl),
         DamCd1 = ifelse(DamCd1 == "", NA, DamCd1),
         DamCd2 = ifelse(DamCd2 == "", NA, DamCd2),
         DamCd3 = ifelse(DamCd3 == "", NA, DamCd3),
         DamCd4 = ifelse(DamCd4 == "", NA, DamCd4),
         DamCd5 = ifelse(DamCd5 == "", NA, DamCd5)) %>%
  filter(CrwnCl != "DD")

PIPO_03_FX_old <- PIPO_03_FX_old %>%
  select(SubFrac, QTR, TagNo, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct) %>%
  mutate(CrwnCl = ifelse(CrwnCl == "", NA, CrwnCl),
         DamCd1 = ifelse(DamCd1 == "", NA, DamCd1),
         DamCd2 = ifelse(DamCd2 == "", NA, DamCd2),
         DamCd3 = ifelse(DamCd3 == "", NA, DamCd3),
         DamCd4 = ifelse(DamCd4 == "", NA, DamCd4),
         DamCd5 = ifelse(DamCd5 == "", NA, DamCd5)) %>%
  filter(CrwnCl != "DD")

PIPO_04_FX_old <- PIPO_04_FX_old %>%
  select(SubFrac, QTR, TagNo, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct) %>%
  mutate(CrwnCl = ifelse(CrwnCl == "", NA, CrwnCl),
         DamCd1 = ifelse(DamCd1 == "", NA, DamCd1),
         DamCd2 = ifelse(DamCd2 == "", NA, DamCd2),
         DamCd3 = ifelse(DamCd3 == "", NA, DamCd3),
         DamCd4 = ifelse(DamCd4 == "", NA, DamCd4),
         DamCd5 = ifelse(DamCd5 == "", NA, DamCd5)) %>%
  filter(CrwnCl != "DD")

PIPO_05_FX_old <- PIPO_05_FX_old %>%
  select(SubFrac, QTR, TagNo, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct) %>%
  mutate(CrwnCl = ifelse(CrwnCl == "", NA, CrwnCl),
         DamCd1 = ifelse(DamCd1 == "", NA, DamCd1),
         DamCd2 = ifelse(DamCd2 == "", NA, DamCd2),
         DamCd3 = ifelse(DamCd3 == "", NA, DamCd3),
         DamCd4 = ifelse(DamCd4 == "", NA, DamCd4),
         DamCd5 = ifelse(DamCd5 == "", NA, DamCd5)) %>%
  filter(CrwnCl != "DD")

PIPO_06_FX_old <- PIPO_06_FX_old %>%
  select(SubFrac, QTR, TagNo, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct) %>%
  mutate(CrwnCl = ifelse(CrwnCl == "", NA, CrwnCl),
         DamCd1 = ifelse(DamCd1 == "", NA, DamCd1),
         DamCd2 = ifelse(DamCd2 == "", NA, DamCd2),
         DamCd3 = ifelse(DamCd3 == "", NA, DamCd3),
         DamCd4 = ifelse(DamCd4 == "", NA, DamCd4),
         DamCd5 = ifelse(DamCd5 == "", NA, DamCd5)) %>%
  filter(CrwnCl != "DD")

PIPO_07_FX_old <- PIPO_07_FX_old %>%
  select(SubFrac, QTR, TagNo, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct) %>%
  mutate(CrwnCl = ifelse(CrwnCl == "", NA, CrwnCl),
         DamCd1 = ifelse(DamCd1 == "", NA, DamCd1),
         DamCd2 = ifelse(DamCd2 == "", NA, DamCd2),
         DamCd3 = ifelse(DamCd3 == "", NA, DamCd3),
         DamCd4 = ifelse(DamCd4 == "", NA, DamCd4),
         DamCd5 = ifelse(DamCd5 == "", NA, DamCd5)) %>%
  filter(CrwnCl != "DD")

PIPO_08_FX_old <- PIPO_08_FX_old %>%
  select(SubFrac, QTR, TagNo, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct) %>%
  mutate(CrwnCl = ifelse(CrwnCl == "", NA, CrwnCl),
         DamCd1 = ifelse(DamCd1 == "", NA, DamCd1),
         DamCd2 = ifelse(DamCd2 == "", NA, DamCd2),
         DamCd3 = ifelse(DamCd3 == "", NA, DamCd3),
         DamCd4 = ifelse(DamCd4 == "", NA, DamCd4),
         DamCd5 = ifelse(DamCd5 == "", NA, DamCd5)) %>%
  filter(CrwnCl != "DD")

PIPO_09_FX_old <- PIPO_09_FX_old %>%
  select(SubFrac, QTR, TagNo, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct) %>%
  mutate(CrwnCl = ifelse(CrwnCl == "", NA, CrwnCl),
         DamCd1 = ifelse(DamCd1 == "", NA, DamCd1),
         DamCd2 = ifelse(DamCd2 == "", NA, DamCd2),
         DamCd3 = ifelse(DamCd3 == "", NA, DamCd3),
         DamCd4 = ifelse(DamCd4 == "", NA, DamCd4),
         DamCd5 = ifelse(DamCd5 == "", NA, DamCd5)) %>%
  filter(CrwnCl != "DD")

PIPO_10_FX_old <- PIPO_10_FX_old %>%
  select(SubFrac, QTR, TagNo, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct) %>%
  mutate(CrwnCl = ifelse(CrwnCl == "", NA, CrwnCl),
         DamCd1 = ifelse(DamCd1 == "", NA, DamCd1),
         DamCd2 = ifelse(DamCd2 == "", NA, DamCd2),
         DamCd3 = ifelse(DamCd3 == "", NA, DamCd3),
         DamCd4 = ifelse(DamCd4 == "", NA, DamCd4),
         DamCd5 = ifelse(DamCd5 == "", NA, DamCd5)) %>%
  filter(CrwnCl != "DD")

PIPO_11_FX_old <- PIPO_11_FX_old %>%
  select(SubFrac, QTR, TagNo, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct) %>%
  mutate(CrwnCl = ifelse(CrwnCl == "", NA, CrwnCl),
         DamCd1 = ifelse(DamCd1 == "", NA, DamCd1),
         DamCd2 = ifelse(DamCd2 == "", NA, DamCd2),
         DamCd3 = ifelse(DamCd3 == "", NA, DamCd3),
         DamCd4 = ifelse(DamCd4 == "", NA, DamCd4),
         DamCd5 = ifelse(DamCd5 == "", NA, DamCd5)) %>%
  filter(CrwnCl != "DD")

PIPO_13_FX_old <- PIPO_13_FX_old %>%
  select(SubFrac, QTR, TagNo, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct) %>%
  mutate(CrwnCl = ifelse(CrwnCl == "", NA, CrwnCl),
         DamCd1 = ifelse(DamCd1 == "", NA, DamCd1),
         DamCd2 = ifelse(DamCd2 == "", NA, DamCd2),
         DamCd3 = ifelse(DamCd3 == "", NA, DamCd3),
         DamCd4 = ifelse(DamCd4 == "", NA, DamCd4),
         DamCd5 = ifelse(DamCd5 == "", NA, DamCd5)) %>%
  filter(CrwnCl != "DD")

PIPO_14_FX_old <- PIPO_14_FX_old %>%
  select(SubFrac, QTR, TagNo, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct) %>%
  mutate(CrwnCl = ifelse(CrwnCl == "", NA, CrwnCl),
         DamCd1 = ifelse(DamCd1 == "", NA, DamCd1),
         DamCd2 = ifelse(DamCd2 == "", NA, DamCd2),
         DamCd3 = ifelse(DamCd3 == "", NA, DamCd3),
         DamCd4 = ifelse(DamCd4 == "", NA, DamCd4),
         DamCd5 = ifelse(DamCd5 == "", NA, DamCd5)) %>%
  filter(CrwnCl != "DD")


################################################################################
# MERGE DATA (NEW ONLY)
################################################################################

PIPO_01 <- merge(PIPO_01_IM, PIPO_01_FX, all = TRUE) %>%
  arrange(SubFrac, QTR, TagNo) %>%
  mutate(Index = row_number())

PIPO_02 <- merge(PIPO_02_IM, PIPO_02_FX, all = TRUE) %>%
  arrange(SubFrac, QTR, TagNo) %>%
  mutate(Index = row_number())

PIPO_03 <- merge(PIPO_03_IM, PIPO_03_FX, all = TRUE) %>%
  arrange(SubFrac, QTR, TagNo) %>%
  mutate(Index = row_number())

PIPO_04 <- merge(PIPO_04_IM, PIPO_04_FX, all = TRUE) %>%
  arrange(SubFrac, QTR, TagNo) %>%
  mutate(Index = row_number())

PIPO_05 <- PIPO_05_FX %>%
  arrange(SubFrac, QTR, TagNo) %>%
  mutate(Index = row_number())

PIPO_06 <- merge(PIPO_06_IM, PIPO_06_FX, all = TRUE) %>%
  arrange(SubFrac, QTR, TagNo) %>%
  mutate(Index = row_number())

PIPO_07 <- merge(PIPO_07_IM, PIPO_07_FX, all = TRUE) %>%
  arrange(SubFrac, QTR, TagNo) %>%
  mutate(Index = row_number())

PIPO_08 <- merge(PIPO_08_IM, PIPO_08_FX, all = TRUE) %>%
  arrange(SubFrac, QTR, TagNo) %>%
  mutate(Index = row_number())

PIPO_09 <- merge(PIPO_09_IM, PIPO_09_FX, all = TRUE) %>%
  arrange(SubFrac, QTR, TagNo) %>%
  mutate(Index = row_number())

PIPO_10 <- merge(PIPO_10_IM, PIPO_10_FX, all = TRUE) %>%
  arrange(SubFrac, QTR, TagNo) %>%
  mutate(Index = row_number())

PIPO_11 <- merge(PIPO_11_IM, PIPO_11_FX, all = TRUE) %>%
  arrange(SubFrac, QTR, TagNo) %>%
  mutate(Index = row_number())

PIPO_13 <- merge(PIPO_13_IM, PIPO_13_FX, all = TRUE) %>%
  arrange(SubFrac, QTR, TagNo) %>%
  mutate(Index = row_number())

PIPO_14 <- merge(PIPO_14_IM, PIPO_14_FX, all = TRUE) %>%
  arrange(SubFrac, QTR, TagNo) %>%
  mutate(Index = row_number())


################################################################################
# MERGE DATA (NEW AND OLD)
################################################################################

PIPO_01_merge <- PIPO_01 %>%
  select(!c(QTR, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct))
PIPO_01_add_old <- merge(PIPO_01_merge, PIPO_01_FX_old, by = c("SubFrac", "TagNo"), all = TRUE) %>%
  mutate(QTR = ifelse(SubFrac == 0.25, 1, QTR),
         Crown.Class = ifelse(is.na(Crown.Class), "", Crown.Class),
         CrwnCl = ifelse(Crown.Class == "Dead and Down (DD)", "DD", CrwnCl),
         reorder = ifelse(is.na(Status), 1, 0)) %>%
  arrange(reorder, SubFrac, QTR, TagNo) %>%
  select(!c(reorder, Crown.Class)) %>%
  mutate(Index = row_number()) %>%
  relocate(Index, .before = SubFrac) %>%
  relocate(QTR, .after = SubFrac) %>%
  relocate(CrwnCl, .after = Status) %>%
  relocate(DRC, .after = LaddMaxHt) %>%
  relocate(DamCd1, .before = DamSev1) %>%
  relocate(DamCd2, .before = DamSev2) %>%
  relocate(DamCd3, .before = DamSev3) %>%
  relocate(DamCd4, .before = DamSev4) %>%
  relocate(DamCd5, .before = DamSev5)


PIPO_02_merge <- PIPO_02 %>%
  select(!c(QTR, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct))
PIPO_02_add_old <- merge(PIPO_02_merge , PIPO_02_FX_old, by = c("SubFrac", "TagNo"), all = TRUE) %>%
  mutate(QTR = ifelse(SubFrac == 0.25, 1, QTR),
         Crown.Class = ifelse(is.na(Crown.Class), "", Crown.Class),
         CrwnCl = ifelse(Crown.Class == "Dead and Down (DD)", "DD", CrwnCl),
         reorder = ifelse(is.na(Status), 1, 0)) %>%
  arrange(reorder, SubFrac, QTR, TagNo) %>%
  select(!c(reorder, Crown.Class)) %>%
  mutate(Index = row_number()) %>%
  relocate(Index, .before = SubFrac) %>%
  relocate(QTR, .after = SubFrac) %>%
  relocate(CrwnCl, .after = Status) %>%
  relocate(DRC, .after = LaddMaxHt) %>%
  relocate(DamCd1, .before = DamSev1) %>%
  relocate(DamCd2, .before = DamSev2) %>%
  relocate(DamCd3, .before = DamSev3) %>%
  relocate(DamCd4, .before = DamSev4) %>%
  relocate(DamCd5, .before = DamSev5)


PIPO_03_merge  <- PIPO_03 %>%
  select(!c(QTR, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct))
PIPO_03_add_old  <- merge(PIPO_03_merge , PIPO_03_FX_old, by = c("SubFrac", "TagNo"), all = TRUE) %>%
  mutate(QTR = ifelse(SubFrac == 0.25, 1, QTR),
         Crown.Class = ifelse(is.na(Crown.Class), "", Crown.Class),
         CrwnCl = ifelse(Crown.Class == "Dead and Down (DD)", "DD", CrwnCl),
         reorder = ifelse(is.na(Status), 1, 0)) %>%
  arrange(reorder, SubFrac, QTR, TagNo) %>%
  select(!c(reorder, Crown.Class)) %>%
  mutate(Index = row_number()) %>%
  relocate(Index, .before = SubFrac) %>%
  relocate(QTR, .after = SubFrac) %>%
  relocate(CrwnCl, .after = Status) %>%
  relocate(DRC, .after = LaddMaxHt) %>%
  relocate(DamCd1, .before = DamSev1) %>%
  relocate(DamCd2, .before = DamSev2) %>%
  relocate(DamCd3, .before = DamSev3) %>%
  relocate(DamCd4, .before = DamSev4) %>%
  relocate(DamCd5, .before = DamSev5)

PIPO_04_merge  <- PIPO_04 %>%
  select(!c(QTR, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct))
PIPO_04_add_old  <- merge(PIPO_04_merge , PIPO_04_FX_old, by = c("SubFrac", "TagNo"), all = TRUE) %>%
  mutate(QTR = ifelse(SubFrac == 0.25, 1, QTR),
         Crown.Class = ifelse(is.na(Crown.Class), "", Crown.Class),
         CrwnCl = ifelse(Crown.Class == "Dead and Down (DD)", "DD", CrwnCl),
         reorder = ifelse(is.na(Status), 1, 0)) %>%
  arrange(reorder, SubFrac, QTR, TagNo) %>%
  select(!c(reorder, Crown.Class)) %>%
  mutate(Index = row_number()) %>%
  relocate(Index, .before = SubFrac) %>%
  relocate(QTR, .after = SubFrac) %>%
  relocate(CrwnCl, .after = Status) %>%
  relocate(DRC, .after = LaddMaxHt) %>%
  relocate(DamCd1, .before = DamSev1) %>%
  relocate(DamCd2, .before = DamSev2) %>%
  relocate(DamCd3, .before = DamSev3) %>%
  relocate(DamCd4, .before = DamSev4) %>%
  relocate(DamCd5, .before = DamSev5)

PIPO_05_merge  <- PIPO_05 %>%
  select(!c(QTR, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct))
PIPO_05_add_old  <- merge(PIPO_05_merge , PIPO_05_FX_old, by = c("SubFrac", "TagNo"), all = TRUE) %>%
  mutate(QTR = ifelse(SubFrac == 0.25, 1, QTR),
         Crown.Class = ifelse(is.na(Crown.Class), "", Crown.Class),
         CrwnCl = ifelse(Crown.Class == "Dead and Down (DD)", "DD", CrwnCl),
         reorder = ifelse(is.na(Status), 1, 0)) %>%
  arrange(reorder, SubFrac, QTR, TagNo) %>%
  select(!c(reorder, Crown.Class)) %>%
  mutate(Index = row_number()) %>%
  relocate(Index, .before = SubFrac) %>%
  relocate(QTR, .after = SubFrac) %>%
  relocate(CrwnCl, .after = Status) %>%
  relocate(DRC, .after = LaddMaxHt) %>%
  relocate(DamCd1, .before = DamSev1) %>%
  relocate(DamCd2, .before = DamSev2) %>%
  relocate(DamCd3, .before = DamSev3) %>%
  relocate(DamCd4, .before = DamSev4) %>%
  relocate(DamCd5, .before = DamSev5)

PIPO_06_merge  <- PIPO_06 %>%
  select(!c(QTR, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct))
PIPO_06_add_old  <- merge(PIPO_06_merge , PIPO_06_FX_old, by = c("SubFrac", "TagNo"), all = TRUE) %>%
  mutate(QTR = ifelse(SubFrac == 0.25, 1, QTR),
         Crown.Class = ifelse(is.na(Crown.Class), "", Crown.Class),
         CrwnCl = ifelse(Crown.Class == "Dead and Down (DD)", "DD", CrwnCl),
         reorder = ifelse(is.na(Status), 1, 0)) %>%
  arrange(reorder, SubFrac, QTR, TagNo) %>%
  select(!c(reorder, Crown.Class)) %>%
  mutate(Index = row_number()) %>%
  relocate(Index, .before = SubFrac) %>%
  relocate(QTR, .after = SubFrac) %>%
  relocate(CrwnCl, .after = Status) %>%
  relocate(DRC, .after = LaddMaxHt) %>%
  relocate(DamCd1, .before = DamSev1) %>%
  relocate(DamCd2, .before = DamSev2) %>%
  relocate(DamCd3, .before = DamSev3) %>%
  relocate(DamCd4, .before = DamSev4) %>%
  relocate(DamCd5, .before = DamSev5)

PIPO_07_merge  <- PIPO_07 %>%
  select(!c(QTR, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct))
PIPO_07_add_old  <- merge(PIPO_07_merge , PIPO_07_FX_old, by = c("SubFrac", "TagNo"), all = TRUE) %>%
  mutate(QTR = ifelse(SubFrac == 0.25, 1, QTR),
         Crown.Class = ifelse(is.na(Crown.Class), "", Crown.Class),
         CrwnCl = ifelse(Crown.Class == "Dead and Down (DD)", "DD", CrwnCl),
         reorder = ifelse(is.na(Status), 1, 0)) %>%
  arrange(reorder, SubFrac, QTR, TagNo) %>%
  select(!c(reorder, Crown.Class)) %>%
  mutate(Index = row_number()) %>%
  relocate(Index, .before = SubFrac) %>%
  relocate(QTR, .after = SubFrac) %>%
  relocate(CrwnCl, .after = Status) %>%
  relocate(DRC, .after = LaddMaxHt) %>%
  relocate(DamCd1, .before = DamSev1) %>%
  relocate(DamCd2, .before = DamSev2) %>%
  relocate(DamCd3, .before = DamSev3) %>%
  relocate(DamCd4, .before = DamSev4) %>%
  relocate(DamCd5, .before = DamSev5)

PIPO_08_merge  <- PIPO_08 %>%
  select(!c(QTR, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct))
PIPO_08_add_old  <- merge(PIPO_08_merge , PIPO_08_FX_old, by = c("SubFrac", "TagNo"), all = TRUE) %>%
  mutate(QTR = ifelse(SubFrac == 0.25, 1, QTR),
         Crown.Class = ifelse(is.na(Crown.Class), "", Crown.Class),
         CrwnCl = ifelse(Crown.Class == "Dead and Down (DD)", "DD", CrwnCl),
         reorder = ifelse(is.na(Status), 1, 0)) %>%
  arrange(reorder, SubFrac, QTR, TagNo) %>%
  select(!c(reorder, Crown.Class)) %>%
  mutate(Index = row_number()) %>%
  relocate(Index, .before = SubFrac) %>%
  relocate(QTR, .after = SubFrac) %>%
  relocate(CrwnCl, .after = Status) %>%
  relocate(DRC, .after = LaddMaxHt) %>%
  relocate(DamCd1, .before = DamSev1) %>%
  relocate(DamCd2, .before = DamSev2) %>%
  relocate(DamCd3, .before = DamSev3) %>%
  relocate(DamCd4, .before = DamSev4) %>%
  relocate(DamCd5, .before = DamSev5)

PIPO_09_merge  <- PIPO_09 %>%
  select(!c(QTR, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct))
PIPO_09_add_old  <- merge(PIPO_09_merge , PIPO_09_FX_old, by = c("SubFrac", "TagNo"), all = TRUE) %>%
  mutate(QTR = ifelse(SubFrac == 0.25, 1, QTR),
         Crown.Class = ifelse(is.na(Crown.Class), "", Crown.Class),
         CrwnCl = ifelse(Crown.Class == "Dead and Down (DD)", "DD", CrwnCl),
         reorder = ifelse(is.na(Status), 1, 0)) %>%
  arrange(reorder, SubFrac, QTR, TagNo) %>%
  select(!c(reorder, Crown.Class)) %>%
  mutate(Index = row_number()) %>%
  relocate(Index, .before = SubFrac) %>%
  relocate(QTR, .after = SubFrac) %>%
  relocate(CrwnCl, .after = Status) %>%
  relocate(DRC, .after = LaddMaxHt) %>%
  relocate(DamCd1, .before = DamSev1) %>%
  relocate(DamCd2, .before = DamSev2) %>%
  relocate(DamCd3, .before = DamSev3) %>%
  relocate(DamCd4, .before = DamSev4) %>%
  relocate(DamCd5, .before = DamSev5)

PIPO_10_merge  <- PIPO_10 %>%
  select(!c(QTR, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct))
PIPO_10_add_old  <- merge(PIPO_10_merge , PIPO_10_FX_old, by = c("SubFrac", "TagNo"), all = TRUE) %>%
  mutate(QTR = ifelse(SubFrac == 0.25, 1, QTR),
         Crown.Class = ifelse(is.na(Crown.Class), "", Crown.Class),
         CrwnCl = ifelse(Crown.Class == "Dead and Down (DD)", "DD", CrwnCl),
         reorder = ifelse(is.na(Status), 1, 0)) %>%
  arrange(reorder, SubFrac, QTR, TagNo) %>%
  select(!c(reorder, Crown.Class)) %>%
  mutate(Index = row_number()) %>%
  relocate(Index, .before = SubFrac) %>%
  relocate(QTR, .after = SubFrac) %>%
  relocate(CrwnCl, .after = Status) %>%
  relocate(DRC, .after = LaddMaxHt) %>%
  relocate(DamCd1, .before = DamSev1) %>%
  relocate(DamCd2, .before = DamSev2) %>%
  relocate(DamCd3, .before = DamSev3) %>%
  relocate(DamCd4, .before = DamSev4) %>%
  relocate(DamCd5, .before = DamSev5)

PIPO_11_merge  <- PIPO_11 %>%
  select(!c(QTR, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct))
PIPO_11_add_old  <- merge(PIPO_11_merge , PIPO_11_FX_old, by = c("SubFrac", "TagNo"), all = TRUE) %>%
  mutate(QTR = ifelse(SubFrac == 0.25, 1, QTR),
         Crown.Class = ifelse(is.na(Crown.Class), "", Crown.Class),
         CrwnCl = ifelse(Crown.Class == "Dead and Down (DD)", "DD", CrwnCl),
         reorder = ifelse(is.na(Status), 1, 0)) %>%
  arrange(reorder, SubFrac, QTR, TagNo) %>%
  select(!c(reorder, Crown.Class)) %>%
  mutate(Index = row_number()) %>%
  relocate(Index, .before = SubFrac) %>%
  relocate(QTR, .after = SubFrac) %>%
  relocate(CrwnCl, .after = Status) %>%
  relocate(DRC, .after = LaddMaxHt) %>%
  relocate(DamCd1, .before = DamSev1) %>%
  relocate(DamCd2, .before = DamSev2) %>%
  relocate(DamCd3, .before = DamSev3) %>%
  relocate(DamCd4, .before = DamSev4) %>%
  relocate(DamCd5, .before = DamSev5)

PIPO_13_merge  <- PIPO_13 %>%
  select(!c(QTR, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct))
PIPO_13_add_old  <- merge(PIPO_13_merge , PIPO_13_FX_old, by = c("SubFrac", "TagNo"), all = TRUE) %>%
  mutate(QTR = ifelse(SubFrac == 0.25, 1, QTR),
         Crown.Class = ifelse(is.na(Crown.Class), "", Crown.Class),
         CrwnCl = ifelse(Crown.Class == "Dead and Down (DD)", "DD", CrwnCl),
         reorder = ifelse(is.na(Status), 1, 0)) %>%
  arrange(reorder, SubFrac, QTR, TagNo) %>%
  select(!c(reorder, Crown.Class)) %>%
  mutate(Index = row_number()) %>%
  relocate(Index, .before = SubFrac) %>%
  relocate(QTR, .after = SubFrac) %>%
  relocate(CrwnCl, .after = Status) %>%
  relocate(DRC, .after = LaddMaxHt) %>%
  relocate(DamCd1, .before = DamSev1) %>%
  relocate(DamCd2, .before = DamSev2) %>%
  relocate(DamCd3, .before = DamSev3) %>%
  relocate(DamCd4, .before = DamSev4) %>%
  relocate(DamCd5, .before = DamSev5)

PIPO_14_merge  <- PIPO_14 %>%
  select(!c(QTR, CrwnCl, DRC, DamCd1, DamCd2, DamCd3, DamCd4, DamCd5, CharHt, ScorchHt, CrScPct))
PIPO_14_add_old  <- merge(PIPO_14_merge , PIPO_14_FX_old, by = c("SubFrac", "TagNo"), all = TRUE) %>%
  mutate(QTR = ifelse(SubFrac == 0.25, 1, QTR),
         Crown.Class = ifelse(is.na(Crown.Class), "", Crown.Class),
         CrwnCl = ifelse(Crown.Class == "Dead and Down (DD)", "DD", CrwnCl),
         reorder = ifelse(is.na(Status), 1, 0)) %>%
  arrange(reorder, SubFrac, QTR, TagNo) %>%
  select(!c(reorder, Crown.Class)) %>%
  mutate(Index = row_number()) %>%
  relocate(Index, .before = SubFrac) %>%
  relocate(QTR, .after = SubFrac) %>%
  relocate(CrwnCl, .after = Status) %>%
  relocate(DRC, .after = LaddMaxHt) %>%
  relocate(DamCd1, .before = DamSev1) %>%
  relocate(DamCd2, .before = DamSev2) %>%
  relocate(DamCd3, .before = DamSev3) %>%
  relocate(DamCd4, .before = DamSev4) %>%
  relocate(DamCd5, .before = DamSev5)


################################################################################
# SAVE DATA
################################################################################

### New Data Only
# write.csv(PIPO_01, paste0(path_csv, "PIPO_01_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
# write.csv(PIPO_02, paste0(path_csv, "PIPO_02_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
# write.csv(PIPO_03, paste0(path_csv, "PIPO_03_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
# write.csv(PIPO_04, paste0(path_csv, "PIPO_04_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
# write.csv(PIPO_05, paste0(path_csv, "PIPO_05_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
# write.csv(PIPO_06, paste0(path_csv, "PIPO_06_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
# write.csv(PIPO_07, paste0(path_csv, "PIPO_07_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
# write.csv(PIPO_08, paste0(path_csv, "PIPO_08_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
# write.csv(PIPO_09, paste0(path_csv, "PIPO_09_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
# write.csv(PIPO_10, paste0(path_csv, "PIPO_10_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
# write.csv(PIPO_11, paste0(path_csv, "PIPO_11_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
# write.csv(PIPO_13, paste0(path_csv, "PIPO_13_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
# write.csv(PIPO_14, paste0(path_csv, "PIPO_14_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")


### New Data Plus Old Data
write.csv(PIPO_01_add_old, paste0(path_csv, "PIPO_01_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
write.csv(PIPO_02_add_old, paste0(path_csv, "PIPO_02_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
write.csv(PIPO_03_add_old, paste0(path_csv, "PIPO_03_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
write.csv(PIPO_04_add_old, paste0(path_csv, "PIPO_04_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
write.csv(PIPO_05_add_old, paste0(path_csv, "PIPO_05_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
write.csv(PIPO_06_add_old, paste0(path_csv, "PIPO_06_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
write.csv(PIPO_07_add_old, paste0(path_csv, "PIPO_07_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
write.csv(PIPO_08_add_old, paste0(path_csv, "PIPO_08_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
write.csv(PIPO_09_add_old, paste0(path_csv, "PIPO_09_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
write.csv(PIPO_10_add_old, paste0(path_csv, "PIPO_10_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
write.csv(PIPO_11_add_old, paste0(path_csv, "PIPO_11_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
write.csv(PIPO_13_add_old, paste0(path_csv, "PIPO_13_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")
write.csv(PIPO_14_add_old, paste0(path_csv, "PIPO_14_AllTrees.csv"), quote=FALSE, row.names = FALSE, na = "")

