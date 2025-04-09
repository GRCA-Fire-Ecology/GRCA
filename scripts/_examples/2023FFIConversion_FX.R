# To convert from FX datasheets to FFI compatible CSVs
# Have one folder for FX protocol (named "FXProtocol") have one folder for TLS protocol (named "TLSProtocol")
# In each folder, create folders named "Trees", "FWD", "CWD", "DuffLitt", "BurnSeverity"


#Read in Excel files

library(tidyverse)
library(readxl)
library(writexl)
library(readr)
library(plyr)
library(reshape2)
library(stringr)
library(rio)


# Trees - Individual ------------------------------------------------------


# FX protocol -------------------------------------------------------------

#Change file path based on own computer
setwd("C:/Users/alalor.NPS/Desktop/FX_Lalor/R/GRCA/test/data_raw/FX") #file path for data
getwd()

fxnames <- list.files(path = "C:/Users/alalor.NPS/Desktop/FX_Lalor/R/GRCA/test/data_raw/FX", # set the path to your folder files
                      pattern = "*.xlsx", # select all excel files in the folder
                      full.names = T) # output full file names (with path)

#Add file names as a column in the output
namecol <- data.frame(file = fxnames,
                      id = as.character(1:length(fxnames))) # id for joining

data <- fxnames %>%
  lapply(readxl::read_excel,sheet="Overstory") %>% # read all the files at once
  lapply(function(x) x[(names(x) %in% c("TreeID","Species_4letter","DBH_cm","Status_L_F_D","Height_m","CBH_m"))]) %>% #this is if you don't want all the columns from the csv files
  bind_rows(.id = "id") %>% # bind all tables into one object, and give id for each
  left_join(namecol) # join name column created earlier

#*****CHANGE "Status_L_F_D" to "Status_L_F_D_S" for actual data processing

data<-as.data.frame(data) %>%
  mutate(Status_L_F_D = replace(Status_L_F_D, Status_L_F_D == "F", "U"),
         Height_m = round(Height_m, 0),
         CBH_m = round(CBH_m, 0))


head(data)

#Columns for csv import
#Index	QTR	TagNo	Spp_GUID	Status	Ht	LiCrBHt	DBH	ScorchHt	CrwnRto	DRC	XCoord
#CrScPct	CKR	DamCd1	Mort	DamSev2	DamSev3	DamSev1	CharHt	NuLiStems	EqDia	DamCd5
#DamSev5	YCoord	NuDeStems	CrwnRad	CrwnCl	Age	UV1	LaddMaxHt	DecayCl	DamCd3	UV2
#LaddBaseHt	GrwthRt	DamCd4	DamCd2	CrFuBHt	Comment	SubFrac	UV3	DamSev4	IsVerified	Species

ffimat<-data.frame(matrix(ncol = 39, nrow = nrow(data))) #create an empty matrix for other FFI variables

ffidat<-cbind(data,ffimat) #join empty required columns with fx data
head(ffidat)

names(data)

#Change column names
colnames(ffidat)<-c("id","Index","Species","DBH","Status","Ht","CrFuBHt","file","QTR","TagNo","Spp_GUID","LiCrBHt",
                    "ScorchHt","CrwnRto","DRC","XCoord","CrScPct","CKR","DamCd1","Mort","DamSev2",
                    "DamSev3","DamSev1","CharHt","NuLiStems","EqDia","DamCd5","DamSev5","YCoord",
                    "NuDeStems","CrwnRad","CrwnCl","Age","UV1","LaddMaxHt","DecayCl","DamCd3",
                    "UV2","LaddBaseHt","GrwthRt","DamCd4","DamCd2","Comment","SubFrac",
                    "UV3","DamSev4","IsVerified")
names(ffidat)

#Reorder to required FFI order
ffidat<-ffidat[,c(2,9:11,5:6,12,4,13:42,7,43:47,3,8)]
names(ffidat)

ffidat$DamCd1<-ifelse(ffidat$Status=="S","FIRE",NA) #change damage code for scorched trees to fire

ffidat<-as.data.frame(ffidat) %>%
  mutate(Status = replace(Status, Status == "S", "D"))

#get unique species identifier from ffi
speclist<-read.csv("C:/Users/LHankin/OneDrive - DOI/Documents/FXProgram/DataManagement/FFIConversion/LocalSpecies.csv") %>% #may need to change file path for your computer
  filter(!LocalSpecies_Symbol %in% c('CADE8', 'UNKN2', 'UNKN3', 'UNKN4'))
head(speclist)
speclist$Species<-substr(speclist$LocalSpecies_Symbol,1,4)
names(speclist)


ffitrees<-merge(ffidat,speclist[,c(2,23)],by="Species") #merge with species code
head(ffitrees)

#Change output filename to separate folder
fun_insert <- function(x, pos, insert) {       # Create own function
  gsub(paste0("^(.{", pos, "})(.*)$"),
       paste0("\\1", insert, "\\2"),
       x)
}

#Apply function
library(stringr)
ffitrees$file<-str_replace(ffitrees$file,'.xlsx','.csv')

ffitrees$file<-fun_insert(x = ffitrees$file,    # Apply own function
                          pos = 83, # **IMPORTANT** with a different file path you will need to change number of characters to right position
                          insert = "Trees/")
head(ffitrees)

ffitrees$Spp_GUID<-ffitrees$LocalSpecies_GUID
names(ffitrees)
ffitrees<-ffitrees[,-47]
ffitrees<-ffitrees[,c(2:45,1,46)]

ffitrees$TagNo <- ffitrees$Index

ffitrees$SubFrac <- 1

ffitrees$IsVerified <- FALSE

ffilist<-split(ffitrees,ffitrees$file)
treesout<-lapply(ffilist, function(x) x[!(names(x) %in% c("file"))])

library(rio)
export_list(treesout,file=names(treesout))


# Surface Fuels -----------------------------------------------------


# FX Protocol -------------------------------------------------------------


fxnames <- list.files(path = "C:/Users/LHankin/OneDrive - DOI/Documents/FXProgram/PlotData/DataCopy_forCoding/FX", # set the path to your folder files
                      pattern = "*.xlsx", # select all excel files in the folder
                      full.names = T) # output full file names (with path)

#Add file names as a column in the output
namecol <- data.frame(file = fxnames,
                      id = as.character(1:length(fxnames))) # id for joining

data <- fxnames %>%
  lapply(readxl::read_excel, sheet="Browns") %>% # read all the files at once
  lapply(function(x) x[,2:5]) %>% #take specific columns
  bind_rows(.id = "id") %>% # bind all tables into one object, and give id for each
  left_join(namecol) # join name column created earlier

data<-as.data.frame(data)
names(data)
data[is.na(data)] <- 0
head(data)

#Fine Woody: columns: Index Transect Azimuth	Slope	OneHr	TenHr	HunHr	Comment	FWDFuConSt

ffimat<-data.frame(matrix(ncol = 5, nrow = nrow(data))) #create empty matrix

ffidat<-cbind(data,ffimat) #join empty required columns with fx data
head(ffidat)


#Change column names
colnames(ffidat)<-c("id","Transect","OneHr","TenHr","HunHr","file","Index","Azimuth","Slope","Comment","FWDFuConSt")

names(ffidat)

#Reorder to required FFI order
ffidat<-ffidat[,c(7,2,8:9,3:5,10:11,6)]
names(ffidat)

ffidat$FWDFuConSt <- "Default" #not sure if this is correct, may need to change

#Change output filename to separate folder
fun_insert <- function(x, pos, insert) {       # Create own function
  gsub(paste0("^(.{", pos, "})(.*)$"),
       paste0("\\1", insert, "\\2"),
       x)
}

#Apply function
ffidat$file<-str_replace(ffidat$file,'.xlsx','.csv')

head(ffidat$file)
ffidat$file<-fun_insert(x = ffidat$file,    # Apply own function
                        pos = 83, #position depends on parent folder name so check this
                        insert = "FWD/")
head(ffidat) #NAs might be a problem, can replace with 0s if needed

ffidat$Index <- 1
ffidat$Transect <- 1
ffidat$Slope <- 0

names(ffidat)

ffilist<-split(ffidat[,-10],ffidat$file)
fwdout<-lapply(ffilist, function(x) x[!(names(x) %in% c("file"))])

export_list(fwdout,file=names(fwdout))



#Coarse Woody:

data <- fxnames %>%
  lapply(readxl::read_excel, sheet="1000HR",) %>% # read all the files at once
  lapply(function(x) x[(names(x) %in% c("Sound_1_or_Rotten_4","Diameter_cm"))]) %>%
  bind_rows(.id = "id") %>% # bind all tables into one object, and give id for each
  left_join(namecol) # join name column created earlier

data<-as.data.frame(data)
names(data)
data<-data[!is.na(data$Diameter_cm),]
head(data)

data$id<-as.factor(data$id)
dat2 <- ddply(data,.(id),transform,LogNum=seq(1:length(id))) #generate log number sequence for each plot
head(dat2)

#3 = sound, 4 = rotten

dat2$DecayCl<-ifelse(dat2$Sound_1_or_Rotten_4=="1-Sound",3,4)

#columns:Index Transect	Slope	LogNum	Dia	DecayCl	Decay Class Description	CWDFuConSt	Comment

ffimat<-data.frame(matrix(ncol = 5, nrow = nrow(dat2)))

ffidat<-cbind(dat2,ffimat) #join empty required columns with fx data
head(ffidat)


#Change column names
colnames(ffidat)<-c("Index","Dia","Sound_Rotten","file","LogNum","DecayCl","Transect",
                    "Slope","Decay Class Description","CWDFuConSt","Comment")

names(ffidat)

#Reorder to required FFI order
ffidat<-ffidat[,c(1,7:8,5,2,6,9:11,4)]
names(ffidat)

ffidat$CWDFuConSt<-"Default" #not sure if this is correct, may need to change
ffidat$`Decay Class Description`<-ifelse(ffidat$DecayCl==3,"Sound (Most bark and most branches <  1 in. diameter missing)",
                                         "Rotten (Looks like class 3 but sapwood is rotten)")
head(ffidat)


#Apply function
ffidat$file<-str_replace(ffidat$file,'.xlsx','.csv')

head(ffidat$file)
ffidat$file<-fun_insert(x = ffidat$file,    # Apply own function
                        pos = 83, #position depends on parent folder name so check this
                        insert = "CWD/")
head(ffidat) #NAs might be a problem, can replace with 0s if needed

ffidat$Transect <- 1
ffidat$Slope <- 0

names(ffidat)
ffilist<-split(ffidat[,-10],ffidat$file)
fwdout<-lapply(ffilist, function(x) x[!(names(x) %in% c("file"))])

export_list(fwdout,file=names(fwdout))


#Duff Litt:

data <- fxnames %>%
  lapply(readxl::read_excel, sheet="Fuels",) %>% # read all the files at once
  lapply(function(x) x[1:2,c(2,4:6)]) %>%
  bind_rows(.id = "id") %>% # bind all tables into one object, and give id for each
  left_join(namecol) # join name column created earlier

data<-as.data.frame(data)
names(data)
head(data)

#columns:Transect	SampLoc	LittDep	DuffDep	DLFuConSt	Comment ##CHECK ABOUT OFFSET, INDEX

ffimat<-data.frame(matrix(ncol = 5, nrow = nrow(data)))

ffidat<-cbind(data,ffimat) #join empty required columns with fx data
head(ffidat)

#Change column names
colnames(ffidat)<-c("id","Transect","DuffDep","LittDep","FuelbedDep", "file","SampLoc","DLFuConSt","Comment", "Offset", "Index")

names(ffidat)

ffidat$Transect<-as.factor(ffidat$Transect)
levels(ffidat$Transect)<-c("1","2")

#Reorder to required FFI order
ffidat<-ffidat[,c(2,7,4,3,5,10,11,8,9,6)]
names(ffidat)

ffidat$DLFuConSt<-"Default" #not sure if this is correct, may need to change


#Apply function
ffidat$file<-str_replace(ffidat$file,'.xlsx','.csv')

head(ffidat$file)
ffidat$file<-fun_insert(x = ffidat$file,    # Apply own function
                        pos = 83, #position depends on parent folder name so check this
                        insert = "DuffLitt/")
head(ffidat) #NAs might be a problem, can replace with 0s if needed

ffidat$SampLoc <- 1
ffidat$Offset <- FALSE
ffidat$Index <- ffidat$Transect

head(ffidat)

ffilist<-split(ffidat[,-10],ffidat$file)
fwdout<-lapply(ffilist, function(x) x[!(names(x) %in% c("file"))])

export_list(fwdout,file=names(fwdout))




# Burn Severity FX Protocol -------------------------------------------------------------


fxnames <- list.files(path = "C:/Users/LHankin/OneDrive - DOI/Documents/FXProgram/PlotData/DataCopy_forCoding/FX", # set the path to your folder files
                      pattern = "*.xlsx", # select all excel files in the folder
                      full.names = T) # output full file names (with path)

#Add file names as a column in the output
namecol <- data.frame(file = fxnames,
                      id = as.character(1:length(fxnames))) # id for joining

data <- fxnames %>%
  lapply(readxl::read_excel, sheet="BurnSeverity",) %>% # read all the files at once
  lapply(function(x) x[(names(x) %in% c("OverallSeverity","SoilBurnSeverity"))]) %>%
  bind_rows(.id = "id") %>% # bind all tables into one object, and give id for each
  left_join(namecol) # join name column created earlier

data<-as.data.frame(data)
names(data)
data<-data[!is.na(data$OverallSeverity),]

head(data)

#0	N/A Preburn
#1	Heavily Burned
#2	Moderately Burned
#3	Lightly Burned
#4	Scorched
#5	Unburned

data$'Sub'<-ifelse(data$SoilBurnSeverity=="Low",3, #these classifications may need to change if we used something else in the data
                   ifelse(data$SoilBurnSeverity=="Moderate",2,
                          ifelse(data$SoilBurnSeverity=="High",1,
                                 ifelse(data$SoilBurnSeverity=="LOW",3, #these classifications may need to change if we used something else in the data
                                        ifelse(data$SoilBurnSeverity=="MODERATE",2,
                                               ifelse(data$SoilBurnSeverity=="HIGH",1,5))))))
data$'Veg'<-ifelse(data$OverallSeverity=="Low",3,
                   ifelse(data$OverallSeverity=="Moderate",2,
                          ifelse(data$OverallSeverity=="High",1,
                                 ifelse(data$OverallSeverity=="LOW",3,
                                        ifelse(data$OverallSeverity=="MODERATE",2,
                                               ifelse(data$OverallSeverity=="HIGH",1,5))))))

head(data)

ffimat<-data.frame(matrix(ncol = 4, nrow = nrow(data)))

ffidat<-cbind(data,ffimat) #join empty required columns with fx data
head(ffidat)
names(ffidat)

#Severity: Transect	TapeDist	Sub	Severity - Sub	Veg	Severity - Veg	Comment

#Reorder to required FFI order
ffidat<-ffidat[,c(7,8,9,5,6,10,4)]
names(ffidat)

#Change column names
colnames(ffidat)<-c("Index","Transect","TapeDist",colnames(ffidat)[4],colnames(ffidat)[5],"Comment","file")

names(ffidat)

#Change output filename to separate folder
fun_insert <- function(x, pos, insert) {       # Create own function
  gsub(paste0("^(.{", pos, "})(.*)$"),
       paste0("\\1", insert, "\\2"),
       x)
}

#Apply function
ffidat$file<-str_replace(ffidat$file,'.xlsx','.csv')

head(ffidat$file)
ffidat$file<-fun_insert(x = ffidat$file,    # Apply own function
                        pos = 83, #position depends on parent folder name so check this
                        insert = "BurnSeverity/")
head(ffidat) #NAs might be a problem, can replace with 0s if needed

ffidat$Index <- 1
ffidat$Transect <- 1

ffilist<-split(ffidat[,-7],ffidat$file)
fwdout<-lapply(ffilist, function(x) x[!(names(x) %in% c("file"))])

export_list(fwdout,file=names(fwdout))


