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

# TLS protocol ---------------------------------------------------------------------

# Trees - Individual ------------------------------------------------------

#For TLS protocol, we will just put the basal area values as a user variable (hopefully this works)

setwd("C:/Users/LHankin/OneDrive - DOI/Documents/FXProgram/PlotData/DataCopy_forCoding/TLS")

fxnames <- list.files(path = "C:/Users/LHankin/OneDrive - DOI/Documents/FXProgram/PlotData/DataCopy_forCoding/TLS", # set the path to your folder files
                      pattern = "*.xlsx", # select all excel files in the folder
                      full.names = T) # output full file names (with path)

#Add file names as a column in the output
namecol <- data.frame(file = fxnames, 
                      id = as.character(1:length(fxnames))) # id for joining

BAF <- fxnames %>% 
  lapply(readxl::read_excel, sheet="inputdata", range = "J2", col_names = 'BAF') %>% 
  bind_rows(.id = "id") %>% # bind all tables into one object, and give id for each
  left_join(namecol)

data <- fxnames %>% 
  lapply(readxl::read_excel, sheet="Basal Area", range = "A1:E13") %>% # read all the files at once
  lapply(function(x) x[,1:5]) %>% #take the first 5 columns
  bind_rows(.id = "id") %>% # bind all tables into one object, and give id for each
  left_join(namecol) %>% # join name column created earlier
  left_join(BAF)
  
data<-as.data.frame(data)
head(data)
data$BAF<-10 #fix some QAQC errors, don't do this if BAF is not 10 for all plots
names(data)

data[is.na(data)] <- 0
colnames(data)[2]<-"Species"

data$L<-data$`Green (total #)`+data$`Partially Red Needle (5-90%)   (total #)`
data$D<-data$`Snag (total #)`+data$`Very Red Needle (>90%) (total #)`

datlong<-melt(data[,c(1,2,7:10)], measure.vars = c("L", "D"))
names(datlong)
head(datlong)
colnames(datlong)[5]<-"Status"
colnames(datlong)[6]<-"UV1" #Basal area becomes UV1
datlong$UV1<-datlong$UV1*datlong$BAF #convert based on prism value

#Columns for csv import
#Index	QTR	TagNo	Spp_GUID	Status	Ht	LiCrBHt	DBH	ScorchHt	CrwnRto	DRC	XCoord	
#CrScPct	CKR	DamCd1	Mort	DamSev2	DamSev3	DamSev1	CharHt	NuLiStems	EqDia	DamCd5	
#DamSev5	YCoord	NuDeStems	CrwnRad	CrwnCl	Age	UV1	LaddMaxHt	DecayCl	DamCd3	UV2	
#LaddBaseHt	GrwthRt	DamCd4	DamCd2	CrFuBHt	Comment	SubFrac	UV3	DamSev4	IsVerified	Species

ffimat<-data.frame(matrix(ncol = 41, nrow = nrow(datlong)))

ffidat<-cbind(datlong,ffimat) #join empty required columns with fx data
head(ffidat)

names(datlong)

#Change column names
colnames(ffidat)<-c("id","Species","file","BAF", "Status","UV1","Index","QTR","TagNo","Spp_GUID","Ht","LiCrBHt","DBH",
                    "ScorchHt","CrwnRto","DRC","XCoord","CrScPct","CKR","DamCd1","Mort","DamSev2",
                    "DamSev3","DamSev1","CharHt","NuLiStems","EqDia","DamCd5","DamSev5","YCoord",
                    "NuDeStems","CrwnRad","CrwnCl","Age","LaddMaxHt","DecayCl","DamCd3",
                    "UV2","LaddBaseHt","GrwthRt","DamCd4","DamCd2","Comment","SubFrac",
                    "UV3","DamSev4","IsVerified")  
names(ffidat)

#Reorder to required FFI order
ffidat<-ffidat[,c(7:10,5,11:34,6,35:47,2,3)]
names(ffidat)

#get unique species identifier from ffi
speclist<-read.csv("C:/Users/LHankin/OneDrive - DOI/Documents/FXProgram/DataManagement/FFIConversion/LocalSpecies.csv") %>% 
  filter(!LocalSpecies_Symbol %in% c('CADE8', 'UNKN2', 'UNKN3', 'UNKN4'))
head(speclist)
speclist$Species<-substr(speclist$LocalSpecies_Symbol,1,4)
names(speclist)

ffidat$Species<-substr(ffidat$Species,1,4)

ffitrees<-merge(ffidat,speclist[,c(2,23)],by="Species") #merge with species code
head(ffitrees)

#Change output filename to separate folder
fun_insert <- function(x, pos, insert) {       # Create own function
  gsub(paste0("^(.{", pos, "})(.*)$"),
       paste0("\\1", insert, "\\2"),
       x)
}

#Apply function
ffitrees$file<-str_replace(ffitrees$file,'.xlsx','.csv')

head(ffitrees$file)
ffitrees$file<-fun_insert(x = ffitrees$file,    # Apply own function
                          pos = 84, #position depends on parent folder name so check this
                          insert = "Trees/")
head(ffitrees)

ffitrees$Spp_GUID<-ffitrees$LocalSpecies_GUID
names(ffitrees)
ffitrees<-ffitrees[,-46]
ffitrees<-ffitrees[,c(2:44,1,45)]

ffitrees$TagNo <- 1

ffitrees$SubFrac <- 1

ffitrees$IsVerified <- FALSE

ffitrees <- filter(ffitrees, UV1 != 0)

ffilist<-split(ffitrees,ffitrees$file)
treesout<-lapply(ffilist, function(x) x[!(names(x) %in% c("file"))])


export_list(treesout,file=names(treesout))




# Surface Fuels -----------------------------------------------------


fxnames <- list.files(path = "C:/Users/LHankin/OneDrive - DOI/Documents/FXProgram/PlotData/DataCopy_forCoding/TLS", # set the path to your folder files
                      pattern = "*.xlsx", # select all excel files in the folder
                      full.names = T) # output full file names (with path)

#Add file names as a column in the output
namecol <- data.frame(file = fxnames, 
                      id = as.character(1:length(fxnames))) # id for joining

data <- fxnames %>% 
  lapply(readxl::read_excel, sheet="inputdata",) %>% # read all the files at once
  lapply(function(x) x[(names(x) %in% c("1HR","10HR","100HR"))]) %>% #take the first 5 columns
  bind_rows(.id = "id") %>% # bind all tables into one object, and give id for each
  left_join(namecol) # join name column created earlier

data<-as.data.frame(data)
names(data)
data[is.na(data)] <- 0

#Fine Woody: columns: Transect	Azimuth	Slope	OneHr	TenHr	HunHr	Comment	FWDFuConSt

ffimat<-data.frame(matrix(ncol = 6, nrow = nrow(data)))

ffidat<-cbind(data,ffimat) #join empty required columns with fx data
head(ffidat)


#Change column names
colnames(ffidat)<-c("id","OneHr","TenHr","HunHr","file","Index", "Transect","Azimuth","Slope","Comment","FWDFuConSt")

names(ffidat)

#Reorder to required FFI order
ffidat<-ffidat[,c(6:9,2:4,10:11,5)]
names(ffidat)

ffidat$FWDFuConSt<-"Default" #not sure if this is correct, may need to change

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
                        pos = 84, #position depends on parent folder name so check this
                        insert = "FWD/")
head(ffidat) #NAs might be a problem, can replace with 0s if needed

ffidat$Index <- 1
ffidat$Transect <- 1
ffidat$Slope <- 0

ffilist<-split(ffidat[,-10],ffidat$file)
fwdout<-lapply(ffilist, function(x) x[!(names(x) %in% c("file"))])

export_list(fwdout,file=names(fwdout))


#Duff Litt: 

data <- fxnames %>% 
  lapply(readxl::read_excel, sheet="FuelsCanopy",) %>% # read all the files at once
  lapply(function(x) x[1:4,c(2:3,5:7)]) %>% 
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
colnames(ffidat)<-c("id","Transect","Location","DuffDep","LittDep","FuelbedDep", "file",
                    "SampLoc","DLFuConSt","Comment", "Offset", "Index")

names(ffidat)
ffidat$Transect<-paste(ffidat$Transect,ffidat$Location,sep="_")
ffidat$Transect<-as.factor(ffidat$Transect)
levels(ffidat$Transect)<-c("1","2","3","4")

#Reorder to required FFI order
ffidat<-ffidat[,-3]
names(ffidat)

ffidat$DLFuConSt<-"Default" #not sure if this is correct, may need to change

#Apply function
ffidat$file<-str_replace(ffidat$file,'.xlsx','.csv')

head(ffidat$file)
ffidat$file<-fun_insert(x = ffidat$file,    # Apply own function
                        pos = 84, #position depends on parent folder name so check this
                        insert = "DuffLitt/")
head(ffidat) #NAs might be a problem, can replace with 0s if needed

ffidat$SampLoc <- 1
ffidat$Offset <- FALSE
ffidat$Index <- ffidat$Transect

ffidat<-ffidat[,c(2,7,4,3,5,10,11,8,9,6)]

head(ffidat)

ffilist<-split(ffidat[,-10],ffidat$file)
fwdout<-lapply(ffilist, function(x) x[!(names(x) %in% c("file"))])

export_list(fwdout,file=names(fwdout))


# Burn severity -----------------------------------------------------------

fxnames <- list.files(path = "C:/Users/LHankin/OneDrive - DOI/Documents/FXProgram/PlotData/DataCopy_forCoding/TLS", # set the path to your folder files
                      pattern = "*.xlsx", # select all excel files in the folder
                      full.names = T) # output full file names (with path)

#Add file names as a column in the output
namecol <- data.frame(file = fxnames, 
                      id = as.character(1:length(fxnames))) # id for joining

col_types_FX = 'text'

data <- fxnames %>% 
  lapply(readxl::read_excel, sheet="inputdata",col_types = col_types_FX) %>% # read all the files at once
  lapply(function(x) x[(names(x) %in% c("OverallSeverity","SoilBurnSeverity"))]) %>% 
  bind_rows(.id = "id") %>% # bind all tables into one object, and give id for each
  left_join(namecol) # join name column created earlier

data<-as.data.frame(data)
names(data)
data[data == 0] <- NA 

data<-data[!is.na(data$OverallSeverity),]

data$SoilBurnSeverity[is.na(data$SoilBurnSeverity)] <- "Unburned" 

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
                        pos = 84, #position depends on parent folder name so check this
                        insert = "BurnSeverity/")
head(ffidat) #NAs might be a problem, can replace with 0s if needed

ffidat$Index <- 1
ffidat$Transect <- 1

ffilist<-split(ffidat[,-7],ffidat$file)
fwdout<-lapply(ffilist, function(x) x[!(names(x) %in% c("file"))])

export_list(fwdout,file=names(fwdout))

