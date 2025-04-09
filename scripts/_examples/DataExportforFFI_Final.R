##Tree Export--Collaborative data sharing with FE Program
# This script takes SCPN-collected shared tree data and formats it 
#FE program can enter into FFI. 

#set up
library(RODBC)
library(tidyverse)

setwd("C:\\Users\\mswan\\OneDrive - DOI\\Documents\\R\\Projects\\DataPulls\\DataPulls\\GRCA_fx")

#Get data from SCPN SQL database
conUpE <- odbcDriverConnect("driver=SQL Server;server=INPSCPNMS01\\Production, 50000; database=SCPN_UplandEvent;trusted_connection=yes;")
#raw tree data
scpntrees<- sqlFetch(conUpE, "view_TreeStemBasalAreaAndStatus_All", as.is=FALSE, stringsAsFactors = FALSE)
#view with most recent tree data to backfill dbh for status 2 trees
scpnmostrecent<-sqlFetch(conUpE, "view_TreeBasalAreaAndStatus_MostRecent", as.is=FALSE, stringsAsFactors=FALSE)
close(conUpE)
#and get guiid lookup file from GRCA FE
guids<-read.csv("C:\\Users\\mswan\\OneDrive - DOI\\Documents\\R\\Projects\\DataPulls\\DataPulls\\GRCA_fx\\lookup_table.csv")

#choose which GRCA ecosite data you want to export below
trees<-scpntrees%>%
  filter(EcoSite %in% "GRCA_M", EventYear == 2022)%>%
  #filter(Plot %in% "GRCA_M21")%>%
  select(Plot, TreeID, Tag, FieldID, TreeStatus, StemID, StemStatus,CrownHealth, Diameter_cm, EventYear, TreeVisitNote, StemNote)%>%
  unite("Note", TreeVisitNote, StemNote, na.rm = TRUE)
#match to most recent data for trees that weren't measured in current 
#as TreeBasalArea_sqmeter so convert to DBH then replace DBH NAs with these values.
treeswithmostrecent<-scpnmostrecent%>%
  select(TreeID, MostRecentTreeStatus =TreeStatus, TreeBasalArea_sqmeter)%>%
  right_join(trees, by = 'TreeID')%>%
  #filter(TreeStatus==2)%>%#convert BA to DBH
  mutate(DBH = sqrt(40000 * TreeBasalArea_sqmeter/3.142))%>%
  mutate(Diameter_cm=ifelse(is.na(Diameter_cm), DBH, Diameter_cm))

#here I need to write to .csv to fix up the comments which have h and cbh data and other text info mixed in. Right now need to do by hand. Separate "Notes" field, move text notes to created "Comments" field, then concatenate the H and CBH into "Notes" separated by _
write.csv(treeswithmostrecent, "grcam2022_trees.csv", row.names = F)
#when fixed read back in
treeswithmostrecentfixed<-read.csv("grcam2022_trees.csv")

#get guids and column names from FFI by reading in outside data from Li. Apparently these are different for different parks?
withguids<-treeswithmostrecentfixed%>%
  rename(TagNo = Tag, ScientificName = FieldID, Status = TreeStatus)%>%
  separate(ScientificName, into = c("Genus", "Spp"), remove = FALSE)%>%
  mutate(Genus = substr(Genus, 1, 2), Spp = substr(Spp, 1, 2))%>%
  unite(Species, Genus, Spp, sep = "")%>%
  mutate(Species=toupper(Species))%>%
  left_join(guids, by = c("Species", "ScientificName"))
#clean up notes field and get CBH and H
cleanwithguids<-withguids%>%
  separate(Note, into=c("Ht", "LiCrBHt"), sep = "_")%>%
  mutate(Comment2=ifelse(!StemID %in% "Main", 
                         (paste0("Stem ", StemID, " of ", TagNo)), 
                         ""))%>%
  unite(Comment, c(Comment2, Comment), sep=" ")%>%
  separate(Plot, into=c(NA, "PlotNo"), 5, remove = FALSE)%>%
  mutate(Diameter_cm=ifelse(Status==3, " ", Diameter_cm))

str(cleanwithguids)


#format for FFI
template<-cleanwithguids%>%
  add_column(QTR = "", UV1="", UV2 = "", UV3 = "", CrwnCl = "", Metadata="")%>%
  mutate(Index = 1, SubFrac = 1, IsVerified = "TRUE", 
         Status = ifelse(Status == 1, "L",
                         ifelse(Status == 2, "D", Status)),
         CrwnCl = ifelse(Status == 3, "DD", CrwnCl))%>%
  unite("Metadata", c(PlotNo, EventYear), sep = " ")%>%
  mutate(Status=ifelse(Status ==3, "D", Status))%>%
  select(Plot, Index, SubFrac, QTR, TagNo, UV1, Species, Spp_GUID, Status, CrwnCl, DBH = Diameter_cm,  Ht, UV2, LiCrBHt, UV3, CrownHealth, Comment, IsVerified, Metadata)%>%
  #group_by(Plot)%>%
  mutate(Index = row_number(Plot))%>%
  mutate(Comment = ifelse(is.na(Comment), "", Comment))%>%
  mutate(DBH = ifelse(is.na(DBH), "", DBH))%>%
  mutate(LiCrBHt = ifelse(is.na(LiCrBHt), "", LiCrBHt))%>%
  mutate(CrownHealth = ifelse(is.na(CrownHealth), "", CrownHealth))%>%
  mutate(Comment = ifelse(Comment %in% "NA", "", Comment))%>%
  filter(!Status == 0)

template$DBH<-as.numeric(template$DBH)
template$Plot<-as.factor(template$Plot)


template<-template%>%
  mutate(DBH = signif(template$DBH, 3))


str(template)

#group by plot and then save each plot as a separate .csv
byplot<-template%>%
  group_by(Plot)%>%
  arrange(TagNo, .by_group = TRUE)%>%
  ungroup()

#export by plot to plot named files
by(byplot, byplot$Plot, FUN=function(i) write.csv(i, paste0(i$Plot[1], ".csv")))

##alternate way to export by plot
sapply(unique(byplot$Plot), function(x) 
  write.csv(byplot[byplot$Plot==x,],paste0(x,".csv"),row.names=FALSE))
