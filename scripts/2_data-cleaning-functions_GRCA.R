###Data cleaning functions

################################################################################
# Sample Event ####
#### Description ----
#' Sample event quality control checks
#' @description
#' The sample_event_qc function is designed to perform quality control
#' checks on sample event data associated with different project units.
#' It takes two arguments, samples (a dataset containing sample event records)
#' and mtype (Project Unit Name). The function first filters the dataset to
#' include only the specified project unit. It then checks for missing values
#' (NAs) in the MonitoringStatus_Name, Protocols, and Visited columns. If any
#' NAs are detected, they are added to a list of flags. Additionally, the function
#' checks for mislabeled or missing monitoring statuses, protocols, and visited
#' values which will be added to flags.*IF ADJUSTING FOR DIFFERENT PROGRAM – you
#' must edit code to include monitoring statuses and protocols specific to your
#' program. This function will produce a visualization that shows which protocols
#' were collected each year and the years a fire occurred.
#' @param
#' samp
#' @param
#' mtype
#'
#' @return A list of flags or data issues in the sample events csv
#' @export
#'
#' @examples
#' sample_event_qc(samp, "PSME")

#### Function ----
sample_event_qc <- function(samp, mtype) {

  samples <<- samp[which(samp$ProjectUnit_Name == mtype),] # filtering for vegetation type

  # checking for NAs in monitoring status
  nas_monitoring_status <- anyNA(samples$MonitoringStatus_Name)
  cat("Any NAs in monitoring status?\n")
  cat(nas_monitoring_status, "\n")

  # checking for NAs in protocols
  nas_protocols <- anyNA(samples$Protocols)
  cat("Any NAs in protocols?\n")
  cat(nas_protocols, "\n")

  # adding any NAs to flags
  if (nas_monitoring_status) {
    flags <- c(flags, "NAs exist in monitoring status column of sample event data")
  }
  if (nas_protocols) {
    flags <- c(flags, "NAs exist in protocol column of sample event data")
  }

  monitoring_status <- c("00PR01", "01Pre", "01Post", "01Year01", "01Year02", "01Year05", "02Pre", "02Post", "02Year01", "02Year02", "02Year10", "03Pre", "03Post", "03Year01", "03Year02", "00PR02", "00PR03", "00PR04", "01Year10", "01Year12", "02Year05", "03Year05","04Post", "04Year01", "04Year02", "04Pre", "04Year05", "05Post", "05Year01", "05Year02",  "02Year20", "01Year20", "00 PRE")

  recorded_monitoring_status <- unique(samples$MonitoringStatus_Name)
  mislabeled_monitoring_status <- setdiff(recorded_monitoring_status, monitoring_status)
  # printing monitoring status checks
  cat("\nWhich monitoring statuses are included?\n")
  cat(paste(recorded_monitoring_status, sep="\n"))
  cat("\n")
  cat("\nWhich monitoring statuses are off-cycle?\n")
  if (length(mislabeled_monitoring_status) == 0) {
    cat("None\n")
  } else {
    cat(mislabeled_monitoring_status, sep = "\n")
    flags <- c(flags, paste("These monitoring statuses are off-cycle:", paste(mislabeled_monitoring_status, collapse = ", "), sep = " "))
  }

  #ms check with years

 # anything that says post should be the same years as a fire

  samples$SampleEvent_Date=as.Date(samples$SampleEvent_Date, "%m/%d/%Y")
    samples$year=str_split_fixed(samples$SampleEvent_Date, "-",3)[,1]


  samples=samples%>%
    mutate(fire="no_fire")



 post=samples[grep("Post", samples$MonitoringStatus_Name),]
  samples[grep("Post", samples$MonitoringStatus_Name),"fire"]="fire"



  postyears=unique(post$year)
  postplots=c()

  for(x in 1:length(postyears)){
    postplot=unique(post[which(post$year==postyears[x]), "MacroPlot_Name"], "\n")
    postplots=c(postplots, postplot)
    cat(paste(c("\nIn", postyears[x],"the following plots burned and were measured: ", postplot), collapse=" "))
  }

  cat("(Warning: some plots may have burned but were only measured in following years)")
  cat("\n")
  after=samples[grep("Year01", samples$MonitoringStatus_Name),]
  samples[grep("Year01", samples$MonitoringStatus_Name),"fire"]="fire+1"

  burnyears=as.numeric(unique(c(samples[which(samples$fire=="fire+1"), "year"])))-1



  afteryears=unique(after$year)
  afterplots=c()

  for(x in 1:length(afteryears)){
    afterplot=unique(after[which(after$year==afteryears[x]), "MacroPlot_Name"], "\n")
    afterplots=c(afterplots, afterplot)
  }
  samples[which(samples$year %in% burnyears & samples$MacroPlot_Name %in% afterplots),"fire"]="fire"

  cat("\n")

  after2=samples[grep("Year02", samples$MonitoringStatus_Name),]
  samples[grep("Year02", samples$MonitoringStatus_Name),"fire"]="fire+2"

  burnyears=as.numeric(unique(c(samples[which(samples$fire=="fire+2"), "year"])))-2


  after2years=unique(after2$year)
  after2plots=c()

  for(x in 1:length(after2years)){
    after2plot=unique(after2[which(after2$year==after2years[x]), "MacroPlot_Name"], "\n")
    after2plots=c(after2plots, after2plot)
  }
  samples[which(samples$year %in% burnyears & samples$MacroPlot_Name %in% after2plots),"fire"]="fire"


  after5=samples[grep("Year05", samples$MonitoringStatus_Name),]
  samples[grep("Year05", samples$MonitoringStatus_Name),"fire"]="fire+5"

  burnyears=as.numeric(unique(c(samples[which(samples$fire=="fire+5"), "year"])))-5


  after5years=unique(after5$year)
  after5plots=c()

  for(x in 1:length(after5years)){
    after5plot=unique(after5[which(after5$year==after5years[x]), "MacroPlot_Name"], "\n")
    after5plots=c(after5plots, after5plot)
  }
  samples[which(samples$year %in% burnyears & samples$MacroPlot_Name %in% after5plots),"fire"]="fire"


  after10=samples[grep("Year10", samples$MonitoringStatus_Name),]
  samples[grep("Year10", samples$MonitoringStatus_Name),"fire"]="fire+10"

  burnyears=as.numeric(unique(c(samples[which(samples$fire=="fire+10"), "year"])))-10


  after10years=unique(after10$year)
  after10plots=c()

  for(x in 1:length(after10years)){
    after10plot=unique(after10[which(after10$year==after10years[x]), "MacroPlot_Name"], "\n")
    after10plots=c(after10plots, after10plot)
  }


  samples=rbind(samples, after, after2, after5, after10)

  samples[which(samples$fire=="fire+1"), "year"]=as.numeric(samples[which(samples$fire=="fire+1"), "year"])-1
  samples[which(samples$fire=="fire+2"), "year"]=as.numeric(samples[which(samples$fire=="fire+2"), "year"])-2
  samples[which(samples$fire=="fire+5"), "year"]=as.numeric(samples[which(samples$fire=="fire+5"), "year"])-5
  samples[which(samples$fire=="fire+10"), "year"]=as.numeric(samples[which(samples$fire=="fire+10"), "year"])-10

  samples[which(samples$fire %in% c("fire+1","fire+2","fire+5","fire+10")), "MonitoringStatus_Name"]="01Post"
  samples[which(samples$fire %in% c("fire+1","fire+2","fire+5","fire+10")), "Visited"]="N"
  samples[which(samples$fire %in% c("fire+1","fire+2","fire+5","fire+10")), "Protocols"]="all"

  samples[which(samples$fire %in% c("fire+1","fire+2","fire+5","fire+10")), "fire"]="fire"

  #plotting stuff
  protocols <- c("Cover - Points (metric)", "Trees (metric)", "Surface Fuels", "Density - Belts (metric)", "Cover - Species Composition (metric)", "Post Burn Severity (metric)")


  a=samples[which(samples$Protocols=="all"),]
  a$Protocols="Cover - Points (metric)"
  b=samples[which(samples$Protocols=="all"),]
  b$Protocols="Trees (metric)"
  c=samples[which(samples$Protocols=="all"),]
  c$Protocols="Surface Fuels"
  d=samples[which(samples$Protocols=="all"),]
  d$Protocols="Density - Belts (metric)"
  e=samples[which(samples$Protocols=="all"),]
  e$Protocols="Cover - Species Composition (metric)"
  f=samples[which(samples$Protocols=="all"),]
  f$Protocols="Post Burn Severity (metric)"

  samples=rbind(samples, a,b,c,d,e,f)
  samples=samples[-which(samples$Protocols=="all"),]



  p=samples %>%
    ggplot(aes(x=year, y=MacroPlot_Name, shape=Visited))


  p=p+geom_point(aes(color=samples$MonitoringStatus_Name), size=5)+
    geom_point(aes(shape = samples$fire), size=3)+
    scale_shape_manual(values=c(16, 13,8, 1), breaks=c('Y', 'N', 'fire','no_fire'))+
    facet_wrap(~Protocols)+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 60, hjust=1, size=5))

  p


  recorded_protocols <- unique(samples$Protocols)
  cat("\n")


  cat("\nValid protocols (used for Saguaro)", "\n")
  cat(protocols, sep = "\n")


  mislabeled_protocols <- setdiff(recorded_protocols, protocols)
  cat("\nDo any protocols have data that shouldn't?\n")
  if (length(mislabeled_protocols) == 0) {
    cat("None\n")
  } else {
    cat(paste(c(mislabeled_protocols,"in year", unique(samples[which(samples$Protocols %in% mislabeled_protocols),"year"]), "for plots", unique(samples[which(samples$Protocols %in% mislabeled_protocols),"MacroPlot_Name"])), collapse = ", "), sep = "\n")
    flags <- c(flags, paste("These protocols have data but do not fall under the list of valid protocols:", paste(c(mislabeled_protocols,"in year", unique(samples[which(samples$Protocols %in% mislabeled_protocols),"year"]), "for plots", unique(samples[which(samples$Protocols %in% mislabeled_protocols),"MacroPlot_Name"])), collapse = ", "), sep = " "))
  }



  output<-list(p, flags)
  return(output)
}
#end of function

################################################################################
# Protocol: Post Burn Severity (metric) ####
#### Description ----
#' Post Burn Severity Substrate Checks
#' @description
#' This function checks for incorrect values for NumTran, TranLen, NumPtsTran, substrate and vegetation values
#' in the post burn severity data frame. Values may need to be edited according to park specific protocol. The
#' default values are used in Saguaro's monitoring program. The function also checks that there are
#' 4 transects for each macroplot with 10 sample points per transect. Any errors are flagged with information
#' about the problem sample event and macroplot.
#'
#' @param pbsev
#'
#' @return A list of flags or data issues with post burn severity substrate data in the post burn severity csv
#' @export
#'
#' @examples
#' pbsev_qc(tree)

#### Function ----
pbsev_qc=function(pbsev){

  # Helper function for validation checks
  validate <- function(column_name, valid_values, error_message) {
    # Extract non-missing values from the specified column
    values <- na.omit(pbsev[[column_name]])

    # Find indices of values that are not in the list of valid_values
    invalid_indices <- which(!values %in% valid_values)

    if (length(invalid_indices) > 0) {
      # Prepare the error message including invalid values and their row details
      error_msg <- paste(error_message, values[invalid_indices],
                         "in events/s", pbsev[which(pbsev[[column_name]] %in% values[invalid_indices]),"MacroPlot.Name"], pbsev[which(pbsev[[column_name]] %in% values[invalid_indices]), "Monitoring.Status"], collapse = "\n")
      # Print the error message and add it to the flags vector
      cat(error_msg, "\n")
      flags<- c(flags, paste("Error:", error_msg))
    } else {
      cat("All", column_name, "values are valid: TRUE\n")
    }
  }


  validate("NumTran", 4, "Invalid NumTran for post burn severity")
  validate("TranLen", 50, "Invalid TranLen for post burn severity")
  validate("NumPtsTran", 10, "Invalid NumPtsTran for post burn severity")
  validate("Sub", c(0,1,2,3,4,5), "Invalid substrate value for post burn severity")
  validate("Veg", c(0,1,2,3,4,5), "Invalid vegetation value for post burn severity")


  postburn=pbsev
  # Group by macro plot and sample event date (monitoring status)
  postburndates <- as.data.frame(str_split(unique(paste(postburn$MacroPlot.Name, postburn$Monitoring.Status, sep = ",")), ","))

  no_errors_t <- c()
  no_errors_s <- c()

  for (i in 1:ncol(postburndates)) {
    rows <- which((postburn[["MacroPlot.Name"]] == postburndates[1, i]) & (postburn[["Monitoring.Status"]] == postburndates[2, i]))

    # Check for errors in postburn transect values
    if (length(setdiff(na.omit(postburn$Transect[rows]), rep(c(1:4), each = 10))) != 0) {
      weird_rows <- which(postburn$Transect[rows] == setdiff(na.omit(postburn$Transect[rows]), rep(c(1:4), each = 10)))
      off_values <- postburn$Transect[weird_rows]

      cat("Error: Not all postburn transect values match the expected pattern.\n")
      cat("Offending values: ", paste(off_values, collapse = ", "), "in sample event", paste(postburndates[, i]), "\n")
      flags<- c(flags, paste("Error: Not all postburn transect values match the expected pattern", "Offending values: ", paste(off_values, collapse = ", "), "in sample event", paste(postburndates[, i]), "\n"), sep = " ")
      no_errors_t <- c(no_errors_t, 1)
    } else {
      no_errors_t <- c(no_errors_t, 0)
    }

    # Check for errors in postburn sample locations
    if (length(setdiff(na.omit(postburn$SampLoc[rows]), rep(c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45), each = 4))) != 0) {
      weird_rows <- which(postburn$SampLoc[rows] == setdiff(na.omit(postburn$SampLoc[rows]), rep(c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45), each = 4)))
      off_values <- postburn$SampLoc[weird_rows]

      cat("Error: Not all postburn sample locations match the expected pattern.\n")
      cat("Offending values: ", paste(off_values, collapse = ", "), "in sample event", paste(postburndates[, i]), "\n")
      no_errors_s <- c(no_errors_s, 1)
      flags<- c(flags, paste("Error: Not all postburn Sample loc values match the expected pattern", "Offending values: ", paste(off_values, collapse = ", "), "in sample event", paste(postburndates[, i]), "\n"), sep = " ")
    } else {
      no_errors_s <- c(no_errors_s, 0)
    }
  }

  if (unique(no_errors_s) == 0) {
    cat("Validation: postburn sample locations are correct.\n")
    cat("\n")
  }

  if (unique(no_errors_t) == 0) {
    cat("Validation: postburn transect values are correct.\n")
    cat("\n")
  }


return(flags)
}


################################################################################
# Protocol: Trees - Individuals (metric) ####
########################################
## Tree CBH and Tree CBH Range
#### Description ----
#' Tree CBH and Tree CBH Range
#' @description
#' The tree_CBH_qc function performs quality control checks on the 'tree' dataset.
#' It ensures that dead trees have blank live crown base height values and flags any
#' discrepancies. For living trees, it checks for unreasonable live crown base height
#' values using the Rosner test and flags outliers. The function also verifies the
#' relationship between ladder base height (LaddBaseHt) and ladder max height (LaddMaxHt),
#' ensuring the former is always less than the latter. It checks that entries with
#' LaddBaseHt values have the correct species symbol ('CANOPY'), status ('L'), SubFrac
#'  (1000), and TagNo (0), flagging any inconsistencies and providing detailed information
#'   about problematic rows. Note that the use of canopy data in the tree dataset is
#'   specific to the Saguaro Monitoring program.
#'
#' @param tree
#'
#' @return A list of flags or data issues with CBH in the tree csv
#' @export
#'
#' @examples
#' tree_CBH_qc(tree)

#### Function ----
tree_CBH_qc=function(tree){
  #check that dead trees do not have live crown base height values

  cat("All dead trees have a blank live crown base height...\n")
  if(length(which(!is.na(tree[which(tree$Status=="D"), "LiCrBHt"])))!=0){
    cat("FALSE, problem events listed in flags\n")
    cat("\n")
    flags<-c(flags, paste("Dead trees", tree[which(!is.na(tree[which(tree$Status=="D"), "LiCrBHt"])), "TagNo"], "have a non blank live crown base height in sample events", tree[which(!is.na(tree[which(tree$Status=="D"), "LiCrBHt"])), "MacroPlot.Name"],tree[which(!is.na(tree[which(tree$Status=="D"), "LiCrBHt"])), "Monitoring.Status"]))
  }else{
    cat("TRUE\n")
    cat("\n")
  }

  #check that living trees have reasonable live crown base height values
  if(length(unique(tree$LiCrBHt))>4){

    cat("Do living trees have any unreasonably high or low live crown base height values?\n")


    test<-suppressWarnings(rosnerTest(tree$LiCrBHt))
    test=test$all.stats
    outliers=test[which(test$Outlier==TRUE),4]

    if(length(outliers)==0){
      cat("No\n")
      cat("\n")
    }else{
      cat(paste(c("Yes, the outlier values according to a rosner test are", outliers, ". They are in rows", which(tree$LiCrBHt %in% outliers), "of the tree data table. The max, min, and mean of tree LiCrBHts are", max(na.omit(tree$LiCrBHt)), min(na.omit(tree$LiCrBHt)), mean(na.omit(tree$LiCrBHt)), "respectively\n"), collapse=" "))
      cat("\n")
      flags<-c(flags,paste(c("The tree LiCrBHt in the tree data set has outlier values according to a rosner test, which are", outliers, ". They are in rows", which(tree$LiCrBHt %in% outliers), "of the tree data table. The max, min, and mean of tree LiCrBHts are", max(na.omit(tree$LiCrBHt)), min(na.omit(tree$LiCrBHt)), mean(na.omit(tree$LiCrBHt)), "respectively"), collapse=" ") )
    }

  }else{
    cat(paste(c("Not enough samples to conduct a rosner test (<4) for the outlier values for live crown base height in the tree dataset. Values are ", na.omit(tree$LiCrBHt),"\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste(c("Not enough samples to conduct a rosner test (<4) for the outlier values for live crown base height in the tree dataset. Values are ", na.omit(tree$LiCrBHt)), collapse=" "))

  }
  #check that living trees have reasonable range (min to max) of live crown base height values and that the range of crown base height values is entered correctly

  cat("Are there values for live crown base height in this dataset?\n")
  if(length(which(!is.na(tree$LaddBaseHt)))!=0){
    cat(paste("Yes, there are ", length(which(!is.na(tree$LaddBaseHt)))), "\n")
    cat("\n")
    cbh=tree[which(!is.na(tree$LaddBaseHt)),]
    #check that laddbaseht values are less than laddmaxht
    cat("Are ladder base ht values all less than ladder max ht values?\n")
    if(length(unique(cbh$LaddBaseHt<cbh$LaddMaxHt))==1){
      if(unique(cbh$LaddBaseHt<cbh$LaddMaxHt)=="TRUE"){
        cat("TRUE\n")
        cat("\n")
      }else{
        cat("Ladder Max Ht values are always higher than ladder base ht values \n")
        cat("\n")
        flags<-c(flags,"Ladder Max Ht values are always higher than ladder base ht values" )
      }

    }else{
      cat(paste("FALSE, values for ladder max height are heigher than base height in these rows:", cbh[which(cbh$LaddBaseHt>cbh$LaddMaxHt),], "\n"))
      cat("\n")
      flags<-c(flags, paste("In tree data set values for ladder max height are heigher than base height in these rows:", cbh[which(cbh$LaddBaseHt>cbh$LaddMaxHt),]))
    }

    #check that species symbol =CANOPY for all entries with ladd base ht values

    cat("Species symbol is canopy for all entries with ladd base ht values?\n")
    if(length(unique(cbh$Species.Symbol=="CANOPY"))==1){
      if(unique(cbh$Species.Symbol=="CANOPY")=="TRUE"){
        cat("TRUE\n")
        cat("\n")
      }else{
        cat("FALSE, species symbol is not canopy for ANY entries with ladd base ht values\n")
        cat("\n")
        flags<-c(flags,"species symbol is not canopy for any entries with ladd base ht values in tree data set" )
      }

    }else{
      cat(paste("FALSE, species symbol is not canopy for entries with ladd base ht values in these rows:", cbh[which(cbh$Species.Symbol!="CANOPY"),], "\n"))
      cat("\n")
      flags<-c(flags, paste("In tree data set species symbol is not canopy for entries with ladd base ht values in these rows:", cbh[which(cbh$Species.Symbol!="CANOPY"),]))
    }


    #status is L for all entries with laddbase ht values
    cat("Status is L for all entries with laddbase ht values ?\n")
    if(length(unique(cbh$Status))==1){
      if(unique(cbh$Status=="L")=="TRUE"){
        cat("TRUE \n")
        cat("\n")
      }else{
        cat("FALSE, status is not L for ANY entries with laddbase ht values\n ")
        cat("\n")
        flags<-c(flags,"status is not L for ANY entries with laddbase ht values in tree dataset" )
      }

    }else{
      cat("FALSE, status is not L for some entries with laddbase ht values in events listed in flags")
      cat("\n")
      flags<-c(flags, paste("In tree data set status is", cbh[which(cbh$Status=="D"),"Status"], "not L for tag", cbh[which(cbh$Status=="D"),"TagNo"],  "with laddbase ht value", cbh[which(cbh$Status=="D"),"LaddBaseHt"], "in these sample events:", cbh[which(cbh$Status=="D"),"MacroPlot.Name"], cbh[which(cbh$Status=="D"),"Monitoring.Status"]))
    }


    #subfrac is 1000 for all entries with laddbase ht values
    cat("SubFrac is 1000 for all entries with laddbase ht values ?\n")
    if(length(unique(cbh$SubFrac==1000))==1){
      if(unique(cbh$SubFrac==1000)=="TRUE"){
        cat("TRUE\n")
        cat("\n")
      }else{
        cat("FALSE, SubFrac is not 1000 for ANY entries with laddbase ht values\n ")
        cat("\n")
        flags<-c(flags,"SubFrac is not 1000 for ANY entries with laddbase ht values in tree dataset" )
      }

    }else{
      cat(paste("FALSE, SubFrac is not 1000 for entries with laddbase ht values in these rows:", cbh[which(cbh$SubFrac!=1000),], "\n"))
      cat("\n")
      flags<-c(flags, paste("In tree data set SubFrac is not 1000 for entries with laddbase ht values in these rows:", cbh[which(cbh$SubFrac!=1000),]))
    }



    #Tag is 0 for all entries with laddbase ht values
    cat("Tag is 0 for all entries with laddbase ht values ?\n")
    if(length(unique(cbh$TagNo==0))==1){
      if(unique(cbh$TagNo==0)=="TRUE"){
        cat("TRUE\n")
        cat("\n")
      }else{
        cat("FALSE, Tag is not 0 for ANY entries with laddbase ht values\n ")
        flags<-c(flags,"Tag is not 0 for ANY entries with laddbase ht values in tree dataset" )
      }

    }else{
      cat(paste("FALSE, Tag is not 0 for entries with laddbase ht values in these rows:", cbh[which(cbh$TagNo!=0),], "\n"))
      cat("\n")
      flags<-c(flags, paste("In tree data set Tag is not 0 for entries with laddbase ht values in these rows:", cbh[which(cbh$TagNo!=0),]))
    }

  }else{
    cat("No\n")
    cat("\n")
  }
  return(flags)
} #end function #verified 9/12 by Eva

########################################
## Tree Crown Class ####
#### Description ----
#' @description
#' The tree_crown_class_qc function classifies trees based on their diameter at
#' breast height (DBH) and validates the correctness of crown class values for
#' different tree statuses. It ensures that for trees with a blank or dead status
#' and blank DBH, the crown class is either "BBD," "CUS," "DD," or blank. In the
#' case of live trees with blank DBH, it verifies that the crown class is blank.
#' For overstory trees with a dead status, the function checks for acceptable crown
#' class values such as "BAD," "CS," "LBS," "RS," or blank. Similarly, for live
#' overstory trees, it ensures crown class values are within categories
#' "C," "D," "I," "O," "SC," or blank. Additionally, for pole trees with a dead status,
#' the function checks for valid crown class values like "BBD," "CUS," "DD," "X," or blank.
#' The function flags instances where crown class values deviate from these specified
#' categories, providing quality control for tree data.
#'
#' @param tree
#'
#' @return A list of flags or data issues with crown class in the tree csv
#' @export
#'
#' @examples
#' tree_crown_class_qc(tree)

#### Function ----
tree_crown_class_qc=function(tree){
  #check for invalid values entered for tree crown class


  #classify dbh
  pole=tree[which(tree$DBH<15.1),]
  pole$treerow=which(tree$DBH<15.1)
  overstory=tree[which(tree$DBH>=15.1),]
  overstory$treerow=which(tree$DBH>=15.1)
  blank=tree[which(tree$DBH=="" | is.na(tree$DBH)),]
  blank$treerow=which(tree$DBH=="" | is.na(tree$DBH))


  #checking that dbh blank status D trees have crown class bbd, cus, dd, or blank
  Dead_CC=setdiff(blank[which(blank$Status=="D"),"CrwnCl"], c("BBD", "CUS", "DD", ""))

  cat("Dbh blank status D trees have crown class bbd, cus, dd, or blank...\n")
  if(length(Dead_CC)==0){
    cat("TRUE\n")
    cat("\n")
  }else{
    blank_D=blank[which(blank$Status=="D"),]
    cat(paste(c("FALSE, values of", Dead_CC, "recorded for crown class, sample events in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Tag number ", blank_D[which(blank_D$CrwnCl%in% Dead_CC),"TagNo"],
                          " in tree data set has", unique(Dead_CC),
                          "recorded for crown class in sample events",
                          blank_D[which(blank_D$CrwnCl%in% Dead_CC),"MacroPlot.Name"],
                          blank_D[which(blank_D$CrwnCl%in% Dead_CC),"Monitoring.Status"],
                          "which is not included in the acceptable list of BBD, CUS, DD, or blank for trees that have no dbh"))
  }

  #checking that dbh blank status L trees have crown class blank
  Live_B=setdiff(blank[which(blank$Status=="L"),"CrwnCl"], c(""))
  cat("Dbh blank status L trees have crown class blank\n")
  if(length(Live_B)==0){
    cat("TRUE\n")
    cat("\n")
  }else{
    blank_L=blank[which(blank$Status=="L"),]
    cat(paste(c("FALSE, values of", Live_B, "recorded for crown class, sample events recorded in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Tag number ", blank_L[which(blank_L$CrwnCl%in%Live_B), "TagNo"],
                          " in tree data set has", unique(Live_B), "recorded for crown class in sample events",
                          blank_L[which(blank_L$CrwnCl%in%Live_B), "MacroPlot.Name"],
                          blank_L[which(blank_L$CrwnCl%in%Live_B), "Monitoring.Status"],
                          "which is not blank, it may mean that a live tree is missing a dbh"))
  }

  #checking that dbh overstory status D trees have crown class BAD, CS, LBS, RS, or blank

  Dead_O=setdiff(overstory[which(overstory$Status=="D"),"CrwnCl"], c("BAD", "CS", "LBS", "RS", ""))
  cat("Dbh overstory status D trees have crown class BAD, CS, LBS, RS, or blank\n")
  if(length(Dead_O)==0){
    cat("TRUE\n")
    cat("\n")
  }else{
    overstory_D=overstory[which(overstory$Status=="D"),]
    cat(paste(c("FALSE, values of", Dead_O, "recorded for crown class, sample events recorded in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Tag number ", overstory_D[which(overstory_D$CrwnCl %in% Dead_O),"TagNo"],
                          " in tree data set has ", unique(Dead_O),
                          "recorded for crown class in sample events",
                          overstory_D[which(overstory_D$CrwnCl %in% Dead_O),"MacroPlot.Name"],
                          overstory_D[which(overstory_D$CrwnCl %in% Dead_O),"Monitoring.Status"],
                          "which is not included in the acceptable list of BAD, CS, LBS, RS, or blank for overstory dead trees"))
  }


  #checking that dbh overstory status L trees have crown class C, D, I, O, SC, or blank
  Live_O=setdiff(overstory[which(overstory$Status=="L"),"CrwnCl"], c("C", "D", "I", "O", "SC", ""))
  cat("Dbh overstory status L trees have crown class C, D, I, O, SC, or blank\n")
  if(length(Live_O)==0){
    cat("TRUE\n")
    cat("\n")
  }else{
    overstory_L=overstory[which(overstory$Status=="L"),]
    cat(paste(c("FALSE, values of", Live_O, "recorded for crown class in events listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Tag number ", overstory_L[which(overstory_L$CrwnCl %in% Live_O), "TagNo"],
                          " in tree data set has", unique(Live_O), "recorded for crown class in events",
                          overstory_L[which(overstory_L$CrwnCl %in% Live_O), "MacroPlot.Name"],
                          overstory_L[which(overstory_L$CrwnCl %in% Live_O), "Monitoring.Status"],
                          "which is not included in the acceptable list of C, D, I, O, SC, or blank for overstory live trees"))
  }

  #checking pole trees have crown class X, or blank
  wrong_cc=setdiff(pole[,"CrwnCl"], c("X",  ""))
  cat("Dbh pole trees have crown class X, or blank\n")
  if(length(wrong_cc)==0){
    cat("TRUE\n")
    cat("\n")
  }else{
    cat(paste(c("FALSE, values of", wrong_cc, "recorded for crown class in sample events listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Tag number ", pole[which(pole$CrwnCl %in% wrong_cc),"TagNo"],
                          " in tree data set has", unique(wrong_cc), "recorded for crown class in sample events",
                          pole[which(pole$CrwnCl %in% wrong_cc),"MacroPlot.Name"],
                          pole[which(pole$CrwnCl %in% wrong_cc),"Monitoring.Status"],
                          "which is not included in the acceptable list of X, or blank for pole trees"))

  }
  return(flags)
} #end function #verified 9/12 by Eva (lots of flags)

########################################
## Tree Damage ####
#### Description ----
#' @description
#' The `tree_damage_qc` function validates the accuracy of tree damage codes in a dataset.
#' It checks if the codes provided (DamCd1, DamCd2, DamCd3, DamCd4, DamCd5) are within the
#' predefined acceptable list of damage codes. The function returns "TRUE" if all codes
#' are correct and included in the acceptable list, and "FALSE" with details of any
#' discrepancies found. The acceptable list includes codes such as "ABGR," "BIRD," "BLIG,"
#' and others. If discrepancies are detected, the function adds corresponding entries
#' to the quality control flags, specifying the MacroPlot.Name and Monitoring.Status
#' for the rows where the issues occur.
#'
#' @param tree
#'
#' @return A list of flags or data issues with damage class in the tree csv
#' @export
#'
#' @examples
#' tree_damage_qc(tree)

#### Function ----
tree_damage_qc=function(tree){

  #check that tree damage codes are entered correctly
  cat("Tree damage codes are all entered correctly...\n")

  damcodes=c("ABGR", "BIRD", "BLIG", "BROK", "BROM", "BURL", "CONK", "CROK", "DTOP", "EPIC", "EPIP", "FIRE", "FORK", "FRST", "GALL", "HOLW", "INSE", "LEAN", "LICH", "LIGT", "MAMM", "MISL", "MOSS", "OZON", "ROOT", "ROTT", "SNAG", "SPAR", "SPRT", "TWIN", "UMAN", "WOND", "")

  if(length(setdiff(na.omit(unique(str_trim(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )))),damcodes))%in%0){
    cat("TRUE\n")
  }else{
    cat(paste("FALSE" , setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes), "included in events listed in flags\n"), sep=" ")
    cat("\n")
    cat("Acceptable list includes:", damcodes, "\n")
    flags<- c(flags, paste(setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes), "-included in row",
                           tree[which(tree[,"DamCd1"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes)|
                                        tree[,"DamCd2"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes) |
                                        tree[,"DamCd3"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes) |
                                        tree[,"DamCd4"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes) |
                                        tree[,"DamCd5"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes)),
                                "MacroPlot.Name"],
                           tree[which(tree[,"DamCd1"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes)|
                                        tree[,"DamCd2"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes) |
                                        tree[,"DamCd3"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes) |
                                        tree[,"DamCd4"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes) |
                                        tree[,"DamCd5"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes)),
                                "Monitoring.Status"],
                           "of tree data set for damage code which is not included in acceptable list", sep=" "))
  }
  return(flags)

} #end function #verified 9/12 by Eva

########################################
## Tree DBH ####
#### Description ----
#' @description
#' The `tree_dbh_qc` function conducts quality control on the Diameter at Breast Height
#' (DBH) values in a tree dataset. It first checks if all DBH values are under 2.5;
#' if any exceed this limit, it flags the events with the specific DBH values,
#' MacroPlot.Name, and Monitoring.Status. Subsequently, the function performs a
#' check for outlier values using a rosner test on the DBH values. If outliers
#' are detected, it adds entries to the quality control flags with details on the
#' outlier values, the corresponding events (MacroPlot.Name and Monitoring.Status),
#' and statistics on the maximum, minimum, and mean of the DBH values in the dataset.
#'
#'
#' @param tree
#'
#' @return A list of flags or data issues with DBH in the tree csv
#' @export
#'
#' @examples
#' tree_dbh_qc(tree)

#### Function ----
tree_dbh_qc=function(tree){


  #checking for DBH under 2.5
  cat("All tree DBHs are over 2.5\n")

  if(any(unique(na.omit((tree$DBH)<2.5)))==TRUE){
    cat(paste(c("Not all DBH are over 2.5 in tree data - ", tree[which(tree$DBH<2.5), "DBH"], "recorded in event/s listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Not all DBH are over 2.5 in tree data- ", tree[which(tree$DBH<2.5), "DBH"], "recorded in event/s",
                          tree[which(tree$DBH<2.5), "MacroPlot.Name"],
                          tree[which(tree$DBH<2.5), "Monitoring.Status"], sep=" "))
  }else{
    cat("TRUE\n")
    cat("\n")
  }

  cat("Are there any outlier values in tree DBH?\n")


  test<-suppressWarnings(rosnerTest(tree$DBH))
  test=test$all.stats
  outliers=test[which(test$Outlier==TRUE),4]

  if(length(outliers)==0){
    cat("No\n")
    cat("\n")
  }else{
    cat(paste(c("Yes, the outlier values in the tree dbh according to a rosner test are", outliers, ". They are in sample events listed in flags of the tree data table. For reference, the max, min, and mean of tree DBH are", max(na.omit(tree$DBH)), min(na.omit(tree$DBH)), mean(na.omit(tree$DBH)), "respectively\n"), collapse=" "))
    cat("\n")
    flags<-c(flags,paste("The tree DBH in the tree data set has outlier values according to a rosner test, which are", outliers, ". They are in events",
                         tree[which(tree$DBH %in% outliers), "MacroPlot.Name"],
                         tree[which(tree$DBH %in% outliers), "Monitoring.Status"],
                         "of the duff data table. For reference, the max, min, and mean of tree DBH's are", max(na.omit(tree$DBH)), min(na.omit(tree$DBH)), mean(na.omit(tree$DBH)), "respectively", collapse=" ") )
  }
  return(flags)
} #end function #verified 9/12 by Eva

########################################
## Tree DD ####
#### Description ----
#' @description
#' The `tree_dd_qc` function focuses on quality control for fallen trees or trees that
#' are too short to be counted as individuals in a dataset. It identifies fallen trees
#' based on specific crown classes (BBD, CUS, DD) and creates a subset named `fallen_trees`.
#' The function then checks if values for Diameter at Breast Height (DBH), tree height
#' (Ht), and live crown base height (LiCrBHt) have been deleted for trees that are no
#' longer counted. For each parameter, it reports "TRUE" if the values have been deleted as
#' expected. If any values are still present, it flags the events with details on the
#' specific parameter values, crown class, and associated events (MacroPlot.Name and
#' Monitoring.Status). The quality control results are stored in the `flags` variable
#' and returned by the function.
#'
#' @param tree
#'
#' @return A list of flags or data issues with dead and down trees in the tree csv
#' @export
#'
#' @examples
#' tree_dd_qc(tree)

#### Function ----
tree_dd_qc=function(tree){

  fallen_trees=tree[which(tree$CrwnCl=="BBD" | tree$CrwnCl=="CUS" | tree$CrwnCl=="DD"),]

  fallen_trees$treerow=which(tree$CrwnCl=="BBD" | tree$CrwnCl=="CUS" | tree$CrwnCl=="DD")

  cat("Values for DBH have been deleted for trees that are no longer standing...\n")
  if(any(is.na(fallen_trees$DBH)==FALSE)){
    cat(paste(c("FALSE, values of", fallen_trees[which(!is.na(fallen_trees$DBH)),10], "are recorded for tree DBH that have crown class", unique(fallen_trees[which(!is.na(fallen_trees$DBH)),13]), ",problem events are listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Values of", fallen_trees[which(!is.na(fallen_trees$DBH)),10], "are recorded for tree DBH that have crown class", fallen_trees[which(!is.na(fallen_trees$DBH)),13], ",problem events are",
                          fallen_trees[which(!is.na(fallen_trees$DBH)),"MacroPlot.Name"] ,
                          fallen_trees[which(!is.na(fallen_trees$DBH)),"Monitoring.Status"] ),collapse=" ")
  }else{
    cat("TRUE\n")
    cat("\n")
  }



  cat("Values for height have been deleted for trees that are no longer standing...\n")
  if(any(is.na(fallen_trees$Ht)==FALSE)){
    cat(paste(c("FALSE, values of", fallen_trees[which(!is.na(fallen_trees$Ht)),"Ht"], "are recorded for tree Ht that have crown class", fallen_trees[which(!is.na(fallen_trees$Ht)),"CrwnCl"], ",problem events are", fallen_trees[which(!is.na(fallen_trees$Ht)),c("MacroPlot.Name", "Monitoring.Status")], "\n" ), collapse=" "))
    cat("\n")
    flags<-c(flags, paste(c("FALSE, values of", fallen_trees[which(!is.na(fallen_trees$Ht)),"Ht"], "are recorded for tree Ht that have crown class", fallen_trees[which(!is.na(fallen_trees$Ht)),"CrwnCl"], ",problem events are", fallen_trees[which(!is.na(fallen_trees$Ht)),c("MacroPlot.Name", "Monitoring.Status")] ), collapse=" "))
  }else{
    cat("TRUE\n")
    cat("\n")
  }



  cat("Values for crown base height have been deleted for trees that are no longer standing...\n")
  if(any(is.na(fallen_trees$LiCrBHt)==FALSE)){
    cat(paste(c("FALSE, values of", fallen_trees[which(!is.na(fallen_trees$LiCrBHt)),"LiCrBHt"], "are recorded for tree LiCrBHt that have crown class", fallen_trees[which(!is.na(fallen_trees$LiCrBHt)),"CrwnCl"], ",problem events are", fallen_trees[which(!is.na(fallen_trees$LiCrBHt)),c("MacroPlot.Name", "Monitoring.Status")], "\n" ), collapse=" "))
    cat("\n")
    flags<-c(flags, paste(c("FALSE, values of", fallen_trees[which(!is.na(fallen_trees$LiCrBHt)),"LiCrBHt"], "are recorded for tree LiCrBHt that have crown class", fallen_trees[which(!is.na(fallen_trees$LiCrBHt)),"CrwnCl"], ",problem events are", fallen_trees[which(!is.na(fallen_trees$LiCrBHt)),c("MacroPlot.Name", "Monitoring.Status")] ), collapse=" "))
  }else{
    cat("TRUE\n")
    cat("\n")
  }
  return(flags)
}#end function #verified 10/2 by Eva


########################################
## Tree Area Multiplier ####
#### Description ----
#' @description
#' The `tree_area_multiplier_qc` function conducts quality control checks on subplot
#' fractions for different tree categories based on Diameter at Breast Height (DBH).
#' It classifies trees into `pole` (DBH < 15.1), `overstory` (DBH ≥ 15.1), and `blank`
#' (missing or blank DBH). For overstory trees, the function ensures that all have a
#' subplot fraction (`SubFrac`) of 1. For pole trees, it verifies a subplot fraction of 0.5.
#' For trees with blank or missing DBH, the function checks for a subplot fraction of 1000
#' (specific to Saguaros monitoring program) or blank. The results are stored in the `flags`
#' variable, containing details about events where subplot fractions deviate from the
#' specified criteria, and are returned by the function.
#'
#' @param tree
#'
#' @return A list of flags or data issues with fraction in the tree csv
#' @export
#'
#' @examples
#' tree_area_multiplier_qc(tree)

#### Function ----
tree_area_multiplier_qc=function(tree){
  ##check that correct subplot fraction is entered for trees
  #classify dbh
  pole=tree[which(tree$DBH<15.1),]
  pole$treerow=which(tree$DBH<15.1)
  overstory=tree[which(tree$DBH>=15.1),]
  overstory$treerow=which(tree$DBH>=15.1)
  blank=tree[which(tree$DBH=="" | is.na(tree$DBH)),]
  blank$treerow=which(tree$DBH=="" | is.na(tree$DBH))
  cat("All pole trees have subplot fraction of 0.25 or 0.5\n")
  if(length(unique(pole$SubFrac)) %in% c(1,2)){
    if(unique(pole$SubFrac) %in% c(0.25,0.5)){
      cat("TRUE\n")
      cat("\n")
    }else{
      wrongsubfract=unique(pole[which(pole$SubFrac %in% setdiff(pole$SubFrac, c(0.25,0.5))), "SubFrac"])
      cat(paste("FALSE, subfrac values for pole trees include", wrongsubfract, "when it should be equal to 0.25 or 0.5. Problem events are:", pole[which(pole$SubFrac %in% wrongsubfract), "MacroPlot.Name"],
                pole[which(pole$SubFrac %in% wrongsubfract), "Monitoring.Status"],"tree",pole[which(pole$SubFrac %in% wrongsubfract), "TagNo"], "\n", collapse=" "))
      cat("\n")
      flags<-c(flags, paste("FALSE, subfrac values for pole trees include", wrongsubfract, "when it should be equal to 0.25 or 0.5. Problem events are:", pole[which(pole$SubFrac %in% wrongsubfract), "MacroPlot.Name"],
                            pole[which(pole$SubFrac %in% wrongsubfract), "Monitoring.Status"], "tree", pole[which(pole$SubFrac %in% wrongsubfract), "TagNo"]))
    }
  }else{
    #more than one result not just one
    wrongsubfract=unique(pole[which(pole$SubFrac %in% setdiff(pole$SubFrac, c(0.25,0.5))), "SubFrac"])
    cat(paste("FALSE, subfrac values for pole trees include", wrongsubfract, "when it should be equal to 0.25 or 0.5. Problem events are:", pole[which(pole$SubFrac %in% wrongsubfract), "MacroPlot.Name"],
              pole[which(pole$SubFrac %in% wrongsubfract), "Monitoring.Status"],"tree",pole[which(pole$SubFrac %in% wrongsubfract), "TagNo"], "\n", collapse=" "))
    cat("\n")
    flags<-c(flags, paste("FALSE, subfrac values for pole trees include", wrongsubfract, "when it should be equal to 0.25 or 0.5. Problem events are:", pole[which(pole$SubFrac %in% wrongsubfract), "MacroPlot.Name"],
                          pole[which(pole$SubFrac %in% wrongsubfract), "Monitoring.Status"], "tree", pole[which(pole$SubFrac %in% wrongsubfract), "TagNo"]))
  }

  cat("All blank dbh trees have subplot fraction of 1000 or blank\n")
  if(length(unique(na.omit(blank$SubFrac)))==1){
    if(unique(na.omit(blank$SubFrac))==1000){
      cat("TRUE\n")
      cat("\n")
    }else{
      wrongsubfract=unique(na.omit(blank$SubFrac))
      wrongsubfract=wrongsubfract[! wrongsubfract==1000]
      if(all(wrongsubfract %in% c(1,0.5,0.25))){
        #and
        if(all(blank[which(blank$SubFrac %in% wrongsubfract), "CrwnCl"] %in% c("DD", "CUS", "BBD"))){
          #all good
          cat("TRUE\n")
          cat("\n")
        }else{
          diffcc=setdiff(unique(blank[which(blank$SubFrac %in% wrongsubfract), "CrwnCl"]), c("DD", "CUS", "BBD"))
          if(all(blank[which(blank$SubFrac %in% wrongsubfract & blank$CrwnCl %in% diffcc), "Status"]=="X")){
            #all good
            cat("TRUE\n")
            cat("\n")
          }else{
            #error
            #df with blank dbh, subfrac thats not 1000, crown class thats not DD, CUS, BBD
            df=blank[which(blank$SubFrac %in% wrongsubfract & blank$CrwnCl %in% diffcc),]
            #of that df, trees which status are not x
            df=df[df$Status!="X", ]
            cat(paste("FALSE, subfrac values for blank dbh trees include", wrongsubfract, "when it should be equal to 1000.(Doesn't have CC DD, CUS, BBD, or status X) Problem events are:", df[, "MacroPlot.Name"],
                      df[, "Monitoring.Status"],"tree",df[, "TagNo"], "\n", collapse=" "))
            cat("\n")
            flags<-c(flags, paste("FALSE, subfrac values for blank trees include", wrongsubfract, "when it should be equal to 1000.(Doesn't have CC DD, CUS, BBD, or status X) Problem events are:", df[, "MacroPlot.Name"],
                                  df[, "Monitoring.Status"], "tree", df[, "TagNo"]))
          }
        }
      }else{
        #error
        #df with blank dbh, subfrac thats not 1000, crown class thats not DD, CUS, BBD
        df=blank[which(blank$SubFrac %in% wrongsubfract & blank$CrwnCl %in% diffcc),]
        #of that df, trees which status are not x
        df=df[df$Status!="X", ]
        cat(paste("FALSE, subfrac values for blank dbh trees include", wrongsubfract, "when it should be equal to 1000.(Doesn't have CC DD, CUS, BBD, or status X) Problem events are:", df[, "MacroPlot.Name"],
                  df[, "Monitoring.Status"],"tree",df[, "TagNo"], "\n", collapse=" "))
        cat("\n")
        flags<-c(flags, paste("FALSE, subfrac values for blank trees include", wrongsubfract, "when it should be equal to 1000.(Doesn't have CC DD, CUS, BBD, or status X) Problem events are:", df[, "MacroPlot.Name"],
                              df[, "Monitoring.Status"], "tree", df[, "TagNo"]))
        }
   }
  }else{
    wrongsubfract=unique(na.omit(blank$SubFrac))
    wrongsubfract=wrongsubfract[! wrongsubfract==1000]
    if(all(wrongsubfract %in% c(1,0.5,0.25))){
      #and
      if(all(blank[which(blank$SubFrac %in% wrongsubfract), "CrwnCl"] %in% c("DD", "CUS", "BBD"))){
        #all good
        cat("TRUE\n")
        cat("\n")
      }else{
        diffcc=setdiff(unique(blank[which(blank$SubFrac %in% wrongsubfract), "CrwnCl"]), c("DD", "CUS", "BBD"))
        if(all(blank[which(blank$SubFrac %in% wrongsubfract & blank$CrwnCl %in% diffcc), "Status"]=="X")){
          #all good
          cat("TRUE\n")
          cat("\n")
        }else{
          #error
          #df with blank dbh, subfrac thats not 1000, crown class thats not DD, CUS, BBD
          df=blank[which(blank$SubFrac %in% wrongsubfract & blank$CrwnCl %in% diffcc),]
          #of that df, trees which status are not x
          df=df[df$Status!="X", ]
          cat(paste("FALSE, subfrac values for blank dbh trees include", wrongsubfract, "when it should be equal to 1000.(Doesn't have CC DD, CUS, BBD, or status X) Problem events are:", df[, "MacroPlot.Name"],
                    df[, "Monitoring.Status"],"tree",df[, "TagNo"], "\n", collapse=" "))
          cat("\n")
          flags<-c(flags, paste("FALSE, subfrac values for blank trees include", wrongsubfract, "when it should be equal to 1000.(Doesn't have CC DD, CUS, BBD, or status X) Problem events are:", df[, "MacroPlot.Name"],
                                df[, "Monitoring.Status"], "tree", df[, "TagNo"]))
        }
      }
    }else{
      #error
      #df with blank dbh, subfrac thats not 1000, crown class thats not DD, CUS, BBD
      df=blank[which(blank$SubFrac %in% wrongsubfract & blank$CrwnCl %in% diffcc),]
      #of that df, trees which status are not x
      df=df[df$Status!="X", ]
      cat(paste("FALSE, subfrac values for blank dbh trees include", wrongsubfract, "when it should be equal to 1000.(Doesn't have CC DD, CUS, BBD, or status X) Problem events are:", df[, "MacroPlot.Name"],
                df[, "Monitoring.Status"],"tree",df[, "TagNo"], "\n", collapse=" "))
      cat("\n")
      flags<-c(flags, paste("FALSE, subfrac values for blank trees include", wrongsubfract, "when it should be equal to 1000.(Doesn't have CC DD, CUS, BBD, or status X) Problem events are:", df[, "MacroPlot.Name"],
                            df[, "Monitoring.Status"], "tree", df[, "TagNo"]))
      }
    }



  cat("All overstory trees have subplot fraction of 1\n")
  if(length(unique(overstory$SubFrac))==1){
    if(unique(overstory$SubFrac)==1){
      cat("TRUE\n")
      cat("\n")
    }else{
      wrongsubfract=unique(overstory[which(overstory$SubFrac!=1), "SubFrac"])
      cat(paste("FALSE, subfrac values for overstory trees include", wrongsubfract, "when it should be equal to 1. Problem events are:", overstory[which(overstory$SubFrac %in% wrongsubfract), "MacroPlot.Name"],
                overstory[which(overstory$SubFrac %in% wrongsubfract), "Monitoring.Status"],"tree",overstory[which(overstory$SubFrac %in% wrongsubfract), "TagNo"], "\n", collapse=" "))
      cat("\n")
      flags<-c(flags, paste("FALSE, subfrac values for overstory trees include", wrongsubfract, "when it should be equal to 1. Problem events are:", overstory[which(overstory$SubFrac %in% wrongsubfract), "MacroPlot.Name"],
                            overstory[which(overstory$SubFrac %in% wrongsubfract), "Monitoring.Status"], "tree", overstory[which(overstory$SubFrac %in% wrongsubfract), "TagNo"], "\n", collapse=" "))
    }
  }else{
    #more than one result not just one
    wrongsubfract=unique(overstory[which(overstory$SubFrac!=1), "SubFrac"])
    cat(paste("FALSE, subfrac values for overstory trees include", wrongsubfract, "when it should be equal to 1. Problem events are:", overstory[which(overstory$SubFrac %in% wrongsubfract), "MacroPlot.Name"],
              overstory[which(overstory$SubFrac %in% wrongsubfract), "Monitoring.Status"],"tree",overstory[which(overstory$SubFrac %in% wrongsubfract), "TagNo"], "\n", collapse=" "))
    cat("\n")
    flags<-c(flags, paste("FALSE, subfrac values for overstory trees include", wrongsubfract, "when it should be equal to 1. Problem events are:", overstory[which(overstory$SubFrac %in% wrongsubfract), "MacroPlot.Name"],
                          overstory[which(overstory$SubFrac %in% wrongsubfract), "Monitoring.Status"], "tree", overstory[which(overstory$SubFrac %in% wrongsubfract), "TagNo"], "\n", collapse=" "))
  }

  return(flags)
}#end function #verified 10/2 by Eva - lots of flags however

########################################
## Tree Sample Area ####
#### Description ----
#' @description
#' The `tree_sample_area_qc` function performs quality control checks on the sample area
#' information of the tree data. It first ensures that all macroplot sizes (`MacroPlotSize`)
#' are 0.1, with detailed information about problematic events stored in the `flags` variable.
#' Similarly, it verifies that all snagplot sizes (`SnagPlotSize`) are 0.1. The function also
#' checks that break point diameters (`BrkPntDia`) are 15.1. For each of these checks, it
#' provides a logical value and details about any non-compliance events stored in the `flags`
#' variable, which is returned by the function.
#'
#' @param tree
#'
#' @return A list of flags or data issues with header in the tree csv
#' @export
#'
#' @examples
#' tree_sample_area_qc(tree)

#### Function ----
tree_sample_area_qc=function(tree){

  #check for incorrect sampling information
  #need to filter for only plot level information - all else will be blank
  #take out rows with tag numbers - these include tree information
  tree=tree[which(is.na(tree$TagNo)),]

  cat("All macroplot sizes are 0.1 or blank...\n")
  if(length(unique(tree$MacroPlotSize))==1){
    if(unique(unique(tree$MacroPlotSize) %in% c(0.1))){
      cat("TRUE\n")
      cat("\n")
    }else{
      #something wrong
      cat(paste(c("FALSE, macroplot size values for trees include", unique(tree$MacroPlotSize), "when it should only be 0.1, problem events listed in flags\n"), collapse=" "))
      cat("\n")
      flags<-c(flags, paste("MacroPlotSize values for all trees in tree data set is", unique(tree$MacroPlotSize), "when it should only be 0.1, problem events:", tree[which(tree$MacroPlotSize==setdiff(unique(tree$MacroPlotSize), c(0.1))), "MacroPlot.Name"],
                            tree[which(tree$MacroPlotSize==setdiff(unique(tree$MacroPlotSize), c(0.1))), "Monitoring.Status"]))
    }
  }else{
    #something wrong
    cat(paste(c("FALSE, macroplot size values for trees include", unique(tree$MacroPlotSize), "when it should only be 0.1, problem events listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("MacroPlotSize values for all trees in tree data set is", unique(tree$MacroPlotSize), "when it should only be 0.1, problem events:", tree[which(tree$MacroPlotSize==setdiff(unique(tree$MacroPlotSize), c(0.1))), "MacroPlot.Name"],
                          tree[which(tree$MacroPlotSize==setdiff(unique(tree$MacroPlotSize), c(0.1))), "Monitoring.Status"]))
  }


  cat("All snagplot sizes are 0.1...\n")
  if(length(unique(tree$SnagPlotSize))==1){
    if(unique(unique(tree$SnagPlotSize) %in% c(0.1))){
      cat("TRUE\n")
      cat("\n")
    }else{
      #something wrong
      cat(paste("FALSE, Snagplot size values for trees include", unique(tree$SnagPlotSize), "when it should only be 0.1, problem events listed in flags\n"), collapse=" ")
      cat("\n")
      flags<-c(flags, paste("SnagPlotSize values for all trees in tree data set is", unique(tree$SnagPlotSize), "when it should only be 0.1, problem events:", tree[which(tree$SnagPlotSize==setdiff(unique(tree$SnagPlotSize), c(0.1))), "MacroPlot.Name"],
                            tree[which(tree$SnagPlotSize==setdiff(unique(tree$SnagPlotSize), c(0.1))), "Monitoring.Status"]))
    }
  }else{
    #something wrong
    cat(paste("FALSE, Snagplot size values for trees include", unique(tree$SnagPlotSize), "when it should only be 0.1, problem events listed in flags\n"), collapse=" ")
    cat("\n")
    flags<-c(flags, paste("SnagPlotSize values for all trees in tree data set is", unique(tree$SnagPlotSize), "when it should only be 0.1, problem events:", tree[which(tree$SnagPlotSize==setdiff(unique(tree$SnagPlotSize), c(0.1))),"MacroPlot.Name"],
                          tree[which(tree$SnagPlotSize==setdiff(unique(tree$SnagPlotSize), c(0.1))), "Monitoring.Status"]))
  }



  cat("All Break point diameters are 15.1 or blank...\n")
  if(length(unique(tree$BrkPntDia))==1){
    if(unique(unique(tree$BrkPntDia) %in% c(15.1))){
      cat("TRUE\n")
      cat("\n")
    }else{
      #something wrong
      cat(paste(c("FALSE, Break point diameter values for trees include", unique(tree$BrkPntDia), "when it should only be 15.1, problem events listed in flags\n"), collapse=" "))
      cat("\n")
      flags<-c(flags, paste("Break Point Diameter values for all trees in tree data set is", unique(tree$BrkPntDia), "when it should only be 15.1, problem events:", tree[which(tree$BrkPntDia==setdiff(unique(tree$BrkPntDia), c(15.1))), "MacroPlot.Name"],
                            tree[which(tree$BrkPntDia==setdiff(unique(tree$BrkPntDia), c(15.1))), "Monitoring.Status"]))
    }
  }else{
    #something wrong
    cat(paste(c("FALSE, Break point diameter values for trees include", unique(tree$BrkPntDia), "when it should only be 15.1, problem events listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Break Point Diameter values for all trees in tree data set is", unique(tree$BrkPntDia), "when it should only be 15.1, problem events:", tree[which(tree$BrkPntDia==setdiff(unique(tree$BrkPntDia), c(15.1))), "MacroPlot.Name"],
                          tree[which(tree$BrkPntDia==setdiff(unique(tree$BrkPntDia), c(15.1))), "Monitoring.Status"]))
  }

  return(flags)
}#end function #verified 10/11 by Eva


########################################
## Tree Height ####
#### Description ----
#' @description
#'The `tree_height_qc` function categorizes trees based on diameter at breast height
#'(DBH) into pole, overstory, and blank trees. For overstory trees, it conducts a
#'Rosner test to identify height outliers, reporting details if four or more valid
#'samples are available. The function also checks pole tree data to ensure entered
#'height classes fall within the acceptable range of 1 to 10 or are blank, flagging
#'any deviations and providing event details in the `flags` variable returned by the function.
#' @param tree
#'
#' @return A list of flags or data issues with height in the tree csv
#' @export
#'
#' @examples
#' tree_height_qc(tree)

#### Function ----
tree_height_qc=function(tree){

  #classify dbh
  pole=tree[which(tree$DBH<15.1),]
  pole$treerow=which(tree$DBH<15.1)
  overstory=tree[which(tree$DBH>=15.1),]
  overstory$treerow=which(tree$DBH>=15.1)
  blank=tree[which(tree$DBH=="" | is.na(tree$DBH)),]
  blank$treerow=which(tree$DBH=="" | is.na(tree$DBH))


  cat("Are there any outlier values in overstory tree height?\n")

  if(length(na.omit(overstory$Ht))>4){
    test<-suppressWarnings(rosnerTest(na.omit(overstory$Ht)))
    test=test$all.stats
    outliers=test[which(test$Outlier==TRUE),4]

    if(length(outliers)==0){
      cat("No\n")
      cat("\n")
    }else{
      cat(paste(c("Yes, the outlier values according to a rosner test are", outliers, ". They are in events listed in flags. The max, min, and mean of overstory tree height are", max(na.omit(overstory$Ht)), min(na.omit(overstory$Ht)), mean(na.omit(overstory$Ht)), "respectively","\n"), collapse=" "))
      cat("\n")
      flags<-c(flags,paste("The overstory tree height in the overstory data set has outlier values according to a rosner test, which are", outliers, ". They are in events", overstory[which(overstory$Ht %in% outliers), "MacroPlot.Name"],
                           overstory[which(overstory$Ht %in% outliers), "Monitoring.Status"], "of the overstory data table.
                           The max, min, and mean of overstory tree height's are", max(na.omit(overstory$Ht)), min(na.omit(overstory$Ht)), mean(na.omit(overstory$Ht)), "respectively", collapse=" ") )
    }
  }else{
    cat(paste(c("Not enough samples to conduct a rosner test (<4) for the outlier values for overstory ht in the tree dataset. Values are ", na.omit(overstory$Ht),"\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste(c("Not enough samples to conduct a rosner test (<4) for the outlier values for overstory ht in the tree dataset. Values are ", na.omit(overstory$Ht)), collapse=" "))
  }


  ###check that pole dbh ht is 1-10 or blank

  cat("Correct height classes entered for pole tree data...\n")
  htcl=setdiff(unique(na.omit(pole$Ht)), c(0:10, NA))

  if(length(htcl)==0){
    cat("TRUE\n")
    cat("\n")
  }else{
    cat(paste(c("FALSE", htcl, "values entered which do not fit with acceptable values of 0-10 or NA, problem events listed in flags\n"), sep=" "), collapse=" ")
    cat("\n")
    flags<-c(flags,paste(htcl, "values entered in height for pole tree data which do not fit with acceptable values of 0-10 or NA, problem rows are", pole[which(pole$Ht %in% htcl),"MacroPlot.Name"],
                         pole[which(pole$Ht %in% htcl),"Monitoring.Status"], sep=" ", collapse=" ") )
  }
  return(flags)
}#end function #verified 10/11 by Eva


########################################
## Tree Severity ####
#### Description ----
#' @description
#' The `tree_severity_qc` function checks if all char heights, scorch heights,
#' and scorch percentage heights are blank (NA) for trees measured before fires, flagging any
#' discrepancies and providing event details in the `flags` variable returned by the function. The function
#' also checks char, scorch and scorch percentage data for the sample events immediately post burn or 1 year after the fire. The user
#' has the option to filter out pole trees for char data checks (some protocols record this data
#' for pole trees and some do not) with the argument filterpoles="Y" or filterpoles="N" (default).
#' The function checks that char height and scorch height are not blank or excessively high for
#' standing trees post burn, that scorch percentage is not over 100 or blank for live trees, and that trees
#' with crown class DD, BBD, or CUS have blank char and scorch heights and scorch percentages of
#' blank or 100. Errors are flagged with details about the problem sample event.
#'
#' @param tree
#'
#' @return A list of flags or data issues with severity in the tree csv
#' @export
#'
#' @examples
#' tree_severity_qc(tree, filterpoles="Y")
#### Function ----
tree_severity_qc=function(tree, filterpoles="N"){

  pretree=tree[which(tree$Monitoring.Status %in% c("00PR01","00PR02","01Pre")),]
  #need to add everything thats not immediate post *Post(nothing after)
  #note if monitoring status format is different change code



  cat("All char heights are blank for pre burn trees...")
  if(length(unique(pretree$CharHt))==1){
    if(is.na(unique(pretree$CharHt))){
      cat("TRUE\n")
      cat("\n")
    }else{
      #something wrong
      cat(paste(c("FALSE, Char height values for pre burn trees in tree data set include", unique(pretree$CharHt), "when it should only be NA, problem events listed in flags\n"), collapse=" "))
      cat("\n")
      flags<-c(flags, paste(c("Char height values for pre burn trees in tree data set include", unique(pretree$CharHt), "when it should only be NA, problem events:",
                              pretree[which(pretree$CharHt==setdiff(unique(pretree$CharHt), NA)),"MacroPlot.Name"],
                              pretree[which(pretree$CharHt==setdiff(unique(pretree$CharHt), NA)),"Monitoring.Status"],
                              "\n"), collapse=" "))
    }
  }else{
    #something wrong
    cat(paste(c("FALSE, Char height values for pre burn trees in tree data set include", unique(pretree$CharHt), "when it should only be NA, problem events listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Char height values for pre burn trees in tree data set include", unique(pretree$CharHt), "when it should only be NA, problem events:",
                          pretree[which(pretree$CharHt==setdiff(unique(pretree$CharHt), NA)),"MacroPlot.Name"],
                          pretree[which(pretree$CharHt==setdiff(unique(pretree$CharHt), NA)),"Monitoring.Status"], "\n"), collapse=" ")
  }


  cat("All Scorch heights are blank for pre burn trees...")
  if(length(unique(pretree$ScorchHt))==1){
    if(is.na(unique(pretree$ScorchHt))){
      cat("TRUE\n")
      cat("\n")
    }else{
      #something wrong
      cat(paste(c("FALSE, Scorch height values for pre burn trees in tree data set include", unique(pretree$ScorchHt), "when it should only be NA, problem events listed in flags\n"), collapse=" "))
      cat("\n")
      flags<-c(flags, paste("Scorch height values for pre burn trees in tree data set include", unique(pretree$ScorchHt), "when it should only be NA, problem rows:",
                            pretree[which(pretree$ScorchHt==setdiff(unique(pretree$ScorchHt), NA)),"MacroPlot.Name"],
                            pretree[which(pretree$ScorchHt==setdiff(unique(pretree$ScorchHt), NA)),"Monitoring.Status"],"\n"), collapse=" ")
    }
  }else{
    #something wrong
    cat(paste(c("FALSE, Scorch height values for pre burn trees in tree data set include", unique(pretree$ScorchHt), "when it should only be NA, problem events listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Scorch height values for pre burn trees in tree data set include", unique(pretree$ScorchHt), "when it should only be NA, problem rows:",
                          pretree[which(pretree$ScorchHt==setdiff(unique(pretree$ScorchHt), NA)),"MacroPlot.Name"],
                          pretree[which(pretree$ScorchHt==setdiff(unique(pretree$ScorchHt), NA)),"Monitoring.Status"], "\n"), collapse=" ")
  }


  cat("All Scorch percentage heights are blank for pre burn trees...")
  if(length(unique(pretree$CrScPct))==1){
    if(is.na(unique(pretree$CrScPct))){
      cat("TRUE\n")
      cat("\n")
    }else{
      #something wrong
      cat(paste(c("FALSE, Scorch percentage height values for pre burn trees in tree data set include", unique(pretree$CrScPct), "when it should only be NA, problem rows listed in flags\n"), collapse=" "))
      cat("\n")
      flags<-c(flags, paste("Scorch percentage height values for pre burn trees in tree data set include", unique(pretree$CrScPct), "when it should only be NA, problem rows:",
                            pretree[which(pretree$CrScPct==setdiff(unique(pretree$CrScPct), NA)),"MacroPlot.Name"], "\n",
                            pretree[which(pretree$CrScPct==setdiff(unique(pretree$CrScPct), NA)),"Monitoring.Status"], "\n"),
               collapse=" ")
    }
  }else{
    #something wrong
    cat(paste(c("FALSE, Scorch percentage height values for pre burn trees in tree data set include", unique(pretree$CrScPct), "when it should only be NA, problem rows listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste(c("Scorch percentage height values for pre burn trees in tree data set include", unique(pretree$CrScPct), "when it should only be NA, problem events:",
                            pretree[which(pretree$CrScPct==setdiff(unique(pretree$CrScPct), NA)),"MacroPlot.Name"],
                            pretree[which(pretree$CrScPct==setdiff(unique(pretree$CrScPct), NA)),"Monitoring.Status"], "\n"), collapse=" "))
  }

  cat("\n")
  test_for_outliers <- function(data, column_name) {
    # Display a message indicating the test being performed
    cat("Are there any outlier values in", column_name, "?\n")

    # Extract non-missing and non-NA/NaN/Inf values from the specified column
    column_values <- na.omit(data[[column_name]])

    # Check if there are any valid values for testing
    if (length(column_values) == 0) {
      # Print a message if no valid values are available for testing
      cat("No valid values to test\n")
      return()  # Exit the function if there are no valid values
    }

    # Perform Rosner test on the valid column values
    test <- rosnerTest(column_values)
    test <- test$all.stats
    outliers <- test[which(test$Outlier == TRUE), 4]

    # Check if any outliers were detected
    if (length(outliers) == 0) {
      # Print a message if no outliers were detected
      cat("No\n")
    } else {
      # Print information about the detected outliers and their details
      cat(paste("Yes, the outlier values according to a Rosner test are", outliers, ". They are in events",
                data[which(data[[column_name]] %in% outliers),"MacroPlot.Name"],data[which(data[[column_name]] %in% outliers),"Monitoring.Status"], "of the data table. For reference, the max, min, and mean of",
                column_name, "are", max(column_values), min(column_values), mean(column_values), "respectively",
                collapse = "\n"), "\n")

      # Add error messages about the detected outliers to the flags vector
      flags<- c(flags, paste("The", column_name, "has outlier values according to a Rosner test, which are", outliers,
                             ". They are in events", data[which(data[[column_name]] %in% outliers),"MacroPlot.Name"],data[which(data[[column_name]] %in% outliers),"Monitoring.Status"],
                             "of the data table. The max, min, and mean of", column_name, "'s are", max(column_values),
                             min(column_values), mean(column_values), "respectively", collapse = "\n"), "\n")
    }
  }


#filter for just immediate post reads
  pattern <- "(0[1-9]|10)Post"
  postburntrees = tree[grep(pattern, tree$Monitoring.Status),]

  #filter out pole trees
  if(filterpoles=="Y"){
    cat("Filtering out pole trees for char data checks\n")
    postburntrees = postburntrees[which(postburntrees$DBH>15.1),]
  }else if(filterpoles=="N"){
    cat("Including pole trees for char data checks (to exclude add argument filterpoles=Y to function.)\n")
  }else{
    cat("Unknown argument for filterpoles, defaulting to N (not filtering out poles for char data checks)")
    #error in argument
  }




  test_for_outliers(postburntrees, "CharHt")
  `%!in%` <- Negate(`%in%`)
  cat("All trees in post read have a char height value\n")
  blank_char=postburntrees[which(postburntrees$CharHt=="" | is.na(postburntrees$CharHt| postburntrees$TagNo!="NA")),]
  if(any(!is.na(blank_char$TagNo) &
         blank_char$CrwnCl %!in% c("DD", "BBD", "CUS"))){
    cat("FALSE, problem sample events listed in flags\n")
    x=which(!is.na(blank_char$TagNo) | blank_char$TagNo!=""| blank_char$TagNo!="NA" &
              blank_char$CrwnCl %!in% c("DD", "BBD", "CUS"))
    flags <- c(flags, paste("Tree", blank_char[x, "TagNo"], " has a blank char height and tag number is not blank and crown class is not DD, BBD, or CUS. Sample event is", blank_char[x, "MacroPlot.Name"],
                            blank_char[x, "Monitoring.Status"]))


  }else{
    cat("TRUE\n")
  }

test_for_outliers(postburntrees, "ScorchHt")
cat("All trees in post read have a scorch height value\n")
blank_scor=postburntrees[which(postburntrees$ScorchHt=="" | is.na(postburntrees$ScorchHt) | postburntrees$ScorchHt=="NA"),]
if(any(!is.na(blank_scor$TagNo) | blank_scor$TagNo!="" | blank_scor$TagNo!="NA")){
  blank_scor_tag=blank_scor[which(!is.na(blank_scor$TagNo) | blank_scor$TagNo!="" | blank_scor$TagNo!="NA"),]
  if(any(blank_scor_tag$Status!="D")){
    cat("FALSE, problem sample events listed in flags\n")
    x=which(!is.na(blank_scor$TagNo) | blank_scor$TagNo!=""| blank_scor$TagNo!="NA")
    y=which(blank_scor$Status!="D")
    x=intersect(x,y)
    flags <- c(flags, paste("Tree", blank_scor[x, "TagNo"], " has a blank scorch height and tag number is not blank and status is not dead. Sample event is", blank_scor[x, "MacroPlot.Name"],
                            blank_scor[x, "Monitoring.Status"]))

  }else{
    cat("TRUE\n")
    #good
  }



}else{
  cat("TRUE\n")
}

cat("All trees in post read have a scorch percentage equal to or under 100%\n")
if(all(na.omit(postburntrees$CrScPct)<=100)){
  cat("TRUE\n")

}else{
  cat("FALSE, problem sample events listed in flags\n")
  x=which(na.omit(postburntrees$CrScPct)>100)
  flags <- c(flags, paste("Tree", postburntrees[x, "TagNo"], " has a a scorch percentage over 100%. Sample event is", postburntrees[x, "MacroPlot.Name"],
                          postburntrees[x, "Monitoring.Status"]))

}

cat("All live trees have a scorch percentage value\n")
blank_scor_p=postburntrees[which(postburntrees$CrScPct=="" | is.na(postburntrees$CrScPct) | postburntrees$CrScPct=="NA"),]
blank_scor_p=blank_scor_p[which(blank_scor_p$Status=="L"),]
blank_scor_p=blank_scor_p[which(!is.na(blank_scor_p$TagNo) | blank_scor_p$TagNo!="" | blank_scor_p$TagNo!="NA"),]
if(nrow(blank_scor_p)>1){
  cat("FALSE, problem sample events listed in flags\n")
  flags <- c(flags, paste("Tree", blank_scor_p[, "TagNo"], "is a live tree with a blank scorch percentage. Sample event is", blank_scor_p[, "MacroPlot.Name"],
                          blank_scor_p[, "Monitoring.Status"]))

}else{
  cat("TRUE\n")
}


cat("All rows with crown class DD, BBD, or CUS have a blank char and scorch height and a scorch percentage of blank or 100\n")
postburntrees_cc=postburntrees[(which(postburntrees$CrwnCl %in% c("DD", "BBD", "CUS"))),]
if(nrow(postburntrees_cc)>1){
  postburntrees_cc_c=postburntrees_cc[which(!is.na(postburntrees_cc$CharHt) | postburntrees_cc$CharHt!="" | postburntrees_cc$CharHt!="NA"),]
  flags <- c(flags, paste("Tree", postburntrees_cc_c[, "TagNo"], "has crown class", postburntrees_cc_c[, "CrwnCl"], "and a non blank char height of", postburntrees_cc_c[, "CharHt"],". Sample event is", postburntrees_cc_c[, "MacroPlot.Name"], postburntrees_cc_c[, "Monitoring.Status"]))
  postburntrees_cc_s=postburntrees_cc[which(!is.na(postburntrees_cc$ScorchHt) | postburntrees_cc$ScorchHt!="" | postburntrees_cc$ScorchHt!="NA"),]
  flags <- c(flags, paste("Tree", postburntrees_cc_s[, "TagNo"], "has crown class", postburntrees_cc_s[, "CrwnCl"], "and a non blank Scorch height of", postburntrees_cc_s[, "ScorchHt"],". Sample event is", postburntrees_cc_s[, "MacroPlot.Name"], postburntrees_cc_s[, "Monitoring.Status"]))
  postburntrees_cc_p=postburntrees_cc[which(!is.na(postburntrees_cc$CrScPct) | postburntrees_cc$CrScPct!="" | postburntrees_cc$CrScPct!="NA" | postburntrees_cc$CrScPct==100),]
  flags <- c(flags, paste("Tree", postburntrees_cc_p[, "TagNo"], "has crown class", postburntrees_cc_p[, "CrwnCl"], "and a non blank or 100 Scorch percentage of", postburntrees_cc_p[, "CrScPct"],". Sample event is", postburntrees_cc_p[, "MacroPlot.Name"], postburntrees_cc_p[, "Monitoring.Status"]))


}else{
  cat("TRUE\n")
}




  return(flags)
}#end function #verified 10/11 by Eva

########################################
## Tree Status and Life Form ####
#### Description ----
#' @description
#' The tree_status_lifeform_qc function assesses the quality of data related to tree status
#' and life form in the provided tree dataset. It ensures that tree statuses are either
#' "L" or "D" and that all preferred life forms are "Tree." The function reports any deviations
#' from these criteria, flagging problematic events and providing details such as macroplot names,
#' monitoring statuses, tag numbers, and comments.
#' @param tree
#'
#' @return A list of flags or data issues with tree status and life form in the tree csv
#' @export
#'
#' @examples
#' tree_status_lifeform_qc(tree)

#### Function ----
tree_status_lifeform_qc=function(tree){
  ###check that tree status is L or D

  cat("Tree statuses are all L or D...")
  htcl=setdiff(unique(na.omit(tree$Status)), c("L", "D", ""))

  if(length(htcl)==0){
    cat("TRUE\n")
    cat("\n")
  }else{
    cat(paste(c("FALSE", htcl, "values entered which do not fit with acceptable values of L or D, problem events are in flags\n"), sep=" "), collapse=" ")
    cat("\n")
    flags<-c(flags,paste(htcl, "values entered for tree status which do not fit with acceptable values of L or D, problem events are",
                         tree[which(tree$Status %in% htcl),"MacroPlot.Name"],
                         tree[which(tree$Status %in% htcl), "Monitoring.Status"],
                         "tree tag is", tree[which(tree$Status %in% htcl), "TagNo"],
                         "comments say", tree[which(tree$Status %in% htcl), "Comment"],
                         sep=" "))
  }




  cat("All preferred lifeforms are tree for trees...")
  if(length(unique(tree$Preferred_Lifeform))==1){
    if(unique(tree$Preferred_Lifeform)=="Tree"){
      cat("TRUE")
      cat("\n")
    }else{
      #something wrong
      cat(paste(c("FALSE, preferred lifeform values for trees in tree data set include:", unique(tree$Preferred_Lifeform), ", when it should only be tree, problem events listed in flags\n"), collapse=" "))
      cat("\n")
      flags<-c(flags, paste("preferred lifeform values for  trees in tree data set include:", unique(tree$Preferred_Lifeform), ", when it should only be tree, problem events:",
                            tree[which(tree$Preferred_Lifeform %in% setdiff(unique(tree$Preferred_Lifeform), "Tree")),"MacroPlot.Name"],
                            tree[which(tree$Preferred_Lifeform %in% setdiff(unique(tree$Preferred_Lifeform), "Tree")),"Monitoring.Status"],
                            collapse=" "))
    }
  }else{
    #something wrong
    cat(paste(c("FALSE, preferred lifeform values fortrees in tree data set include", unique(tree$Preferred_Lifeform), "when it should only be tree, problem events listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste(c("preferred lifeform values for trees in tree data set include", unique(tree$Preferred_Lifeform), "when it should only be tree, problem events:",
                            paste(tree[which(tree$Preferred_Lifeform %in% setdiff(unique(tree$Preferred_Lifeform), "Tree")), "MacroPlot.Name"],
                                  tree[which(tree$Preferred_Lifeform %in% setdiff(unique(tree$Preferred_Lifeform), "Tree")), "Monitoring.Status"], ", ")),
                          collapse=" "))
  }
  return(flags)
}#end function #verified 10/11 by Eva

########################################
## Tree Duplicates ####
#### Description ----
#' @description
#' The tree_duplicates_qc function addresses the identification of duplicate tree tags
#' in the dataset. It groups the data by macro plot and sample event date, then checks for
#' duplicated tag numbers within each group. If duplicates are found, it reports the error
#' events, highlighting the affected macroplot names and monitoring statuses. Additionally,
#'  the function compares differences in data for the duplicated tag numbers, pointing out
#'  any discrepancies. The final results are presented in the flags variable, which includes
#'  information about duplicate tree tags and the detected issues.
#'
#' @param tree
#'
#' @return A list of flags or data issues with duplicates in the tree csv
#' @export
#'
#' @examples
#' tree_duplicates_qc(tree)

#### Function ----
tree_duplicates_qc=function(tree){
  #group by macro plot and sample event date (monitoring status)
  treedates=as.data.frame(str_split(unique(paste(tree$MacroPlot.Name, tree$Monitoring.Status, sep =",")), ","))
  tree[which(tree$TagNo==999),55]="Tag number 999 replaced with NA"
  tree[which(tree$TagNo==999),8]=NA

  verf=c()
  error_events=c()
  duplicates=c()
  badtags=c()
  #for loop
  for(i in 1:ncol(treedates)){
    rows=which((tree[,1]==treedates[1,i]) & (tree[,2]==treedates[2,i]))
    eve=tree[rows,]
    if(length(unique(na.omit(eve$TagNo)))==length(na.omit(eve$TagNo))){
      verf[i]="verified"
    }else{
      verf[i]="error found"
      ers=unique(na.omit(eve[which(duplicated(eve$TagNo)==TRUE), 8]))
      error_events=c(error_events, paste(c("Error found in sample event", treedates[,i], "tag numbers", unique(na.omit(eve[which(duplicated(eve$TagNo)==TRUE), 8])), "are duplicated"), collapse=" "))

      for(t in 1:length(ers)){
        duplicates=c(duplicates, eve[which(eve$TagNo==ers[t]),c(1,2,3,8,9,10,11)])
        badtags=c(badtags,unique(eve[which(eve$TagNo==ers[t]),8]))
      }

    }

  }
  badtags<<-unique(badtags)

  ###loop to find difference between tag numbers

  diff=c()
  diff2=c()
  norm=c()
  rep=rep(colnames(tree)[c(1,2,3,8,9,10,11)],length(duplicates)/7) #FIX THIS


  for(x in 1:length(duplicates)){
    if(length(setdiff(duplicates[[c(x, 1)]], duplicates[[c(x, 2)]]))==0){
      diff=c(diff, NA)
      diff2=c(diff2, NA)
      norm=c(norm,duplicates[[c(x, 1)]] )
    }else{
      diff=c(diff, setdiff(duplicates[[c(x, 1)]], duplicates[[c(x, 2)]]))
      diff2=c(diff2, setdiff(duplicates[[c(x, 2)]], duplicates[[c(x, 1)]]))
      norm=c(norm,NA )
    }

  }

  #this is so messy but it works I promise
  results=as.data.frame(rbind(rep, norm, diff, diff2))
  results[,which(!is.na(results[3,]))]

  for(g in 1:length(error_events)){
    m=seq(1,length(duplicates),7)[g]
    bad=which(!is.na(results[3,m:(m+6)]))
    bad=bad+(m-1)
    if(length(bad)==0){
      error_events[g]=paste(error_events[g],"no differences between data exist")
    }else{
      error_events[g]=paste(c(error_events[g],"; differences between data are in", results[1,bad], "which have different values of" , results[3,bad], "and", results[4,bad]),collapse=" ")
    }

  }

  cat("Any duplicate tree tags?\n")
  if(length(error_events)==0){
    cat("No\n")
    cat("\n")
  }else{
    cat(paste(error_events, "\n"), sep="\n")
    cat("\n")
  }


  ##change to quarter and subplot fraction - add this information, species too - add a comment about which trees need different tags, could be overstory and pole tagged the same number


  flags<-c(flags, error_events)

  return(flags)
}#end function

########################################
## Tree Dead to Alive and Changing DBH ####
#### Description ----
#' @description
#'The tree_dead_to_alive_DBH_change_qc function examines tree data for instances of dynamic
#'status changes and potential Diameter at Breast Height (DBH) alterations across multiple
#'sampling events. It systematically identifies trees that consistently exhibit DBH changes
#' despite being recorded as dead. Additionally, the function captures cases where trees
#' switch between alive and dead statuses, providing specific information on dates and
#' circumstances. Flagged issues are reported in the 'flags' variable, separating DBH-related
#' concerns and occurrences of trees transitioning from dead to alive.
#' @param tree
#'
#' @return A list of flags or data issues with trees resurrecting and changing dbh when dead in the tree csv
#' @export
#'
#' @examples
#' tree_dead_to_alive_DBH_change_qc(tree)

#### Function ----
tree_dead_to_alive_DBH_change_qc=function(tree){
  events=unique(tree$Monitoring.Status)
  plots=unique(tree$MacroPlot.Name)
  results=matrix(nrow =0, ncol = 6)


  for(pl in 1:length(plots)){

    plot_table=tree[which(tree$MacroPlot.Name==plots[pl]),]

    for(ev in 1:length(events)){

      tab=plot_table[which(plot_table$Monitoring.Status==events[ev]),]



      if(nrow(tab)>1){
        for(r in 1:nrow(tab)){

          results=rbind(results,
                        c(tab[r,"TagNo"], tab[r,"MacroPlot.Name"],
                          tab[r,"Monitoring.Status"], "Status", tab[r,"Status"], tab[r,"Date"]),
                        c(tab[r,"TagNo"], tab[r,"MacroPlot.Name"],
                          tab[r,"Monitoring.Status"], "DBH", tab[r,"DBH"], tab[r,"Date"]))
        }
      }


    }
  }
  results=as.data.frame(results)
  colnames(results)=c("TagNo", "Plot", "Monitoring_Status", "Variable", "Value", "Date")


  results=na.omit(results)

  results$Date <- as.Date(results$Date, format = "%m/%d/%Y %I:%M:%S %p")
  #if length of unique status is 1 - either all dead or alive
    #if all alive - nothing
     #if all dead - dbh check
      #flag any changes in DBH with dates
     #else(length is greater than 1) - switching it up
        #if min date has live status
          #find min dead status  - save date
          #find live status greater than save date - flag as date tree came back alive
        #else min date has dead status
           #find min live status - flag as date tree came back alive
          ##DBH check
            #isolate years where tree is dead
            #flag any changes in DBH with dates
  error_messages_DBH=c()
  error_messages_resurrections=c()
  tags=unique(results$TagNo)
  for(check in 1:length(tags)){
    tagnumber=results[which(results$TagNo==tags[check]),]
    plots=unique(tagnumber$Plot)
    for(plot in 1:length(plots)){
      individual_tree=tagnumber[which(tagnumber$Plot==plots[plot]),]
      individual_tree_s=individual_tree[which(individual_tree$Variable=="Status"),]
      individual_tree_d=individual_tree[which(individual_tree$Variable=="DBH"),]
      if(tags[check] %in% badtags){#SKIP TOO MESSY SOMETHING IS WRONG
      }else{ #running checks
        if(length(unique(individual_tree_s$Value))==1){ #either all dead or all alive"
          if(unique(individual_tree_s$Value)=="L"){#all alive - nothing to do
          }else{#all dead - DBH change check
            if(length(unique(individual_tree_d$Value))==1){#DBH doesn't change, nothing to do
            }else{#DBH DOES change - flag if the difference is greater than 1
              individual_tree_d$Value=as.numeric(individual_tree_d$Value)
              if(all(abs(diff(unique(individual_tree_d$Value)))<1)){#doesn't meet threshold of concern
              }else{#meets threshold - flag
                dates=unique(format(individual_tree_d$Date, "%Y-%m-%d"))
                error_messages_DBH=c(error_messages_DBH, paste(c("Tree number", tags[check],"in plot", unique(individual_tree$Plot),
                                                                 "has been dead for every sampling event but its DBH changes in the following values:",
                                                                 paste(individual_tree_d$Value, sep=", "),
                                                                 "on the following dates", paste(dates, collapse=", ")),collapse=" "))
              } #closing flag
            } #closing DBH does change
          } #closing all dead dbh check
        }else{  #not all dead or alive, some entries dead some alive
          if(length(which(individual_tree_s$Date==min(individual_tree_s$Date)))>1){#2 entries for min date? - probably two trees with same tag
            error_messages_resurrections=c(error_messages_resurrections, paste(c("Tree number", tags[check],"in plot", unique(individual_tree$Plot), "has two different entries for the min date - one alive and one dead, min date is", min(individual_tree_s$Date)), collapse = " "))
          }else{#only one entry for min date
            if(individual_tree_s[which(individual_tree_s$Date==min(individual_tree_s$Date)),"Value"]=="L"){#tree starts out alive - switches to dead then alive
              min_dead_date=min(individual_tree_s[which(individual_tree_s$Value=="D"),"Date"]) #record min dead date
              min_dead_date=as.Date(min_dead_date)
              new_alive_date=individual_tree_s[which(individual_tree_s$Value=="L" & individual_tree_s$Date>min_dead_date),"Date"] #record which dates are greater than min dead date with live status
              new_alive_date=as.Date(new_alive_date)
              if(length(new_alive_date)==0){ #no dates where tree is alive and was previously dead
              }else{ #there ARE dates where tree is alive and was previously dead
                error_messages_resurrections=c(error_messages_resurrections, paste(c("Tree number", tags[check],"in plot", unique(individual_tree$Plot), "started out alive in", format(min(individual_tree_s$Date), "%Y-%m-%d"), "was recorded dead in", format(min_dead_date, "%Y-%m-%d"), "and then recorded to be alive again in", format(new_alive_date, "%Y-%m-%d")), collapse = " "))
              }#close error message
            }else{#tree starts out dead - switches to alive
              new_alive_date=min(individual_tree_s[which(individual_tree_s$Value=="L"),"Date"]) #min alive date
              new_alive_date=as.Date(new_alive_date)
              error_messages_resurrections=c(error_messages_resurrections, paste(c("Tree number", tags[check], "in plot", unique(individual_tree$Plot),"started out dead in", format(min(individual_tree_s$Date), "%Y-%m-%d"), "and then recorded to be alive again in", format(new_alive_date, "%Y-%m-%d")), collapse = " "))
            }#DBH check for fluctuating trees
            dead_tree_dates=individual_tree_s[which(individual_tree_s$Value=="D"),"Date"]
            if(length(unique(individual_tree_d[which(individual_tree_d$Date %in% dead_tree_dates),5]))==1){ #no change in DBH - nothing to do
            }else{ #change in dbh
              individual_tree_d$Value=as.numeric(individual_tree_d$Value)
              if(all(abs(diff(unique(individual_tree_d$Value)))<1)){ #doesn't meet threshold of concern (1)
              }else{#meets threshold - difference is greater than 1
                dates=unique(format(individual_tree_d$Date, "%Y-%m-%d"))
                error_messages_DBH=c(error_messages_DBH, paste(c("Tree number", tags[check],"in plot", unique(individual_tree$Plot),
                                                                 "has been changing DBH when its dead in the following values:",
                                                                 paste(individual_tree_d$Value, sep=", "),
                                                                 "on the following dates",paste(dates, collapse=",")),  collapse=" "))
              }#closing error message
            }#closing change in dbh while dead
        }#closing only one entry for min date
        }#closing fluctuating from dead to alive
      }#closing not a bad tag - running checks
    }#closing loop through plots
  }#closing loop through tags
#flagging errors
  flags<-c(flags, error_messages_DBH) #add dates #check for increase in dbh which would be weird
  flags<-c(flags, error_messages_resurrections)
  cat(paste(error_messages_DBH,"\n"), sep="\n")
  cat("\n")
  cat(paste(error_messages_resurrections,"\n"), sep="\n")

#INFO ABOUT TREE RESURRECTIONS
  #tree data - trees are tagged, in theory once that tree dies it shouldn't come back alive, it shouldn't shrink - make sure it doesn't
  #how much should it matter for density calculations...
  #case by case, look at data sheet and see what happened, could be a different tree but trees have tags in them. sometimes duplicate numbers
  #once a tree dies dbh stays the same
  #is data useful and necessary
  #how do trees look over time
  #number the checks for easy reference?
  #notes of things to change: is rows the most useful to go back and correct data? maybe macroplot and sample event is better
  #add number of each test for reference?
  return(flags)
}#end function

#end of document

########################################
## Tree All Comments Ever ####
#### Description ----
#separate by protocol
#' Comments
#' @description
#' The `comments` function compiles unique, non-empty comments from protocols cover, 1000
#' HR Fuels, Duff, Fine Fuels, Saplings, Seedlings, and Trees, excluding common placeholders.
#' These are protocols typically used in Saguaro’s monitoring program, but the list should be
#' edited for different programs.  It categorizes comments by protocol, including macro plot
#'  names, monitoring statuses, and associated comments.
#' @param
#' cover
#' @param
#' fuel1000
#' @param
#' duff
#' @param
#' fine
#' @param
#' saps
#' @param
#' seeds
#' @param
#' tree
#'
#' @return A list of comments excluding some repeated ones that aren't relevant
#' @export
#'
#' @examples
#' comments(cover, fuel1000,duff, fine, saps, seeds, tree)

#### Function ----
comments=function(cover, fuel1000, duff, fine, tree){
  comments=c()
  cover_comments=unique(na.omit(cover$Comment))
  cover_comments=cover_comments[! cover_comments %in% c('no data collected', 'data not collected', '')]
  fuel1000_comments=unique(na.omit(fuel1000$Comment))
  fuel1000_comments=fuel1000_comments[! fuel1000_comments %in% c('no data collected', 'data not collected', '')]
  duff_comments=unique(na.omit(duff$Comment))
  duff_comments=duff_comments[! duff_comments %in% c('no data collected', 'data not collected', '')]
  fine_comments=unique(na.omit(fine$Comment))
  fine_comments=fine_comments[! fine_comments %in% c('no data collected', 'data not collected', '')]
  # if(is.vector(saps)){
  #   #skip
  # }else{
  # saps_comments=unique(na.omit(saps$Comment))
  # saps_comments=saps_comments[! saps_comments %in% c('no data collected', 'data not collected', '')]
  # }
  # seeds_comments=unique(na.omit(seeds$Comment))
  # seeds_comments=seeds_comments[! seeds_comments %in% c('no data collected', 'data not collected', '')]
  tree_comments=unique(na.omit(tree$Comment))
  tree_comments=tree_comments[! tree_comments %in% c('no data collected', 'data not collected', '', 'missing tag', 'NEEDS DBH', 'Tag missing')]
  comments=c("COVER PROTOCOL",
             paste(cover[which(cover$Comment %in% cover_comments), "MacroPlot.Name"],
                   cover[which(cover$Comment %in% cover_comments), "Monitoring.Status"],
                   cover[which(cover$Comment %in% cover_comments), "Comment"], sep=", "),
             "1000 HR FUELS PROTOCOL",
             paste(fuel1000[which(fuel1000$Comment %in% fuel1000_comments), "Index"],
                   fuel1000[which(fuel1000$Comment %in% fuel1000_comments), "MacroPlot.Name"],
                   fuel1000[which(fuel1000$Comment %in% fuel1000_comments), "Monitoring.Status"],
                   fuel1000[which(fuel1000$Comment %in% fuel1000_comments), "Comment"], sep=", "),

             "DUFF PROTOCOL",
             paste(
                   duff[which(duff$Comment %in% duff_comments), "MacroPlot.Name"],
                   duff[which(duff$Comment %in% duff_comments), "Monitoring.Status"],
                   duff[which(duff$Comment %in% duff_comments), "Comment"], sep=", "),


             "FINE FUELS PROTOCOL",
             paste("Index",  fine[which(fine$Comment %in% fine_comments), "Index"],
                   fine[which(fine$Comment %in% fine_comments), "MacroPlot.Name"],
                   fine[which(fine$Comment %in% fine_comments), "Monitoring.Status"],
                   fine[which(fine$Comment %in% fine_comments), "Comment"], sep=", "),


             # "SEEDLINGS PROTOCOL",
             # paste("Index",  seeds[which(seeds$Comment %in% seeds_comments), "Index"],
             #       seeds[which(seeds$Comment %in% seeds_comments), "MacroPlot.Name"],
             #       seeds[which(seeds$Comment %in% seeds_comments), "Monitoring.Status"],
             #       seeds[which(seeds$Comment %in% seeds_comments), "Comment"], sep=", "),


             "TREES PROTOCOL",
             paste( tree[which(tree$Comment %in% tree_comments), "TagNo"],
                    tree[which(tree$Comment %in% tree_comments), "MacroPlot.Name"],
                    tree[which(tree$Comment %in% tree_comments), "Monitoring.Status"],
                    tree[which(tree$Comment %in% tree_comments), "Comment"], sep=", ")


  )

#   if(is.vector(saps)){
#     #skip
#   }else{
#
#   comments=c(comments,
#   "SAPLINGS PROTOCOL",
#   paste(saps[which(saps$Comment %in% saps_comments), "Index"],
#         saps[which(saps$Comment %in% saps_comments), "MacroPlot.Name"],
#         saps[which(saps$Comment %in% saps_comments), "Monitoring.Status"],
#         saps[which(saps$Comment %in% saps_comments), "Comment"], sep=", "))
# }
  return(comments)
}

########################################
## Tree Function to Separate Flags by Plot ####
#### Description ----
#' @description
#' The format_flags function generates an Excel output that consists of multiple sheets,
#'  each corresponding to a distinct macro plot and an additional sheet for comments.
#'  For each macro plot sheet, the data is organized with a column named "Issue" containing
#'  flagged information. Additional columns include "Resolved," "Resolved_by," "Action_need,"
#'  and "Other_notes," providing a structured format for documenting and tracking the
#'  resolution status of flagged issues.
#' @param
#' flags
#' @param
#' samp
#' @param
#' mtype
#' @param
#' comments
#'
#' @return Excel file separating issues by plot
#' @export
#'
#' @examples
#' format_flags(flags, samp, mtype, comments)

#### Function ----
format_flags=function(flags, samp, mtype, comments){
  #blank list

  plots=unique(samp[which(samp$ProjectUnit_Name == mtype), "MacroPlot_Name"])



  df_flags <- data.frame(matrix(ncol = length(plots), nrow = 0))

  for(i in 1:length(flags)){
    for(x in 1:length(plots)){
      if(grepl(plots[x], flags[i])){
        df_flags[nrow(df_flags)+1, x]=flags[i]
      }else{
        #nothing
      }
    }
  }
  plots=gsub(pattern=":", x=plots, replacement="_")
  colnames(df_flags)=plots
  #df_flags$Comments=c(comments_list, rep(NA,nrow(df_flags)-length(comments_list)))

  data = list()

  for(p in 1:ncol(df_flags)){
    col=na.omit(df_flags[,p])
    x1=as.data.frame(col)
    data[p]=x1
    #cat(colnames(df_flags[p]))
    #cat("\n")
    #cat("\n")
    #cat("\n")
    #cat("\n")
    #cat(paste(col, "\n"), sep="\n")
  }

  for(t in 1:(length(plots)+1)){
    data[[t]]=as.data.frame(data[[t]])
    colnames(data[[t]])="Issue"
    data[[t]]$Resolved=rep(NA, nrow(data[[t]]))
    data[[t]]$Resolved_by=rep(NA, nrow(data[[t]]))
    data[[t]]$Action_need=rep(NA, nrow(data[[t]]))
    data[[t]]$Other_notes=rep(NA, nrow(data[[t]]))
  }

  names(data)=c(plots, "Comments")
  todaysdate=Sys.Date()


  write.xlsx(
    x=data,
    file = paste(mtype,"flags_QAQC", todaysdate, ".xlsx"),
    col_names = TRUE,
    format_headers = TRUE,
    use_zip64 = FALSE
  )

}

