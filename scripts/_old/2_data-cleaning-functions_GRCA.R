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
# Protocol: Trees - Individuals (metric) ####
########################################
## Tree Damage (much simpler than my current code) ####
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

  if(length(setdiff(na.omit(unique(str_trim(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )))),damcodes))%in%0){
    cat("TRUE\n")

########################################
## Tree Duplicates (changing tag numbers over time?) ####
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

