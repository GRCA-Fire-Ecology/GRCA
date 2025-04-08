## Before Starting

### Setup

```{r setup, include=FALSE}
## Markdown Options
knitr::opts_chunk$set(echo = FALSE, error = FALSE, warning = FALSE, message = FALSE)
```

```{r install packages, include=FALSE}
## Install packages (if needed)

# install.packages("tidyverse")
# install.packages("kableExtra")
# install.packages("ggplot2")
```

```{r load packages, include=FALSE}
## Load Packages

# "here" helps you easily find and reference your working directory
library(here)
# "tidyverse" has lots of useful functions for data cleaning
library(tidyverse)
# "kableExtra" is used to produce formatted tables
library(kableExtra)
library(ggplot2)
```

### Adjust File Paths

```{r import data, include=FALSE}
# Identify working directory
here()

# Load in data.
path_data <- "X:/FFI Data Management/Exports from FFI/GRCA_FMH/metadata/"
path_output <- paste0(here(), "/output/")
path_data <- "D:/Allie/_FireFX/FFI Data Management/Exports from FFI/metadata/"
```

### Load Data

```{r load data, include=FALSE}
## Load Data

# GRCA
GRCA_MacroplotReport <- read.csv(paste0(path_data, "GRCA_MacroplotReport.csv"))
GRCA_SampleEventReport <- read.csv(paste0(path_data, "GRCA_SampleEventReport.csv"))
#GRCA_ProjectUnitReport <- read.csv(paste0(path_data, "GRCA_ProjectUnitAssignmentReport.csv"))

# I&M
IM_MacroplotReport <- read.csv(paste0(path_data, "I&M_MacroplotReport.csv"))
IM_SampleEventReport <- read.csv(paste0(path_data, "I&M_SampleEventReport.csv"))
#IM_ProjectUnitReport <- read.csv(paste0(path_data, "I&M_ProjectUnitAssignmentReport.csv"))

# WACA
WACA_MacroplotReport <- read.csv(paste0(path_data, "WACA_MacroplotReport.csv"))
WACA_SampleEventReport <- read.csv(paste0(path_data, "WACA_SampleEventReport.csv"))
#WACA_ProjectUnitReport <- read.csv(paste0(path_data, "WACA_ProjectUnitAssignmentReport.csv"))
```

### Clean Data

-   remove trailing/leading whitespace from character columns

-   reformat date columns

-   make column names match between reports

#### Sample Event Report

```{r}
GRCA_SampleEventReport <- GRCA_SampleEventReport %>%

  # remove trailing/leading whitespace
  mutate(across(where(is.character), str_trim)) %>%

  # separate Date columns into Date and Time columns
  separate(SampleEvent_Date, sep = " ", into = c("SampleEventDate", "SampleEventTime")) %>%
  # reformate Date columns
  mutate(SampleEventDate = as.Date(SampleEventDate, "%m/%d/%Y")) %>%
  # separate Date columns into Year, Month, and Day columns
  separate(SampleEventDate, sep = "-", into = c("SampleEventDate_Year", "SampleEventDate_Month", "SampleEventDate_Day"), remove = FALSE) %>%
  # remove unnecessary Date columns
  select(!c(SampleEventTime, SampleEventDate_Month, SampleEventDate_Day)) %>%

  # make column names match those in MacroplotReport
  mutate(Macroplot = MacroPlot_Name,
         LegacyMonitoringStatus = SampleEvent_LegacyMonitoringStatus,
         MonStatus = MonitoringStatus_Name) %>%
  select(!c(MacroPlot_Name, SampleEvent_LegacyMonitoringStatus, MonitoringStatus_Name))

##############################################

IM_SampleEventReport <- IM_SampleEventReport %>%

  # remove trailing/leading whitespace
  mutate(across(where(is.character), str_trim)) %>%

  # separate Date columns into Date and Time columns
  separate(SampleEvent_Date, sep = " ", into = c("SampleEventDate", "SampleEventTime")) %>%
  # reformate Date columns
  mutate(SampleEventDate = as.Date(SampleEventDate, "%m/%d/%Y")) %>%
  # separate Date columns into Year, Month, and Day columns
  separate(SampleEventDate, sep = "-", into = c("SampleEventDate_Year", "SampleEventDate_Month", "SampleEventDate_Day"), remove = FALSE) %>%
  # remove unnecessary Date columns
  select(!c(SampleEventTime, SampleEventDate_Month, SampleEventDate_Day)) %>%

  # make column names match those in MacroplotReport
  mutate(Macroplot = MacroPlot_Name,
         LegacyMonitoringStatus = SampleEvent_LegacyMonitoringStatus,
         MonStatus = MonitoringStatus_Name) %>%
  select(!c(MacroPlot_Name, SampleEvent_LegacyMonitoringStatus, MonitoringStatus_Name)) %>%

  # edit Macroplot column
  separate(Macroplot, sep = "_", into = c("Park", "Macroplot"), remove = F) %>%
  select(!Park)

##############################################

WACA_SampleEventReport <- WACA_SampleEventReport %>%

  # remove trailing/leading whitespace
  mutate(across(where(is.character), str_trim)) %>%

  # separate Date columns into Date and Time columns
  separate(SampleEvent_Date, sep = " ", into = c("SampleEventDate", "SampleEventTime")) %>%
  # reformate Date columns
  mutate(SampleEventDate = as.Date(SampleEventDate, "%m/%d/%Y")) %>%
  # separate Date columns into Year, Month, and Day columns
  separate(SampleEventDate, sep = "-", into = c("SampleEventDate_Year", "SampleEventDate_Month", "SampleEventDate_Day"), remove = FALSE) %>%
  # remove unnecessary Date columns
  select(!c(SampleEventTime, SampleEventDate_Month, SampleEventDate_Day)) %>%

  # make column names match those in MacroplotReport
  mutate(Macroplot = MacroPlot_Name,
         LegacyMonitoringStatus = SampleEvent_LegacyMonitoringStatus,
         MonStatus = MonitoringStatus_Name) %>%
  select(!c(MacroPlot_Name, SampleEvent_LegacyMonitoringStatus, MonitoringStatus_Name)) %>%

  # edit Macroplot and ProjectUnit_Name columns
  mutate(Macroplot = ifelse(Macroplot == "B:FPIED1D02:01", "PIED 01",
                            ifelse(Macroplot == "B:FPIED1D02:02", "PIED 02", Macroplot))) %>%
  mutate(ProjectUnit_Name = ifelse(ProjectUnit_Name == "FPIED1D02", "PIED", ProjectUnit_Name))
```

#### Macroplot Report

```{r}
GRCA_MacroplotReport <- GRCA_MacroplotReport %>%

  # remove trailing/leading whitespace
  mutate(across(where(is.character), str_trim)) %>%

  # separate Date columns into Date and Time columns
  separate(SampleEventDate, sep = " ", into = c("SampleEventDate", "SampleEventTime")) %>%
  separate(DateIn, sep = " ", into = c("DateIn", "DateInTime")) %>%
  # reformate Date columns
  mutate(SampleEventDate = as.Date(SampleEventDate, "%m/%d/%Y"),
         DateIn = as.Date(DateIn, "%m/%d/%Y")) %>%
  # separate Date columns into Year, Month, and Day columns
  separate(SampleEventDate, sep = "-", into = c("SampleEventDate_Year", "SampleEventDate_Month", "SampleEventDate_Day"), remove = FALSE) %>%
  separate(DateIn, sep = "-", into = c("DateIn_Year", "DateIn_Month", "DateIn_Day"), remove = FALSE) %>%
  # remove unnecessary Date columns
  select(!c(SampleEventTime, SampleEventDate_Month, SampleEventDate_Day,
            DateInTime, DateIn_Month, DateIn_Day))

##############################################

IM_MacroplotReport <- IM_MacroplotReport %>%

  # remove trailing/leading whitespace
  mutate(across(where(is.character), str_trim)) %>%

  # separate Date columns into Date and Time columns
  separate(SampleEventDate, sep = " ", into = c("SampleEventDate", "SampleEventTime")) %>%
  separate(DateIn, sep = " ", into = c("DateIn", "DateInTime")) %>%
  # reformate Date columns
  mutate(SampleEventDate = as.Date(SampleEventDate, "%m/%d/%Y"),
         DateIn = as.Date(DateIn, "%m/%d/%Y")) %>%
  # separate Date columns into Year, Month, and Day columns
  separate(SampleEventDate, sep = "-", into = c("SampleEventDate_Year", "SampleEventDate_Month", "SampleEventDate_Day"), remove = FALSE) %>%
  separate(DateIn, sep = "-", into = c("DateIn_Year", "DateIn_Month", "DateIn_Day"), remove = FALSE) %>%
  # remove unnecessary Date columns
  select(!c(SampleEventTime, SampleEventDate_Month, SampleEventDate_Day,
            DateInTime, DateIn_Month, DateIn_Day)) %>%

  # edit Macroplot column
  separate(Macroplot, sep = "_", into = c("Park", "Macroplot"), remove = F)

##############################################

WACA_MacroplotReport <- WACA_MacroplotReport %>%

  # remove trailing/leading whitespace
  mutate(across(where(is.character), str_trim)) %>%

  # separate Date columns into Date and Time columns
  separate(SampleEventDate, sep = " ", into = c("SampleEventDate", "SampleEventTime")) %>%
  separate(DateIn, sep = " ", into = c("DateIn", "DateInTime")) %>%
  # reformate Date columns
  mutate(SampleEventDate = as.Date(SampleEventDate, "%m/%d/%Y"),
         DateIn = as.Date(DateIn, "%m/%d/%Y")) %>%
  # separate Date columns into Year, Month, and Day columns
  separate(SampleEventDate, sep = "-", into = c("SampleEventDate_Year", "SampleEventDate_Month", "SampleEventDate_Day"), remove = FALSE) %>%
  separate(DateIn, sep = "-", into = c("DateIn_Year", "DateIn_Month", "DateIn_Day"), remove = FALSE) %>%
  # remove unnecessary Date columns
  select(!c(SampleEventTime, SampleEventDate_Month, SampleEventDate_Day,
            DateInTime, DateIn_Month, DateIn_Day)) %>%

  # edit Metadata Macroplot columns
  mutate(MetaData = ifelse(Macroplot == "B:FPIED1D02:01" & Purpose == "PIED", Macroplot,
                           ifelse(Macroplot == "B:FPIED1D02:02" & Purpose == "PIED", Macroplot, MetaData))) %>%
  mutate(Macroplot = ifelse(Macroplot == "B:FPIED1D02:01", "PIED 01",
                            ifelse(Macroplot == "B:FPIED1D02:02", "PIED 02", Macroplot)))
```

### Filter Data

#### Sample Event Report

The Sample Event Report contains multiple project units, and each plot can be assigned to any project unit. To reduce duplicates, select only the core project units.

```{r}
GRCA_SampleEventReport_merge <- GRCA_SampleEventReport %>%
  filter(!ProjectUnit_Name %in% c("z2014 + 2017_Slopes RX", "z2017_Tipover East RX", "z2017_Slopes RX", "zHiSevMSO", "zSRIMRx2024Spring_Latest")) %>%
  filter(!ProjectUnit_Name %in% c("ALL_FMH", "ALL_RAP_NRim", "ALL_RAP_SRim")) %>%
  select(!c(Multi_PU, SampleEventDate_Year, LegacyMonitoringStatus))

##############################################

IM_SampleEventReport_merge <- IM_SampleEventReport %>%
  filter(!ProjectUnit_Name %in% c("z2014 + 2017_Slopes RX")) %>%
  select(!c(Multi_PU, SampleEventDate_Year, LegacyMonitoringStatus))

##############################################

WACA_SampleEventReport_merge <- WACA_SampleEventReport %>%
  select(!c(Multi_PU, SampleEventDate_Year, LegacyMonitoringStatus))
```

#### Macroplot Report

```{r}
GRCA_MacroplotReport_MetaData <- GRCA_MacroplotReport %>%
  filter(is.na(SampleEventDate)) %>%
  select(!c(SampleEventDate, SampleEventDate_Year, SampleEventTeam, SampleEventComment, MonStatusOrd, LegacyMonStatus, MonStatus))

GRCA_MacroplotReport_SampleEvents <- GRCA_MacroplotReport %>%
  filter(!is.na(SampleEventDate)) %>%
  select(c(Macroplot, SampleEventDate, SampleEventDate_Year, SampleEventTeam, SampleEventComment, MonStatusOrd, LegacyMonStatus, MonStatus))

##############################################

IM_MacroplotReport_MetaData <- IM_MacroplotReport %>%
  filter(is.na(SampleEventDate)) %>%
  select(!c(SampleEventDate, SampleEventDate_Year, SampleEventTeam, SampleEventComment, MonStatusOrd, LegacyMonStatus, MonStatus))

IM_MacroplotReport_SampleEvents <- IM_MacroplotReport %>%
  filter(!is.na(SampleEventDate)) %>%
  select(c(Macroplot, SampleEventDate, SampleEventDate_Year, SampleEventTeam, SampleEventComment, MonStatusOrd, LegacyMonStatus, MonStatus))

##############################################

WACA_MacroplotReport_MetaData <- WACA_MacroplotReport %>%
  filter(is.na(SampleEventDate)) %>%
  select(!c(SampleEventDate, SampleEventDate_Year, SampleEventTeam, SampleEventComment, MonStatusOrd, LegacyMonStatus, MonStatus))

WACA_MacroplotReport_SampleEvents <- WACA_MacroplotReport %>%
  filter(!is.na(SampleEventDate)) %>%
  select(c(Macroplot, SampleEventDate, SampleEventDate_Year, SampleEventTeam, SampleEventComment, MonStatusOrd, LegacyMonStatus, MonStatus))
```

### Add Columns

-   Disturbance_Type (burn, thin, etc...)

-   PlotType_Name (FMH, RAP, etc...)

-   Park (GRCA, WACA)

-   Park_Unit (North, South)

-   Burn_Unit

-   Macroplot_Code

-   Macroplot_Num

```{r}
GRCA_MacroplotReport_MetaData <- GRCA_MacroplotReport_MetaData %>%

  # PlotType_Name
  mutate(PlotType_Name = ifelse(Purpose %in% c("PIED","PIPO","PIPN","PIAB","PIEN"), "FMH",
                                ifelse (Purpose %in% c("GRIN","GRED"), "Meadow", "RAP"))) %>%

  # Park
  mutate(Park = "GRCA") %>%

  # Macroplot_Code and Macroplot_Number
  separate(Macroplot, sep = " ", into = c("Macroplot_Code", "Macroplot_Num"), remove = F) %>%
  separate(Macroplot, sep = "(?<=[A-Z])(?=[0-9])", into = c("Macroplot_Code1", "Macroplot_Num1"), remove = F) %>%
  mutate(Macroplot_Code = ifelse(PlotType_Name == "RAP", Macroplot_Code1, Macroplot_Code)) %>%
  mutate(Macroplot_Num = ifelse(PlotType_Name == "RAP", Macroplot_Num1, Macroplot_Num)) %>%
  select(!c(Macroplot_Code1, Macroplot_Num1)) %>%

  # Park_Unit
  mutate(Park_Unit = ifelse(Purpose %in% c("PIED", "PIPO", "Moqui", "Picnic", "Quarry", "Tusayan Pueblo (Thinning)"), "South", "North")) %>%

  # Burn_Unit
  separate(UV1, sep = " Project Unit", into = c("Burn_Unit"), remove = F) %>%

  # Disturbance_Type
  mutate(Disturbance_Type = ifelse(Purpose %in% c("Tusayan Pueblo (Thinning)"), "thin", "burn")) %>%

  # reorder columns
  relocate(PlotType_Name, .before = Macroplot) %>%
  relocate(Park, .before = PlotType_Name) %>%
  relocate(Type, .after = PlotType_Name) %>%
  relocate(Park_Unit, .after = Type) %>%
  relocate(Purpose, .after = Park_Unit) %>%
  relocate(Burn_Unit, .after = Purpose) %>%
  relocate(MetaData, .after = Burn_Unit)

##############################################

IM_MacroplotReport_MetaData <- IM_MacroplotReport_MetaData %>%

  # PlotType_Name
  mutate(PlotType_Name = "I&M") %>%

  # Macroplot_Code and Macroplot_Number
  separate(Macroplot, sep = "(?<=[A-Z])(?=[0-9])", into = c("Macroplot_Code", "Macroplot_Num"), remove = F) %>%

  # Purpose
  mutate(Purpose = ifelse(Macroplot_Code == "M", "Mixed Conifer", "Pinyon Juniper")) %>%

  # Park_Unit
  mutate(Park_Unit = ifelse(Macroplot_Code == "M", "North", "South")) %>%

  # Burn_Unit
  separate(UV1, sep = " Project Unit", into = c("Burn_Unit"), remove = F) %>%

  # Disturbance_Type
  mutate(Disturbance_Type = "burn") %>%

  # reorder columns
  relocate(PlotType_Name, .before = Macroplot) %>%
  relocate(Park, .before = PlotType_Name) %>%
  relocate(Type, .after = PlotType_Name) %>%
  relocate(Park_Unit, .after = Type) %>%
  relocate(Purpose, .after = Park_Unit) %>%
  relocate(Burn_Unit, .after = Purpose) %>%
  relocate(MetaData, .after = Burn_Unit)

##############################################

WACA_MacroplotReport_MetaData <- WACA_MacroplotReport_MetaData %>%

  # PlotType_Name
  mutate(PlotType_Name = "FMH") %>%

  # Park
  mutate(Park = "WACA") %>%

  # Macroplot_Code and Macroplot_Number
  separate(Macroplot, sep = " ", into = c("Macroplot_Code", "Macroplot_Num"), remove = F) %>%
  # Park_Unit
  mutate(Park_Unit = ifelse(Macroplot %in% c("PIPO 06", "PIPO 07", "PIPO 08", "PIPO 09", "PIPO 13", "PIPO 14"), "South", "North")) %>%

  # Burn_Unit
  separate(UV1, sep = " Project Unit", into = c("Burn_Unit"), remove = F) %>%

  # Disturbance_Type
  mutate(Disturbance_Type = "burn") %>%

  # reorder columns
  relocate(PlotType_Name, .before = Macroplot) %>%
  relocate(Park, .before = PlotType_Name) %>%
  relocate(Type, .after = PlotType_Name) %>%
  relocate(Park_Unit, .after = Type) %>%
  relocate(Purpose, .after = Park_Unit) %>%
  relocate(Burn_Unit, .after = Purpose) %>%
  relocate(MetaData, .after = Burn_Unit)
```

## Summary Tables

### Monitoring Status

Create columns for Entry (number of disturbances), Status (pre or post disturbance), and Time Since disturbance (i.e., Year 1) via break down of the Monitoring Status field

```{r}
# Isolate different Monitoring Statuses from each park
GRCA_MonStatus <- GRCA_MacroplotReport_SampleEvents %>%
  select(MonStatusOrd, MonStatus) %>%
  mutate(MonStatusOrd = NA)

IM_MonStatus <- IM_MacroplotReport_SampleEvents %>%
  select(MonStatusOrd, MonStatus) %>%
  mutate(MonStatusOrd = NA)

WACA_MonStatus <- WACA_MacroplotReport_SampleEvents %>%
  select(MonStatusOrd, MonStatus) %>%
  mutate(MonStatusOrd = NA)

# Combine Monitoring Statuses, keep only unique values
all_MonStatus <- rbind(GRCA_MonStatus, IM_MonStatus, WACA_MonStatus) %>%
  unique() %>%
  filter(MonStatus != "")

# Create column with consistent naming
all_MonStatus <- all_MonStatus %>%
  mutate(MonStatus_pre = ifelse(grepl("Measurement", MonStatus), "00Measurement0",
                                ifelse(grepl("Pre|PR", MonStatus), "00Pre0", NA))) %>%
  mutate(MonStatus_post = ifelse(grepl("01Post|01 Post", MonStatus), "01Post0",
                                 ifelse(grepl("02Post", MonStatus), "02Post0",
                                        ifelse(grepl("03Post", MonStatus), "03Post0",
                                               ifelse(grepl("04Post", MonStatus), "04Post0", NA))))) %>%
  mutate(MonStatus_year = ifelse(grepl("01 yr01", MonStatus), "01Year1",
                                 ifelse(grepl("01 yr02", MonStatus), "01Year2", NA))) %>%
  mutate(MonStatus_new = ifelse(!is.na(MonStatus_pre), MonStatus_pre,
                                ifelse(!is.na(MonStatus_post), MonStatus_post,
                                       ifelse(!is.na(MonStatus_year), MonStatus_year, MonStatus)))) %>%
  select(!c(MonStatus_pre, MonStatus_post, MonStatus_year))

# Extract info
all_MonStatus <- all_MonStatus %>%
  unique() %>%
  # extract info based on monitoring status names
  extract(MonStatus_new, into = c("Entry","Status","TimeSince"),
          regex = "([0-9]+)([A-Z][a-z]+)([0-9]+)",
          remove = FALSE) %>%
  # fix up classifications
  mutate(Status = ifelse(Status == "Year", "Post", Status)) %>%
  mutate(Status = ifelse(Status == "Measurement", "Pre", Status)) %>%
  mutate(TimeSince = ifelse(Status == "Pre", NA, TimeSince)) %>%
  # make columns numeric
  mutate(Entry = as.numeric(Entry),
         TimeSince = as.numeric(TimeSince)) %>%
  # create factor order
  mutate(MonStatus = factor(MonStatus, levels = c("Measurement_1", "Measurement_2",
                                                  "Measurement_3", "Measurement_4",
                                                  "PR01", "PR02", "PR03", "PR04", "PR05",
                                                  "Pre", "00 PRE",
                                                  "01Post", "01PostB", "01 Post",
                                                  "01Year1", "01 yr01",
                                                  "01Year2", "01 yr02",
                                                  "01Year3" ,
                                                  "01Year5", "01Year10", "01Year20",
                                                  "02PR01", "02PR02", "02Pre",
                                                  "02Post", "02PostB",
                                                  "02Year1", "02Year2",
                                                  "02Year5", "02Year10", "02Year20",
                                                  "03Post",
                                                  "03Year1", "03Year2",
                                                  "03Year5", "03Year10",
                                                  "04Post",
                                                  "04Year1", "04Year2",
                                                  "04Year5"))) %>%
  arrange(MonStatus) %>%
  mutate(MonStatusOrd = row_number()) %>%
  select(!c(MonStatus_new))
```

### Disturbance History

Create columns for Entry (number of disturbances), Disturbance_Name (i.e., fire name or project name), Disturbance_Desc (i.e., RX, WF, Supp), DisturbanceDate, and Severity (i.e., low, mod, high).

```{r}
# Ensure blanks in UV columns are NA
GRCA_MacroplotReport_MetaData$UV2[GRCA_MacroplotReport_MetaData$UV2==""] <- NA
GRCA_MacroplotReport_MetaData$UV3[GRCA_MacroplotReport_MetaData$UV3==""] <- NA
GRCA_MacroplotReport_MetaData$UV4[GRCA_MacroplotReport_MetaData$UV4==""] <- NA
GRCA_MacroplotReport_MetaData$UV5[GRCA_MacroplotReport_MetaData$UV5==""] <- NA
#GRCA_MacroplotReport_MetaData$UV6[GRCA_MacroplotReport_MetaData$UV6==""] <- NA
#GRCA_MacroplotReport_MetaData$UV7[GRCA_MacroplotReport_MetaData$UV7==""] <- NA
#GRCA_MacroplotReport_MetaData$UV8[GRCA_MacroplotReport_MetaData$UV8==""] <- NA

IM_MacroplotReport_MetaData$UV2[IM_MacroplotReport_MetaData$UV2==""] <- NA

WACA_MacroplotReport_MetaData$UV2[WACA_MacroplotReport_MetaData$UV2==""] <- NA
```

#### 01 Disturbance

```{r}
# add add relevant columns
GRCA_Disturbance01 <- GRCA_MacroplotReport_MetaData %>%
  select(c(Park, PlotType_Name, Macroplot, Disturbance_Type, UV2)) %>%
  filter(!is.na(UV2)) %>%
  separate(UV2, sep = ": ", into = c("Entry", "Disturbance_Info"), remove = FALSE) %>%
  separate(Disturbance_Info, sep = ",", into = c("Disturbance_Name", "Disturbance_Desc", "DisturbanceDate", "Severity"), remove = FALSE) %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(Entry = 1)

# specific edits
GRCA_Disturbance01 <- GRCA_Disturbance01 %>%
  mutate(Disturbance_Name = ifelse(Macroplot == "SC03", "Tipover East", Disturbance_Name),
         Disturbance_Desc = ifelse(Macroplot == "SC03", "Rx", Disturbance_Desc),
         DisturbanceDate = ifelse(Macroplot == "SC03", "12/22/17", DisturbanceDate),
         Severity = ifelse(Macroplot == "SC03", "(POST read '18)", Severity)) %>%
  mutate(Disturbance_Name = ifelse(Macroplot == "SC16", "Tipover East", Disturbance_Name),
         Disturbance_Desc = ifelse(Macroplot == "SC16", "Rx", Disturbance_Desc),
         DisturbanceDate = ifelse(Macroplot == "SC16", "12/22/17", DisturbanceDate),
         Severity = ifelse(Macroplot == "SC16", "(POST read '18)", Severity)) %>%
  mutate(Disturbance_Name = ifelse(Macroplot == "PIAB 06", "NW 1,3,5", Disturbance_Name),
         Disturbance_Desc = ifelse(Macroplot == "PIAB 06", "Rx", Disturbance_Desc),
         DisturbanceDate = ifelse(Macroplot == "PIAB 06", "11/9/2007", DisturbanceDate),
         Severity = ifelse(Macroplot == "PIAB 06", "Low Severity", Severity)) %>%
  mutate(Disturbance_Name = ifelse(Macroplot == "PIAB 24", "NW 1,3,5", Disturbance_Name),
         Disturbance_Desc = ifelse(Macroplot == "PIAB 24", "Rx", Disturbance_Desc),
         DisturbanceDate = ifelse(Macroplot == "PIAB 24", "11/10/2007", DisturbanceDate),
         Severity = ifelse(Macroplot == "PIAB 24", "Moderate/Low Severity", Severity)) %>%
  mutate(Disturbance_Desc = ifelse(Disturbance_Desc == "Rx", "RX", Disturbance_Desc))

# edit date column
GRCA_Disturbance01 <- GRCA_Disturbance01 %>%
  mutate(DisturbanceDate = parse_date_time(DisturbanceDate, orders = c("mdy", "my", "y"))) %>%
  separate(DisturbanceDate, sep = "-", into = c("DisturbanceDate_Year", "Month", "Day"), remove = F) %>%
  select(!c(Month, Day, UV2, Disturbance_Info)) %>%
  relocate(DisturbanceDate_Year, .after = DisturbanceDate) %>%
  relocate(Disturbance_Type, .before = Disturbance_Name)

##############################################

IM_Disturbance01 <- IM_MacroplotReport_MetaData %>%
  # filter for relevant columns
  select(c(Park, PlotType_Name, Macroplot, Disturbance_Type, UV2)) %>%
  filter(!is.na(UV2)) %>%
  # add relevant columns
  separate(UV2, sep = ": ", into = c("Entry", "Disturbance_Info"), remove = FALSE) %>%
  separate(Disturbance_Info, sep = ",", into = c("Disturbance_Name", "Disturbance_Desc", "DisturbanceDate", "Severity"), remove = FALSE) %>%
  # trim white space
  mutate(across(where(is.character), str_trim)) %>%
  # create Entry column
  mutate(Entry = 1) %>%
  # edit Date column
  mutate(DisturbanceDate = parse_date_time(DisturbanceDate, orders = c("mdy", "my", "y"))) %>%
  separate(DisturbanceDate, sep = "-", into = c("DisturbanceDate_Year", "Month", "Day"), remove = F) %>%
  select(!c(Month, Day, UV2, Disturbance_Info)) %>%
  # reorder columns
  relocate(DisturbanceDate_Year, .after = DisturbanceDate) %>%
  relocate(Disturbance_Type, .before = Disturbance_Name)

##############################################

WACA_Disturbance01 <- WACA_MacroplotReport_MetaData %>%
  # filter for relevant columns
  select(c(Park, PlotType_Name, Macroplot, Disturbance_Type, UV2)) %>%
  filter(!is.na(UV2)) %>%
  # add relevant columns
  separate(UV2, sep = ": ", into = c("Entry", "Disturbance_Info"), remove = FALSE) %>%
  separate(Disturbance_Info, sep = ",", into = c("Disturbance_Name", "Disturbance_Desc", "DisturbanceDate", "Severity"), remove = FALSE) %>%
  # trim white space
  mutate(across(where(is.character), str_trim)) %>%
  # create Entry column
  mutate(Entry = 1) %>%
  # edit Date column
  mutate(DisturbanceDate = parse_date_time(DisturbanceDate, orders = c("mdy", "my", "y"))) %>%
  separate(DisturbanceDate, sep = "-", into = c("DisturbanceDate_Year", "Month", "Day"), remove = F) %>%
  select(!c(Month, Day, UV2, Disturbance_Info)) %>%
  # reorder columns
  relocate(DisturbanceDate_Year, .after = DisturbanceDate) %>%
  relocate(Disturbance_Type, .before = Disturbance_Name)
```

#### 02 Disturbance

```{r}
# add add relevant columns
GRCA_Disturbance02 <- GRCA_MacroplotReport_MetaData %>%
  select(c(Park, PlotType_Name, Macroplot, Disturbance_Type, UV3)) %>%
  filter(!is.na(UV3)) %>%
  separate(UV3, sep = ": ", into = c("Entry", "Disturbance_Info"), remove = FALSE) %>%
  separate(Disturbance_Info, sep = ",", into = c("Disturbance_Name", "Disturbance_Desc", "DisturbanceDate", "Severity"), remove = FALSE) %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(Entry = 2) %>%
  select(!c("Disturbance_Info"))

# specific edits
GRCA_Disturbance02 <-GRCA_Disturbance02 %>%
  mutate(Disturbance_Name = ifelse(Macroplot == "PIAB 07", "NW 1,3,5", Disturbance_Name),
         Disturbance_Desc = ifelse(Macroplot == "PIAB 07", "Rx", Disturbance_Desc),
         DisturbanceDate = ifelse(Macroplot == "PIAB 07", "11/10/2007", DisturbanceDate),
         Severity = ifelse(Macroplot == "PIAB 07", "Low Severity", Severity)) %>%
  mutate(Disturbance_Name = ifelse(Macroplot == "PIAB 25", "NW 1,3,5", Disturbance_Name),
         Disturbance_Desc = ifelse(Macroplot == "PIAB 25", "Rx", Disturbance_Desc),
         DisturbanceDate = ifelse(Macroplot == "PIAB 25", "11/10/2007", DisturbanceDate),
         Severity = ifelse(Macroplot == "PIAB 25", "Moderate/Low Severity", Severity)) %>%
  mutate(Disturbance_Name = ifelse(Macroplot == "PIPN 01", "NW 1,3,5", Disturbance_Name),
         Disturbance_Desc = ifelse(Macroplot == "PIPN 01", "Rx", Disturbance_Desc),
         DisturbanceDate = ifelse(Macroplot == "PIPN 01", "11/10/2007", DisturbanceDate),
         Severity = ifelse(Macroplot == "PIPN 01", "Moderate/Low Severity", Severity)) %>%
  mutate(Disturbance_Name = ifelse(Macroplot == "PIPN 02", "NW 1,3,5", Disturbance_Name),
         Disturbance_Desc = ifelse(Macroplot == "PIPN 02", "Rx", Disturbance_Desc),
         DisturbanceDate = ifelse(Macroplot == "PIPN 02", "11/10/2007", DisturbanceDate),
         Severity = ifelse(Macroplot == "PIPN 02", "Moderate/Low Severity", Severity)) %>%
  mutate(Disturbance_Name = ifelse(Macroplot == "PIPO 24", "Watson 2,3,4", Disturbance_Name),
         Disturbance_Desc = ifelse(Macroplot == "PIPO 24", "Rx", Disturbance_Desc),
         DisturbanceDate = ifelse(Macroplot == "PIPO 24", "10/14/2011", DisturbanceDate),
         Severity = ifelse(Macroplot == "PIPO 24", "Low Severity", Severity)) %>%
  mutate(Disturbance_Desc = ifelse(Disturbance_Desc == "Rx", "RX", Disturbance_Desc)) %>%
  select(!c("UV3"))

# edit date column
GRCA_Disturbance02 <- GRCA_Disturbance02 %>%
  mutate(DisturbanceDate = parse_date_time(DisturbanceDate, orders = c("mdy", "my", "y"))) %>%
  separate(DisturbanceDate, sep = "-", into = c("DisturbanceDate_Year", "Month", "Day"), remove = F) %>%
  select(!c("Month", "Day")) %>%
  relocate(DisturbanceDate_Year, .after = DisturbanceDate) %>%
  relocate(Disturbance_Type, .before = Disturbance_Name)
```

#### 03 Disturbance

```{r}
# add add relevant columns
GRCA_Disturbance03 <- GRCA_MacroplotReport_MetaData %>%
  select(c(Park, PlotType_Name, Macroplot, Disturbance_Type, UV4)) %>%
  filter(!is.na(UV4)) %>%
  separate(UV4, sep = ": ", into = c("Entry", "Disturbance_Info"), remove = FALSE) %>%
  separate(Disturbance_Info, sep = ",", into = c("Disturbance_Name", "Disturbance_Desc", "DisturbanceDate", "Severity"), remove = FALSE) %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(Entry = 3) %>%
  select(!c("Disturbance_Info"))

# specific edits
GRCA_Disturbance03 <- GRCA_Disturbance03 %>%
  mutate(Disturbance_Name = ifelse(Macroplot == "PIPO 17", "Watson 2,3,4", Disturbance_Name),
         Disturbance_Desc = ifelse(Macroplot == "PIPO 17", "Rx", Disturbance_Desc),
         DisturbanceDate = ifelse(Macroplot == "PIPO 17", "10/14/2011", DisturbanceDate),
         Severity = ifelse(Macroplot == "PIPO 17", "Low Severity", Severity)) %>%
  mutate(Disturbance_Name = ifelse(Macroplot == "PIPO 20", "Watson 2,3,4", Disturbance_Name),
         Disturbance_Desc = ifelse(Macroplot == "PIPO 20", "Rx", Disturbance_Desc),
         DisturbanceDate = ifelse(Macroplot == "PIPO 20", "10/14/2011", DisturbanceDate),
         Severity = ifelse(Macroplot == "PIPO 20", "Low Severity", Severity)) %>%
  mutate(Disturbance_Name = ifelse(Macroplot == "PIPO 21", "Watson 2,3,4", Disturbance_Name),
         Disturbance_Desc = ifelse(Macroplot == "PIPO 21", "Rx", Disturbance_Desc),
         DisturbanceDate = ifelse(Macroplot == "PIPO 21", "10/14/2011", DisturbanceDate),
         Severity = ifelse(Macroplot == "PIPO 21", "Low Severity", Severity)) %>%
  mutate(Disturbance_Name = ifelse(Macroplot == "PIPO 22", "Watson 2,3,4", Disturbance_Name),
         Disturbance_Desc = ifelse(Macroplot == "PIPO 22", "Rx", Disturbance_Desc),
         DisturbanceDate = ifelse(Macroplot == "PIPO 22", "10/14/2011", DisturbanceDate),
         Severity = ifelse(Macroplot == "PIPO 22", "Low Severity", Severity)) %>%
  mutate(Disturbance_Desc = ifelse(Disturbance_Desc == "Rx", "RX", Disturbance_Desc)) %>%
  select(!c("UV4"))

# edit date column
GRCA_Disturbance03 <- GRCA_Disturbance03 %>%
  mutate(DisturbanceDate = parse_date_time(DisturbanceDate, orders = c("mdy", "my", "y"))) %>%
  separate(DisturbanceDate, sep = "-", into = c("DisturbanceDate_Year", "Month", "Day"), remove = F) %>%
  select(!c("Month", "Day")) %>%
  relocate(DisturbanceDate_Year, .after = DisturbanceDate) %>%
  relocate(Disturbance_Type, .before = Disturbance_Name)
```

#### 04 Disturbance

```{r}
GRCA_Disturbance04 <- GRCA_MacroplotReport_MetaData %>%
  select(c(Park, PlotType_Name, Macroplot, Disturbance_Type, UV5)) %>%
  filter(!is.na(UV5)) %>%
  # add add relevant columns
  separate(UV5, sep = ": ", into = c("Entry", "Disturbance_Info"), remove = FALSE) %>%
  separate(Disturbance_Info, sep = ",", into = c("Disturbance_Name", "Disturbance_Desc", "DisturbanceDate", "Severity"), remove = FALSE) %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(Entry = 4) %>%
  select(!c("Disturbance_Info", "UV5")) %>%
  # edit date column
  mutate(DisturbanceDate = parse_date_time(DisturbanceDate, orders = c("mdy", "my", "y"))) %>%
  separate(DisturbanceDate, sep = "-", into = c("DisturbanceDate_Year", "Month", "Day"), remove = F) %>%
  select(!c("Month", "Day")) %>%
  relocate(DisturbanceDate_Year, .after = DisturbanceDate) %>%
  relocate(Disturbance_Type, .before = Disturbance_Name)
```

#### All Disturbances

```{r}
GRCA_DisturbanceHistory <- rbind(GRCA_Disturbance01, GRCA_Disturbance02, GRCA_Disturbance03, GRCA_Disturbance04)

##############################################

IM_DisturbanceHistory <- rbind(IM_Disturbance01)

##############################################

WACA_DisturbanceHistory <- rbind(WACA_Disturbance01)
```

```{r}
# merge all disturbances
all_DisturbanceHistory <- rbind(GRCA_DisturbanceHistory, IM_DisturbanceHistory, WACA_DisturbanceHistory)

# reorganize severity column for consistency
all_DisturbanceHistory <- all_DisturbanceHistory %>%
  mutate(Disturbance_Severity = ifelse(Severity %in% c("Low Severity"), "Low",
                                       ifelse(Severity %in% c("Moderate/Low Severity", "Moderate/Low Sev.", "Mod/Low Severity"), "Low/Mod",
                                              ifelse(Severity %in% c("Moderate Severity"), "Mod",
                                                     ifelse(Severity %in% c("Moderate/High Severity", "Mod-High Severity"), "Mod/High",
                                                            ifelse(Severity %in% c("High Severity"), "High",
                                                                   ifelse(Severity %in% c("Unburned"), "Unburned", NA))))))) %>%
  select(!Severity)
```

## Merge

```{r}
# merge metadata and sample events
GRCA_MacroplotReport_all <- merge(GRCA_MacroplotReport_MetaData, GRCA_MacroplotReport_SampleEvents, by = c("Macroplot")) %>%
  select(!MonStatusOrd)

# merge Macroplot Report and Sample Event Report
GRCA_MetadataReport <- merge(GRCA_MacroplotReport_all, GRCA_SampleEventReport_merge, by = c("Macroplot", "SampleEventDate", "MonStatus")) %>%
  relocate(AdministrationUnit_Name, .before = Park) %>%
  relocate(ProjectUnit_Name, .before = Purpose) %>%
  relocate(Macroplot, .after = MetaData) %>%
  relocate(SampleEventDate, .before = SampleEventDate_Year) %>%
  relocate(MonStatus, .after = LegacyMonStatus)

##############################################

# merge metadata and sample events
IM_MacroplotReport_all <- merge(IM_MacroplotReport_MetaData, IM_MacroplotReport_SampleEvents, by = c("Macroplot")) %>%
  select(!MonStatusOrd)

# merge Macroplot Report and Sample Event Report
IM_MetadataReport <- merge(IM_MacroplotReport_all, IM_SampleEventReport_merge, by = c("Macroplot", "SampleEventDate", "MonStatus")) %>%
  relocate(AdministrationUnit_Name, .before = Park) %>%
  relocate(ProjectUnit_Name, .before = Purpose) %>%
  relocate(Macroplot, .after = MetaData) %>%
  relocate(SampleEventDate, .before = SampleEventDate_Year) %>%
  relocate(MonStatus, .after = LegacyMonStatus)

##############################################

# merge metadata and sample events
WACA_MacroplotReport_all <- merge(WACA_MacroplotReport_MetaData, WACA_MacroplotReport_SampleEvents, by = c("Macroplot")) %>%
  select(!MonStatusOrd)

# merge Macroplot Report and Sample Event Report
WACA_MetadataReport <- merge(WACA_MacroplotReport_all, WACA_SampleEventReport_merge, by = c("Macroplot", "SampleEventDate", "MonStatus")) %>%
  relocate(AdministrationUnit_Name, .before = Park) %>%
  relocate(ProjectUnit_Name, .before = Purpose) %>%
  relocate(Macroplot, .after = MetaData) %>%
  relocate(SampleEventDate, .before = SampleEventDate_Year) %>%
  relocate(MonStatus, .after = LegacyMonStatus)
```

```{r}
# merge all admin units
all_MetadataReport <- rbind(GRCA_MetadataReport, IM_MetadataReport, WACA_MetadataReport)

# merge monitoring status summary
all_MetadataReport <- merge(all_MetadataReport, all_MonStatus, by = c("MonStatus"), all = T) %>%
  relocate(MonStatus, .before = MonStatusOrd) %>%
  relocate(LegacyMonStatus, .before = MonStatus)

# merge disturbance history summary
all_MetadataReport <- merge(all_MetadataReport, all_DisturbanceHistory, by = c("Park", "PlotType_Name", "Macroplot", "Entry", "Disturbance_Type"), all = T) %>%
  relocate(AdministrationUnit_Name, .before = Park) %>%
  relocate(Macroplot, .after = MetaData) %>%
  relocate(Entry, .after = MonStatus) %>%
  relocate(Disturbance_Type, .before = Disturbance_Name)
```
