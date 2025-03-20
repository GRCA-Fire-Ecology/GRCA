# Consolidate actual data
data_temp1 <- HerbsPoints_data %>%
  filter(Order == 1) %>%
  group_by(MacroPlot.Name, Date, Transect, Point, Order) %>%
  dplyr::summarize()

# Consolidate ideal data
data_temp_macroplots <- HerbsPoints_header %>%
  select(MacroPlot.Name, Date)
data_temp_transects <- as.data.frame(1:2)
colnames(data_temp_transects) <- "Transect"
data_temp_points <- as.data.frame(1:166)
colnames(data_temp_points) <- "Point"
data_temp_order <- as.data.frame(1)
colnames(data_temp_order) <- "Order"
data_temp_merge <- merge(data_temp_macroplots, data_temp_transects)
data_temp_merge <- merge(data_temp_merge, data_temp_points)
data_temp_merge <- merge(data_temp_merge, data_temp_order) %>%
  arrange(MacroPlot.Name, Date, Transect, Point, Order)

# Isolate missing data and add to main
data_temp2 <- anti_join(data_temp_merge, data_temp1) %>%
  mutate(Missing = Point)
data_temp <- merge(data_temp1, data_temp2, all = TRUE)
data_temp$Missing[is.na(data_temp$Missing)] <- 0

# Remove excess data frames
remove(data_temp1, data_temp2, data_temp_macroplots, data_temp_points, data_temp_transects, data_temp_order, data_temp_merge)


# Set parameters
data <- merge(HerbsPoints_data, data_temp, by = c("MacroPlot.Name", "Date", "Transect", "Point", "Order"), all = T)
query <- "Cvr Pts Missing"
query_message <- paste0("Transect = ", data$Transect, ". ", "Order = 1. Missing Point")
values_data <- data$Missing
values_valid <- 0
values_check <- (values_data == values_valid) %>% replace_na(TRUE)

# Identify errors
errors_HerbsPoints_Point_Missing <- qaqc(data, query, query_message, values_check)
