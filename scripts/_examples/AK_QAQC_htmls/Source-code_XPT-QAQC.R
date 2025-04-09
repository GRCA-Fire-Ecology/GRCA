# Source Code for FFI Export QAQC

# Create Blank error table
errors_blank <- data.frame(SavedQuery = "",
                           MacroPlot.Name = "",
                           Date = "",
                           Error = "No Error", 
                           Comment = "")

# Create Error checking functions
qaqc_header <- function(Pts_header, query, query_message, values_check) {
  # Identify errors in the data based on the provided check condition
  errors <- Pts_header %>%   
    # Fill new columns: "SavedQuery" with the query parameter and "Error"
    # which concatenates the query_message and values_data
    mutate(SavedQuery = query, 
           MacroPlot.Name = MacroPlot.Name,
           Date = Date,
           Error = paste(query_message, " = ", values_data),
           Comment = Comment) %>%
    # Filter the data to find rows where values_check condition is TRUE
    # use replace_na to treat NA values as TRUE (which means they pass the check)
    filter(!(values_check) %>% replace_na(TRUE)) %>%   
    # Select specific columns to include the errors data frame
    select(SavedQuery, MacroPlot.Name, Date, Error, Comment)
  
  # If no errors are identified, add "No Errors" to existing data frame 
  if (nrow(errors) == 0) {   
    # If no errors, replace errors with blank data frame and add the Saved Query
    errors <- errors_blank %>%     
      mutate(SavedQuery = query) 
  } else {   
    # if there are errors, retain the errors data frame as is
    errors <- errors
  }
}

qaqc_data <- function(Pts_data, query, query_message, values_check) {
  # Identify errors in the data based on the provided check condition
  errors <- Pts_data %>%   
    # Fill new columns: "SavedQuery" with the query parameter and "Error"
    # which concatenates the query_message and values_data
    mutate(SavedQuery = query, 
           MacroPlot.Name = MacroPlot.Name,
           Date = Date,
           Error = paste(query_message, " = ", values_data),
           Comment = Comment) %>%
    # Filter the data to find rows where values_check condition is TRUE
    # use replace_na to treat NA values as TRUE (which means they pass the check)
    filter(!(values_check) %>% replace_na(TRUE)) %>%   
    # Select specific columns to include the errors data frame
    select(SavedQuery, MacroPlot.Name, Date, Error, Comment)
  
  # If no errors are identified, add "No Errors" to existing data frame 
  if (nrow(errors) == 0) {   
    # If no errors, replace errors with blank data frame and add the Saved Query
    errors <- errors_blank %>%     
      mutate(SavedQuery = query) 
  } else {   
    # if there are errors, retain the errors data frame as is
    errors <- errors
  }
}
