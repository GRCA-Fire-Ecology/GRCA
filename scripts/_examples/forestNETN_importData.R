#' @title importData: Import views directly from NETN forest database
#'
#' @description This function imports all views in the ANALYSIS schema of the NETN_Forest backend. Each view
#' is added to a VIEWS_NETN environment in your workspace, or to your global environment based on whether
#' new_env = TRUE or FALSE. You must have the latest ODBC SQL driver installed for this function to
#' work. It can be downloaded from: https://go.microsoft.com/fwlink/?linkid=2168524
#'
#' @importFrom dplyr collect rename tbl
#' @importFrom magrittr %>%
#'
#' @param instance Specify whether you are connecting to the local instance or server.
#' \describe{
#' \item{"local"}{Default. Connects to local install of backend database}
#' \item{"server"}{Connects to main backend on server. Note that you must have permission to access the server, and
#' connection speeds are likely to be much slower than the local instance. You must also be connected to VPN or NPS network.}}
#'
#' @param server Quoted name of the server to connect to, if instance = "server". Valid input is the server
#' address to connect to the main database. If connecting to the local instance, leave blank.
#'
#' @param new_env Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
#' views in VIEWS_NETN environment. If \code{FALSE}, stores views in global environment
#'
#' @param name Character. Specifies the name of the database. Default is "NETN_Forest"
#'
#'
#' @examples
#' \dontrun{
#' # Import using default settings of local instance, server = 'localhost' and add VIEWS_NETN environment
#' importData()
#'
#' # Import using computer name (# should be real numbers)
#' importData(server = "INPNETN-######", new_env = TRUE)
#'
#' # Import from main database on server
#' importData(server = "INP###########\\########", instance = "server", new_env = TRUE)
#' }
#'
#' @return NETN database views in specified environment
#'
#' @export


importData <- function(instance = c("local", "server"), server = NA, name = "NETN_Forest", new_env = TRUE){

  options(scipen = 100) # for TSNs
  instance <- match.arg(instance)

  # Check that suggested package required for this function are installed
  if(!requireNamespace("odbc", quietly = TRUE)){
    stop("Package 'odbc' needed for this function to work. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("DBI", quietly = TRUE)){
    stop("Package 'DBI' needed for this function to work. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("dbplyr", quietly = TRUE)){
    stop("Package 'dbplyr' needed for this function to work. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("sf", quietly = TRUE)){
    stop("Package 'sf' needed for this function to work. Please install it.", call. = FALSE)
  }

  # Set up connection
  server_con <- ifelse(instance == 'local', "localhost\\SQLEXPRESS", server)

  error_mess <- paste("Unable to connect to SQL database.",
                      ifelse(instance == 'server',
                             paste0("Make sure you are connected to VPN or NPS network, and server is spelled correctly."),
                             paste0("Make sure you have a local installation of the database (see examples).")))

  # Test connection. If successful, continues to next step. If fails, exits function with error message.

  tryCatch(
    con <- odbc::dbConnect(odbc::odbc(),
                           Driver = "ODBC Driver 17 for SQL Server",
                           Server = server_con,
                           Database = name,
                           Trusted_Connection = "Yes"),

    error = function(e){
      stop(error_mess)
    },
    warning = function(w){
      stop(error_mess)
    }
  )

  # Fetch names of views
  view_list_db <- DBI::dbListTables(con, schema = "ANALYSIS")

  # Setup progress bar
  pb <- txtProgressBar(min = 0, max = length(view_list_db), style = 3)

  # Import views using their names and show progress bar
  view_import <- lapply(seq_along(view_list_db), function(x){
    setTxtProgressBar(pb, x)
    view <- view_list_db[x]
    tab <- tbl(con, dbplyr::in_schema("ANALYSIS", view)) %>% collect() %>%
      as.data.frame()
    return(tab)
  })

  close(pb)
  DBI::dbDisconnect(con)

  # Rename to all NETN at the end
  view_list <- lapply(seq_along(view_list_db), function(x){
    paste0(substr(view_list_db[[x]], 1,
                  nchar(view_list_db[[x]]) - 5),
           "_NETN")
  })

  view_import <- setNames(view_import, view_list)

  if(new_env == TRUE){
    VIEWS_NETN <<- new.env()
    list2env(view_import, envir = VIEWS_NETN)
  } else {
    list2env(view_import, envir = .GlobalEnv)}

  # Add Lat/Long to Plots_NETN
  env <- if(exists("VIEWS_NETN")){VIEWS_NETN} else {.GlobalEnv}
  plots <- get("Plots_NETN", envir = env)

  plots_sf19 <- plots |> filter(ZoneCode == "19N") |>
    select(Plot_Name, xCoordinate, yCoordinate) |>
    sf::st_as_sf(coords = c("xCoordinate", "yCoordinate"), crs = 26919) |>
    sf::st_transform(crs = 4326)

  plots_sf18 <- plots |> filter(ZoneCode == "18N") |>
    select(Plot_Name, xCoordinate, yCoordinate) |>
    sf::st_as_sf(coords = c("xCoordinate", "yCoordinate"), crs = 26918) |>
    sf::st_transform(crs = 4326)

  plots_19 <- data.frame(Plot_Name = plots_sf19$Plot_Name,
                         Long = sf::st_coordinates(plots_sf19)[,1],
                         Lat = sf::st_coordinates(plots_sf19)[,2])

  plots_18 <- data.frame(Plot_Name = plots_sf18$Plot_Name,
                         Long = sf::st_coordinates(plots_sf18)[,1],
                         Lat = sf::st_coordinates(plots_sf18)[,2])

  plots_wgs1 <- rbind(plots_19, plots_18)

  plot_wgs <- left_join(plots, plots_wgs1, by = "Plot_Name")

  if(new_env == TRUE){VIEWS_NETN$Plots_NETN <- plot_wgs
  } else Plots_NETN <- plot_wgs

  print(ifelse(new_env == TRUE,
               paste0("Import complete. Views are located in VIEWS_NETN environment."),
               paste0("Import complete. Views are located in global environment.")), quote = FALSE)

}
