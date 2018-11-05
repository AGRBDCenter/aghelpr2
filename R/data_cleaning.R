# This script houses data cleaning functions for different types of data.

#' Clean NIR data and put into tidy format. This requires that JA's input identifiers as unique and standardized. 
#' Format: First three letters of the project name, followed by an underscore, followed by the first three letters of the crop, 
#' followed by an underscore, followed by the treatment number as "T1", followed by the block as "B1". If my project were "Mosaic Potato",
#' and I were running treatment 1, block 3, the format would be: "Mos_Pot_T1B3".
#'
#' @param path The path to the file in the directory.
#' @param proj_name The name of the project exactly as found in the "Admin Sheet" on OSF.
#' @param label The first three letters of the project and the first three letters of the crop. ie "Mos_Pot". Whatever was put into NIR machine.
#' @export clean_nir

clean_nir <- function(path, proj_name, label) {
  # Read in the data and filter by the label
  enc <- readr::guess_encoding(path)[[1]][1]
  data <- read.csv(path, fileEncoding = enc, header = FALSE)
  data <- filter(data, grepl(label, data$V4))
  
  # Move the column headers from row values to the next column's name
  for (i in 6:length(data)) {
    if (i %% 2 == 0) {
      data[[i]] <- as.character(data[[i]])
      name <- unique(data[[i]])
      names(data)[[i + 1]] <- name
    }
  }
  
  # Remove columns which only contain column names
  for (i in 6:length(data)) {
    colname <- paste0("V", i)
    for (name in names(data)) {
      if (name == colname) {
        data <- data[, names(data) != name]
      }
    }
  }
  
  # Rub-a-dub-dub squeeky clean data
  data <- data %>% 
    gather(variables, value, 6:length(data)) %>% 
    filter(!is.na(value)) %>% 
    mutate(project_name = proj_name) %>% 
    rename(date_collected = V1) %>% 
    mutate(treatments = gsub("T", "", regmatches(V4, regexpr("T([0-9]+)", V4)), ignore.case = TRUE),
           replications = gsub("B", "", regmatches(V4, regexpr("B([0-9]+)", V4)), ignore.case = TRUE),
           date_collected = lubridate::mdy(date_collected)) %>% 
    select(date_collected, project_name, variables, treatments, replications, value)
  
  return(data)
}


#' Clean harvester (Haldrup) data and put into tidy format. This requires that we know the direction of harvest, the direction of the plot map, and a standardized name for the harvest.
#'
#' @param path The path to the file in the directory.
#' @export clean_harvester

clean_harvester <- function(path) {
  return(data)
}


#' Clean Dr. Hopkin's lab results data. This requires that the project name be included in the file treatments column.
#'
#' @param path The path to the file in the directory.
#' @export clean_labresults

clean_labresults <- function(path) {
  return(data)
}


#' Clean potato weights (harvest) data. This requires that the data be in a standardized format which will be determined.
#'
#' @param path The path to the file in the directory.
#' @export clean_potato

clean_potato <- function(path) {
  return(data)
}


#' Clean specific gravity. This requires that the data be in a standardized format which will be determined.
#'
#' @param path The path to the file in the directory.
#' @export clean_specgrav

clean_specgrav <- function(path) {
  return(data)
}