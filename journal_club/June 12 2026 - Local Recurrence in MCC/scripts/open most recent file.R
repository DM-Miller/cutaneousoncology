# SUMMARY:
# This function `open_recent_file()` searches for the most recently modified file 
# in a specified directory with one of the allowed extensions (.rds, .xlsx, .csv).
# It excludes temporary files, identifies the most recent file, reads it based on 
# its format, and returns the data after removing empty columns.

# Load necessary libraries
library(tidyverse)  # Collection of packages for data manipulation and visualization
library(readxl)      # Enables reading Excel files

# Function to open the most recently modified file in a given directory
open_recent_file <- function(
    directory,
    ext = c(".rds", ".xlsx", ".csv")
){
  files <- list.files(path = directory, pattern = paste(ext, collapse = "|"), full.names = TRUE)
  files <- files[!grepl("~\\$", basename(files))]
  
  if(length(files) == 0) {
    cat("No files found with the specified extensions in the directory.")
    return(NULL)
  }
  
  files_info <- file.info(files)
  most_recently_modified <- files[which.max(files_info$mtime)]
  file_ext <- tools::file_ext(most_recently_modified)
  
  most_recent_data <- NULL
  
  if (file_ext == "rds") {
    most_recent_data <- readRDS(most_recently_modified)
  } else if (file_ext == "xlsx") {
    most_recent_data <- readxl::read_xlsx(most_recently_modified)
  } else if (file_ext == "csv") {
    most_recent_data <- read_csv(most_recently_modified)
  } else {
    cat("The file extension is not supported.")
    return(NULL)
  }
  
  # Only try removing NA columns if it's a data frame
  if (is.data.frame(most_recent_data)) {
    most_recent_data <- most_recent_data[, !apply(is.na(most_recent_data), 2, all)]
  }
  
  return(most_recent_data)
}
