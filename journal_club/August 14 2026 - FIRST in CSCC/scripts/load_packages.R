library(tidyverse)
library(plotly)
library(gt)
library(glue)
library(here)
library(survminer)
library(survival)
source(file.path(scripts_dir, "save files.R"))
source(file.path(scripts_dir, "open most recent file.R"))

#------------------------------------------

get_most_recent_file_name <- function(
    directory,
    ext = c(".rds", ".xlsx", ".csv")
){
  # List all files matching extension
  files <- list.files(
    path = directory,
    pattern = paste(ext, collapse = "|"),
    full.names = FALSE  # just the filename
  )
  
  # Exclude temporary files
  files <- files[!grepl("~\\$", basename(files))]
  
  if(length(files) == 0){
    stop("No files found with the specified extensions in the directory.")
  }
  
  # Get info and pick most recent
  files_info <- file.info(file.path(directory, files))
  most_recent_file <- files[which.max(files_info$mtime)]
  
  return(most_recent_file)
}