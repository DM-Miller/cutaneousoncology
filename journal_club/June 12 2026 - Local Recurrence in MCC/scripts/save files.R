# Load necessary libraries
library(tidyverse)  # Collection of packages for data manipulation and visualization
library(here)       # Helps manage file paths in a project-friendly way
library(writexl)    # Enables writing data frames to Excel files

# Define a function to save files in different formats
save_files <- function(
    save_object = dt,  # The object (data frame, list, etc.) to be saved
    filename = "",     # The base filename without extension
    directory = here::here(), # Default directory is the project's root
    subD = "files",    # Subdirectory where the file will be saved
    extension = ".rds" # File extension to determine the format
){
  
  # Define the main directory path
  followup_study_dir <- directory
  
  # Store the subdirectory name
  subD <- subD
  
  # Store the provided filename
  name <- filename 
  
  # Get the current system time formatted as "YYYY-MM-DD-HH-MM-SS" for unique filenames
  file_date <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S") 
  
  # Store the provided file extension
  ext <- extension
  
  # Construct the full file path
  file_name_save <- paste0(
    followup_study_dir, # Base directory
    "/",
    subD,              # Subdirectory
    "/",
    name,              # File name
    "-",
    file_date,         # Timestamp to ensure unique filename
    ext                # File extension
  )
  
  # Check the file extension and save the object accordingly
  if (ext == ".rds") {
    saveRDS(
      object = save_object, # Save the object in RDS format (for R data storage)
      file = file_name_save # Save to the constructed file path
    )
  } else if (ext == ".xlsx") {
    write_xlsx(
      x = save_object,      # Save the object in Excel format
      path = file_name_save # Save to the constructed file path
    )
  } else if (ext == ".csv") {
    write_csv(
      x = save_object,      # Save the object in CSV format
      file = file_name_save # Save to the constructed file path
    )
  } else {
    # Stop execution if an unsupported extension is provided
    stop("Unsupported file extension.")
  }
  
  # Print a message indicating where the file was saved
#  cat("File saved to:", file_name_save, "\n")
}
