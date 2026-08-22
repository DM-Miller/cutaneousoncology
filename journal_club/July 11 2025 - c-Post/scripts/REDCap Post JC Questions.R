# Load packages
# Define the base path for the project
base_path <- here::here()  # For Journal Club, use project root

# For JoCO, it might be:
# base_path <- here::here("perspectives", "Vol_2_Issue_2", "nivo_rela_nivo_ipi_itc")

# Define directories relative to `base_path`
files_dir <- file.path(base_path, "files")
img_dir <- file.path(base_path, "img")
scripts_dir <- file.path(base_path, "scripts")

source(file.path(scripts_dir, "load_packages.R"))



# Load Data
# Function: Try REDCap download with fallback
load_or_fallback_redcap <- function(
    redcap_uri,
    token,
    files_dir,
    subfolder = "Post_JC_survey_unprocessed",
    filename = "survey_results_pre_test.rds",
    timeout_seconds = 15
) {
  # Full subdirectory path for saving/opening
  subdir_path <- file.path(files_dir, subfolder)
  
  # Helper to save
  save_data <- function(data) {
    saveRDS(data, file = file.path(subdir_path, filename))
  }
  
  # Helper to open most recent
  open_recent <- function() {
    open_recent_file(directory = subdir_path)
  }
  
  # Try block
  result <- tryCatch(
    {
      # Attempt REDCap pull with timeout
      message("Attempting to pull data from REDCap...")
      # Temporarily set timeout
      old_timeout <- getOption("timeout")
      options(timeout = timeout_seconds)
      
      redcapr_dt <- REDCapR::redcap_read_oneshot(
        redcap_uri = redcap_uri,
        token = token
      )
      
      # Reset timeout
      options(timeout = old_timeout)
      
      # Extract data
      dt1 <- redcapr_dt$data
      
      # Save
      save_data(dt1)
      message("Successfully downloaded and saved REDCap data.")
      
      # Return the data
      dt1
    },
    error = function(e) {
      # On error
      message("Error occurred: ", conditionMessage(e))
      message("Falling back to loading most recent saved file.")
      
      # Fallback to most recent file
      open_recent()
    }
  )
  
  return(result)
}


dt <- load_or_fallback_redcap(
  redcap_uri = "https://redcap.partners.org/redcap/api/",
  token = "3910D018AF3D663F7D1F2BA5266A7484",
  files_dir = files_dir,
  subfolder = "Post_JC_survey_unprocessed",
  filename = "survey_results_post_test.rds",
  timeout_seconds = 15
)


# Apply formatting corrections to columns 3-
for (col in 1:3) {
  dt[, col] <- stringr::str_replace_all(
    string = dt[, col],
    pattern = regex("__"),
    replacement = "-"
  )
  
  dt[, col] <- stringr::str_replace_all(
    string = dt[, col],
    pattern = regex("_1"),
    replacement = "-1"
  )
  
  dt[, col] <- stringr::str_replace_all(
    string = dt[, col],
    pattern = regex("_"),
    replacement = " "
  )
  
  dt[, col] <- stringr::str_to_title(
    string = dt[, col]
  )
}
# Save processed data

save_files(
  directory = "",
  save_object = dt,
  filename = "survey_results_post_test_processed",
  subD = file.path(
    files_dir,
    "Post_JC_survey_processed")
)

