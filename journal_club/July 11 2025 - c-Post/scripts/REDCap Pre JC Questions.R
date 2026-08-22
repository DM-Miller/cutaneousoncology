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
    subfolder = "Pre_JC_survey_unprocessed",
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
  token = "95FCBA5C9E231A6CDE8C7886836042E2",
  files_dir = files_dir,
  subfolder = "Pre_JC_survey_unprocessed",
  filename = "survey_results_pre_test.rds",
  timeout_seconds = 15
)


#redcapr_dt <- REDCapR::redcap_read_oneshot(
#  redcap_uri = "https://redcap.partners.org/redcap/api/",
#  token = "95FCBA5C9E231A6CDE8C7886836042E2",
#)
#
#dt1 <- redcapr_dt$data

dt[,2] <- stringr::str_replace_all(
  string = dt[,2],
  pattern = regex("__"),
  replacement = "-"
)


dt[,2] <- stringr::str_replace_all(
  string = dt[,2],
  pattern = regex("_1"),
  replacement = ", "
)

dt[,2] <- stringr::str_replace_all(
  string = dt[,2],
  pattern = regex("_"),
  replacement = " "
)

dt[,2] <- stringr::str_to_title(
  string = dt[,2]
)

dt[,3] <- stringr::str_replace_all(
  string = dt[,3],
  pattern = regex("__"),
  replacement = "-"
)

dt[,3] <- stringr::str_replace_all(
  string = dt[,3],
  pattern = regex("_1"),
  replacement = ", "
)

dt[,3] <- stringr::str_replace_all(
  string = dt[,3],
  pattern = regex("_"),
  replacement = " "
)

dt[,3] <- stringr::str_to_title(
  string = dt[,3]
)

dt[,4] <- stringr::str_replace_all(
  string = dt[,4],
  pattern = regex("__"),
  replacement = "-"
)

dt[,4] <- stringr::str_replace_all(
  string = dt[,4],
  pattern = regex("_1"),
  replacement = "-1"
)

dt[,4] <- stringr::str_replace_all(
  string = dt[,4],
  pattern = regex("_"),
  replacement = " "
)

dt[,4] <- stringr::str_to_title(
  string = dt[,4]
)

dt[,5] <- stringr::str_replace_all(
  string = dt[,5],
  pattern = regex("__"),
  replacement = "-"
)

dt[,5] <- stringr::str_replace_all(
  string = dt[,5],
  pattern = regex("_1"),
  replacement = ", "
)

dt[,5] <- stringr::str_replace_all(
  string = dt[,5],
  pattern = regex("_"),
  replacement = " "
)

dt[,5] <- stringr::str_to_title(
  string = dt[,5]
)

dt[,6] <- stringr::str_replace_all(
  string = dt[,6],
  pattern = regex("__"),
  replacement = "-"
)

dt[,6] <- stringr::str_replace_all(
  string = dt[,6],
  pattern = regex("_1"),
  replacement = ", "
)

dt[,6] <- stringr::str_replace_all(
  string = dt[,6],
  pattern = regex("_"),
  replacement = " "
)

dt[,6] <- stringr::str_to_title(
  string = dt[,6]
)
dt[,7] <- stringr::str_replace_all(
  string = dt[,7],
  pattern = regex("__"),
  replacement = "-"
)

dt[,7] <- stringr::str_replace_all(
  string = dt[,7],
  pattern = regex("_1"),
  replacement = "-1"
)

dt[,7] <- stringr::str_replace_all(
  string = dt[,7],
  pattern = regex("_"),
  replacement = " "
)

dt[,7] <- stringr::str_to_title(
  string = dt[,7]
)
dt[,8] <- stringr::str_replace_all(
  string = dt[,8],
  pattern = regex("__"),
  replacement = "-"
)

dt[,8] <- stringr::str_replace_all(
  string = dt[,8],
  pattern = regex("_1"),
  replacement = "-1"
)

dt[,8] <- stringr::str_replace_all(
  string = dt[,8],
  pattern = regex("_"),
  replacement = " "
)

dt[,8] <- stringr::str_to_title(
  string = dt[,8]
)

dt[,9] <- stringr::str_replace_all(
  string = dt[,9],
  pattern = regex("_"),
  replacement = " "
)

#dt[,9] <- stringr::str_replace_all(
#  string = dt[,9],
#  pattern = regex("_1"),
#  replacement = "-1"
#)

#dt[,9] <- stringr::str_replace_all(
#  string = dt[,9],
#  pattern = regex("_"),
#  replacement = " "
#)

dt[,9] <- stringr::str_to_title(
  string = dt[,9]
)

dt[,10] <- stringr::str_replace_all(
  string = dt[,10],
  pattern = regex("__"),
  replacement = "-"
)

#dt[,10] <- stringr::str_replace_all(
#  string = dt[,10],
#  pattern = regex("_1"),
#  replacement = "-1"
#)

dt[,10] <- stringr::str_replace_all(
  string = dt[,10],
  pattern = regex("_"),
  replacement = " "
)

dt[,10] <- stringr::str_to_title(
  string = dt[,10]
)
dt[,11] <- stringr::str_replace_all(
  string = dt[,11],
  pattern = regex("__"),
  replacement = "-"
)

#dt[,11] <- stringr::str_replace_all(
#  string = dt[,11],
#  pattern = regex("_1"),
#  replacement = "-1"
#)

dt[,11] <- stringr::str_replace_all(
  string = dt[,11],
  pattern = regex("_"),
  replacement = " "
)

dt[,11] <- stringr::str_to_title(
  string = dt[,11]
)

dt[,12] <- stringr::str_replace_all(
  string = dt[,12],
  pattern = regex("__"),
  replacement = "-"
)

#dt[,12] <- stringr::str_replace_all(
#  string = dt[,12],
#  pattern = regex("_1"),
#  replacement = "-1"
#)

dt[,12] <- stringr::str_replace_all(
  string = dt[,12],
  pattern = regex("_"),
  replacement = " "
)

dt[,12] <- stringr::str_to_title(
  string = dt[,12]
)

# Apply formatting corrections to columns 12 through 22
for (col in 1:14) {
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

save_files(
  directory = "", # this is b/c files_dir has the relevent information
  save_object = dt,
  filename = "survey_results_pre_test_processed",
  subD = file.path(
    files_dir,
    "Pre_JC_survey_processed"),
  extension = ".csv"
)

save_files(
  directory = "", # this is b/c files_dir has the relevent information
  save_object = dt,
  filename = "survey_results_pre_test_processed",
  subD = file.path(
    files_dir,
    "Pre_JC_survey_processed")
)

