# Process_Survey_Data.R
# Main script to load and clean REDCap survey data

library(tidyverse)
library(REDCapR)
library(here)

# ---- Setup paths ----
base_path <- here::here()
files_dir <- file.path(base_path, "files")
scripts_dir <- file.path(base_path, "scripts")

# ---- Load helpers and config ----
source(file.path(scripts_dir, "REDCap Helpers.R"))
source(file.path(scripts_dir, "load_packages.R"))
source(file.path(files_dir, "config/config.R"))

# ---- Load data dictionary ----
choice_map <- get_dictionary_choices()

# ---- Pull REDCap data ----
dt <- load_or_fallback_redcap(
  redcap_uri = REDCAP_URI,
  token = REDCAP_TOKEN,
  files_dir = files_dir,
  subfolder = "Pre_JC_survey_unprocessed",
  filename = "survey_results_test.rds"
)

# ---- Clean radio/dropdown fields ----
dt_clean <- clean_all_survey_data(dt, choice_map)

# ---- Prepare all checkbox data for plotting ----
checkbox_data <- prepare_all_checkboxes(dt_clean, choice_map)

# ---- View results ----
#View(dt_clean)
names(checkbox_data)  # See which checkbox variables were found
#View(checkbox_data$barriers_nivo_ipi)
