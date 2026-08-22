## Clear workspace for reproducibility
rm(list = ls())

# Function to load/install packages (avoids clutter & repetition)
load_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# Load core data manipulation and plotting packages
core_packages <- c(
  "tidyverse",   # dplyr, ggplot2, readr, etc.
  "data.table",  # Fast reading and manipulation
  "MASS",        # Statistical models
  "splines",     # Spline functions for survival curves
  "survival",    # Survival analysis
  "survminer",   # Publication-ready survival plots
  "ggplot2",     # Visualization
  "scales",      # Axis scaling for ggplots
  "grid", "gridExtra",  # Grid layouts for complex plots
  "gt", "gtsummary",    # Beautiful tables
  "knitr", "kableExtra", # Reporting & enhanced tables
  "here",        # Reproducible project paths
  "jpeg",        # Import JPEG images
  "sandwich",    # Robust standard errors
  "survey",      # Complex survey design / weighting
  "htmltools",   # HTML manipulation (for scrollable tables)
  "shiny"        # Optional: Shiny app integration
)
# Load all core packages
lapply(core_packages, load_pkg)

# Add guyot functions
source(here::here("scripts/guyot functions.R"))
source(here::here("scripts/guyot functions-modified.R"))

# Define file paths using here::here() for portability
# Read digitized KM curve data and number at risk table
data_path <- here::here("files/webplot digitizer") # Change to accomodate the path of your files
data_path
km_file <- file.path(data_path, "KN630_rfs_placebo_cleaned.csv")
file.exists(km_file)

nrisk_file <- file.path(data_path, "KN630_rfs_placebo_NumAtRisk.csv")
file.exists(nrisk_file)

digC <- fread(km_file)
# Load digitized Kaplan-Meier curve data
digC <- fread(km_file) %>%
  mutate(Curve1 = RFS, x = Time) %>%  # Rename 'y' to 'Curve1' for clarity
  dplyr::select(x, Curve1) |> 
  mutate(Curve1 = Curve1 / 100)


# Load number-at-risk data
nRisk <- fread(nrisk_file) 
# Load number-at-risk data and remove rows where Nrisk is zero (done for analysis purposes)
nRisk <- fread(nrisk_file) %>%
  filter(Nrisk != 0)


# Execute Guyot functions
# Run the reconstruction pipeline
result <- process_KM_data(digC, nRisk)

# Extract the reconstructed individual patient dataset
reconstructed_ipd <- result$IPD
reconstructed_ipd <- reconstructed_ipd |> mutate(t = as.numeric(t)) |> arrange(t)
# Sample gt table
reconstructed_ipd_table <- 
  reconstructed_ipd |> 
  gt() %>%
  tab_header(
    title = html("<b>Preview of Reconstructed IPD</b>")
  )%>%
  cols_align(align = "center", columns = everything()) %>%
  tab_style(
    style = cell_text(
      weight = "bold",
      style = "italic",
      color = "#2C3E50",     # Dark slate blue-gray tone
      font = "Arial"
    ),
    locations = cells_column_labels(columns = everything())
  )
reconstructed_ipd_table
events <- sum(reconstructed_ipd$ev)
events


# Create a survival object
surv_obj <- Surv(
  time = reconstructed_ipd$t, 
  event = reconstructed_ipd$ev)

# Fit the Kaplan-Meier model
km_fit <- survfit(surv_obj ~ 1, data = reconstructed_ipd)

# Plot the survival curve with the risk table
ggsurvplot(
  km_fit,
  data = reconstructed_ipd,
  risk.table = TRUE,               # Show number at risk table
  risk.table.y.text = TRUE,        # Add labels to risk table rows
  risk.table.height = 0.2,         # Adjust risk table size
  break.time.by = 6,               # X-axis breaks every 4 months
  xlab = "Time (Months)",
  ylab = "Survival Probability",
  surv.median.line = "hv",         # Add horizontal/vertical line at median survival
  ggtheme = theme_minimal(),
  legend = "none"
)

# Summarize each interval for diagnostics
interval_summary <- tibble(
  Interval = 1:(n.int - 1),
  TimeStart = t.S[lower[1:(n.int - 1)]],
  TimeEnd = t.S[lower[2:n.int]],
  N_at_risk_start = n.risk[1:(n.int - 1)],
  N_at_risk_end = n.risk[2:n.int],
  N_censor_est = n.censor,
  Events_in_interval = sapply(1:(n.int - 1), function(i) sum(d[lower[i]:upper[i]]))
)

print(interval_summary)

