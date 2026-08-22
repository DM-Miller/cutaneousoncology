# Load necessary library
library(dplyr)

library(readr)
KN630_Placebo <- read_csv("files/webplot digitizer/KN630_rfs_placebo.csv",
                          col_names = FALSE)


# Function to clean WebPlotDigitizer survival data
clean_survival_data <- function(csv_path) {
  # Read the data
  df <- read.csv(csv_path, header = FALSE)
  
  # Rename columns for clarity
  colnames(df) <- c("Time", "RFS")
  
  # Ensure numeric columns
  df <- df %>%
    mutate(
      Time = as.numeric(Time),
      RFS = as.numeric(RFS)
    )
  
  # Add checks for monotonicity
  df_cleaned <- df %>%
    mutate(
      Time_diff = Time - lag(Time, default = first(Time)),
      RFS_diff = RFS - lag(RFS, default = first(RFS))
    ) %>%
    # Keep rows where Time increases (or stays equal) and RFS decreases (or stays equal)
    filter(
      Time_diff >= 0,
      RFS_diff <= 0
    ) %>%
    # Drop helper columns
    select(Time, RFS)
  
  return(df_cleaned)
}

# Example usage:
cleaned_df <- clean_survival_data("files/webplot digitizer/KN630_rfs_placebo.csv")


# To save the cleaned data:
write.csv(cleaned_df, 
          "files/webplot digitizer/KN630_rfs_placebo_cleaned_survival_data.csv", 
          row.names = FALSE)



# Function to mark rows to remove, with specific flags
mark_survival_data <- function(df) {
  df_marked <- df %>%
    mutate(
      Time = as.numeric(Time),
      RFS = as.numeric(RFS),
      Time_diff = Time - lag(Time, default = first(Time)),
      RFS_diff = RFS - lag(RFS, default = first(RFS)),
      Flag_Time = Time_diff < 0,
      Flag_RFS = RFS_diff > 0,
      Flag_Remove = Flag_Time | Flag_RFS
    )
  
  return(df_marked)
}


# Example usage:
# Read CSV without headers
KN630_Placebo <- readr::read_csv(
  "files/webplot digitizer/KN630_rfs_placebo_cleaned_survival_data.csv",
  col_names = FALSE
)
colnames(KN630_Placebo) <- c("Time", "RFS")

# Mark rows
KN630_Placebo_marked <- mark_survival_data(KN630_Placebo)

# View which rows would be removed
KN630_Placebo_marked %>% filter(Flag_Remove == TRUE)

# Cleaned data: keep only rows where Flag_Remove == FALSE
KN630_Placebo_cleaned <- KN630_Placebo_marked %>%
  filter(!Flag_Remove) %>%
  select(Time, RFS)

# View cleaned data
head(KN630_Placebo_cleaned)

# To save the cleaned data:
write.csv(
  KN630_Placebo_cleaned, 
  "files/webplot digitizer/KN630_rfs_placebo_cleaned.csv", 
  row.names = FALSE
  )
