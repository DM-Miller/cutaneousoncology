# SoCO Journal Club — June 12, 2026
# Attendance processing
# Postoperative Management of Merkel Cell Carcinoma
#
# Expected structure:
#
# journal_club/
# └── June 12 2026 - Local Recurrence in MCC/
#     ├── index.qmd
#     ├── scripts/
#     │   └── process_attendance.R
#     └── files/
#         └── meeting recap-attendance/
#             └── [Teams attendance .xls/.xlsx/.csv]
#
# Outputs:
#   files/meeting recap-attendance/attendance_clean.csv
#   files/meeting recap-attendance/attendance_summary.csv
#
# IMPORTANT:
# - When sourced by index.qmd, meeting_dir is supplied by the QMD.
# - When sourced directly from RStudio, this script infers the meeting folder
#   from its own location.
# - Teams exports are not assumed to place section labels in column 1.
#   The parser searches ALL CELLS in each row.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
})

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop(
    "Package 'readxl' is required for Excel attendance files. ",
    "Install with install.packages('readxl')."
  )
}


# =============================================================================
# 1. Resolve meeting directory
# =============================================================================

if (!exists("meeting_dir", inherits = TRUE)) {
  
  get_script_path <- function() {
    
    # RStudio Source button
    if (
      interactive() &&
      requireNamespace("rstudioapi", quietly = TRUE)
    ) {
      
      ctx <- tryCatch(
        rstudioapi::getSourceEditorContext(),
        error = function(e) NULL
      )
      
      if (
        !is.null(ctx) &&
        !is.null(ctx$path) &&
        nzchar(ctx$path)
      ) {
        
        return(
          normalizePath(
            ctx$path,
            winslash = "/",
            mustWork = TRUE
          )
        )
      }
    }
    
    # Rscript process_attendance.R
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    
    if (length(file_arg) > 0) {
      
      return(
        normalizePath(
          sub("^--file=", "", file_arg[1]),
          winslash = "/",
          mustWork = TRUE
        )
      )
    }
    
    stop(
      paste0(
        "Could not determine the meeting directory. ",
        "Render index.qmd or source this script from RStudio."
      )
    )
  }
  
  script_path <- get_script_path()
  script_dir <- dirname(script_path)
  
  # Expected:
  # meeting_folder/scripts/process_attendance.R
  meeting_dir <- dirname(script_dir)
}

meeting_dir <- normalizePath(
  meeting_dir,
  winslash = "/",
  mustWork = TRUE
)

attendance_dir <- file.path(
  meeting_dir,
  "files",
  "meeting recap-attendance"
)

message("Meeting directory: ", meeting_dir)
message("Attendance directory: ", attendance_dir)

if (!dir.exists(attendance_dir)) {
  dir.create(
    attendance_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
}


# =============================================================================
# 2. Locate Teams attendance export
# =============================================================================

attendance_candidates <- list.files(
  attendance_dir,
  pattern = "(Attendance report|attendance).*\\.(xls|xlsx|csv)$",
  full.names = TRUE,
  ignore.case = TRUE
)

# Permit the source file to sit directly in files/ during setup.
if (length(attendance_candidates) == 0) {
  
  attendance_candidates <- list.files(
    file.path(meeting_dir, "files"),
    pattern = "(Attendance report|attendance).*\\.(xls|xlsx|csv)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
}

# Do not accidentally select our own generated CSVs.
attendance_candidates <- attendance_candidates[
  !basename(attendance_candidates) %in%
    c("attendance_clean.csv", "attendance_summary.csv")
]

if (length(attendance_candidates) == 0) {
  
  stop(
    paste0(
      "No Teams attendance export found.\n",
      "Place the .xls, .xlsx, or .csv file in:\n",
      attendance_dir
    )
  )
}

attendance_file <- attendance_candidates[
  which.max(file.info(attendance_candidates)$mtime)
]

message("Using attendance file: ", attendance_file)


# =============================================================================
# 3. Helpers
# =============================================================================

duration_to_minutes <- function(x) {
  
  x <- replace_na(as.character(x), "")
  
  hours <- str_extract(x, "\\d+(?=h)") |>
    replace_na("0") |>
    as.numeric()
  
  mins <- str_extract(x, "\\d+(?=m)") |>
    replace_na("0") |>
    as.numeric()
  
  secs <- str_extract(x, "\\d+(?=s)") |>
    replace_na("0") |>
    as.numeric()
  
  60 * hours + mins + secs / 60
}


credential_pattern <- paste0(
  "\\b(",
  paste(
    c(
      "MD", "DO", "PhD", "Ph\\.D", "MPH", "MBA",
      "MS", "MSc", "CNP", "NP", "PA-C", "PA",
      "RN", "BSN", "MSN", "FACS", "FAAD", "FACEP"
    ),
    collapse = "|"
  ),
  ")\\b\\.?"
)


clean_person_name <- function(x) {
  
  x <- x |>
    replace_na("") |>
    str_replace_all("[\u2018\u2019]", "'") |>
    str_remove_all("\\s*\\([^)]*\\)") |>
    str_remove_all("\\s*\\[[^]]*\\]") |>
    str_replace_all(
      regex(
        credential_pattern,
        ignore_case = TRUE
      ),
      ""
    ) |>
    str_replace_all("\\s+", " ") |>
    str_squish()
  
  map_chr(
    x,
    function(nm) {
      
      if (is.na(nm) || nm == "") {
        return(NA_character_)
      }
      
      pieces <- str_split(
        nm,
        ",",
        simplify = FALSE
      )[[1]] |>
        str_squish()
      
      pieces <- pieces[pieces != ""]
      
      if (length(pieces) >= 2) {
        
        last <- pieces[1]
        given <- paste(
          pieces[-1],
          collapse = " "
        )
        
        nm <- str_squish(
          paste(
            given,
            last
          )
        )
      }
      
      nm |>
        str_replace_all("\\s*,\\s*", " ") |>
        str_replace_all("\\s+", " ") |>
        str_squish()
    }
  )
}


first_last_key <- function(x) {
  
  clean <- clean_person_name(x)
  
  map_chr(
    clean,
    function(nm) {
      
      if (is.na(nm) || nm == "") {
        return(NA_character_)
      }
      
      words <- nm |>
        str_to_lower() |>
        str_replace_all(
          "[^a-z0-9' -]",
          " "
        ) |>
        str_squish() |>
        str_split(
          "\\s+",
          simplify = FALSE
        )
      
      words <- words[[1]]
      words <- words[words != ""]
      
      if (length(words) == 0) {
        return(NA_character_)
      }
      
      if (length(words) == 1) {
        
        return(
          str_replace_all(
            words,
            "[^a-z0-9]",
            ""
          )
        )
      }
      
      paste0(
        str_replace_all(
          words[1],
          "[^a-z0-9]",
          ""
        ),
        str_replace_all(
          words[length(words)],
          "[^a-z0-9]",
          ""
        )
      )
    }
  )
}


clean_display_name <- function(x) {
  
  case_when(
    str_detect(
      str_to_lower(x),
      "^sameer g$"
    ) ~ "Samir Gupta",
    
    str_detect(
      str_to_lower(x),
      "howard kaufman"
    ) ~ "Howard Kaufman",
    
    str_detect(
      str_to_lower(x),
      "kaufman howard"
    ) ~ "Howard Kaufman",
    
    str_detect(
      str_to_lower(x),
      "vernon.*sondak"
    ) ~ "Vern Sondak",
    
    str_detect(
      str_to_lower(x),
      "sorrentino"
    ) ~ "Alex Sorrentino",
    
    TRUE ~ x
  )
}


# =============================================================================
# 4. Read Teams report
# =============================================================================

ext <- tolower(
  tools::file_ext(
    attendance_file
  )
)

if (ext %in% c("xls", "xlsx")) {
  
  raw <- readxl::read_excel(
    attendance_file,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )
  
} else {
  
  raw <- tryCatch(
    
    readr::read_csv(
      attendance_file,
      col_names = FALSE,
      show_col_types = FALSE,
      progress = FALSE,
      name_repair = "minimal"
    ),
    
    error = function(e) {
      
      txt <- readr::read_file(
        attendance_file,
        locale = readr::locale(
          encoding = "UTF-16LE"
        )
      )
      
      readr::read_delim(
        I(txt),
        delim = "\t",
        col_names = FALSE,
        show_col_types = FALSE,
        name_repair = "minimal"
      )
    }
  )
}

# -------------------------------------------------------------------------
# IMPORTANT: give every imported column a valid temporary name BEFORE
# calling dplyr::mutate(). Teams Excel exports can contain blank column names,
# and dplyr refuses to transform a data frame with NA / "" names.
# -------------------------------------------------------------------------

names(raw) <- paste0(
  "col_",
  seq_len(ncol(raw))
)

# Convert every cell to clean text.
raw <- raw |>
  mutate(
    across(
      everything(),
      ~ replace_na(
        as.character(.x),
        ""
      ) |>
        str_squish()
    )
  )


# =============================================================================
# 5. Locate Teams sections robustly
# =============================================================================

# Teams Excel exports can place the section name in a different column
# depending on version / workbook formatting. Therefore search the entire row.

row_text <- apply(
  as.data.frame(raw),
  1,
  function(z) {
    paste(
      z[z != ""],
      collapse = " | "
    ) |>
      str_squish()
  }
)

participants_section <- which(
  str_detect(
    row_text,
    regex(
      "(^|\\|\\s*)2\\.\\s*Participants\\b",
      ignore_case = TRUE
    )
  )
)[1]

activities_section <- which(
  str_detect(
    row_text,
    regex(
      "(^|\\|\\s*)3\\.\\s*In[- ]Meeting Activities\\b",
      ignore_case = TRUE
    )
  )
)[1]


if (
  is.na(participants_section) ||
  is.na(activities_section)
) {
  
  # Helpful debugging output if Microsoft changes the layout again.
  likely_rows <- which(
    str_detect(
      row_text,
      regex(
        "participant|meeting activit|duration|camera|unmute|raise",
        ignore_case = TRUE
      )
    )
  )
  
  preview <- paste(
    paste0(
      likely_rows,
      ": ",
      row_text[likely_rows]
    ),
    collapse = "\n"
  )
  
  stop(
    paste0(
      "Could not identify the Teams participant/activity sections.\n\n",
      "Rows containing likely attendance keywords:\n",
      preview
    )
  )
}

if (activities_section <= participants_section) {
  
  stop(
    paste0(
      "Teams section order is unexpected: Participants row = ",
      participants_section,
      "; Activities row = ",
      activities_section
    )
  )
}

message(
  "Participants section row: ",
  participants_section
)

message(
  "Activities section row: ",
  activities_section
)


# =============================================================================
# 6. Locate actual participant header row
# =============================================================================

# Do NOT assume it is participants_section + 1.
# Search between the section marker and the Activities section.

candidate_header_rows <- seq(
  participants_section + 1,
  activities_section - 1
)

header_scores <- map_int(
  candidate_header_rows,
  function(i) {
    
    vals <- unlist(
      raw[i, , drop = FALSE],
      use.names = FALSE
    ) |>
      as.character() |>
      str_squish()
    
    sum(
      vals %in% c(
        "Name",
        "First Join",
        "Last Leave",
        "In-Meeting Duration",
        "Email",
        "Participant ID",
        "Role",
        "Engagement: Camera On",
        "Engagement: Raise Hands",
        "Engagement: Unmute",
        "Reactions"
      )
    )
  }
)

header_row <- candidate_header_rows[
  which.max(header_scores)
]

if (
  length(header_row) == 0 ||
  max(header_scores) < 2
) {
  
  stop(
    paste0(
      "Could not identify the Teams participant header row.\n",
      "Best header score was ",
      max(header_scores),
      "."
    )
  )
}

message(
  "Participant header row: ",
  header_row
)

header <- raw[
  header_row,
  ,
  drop = FALSE
] |>
  unlist(
    use.names = FALSE
  ) |>
  as.character() |>
  str_squish()

empty_header <- is.na(header) |
  header == ""

header[empty_header] <- paste0(
  "X",
  which(empty_header)
)

header <- make.unique(
  header
)


# =============================================================================
# 7. Extract participant rows
# =============================================================================

participant_rows <- seq(
  header_row + 1,
  activities_section - 1
)

participants_raw <- raw[
  participant_rows,
  ,
  drop = FALSE
]

names(participants_raw) <- header

required <- c(
  "Name",
  "In-Meeting Duration",
  "Engagement: Camera On",
  "Engagement: Raise Hands",
  "Engagement: Unmute"
)

missing_required <- setdiff(
  required,
  names(participants_raw)
)

if (length(missing_required) > 0) {
  
  stop(
    paste0(
      "Missing expected Teams columns: ",
      paste(
        missing_required,
        collapse = ", "
      ),
      "\n\nColumns actually found:\n",
      paste(
        names(participants_raw),
        collapse = " | "
      )
    )
  )
}


# =============================================================================
# 8. Clean + deduplicate people
# =============================================================================

participants <- participants_raw |>
  transmute(
    name_raw = as.character(Name),
    
    Name = clean_display_name(
      clean_person_name(Name)
    ),
    
    match_key = first_last_key(Name),
    
    duration_min = duration_to_minutes(
      `In-Meeting Duration`
    ),
    
    camera_on = suppressWarnings(
      as.numeric(
        `Engagement: Camera On`
      )
    ) |>
      replace_na(0),
    
    hand_raise = suppressWarnings(
      as.numeric(
        `Engagement: Raise Hands`
      )
    ) |>
      replace_na(0),
    
    unmute = suppressWarnings(
      as.numeric(
        `Engagement: Unmute`
      )
    ) |>
      replace_na(0)
  ) |>
  filter(
    !is.na(Name),
    Name != "",
    
    !str_detect(
      Name,
      regex(
        "read\\.ai|meeting notes|notetaker|otter",
        ignore_case = TRUE
      )
    )
  )


people <- participants |>
  group_by(match_key) |>
  summarise(
    Name = first(Name),
    
    duration_min = sum(
      duration_min,
      na.rm = TRUE
    ),
    
    camera_on = sum(
      camera_on,
      na.rm = TRUE
    ),
    
    hand_raise = sum(
      hand_raise,
      na.rm = TRUE
    ),
    
    unmute = sum(
      unmute,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) |>
  mutate(
    any_camera = camera_on > 0,
    any_hand_raise = hand_raise > 0,
    any_unmute = unmute > 0,
    
    any_active_engagement =
      any_hand_raise |
      any_unmute
  ) |>
  arrange(Name)


headline <- people |>
  summarise(
    people = n(),
    
    median_minutes = median(
      duration_min,
      na.rm = TRUE
    ),
    
    pct_60min_plus = mean(
      duration_min >= 60,
      na.rm = TRUE
    ),
    
    pct_camera = mean(
      any_camera,
      na.rm = TRUE
    ),
    
    pct_unmute = mean(
      any_unmute,
      na.rm = TRUE
    ),
    
    pct_hand_raise = mean(
      any_hand_raise,
      na.rm = TRUE
    ),
    
    pct_active = mean(
      any_active_engagement,
      na.rm = TRUE
    )
  )


# =============================================================================
# 9. Save outputs
# =============================================================================

clean_path <- file.path(
  attendance_dir,
  "attendance_clean.csv"
)

summary_path <- file.path(
  attendance_dir,
  "attendance_summary.csv"
)

readr::write_csv(
  people,
  clean_path
)

readr::write_csv(
  headline,
  summary_path
)


# =============================================================================
# 10. Console summary
# =============================================================================

message("")
message("Attendance processing complete.")
message(
  "Cleaned unique people: ",
  headline$people
)

message(
  "Median minutes: ",
  round(
    headline$median_minutes,
    1
  )
)

message(
  "Stayed >=60 min: ",
  scales::percent(
    headline$pct_60min_plus,
    accuracy = 1
  )
)

message(
  "Camera on: ",
  scales::percent(
    headline$pct_camera,
    accuracy = 1
  )
)

message(
  "Unmuted: ",
  scales::percent(
    headline$pct_unmute,
    accuracy = 1
  )
)

message(
  "Raised hand: ",
  scales::percent(
    headline$pct_hand_raise,
    accuracy = 1
  )
)

message("")
message("Wrote:")
message("  ", clean_path)
message("  ", summary_path)
