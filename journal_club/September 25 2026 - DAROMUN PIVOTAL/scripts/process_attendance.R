# SoCO Journal Club — September 25, 2026
# Attendance processing
# DAROMUN / PIVOTAL
# Version: 2026-09-25 v4 (UTF-16LE decoding + ragged Teams export fix)
#
# Put this file here:
#
# journal_club/
# └── September 25 2026 - DAROMUN PIVOTAL/
#     ├── index.qmd
#     ├── scripts/
#     │   └── process_attendance.R
#     └── files/
#         └── meeting recap-attendance/
#             └── SoCO Meeting - Attendance report 9-25-26.csv
#
# Outputs:
#   files/meeting recap-attendance/attendance_clean.csv
#   files/meeting recap-attendance/attendance_summary.csv
#
# HOW TO RUN:
# 1. Open this file in RStudio and click Source, OR
# 2. Render index.qmd; the recap will source this script automatically.
#
# Notes:
# - The script infers the meeting folder from its own location when run directly.
# - It also accepts meeting_dir supplied by index.qmd.
# - Teams exports are not assumed to use a fixed encoding, delimiter, or row layout.
# - Duplicate joins are combined by a normalized first+last-name key.
# - Automated meeting-note accounts are removed.

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
  
  # Expected: meeting_folder/scripts/process_attendance.R
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

# During first setup, permit the raw file to sit directly in files/.
if (length(attendance_candidates) == 0) {
  attendance_candidates <- list.files(
    file.path(meeting_dir, "files"),
    pattern = "(Attendance report|attendance).*\\.(xls|xlsx|csv)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
}

# Do not accidentally select generated outputs.
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
      regex(credential_pattern, ignore_case = TRUE),
      ""
    ) |>
    str_replace_all("\\s+", " ") |>
    str_squish()
  
  map_chr(
    x,
    function(nm) {
      if (is.na(nm) || nm == "") return(NA_character_)
      
      pieces <- str_split(nm, ",", simplify = FALSE)[[1]] |>
        str_squish()
      pieces <- pieces[pieces != ""]
      
      if (length(pieces) >= 2) {
        last <- pieces[1]
        given <- paste(pieces[-1], collapse = " ")
        nm <- str_squish(paste(given, last))
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
      if (is.na(nm) || nm == "") return(NA_character_)
      
      words <- nm |>
        str_to_lower() |>
        str_replace_all("[^a-z0-9' -]", " ") |>
        str_squish() |>
        str_split("\\s+", simplify = FALSE)
      
      words <- words[[1]]
      words <- words[words != ""]
      
      if (length(words) == 0) return(NA_character_)
      if (length(words) == 1) {
        return(str_replace_all(words, "[^a-z0-9]", ""))
      }
      
      paste0(
        str_replace_all(words[1], "[^a-z0-9]", ""),
        str_replace_all(words[length(words)], "[^a-z0-9]", "")
      )
    }
  )
}

# Small, conservative display-name corrections accumulated across SoCO meetings.
# These do not affect the underlying Teams source file.
clean_display_name <- function(x) {
  case_when(
    str_detect(str_to_lower(x), "^m\\s*kkw$") ~ "Michael K. K. Wong",
    str_detect(str_to_lower(x), "^sameer g$") ~ "Samir Gupta",
    str_detect(str_to_lower(x), "howard kaufman") ~ "Howard Kaufman",
    str_detect(str_to_lower(x), "kaufman howard") ~ "Howard Kaufman",
    str_detect(str_to_lower(x), "vernon.*sondak") ~ "Vern Sondak",
    str_detect(str_to_lower(x), "sorrentino") ~ "Alex Sorrentino",
    TRUE ~ x
  )
}


# =============================================================================
# 4. Read Teams report robustly
# =============================================================================

ext <- tolower(tools::file_ext(attendance_file))

# Helper for Microsoft Teams text exports that are really ragged TSV files.
# The report has 1-2 columns in the Summary section but ~15 columns in the
# Participants section. readr::read_delim() can infer the early, narrow rows
# and then complain/truncate when it reaches the wider participant table.
# Reading line-by-line and padding every row avoids that problem entirely.
read_ragged_tsv <- function(path, encoding = "UTF-8") {
  
  # IMPORTANT: use readr for decoding rather than base::readLines(path,
  # encoding = ...).  On some macOS/R builds, readLines() can leave the
  # UTF-16LE BOM bytes (FF FE) attached to the first string without actually
  # transcoding them, which makes later string operations fail with
  # "input string is invalid".  readr::read_lines() performs the
  # transcoding to valid UTF-8 first.
  lines <- readr::read_lines(
    path,
    locale = readr::locale(encoding = encoding),
    progress = FALSE,
    skip = 0
  )
  
  if (length(lines) == 0) {
    stop("Attendance file appears to be empty.")
  }
  
  # A decoded BOM, if retained, is harmless because it occurs only on the
  # Summary heading.  Remove it only after successful UTF-8 transcoding.
  lines[1] <- stringr::str_remove(lines[1], "^\ufeff")
  
  fields <- strsplit(lines, "\t", fixed = TRUE)
  n_fields <- lengths(fields)
  max_fields <- max(n_fields, na.rm = TRUE)
  
  if (!is.finite(max_fields) || max_fields < 1) {
    stop("Could not parse the attendance file.")
  }
  
  mat <- matrix(
    "",
    nrow = length(fields),
    ncol = max_fields
  )
  
  for (i in seq_along(fields)) {
    if (length(fields[[i]]) > 0) {
      mat[i, seq_along(fields[[i]])] <- fields[[i]]
    }
  }
  
  tibble::as_tibble(
    mat,
    .name_repair = "minimal"
  )
}

if (ext %in% c("xls", "xlsx")) {
  
  raw <- readxl::read_excel(
    attendance_file,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )
  
} else {
  
  # Teams sometimes gives a UTF-16LE, tab-delimited file a .csv extension.
  con <- file(attendance_file, open = "rb")
  first_bytes <- readBin(con, what = "raw", n = 4)
  close(con)
  
  is_utf16le <-
    length(first_bytes) >= 2 &&
    as.integer(first_bytes[1]) == 255 &&
    as.integer(first_bytes[2]) == 254
  
  if (is_utf16le) {
    
    message("Detected UTF-16LE Teams export; reading as ragged tab-delimited text.")
    raw <- read_ragged_tsv(
      attendance_file,
      encoding = "UTF-16LE"
    )
    
  } else {
    
    # Look for a Teams-style tab-delimited export first.
    first_lines <- readLines(
      attendance_file,
      n = 25,
      warn = FALSE,
      encoding = "UTF-8",
      skipNul = TRUE
    )
    
    looks_tab_delimited <- any(
      grepl("Name\tFirst Join", first_lines, fixed = TRUE) |
        grepl("2. Participants", first_lines, fixed = TRUE) &
        grepl("\t", first_lines, fixed = TRUE)
    )
    
    if (looks_tab_delimited) {
      
      message("Detected tab-delimited Teams export.")
      raw <- read_ragged_tsv(
        attendance_file,
        encoding = "UTF-8"
      )
      
    } else {
      
      # Conventional comma-delimited CSV.  Use count.fields() to determine
      # the widest row before importing so the early 2-column Summary block
      # does not determine the width of the later Participants table.
      field_counts <- utils::count.fields(
        attendance_file,
        sep = ",",
        quote = "\"",
        blank.lines.skip = FALSE,
        comment.char = ""
      )
      
      max_fields <- max(field_counts, na.rm = TRUE)
      
      raw <- utils::read.table(
        attendance_file,
        header = FALSE,
        sep = ",",
        quote = "\"",
        fill = TRUE,
        col.names = paste0("V", seq_len(max_fields)),
        stringsAsFactors = FALSE,
        check.names = FALSE,
        comment.char = "",
        blank.lines.skip = FALSE,
        fileEncoding = "UTF-8"
      ) |>
        tibble::as_tibble(.name_repair = "minimal")
    }
  }
}

message("Imported report grid: ", nrow(raw), " rows x ", ncol(raw), " columns")

# Give every imported column a valid temporary name before dplyr transforms it.
names(raw) <- paste0("col_", seq_len(ncol(raw)))

raw <- raw |>
  mutate(
    across(
      everything(),
      ~ replace_na(as.character(.x), "") |>
        str_squish()
    )
  )


# =============================================================================
# 5. Locate Teams sections robustly
# =============================================================================

row_text <- apply(
  as.data.frame(raw),
  1,
  function(z) {
    paste(z[z != ""], collapse = " | ") |>
      str_squish()
  }
)

participants_section <- which(
  str_detect(
    row_text,
    regex("(^|\\|\\s*)2\\.\\s*Participants\\b", ignore_case = TRUE)
  )
)[1]

activities_section <- which(
  str_detect(
    row_text,
    regex("(^|\\|\\s*)3\\.\\s*In[- ]Meeting Activities\\b", ignore_case = TRUE)
  )
)[1]

if (is.na(participants_section) || is.na(activities_section)) {
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
    paste0(likely_rows, ": ", row_text[likely_rows]),
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

message("Participants section row: ", participants_section)
message("Activities section row: ", activities_section)


# =============================================================================
# 6. Locate actual participant header row
# =============================================================================

candidate_header_rows <- seq(
  participants_section + 1,
  activities_section - 1
)

header_scores <- map_int(
  candidate_header_rows,
  function(i) {
    vals <- unlist(raw[i, , drop = FALSE], use.names = FALSE) |>
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
        "Participant ID (UPN)",
        "Role",
        "Engagement: Camera On",
        "Engagement: Raise Hands",
        "Engagement: Unmute",
        "Reactions"
      )
    )
  }
)

header_row <- candidate_header_rows[which.max(header_scores)]

if (length(header_row) == 0 || max(header_scores) < 2) {
  stop(
    paste0(
      "Could not identify the Teams participant header row.\n",
      "Best header score was ",
      max(header_scores),
      "."
    )
  )
}

message("Participant header row: ", header_row)

header <- raw[header_row, , drop = FALSE] |>
  unlist(use.names = FALSE) |>
  as.character() |>
  str_squish()

empty_header <- is.na(header) | header == ""
header[empty_header] <- paste0("X", which(empty_header))
header <- make.unique(header)


# =============================================================================
# 7. Extract participant rows
# =============================================================================

participant_rows <- seq(header_row + 1, activities_section - 1)
participants_raw <- raw[participant_rows, , drop = FALSE]
names(participants_raw) <- header

required <- c(
  "Name",
  "In-Meeting Duration",
  "Engagement: Camera On",
  "Engagement: Raise Hands",
  "Engagement: Unmute"
)

missing_required <- setdiff(required, names(participants_raw))

if (length(missing_required) > 0) {
  stop(
    paste0(
      "Missing expected Teams columns: ",
      paste(missing_required, collapse = ", "),
      "\n\nColumns actually found:\n",
      paste(names(participants_raw), collapse = " | ")
    )
  )
}


# =============================================================================
# 8. Clean + deduplicate people
# =============================================================================

participants <- participants_raw |>
  transmute(
    name_raw = as.character(Name),
    Name = clean_display_name(clean_person_name(Name)),
    match_key = first_last_key(Name),
    duration_min = duration_to_minutes(`In-Meeting Duration`),
    camera_on = suppressWarnings(as.numeric(`Engagement: Camera On`)) |>
      replace_na(0),
    hand_raise = suppressWarnings(as.numeric(`Engagement: Raise Hands`)) |>
      replace_na(0),
    unmute = suppressWarnings(as.numeric(`Engagement: Unmute`)) |>
      replace_na(0)
  ) |>
  filter(
    !is.na(Name),
    Name != "",
    !str_detect(
      Name,
      regex("read\\.ai|meeting notes|notetaker|otter", ignore_case = TRUE)
    )
  )

people <- participants |>
  group_by(match_key) |>
  summarise(
    Name = first(Name),
    duration_min = sum(duration_min, na.rm = TRUE),
    camera_on = sum(camera_on, na.rm = TRUE),
    hand_raise = sum(hand_raise, na.rm = TRUE),
    unmute = sum(unmute, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    any_camera = camera_on > 0,
    any_hand_raise = hand_raise > 0,
    any_unmute = unmute > 0,
    any_active_engagement = any_hand_raise | any_unmute
  ) |>
  arrange(Name)

headline <- people |>
  summarise(
    people = n(),
    median_minutes = median(duration_min, na.rm = TRUE),
    pct_60min_plus = mean(duration_min >= 60, na.rm = TRUE),
    pct_camera = mean(any_camera, na.rm = TRUE),
    pct_unmute = mean(any_unmute, na.rm = TRUE),
    pct_hand_raise = mean(any_hand_raise, na.rm = TRUE),
    pct_active = mean(any_active_engagement, na.rm = TRUE)
  )


# =============================================================================
# 9. Save outputs
# =============================================================================

clean_path <- file.path(attendance_dir, "attendance_clean.csv")
summary_path <- file.path(attendance_dir, "attendance_summary.csv")

readr::write_csv(people, clean_path)
readr::write_csv(headline, summary_path)


# =============================================================================
# 10. Console summary
# =============================================================================

message("")
message("Attendance processing complete.")
message("Cleaned unique people: ", headline$people)
message("Median minutes: ", round(headline$median_minutes, 1))
message(
  "Stayed >=60 min: ",
  scales::percent(headline$pct_60min_plus, accuracy = 1)
)
message(
  "Camera on: ",
  scales::percent(headline$pct_camera, accuracy = 1)
)
message(
  "Unmuted: ",
  scales::percent(headline$pct_unmute, accuracy = 1)
)
message(
  "Raised hand: ",
  scales::percent(headline$pct_hand_raise, accuracy = 1)
)
message("")
message("Wrote:")
message("  ", clean_path)
message("  ", summary_path)
