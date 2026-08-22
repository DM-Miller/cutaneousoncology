# C-POST Journal Club attendance processor
# July 11, 2025
# ------------------------------------------------------------
# Robust to UTF-16LE Teams exports with BOM.
#
# Intended location:
# journal_club/July 11 2025 - c-Post/scripts/process_attendance.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

# ------------------------------------------------------------
# Resolve THIS meeting directory
# ------------------------------------------------------------

is_cpost_meeting_dir <- function(x) {
  if (length(x) != 1 || is.na(x) || !dir.exists(x)) return(FALSE)
  
  b <- basename(normalizePath(x, winslash = "/", mustWork = TRUE))
  
  grepl("July 11 2025", b, ignore.case = TRUE) &&
    grepl("C.?POST|Cemiplimab", b, ignore.case = TRUE)
}

resolve_meeting_dir <- function() {
  
  if (exists("meeting_dir", envir = .GlobalEnv, inherits = FALSE)) {
    x <- get("meeting_dir", envir = .GlobalEnv)
    
    if (is_cpost_meeting_dir(x)) {
      return(normalizePath(x, winslash = "/", mustWork = TRUE))
    }
  }
  
  exact_candidates <- c(
    here::here("journal_club", "July 11 2025 - c-Post"),
    here::here("journal_club", "July 11 2025 - C-Post"),
    here::here("journal_club", "July 11 2025 - C-POST"),
    here::here(
      "journal_club",
      "July 11 2025 - Adjuvant Cemiplimab in High-Risk SCC"
    )
  )
  
  hits <- exact_candidates[dir.exists(exact_candidates)]
  
  if (length(hits) >= 1) {
    return(normalizePath(hits[[1]], winslash = "/", mustWork = TRUE))
  }
  
  jc_dir <- here::here("journal_club")
  
  if (dir.exists(jc_dir)) {
    hits <- list.dirs(
      jc_dir,
      recursive = FALSE,
      full.names = TRUE
    )
    
    hits <- hits[
      grepl("July 11 2025", basename(hits), ignore.case = TRUE) &
        grepl("C.?POST|Cemiplimab", basename(hits), ignore.case = TRUE)
    ]
    
    if (length(hits) == 1) {
      return(normalizePath(hits[[1]], winslash = "/", mustWork = TRUE))
    }
  }
  
  stop("Could not locate the July 11, 2025 C-POST meeting folder.")
}

meeting_dir <- resolve_meeting_dir()

attendance_dir <- file.path(
  meeting_dir,
  "files",
  "meeting recap-attendance"
)

message("Resolved meeting directory:")
message(meeting_dir)

# ------------------------------------------------------------
# Find the original Teams export only
# ------------------------------------------------------------

attendance_files <- list.files(
  attendance_dir,
  pattern = "Attendance report.*\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

attendance_files <- attendance_files[
  !basename(attendance_files) %in%
    c("attendance_clean.csv", "attendance_summary.csv")
]

if (length(attendance_files) == 0) {
  stop(
    "No original Teams attendance report found in:\n",
    attendance_dir
  )
}

attendance_file <- attendance_files[
  which.max(file.info(attendance_files)$mtime)
]

message("")
message("Using raw Teams file:")
message(basename(attendance_file))

# ------------------------------------------------------------
# Read raw Teams export robustly
# ------------------------------------------------------------

read_teams_lines <- function(path) {
  
  # First try the known Teams encoding.
  x <- tryCatch(
    readr::read_lines(
      path,
      locale = readr::locale(encoding = "UTF-16LE"),
      progress = FALSE
    ),
    error = function(e) character(0)
  )
  
  # Strip any BOM that survives decoding.
  if (length(x)) {
    x <- sub("^\ufeff", "", x)
  }
  
  if (
    length(x) &&
    any(stringr::str_detect(
      x,
      stringr::fixed("Name\tFirst Join\tLast Leave")
    ))
  ) {
    return(x)
  }
  
  # Fallback to UTF-8.
  x <- readr::read_lines(
    path,
    locale = readr::locale(encoding = "UTF-8"),
    progress = FALSE
  )
  
  sub("^\ufeff", "", x)
}

raw_lines <- read_teams_lines(attendance_file)

# ------------------------------------------------------------
# Extract SECTION 2: Participants
# ------------------------------------------------------------

participant_header <- which(
  stringr::str_detect(
    raw_lines,
    stringr::fixed(
      "Name\tFirst Join\tLast Leave\tIn-Meeting Duration"
    )
  )
)[1]

if (is.na(participant_header)) {
  stop(
    "Could not find the Section 2 participant table header in:\n",
    attendance_file,
    "\n\nFirst few decoded lines were:\n",
    paste(utils::head(raw_lines, 15), collapse = "\n")
  )
}

section3_start <- which(
  seq_along(raw_lines) > participant_header &
    stringr::str_detect(
      raw_lines,
      regex("^3\\.\\s+In-Meeting Activities", ignore_case = TRUE)
    )
)[1]

if (is.na(section3_start)) {
  stop("Could not find the start of Section 3: In-Meeting Activities.")
}

participant_text <- paste(
  raw_lines[participant_header:(section3_start - 1)],
  collapse = "\n"
)

participants <- readr::read_tsv(
  I(participant_text),
  show_col_types = FALSE,
  progress = FALSE,
  trim_ws = TRUE
) |>
  filter(
    !is.na(Name),
    str_trim(Name) != ""
  )

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

duration_seconds <- function(x) {
  x <- replace_na(as.character(x), "")
  
  h <- suppressWarnings(as.numeric(str_match(x, "(\\d+)h")[, 2]))
  m <- suppressWarnings(as.numeric(str_match(x, "(\\d+)m")[, 2]))
  s <- suppressWarnings(as.numeric(str_match(x, "(\\d+)s")[, 2]))
  
  h <- replace_na(h, 0)
  m <- replace_na(m, 0)
  s <- replace_na(s, 0)
  
  h * 3600 + m * 60 + s
}

clean_name <- function(x) {
  x |>
    str_replace_all(
      regex("\\s*\\((Unverified|External)\\)\\s*", ignore_case = TRUE),
      " "
    ) |>
    str_replace_all("\\s*\\[[A-Z]\\]\\s*", " ") |>
    str_replace_all(
      regex(
        "\\s*\\([^)]*(NIH|NIAMS|BIDMC|HMFP|Medicine Administration|Dermatology|Hematology|MRSP)[^)]*\\)\\s*",
        ignore_case = TRUE
      ),
      " "
    ) |>
    str_replace_all(
      regex("\\s*\\(Ade\\)\\s*", ignore_case = TRUE),
      " "
    ) |>
    str_replace(
      regex(
        ",?\\s*(MD|PhD|CNP|MS|PGY4)(,\\s*(MD|PhD|CNP|MS|PGY4))*\\s*$",
        ignore_case = TRUE
      ),
      ""
    ) |>
    str_squish() |>
    str_remove("^,|,$") |>
    str_trim()
}

# ------------------------------------------------------------
# Clean participant-level data
# ------------------------------------------------------------

attendance_rows <- participants |>
  transmute(
    raw_name = Name,
    name = clean_name(Name),
    duration_seconds = duration_seconds(`In-Meeting Duration`),
    email = replace_na(Email, ""),
    role = replace_na(Role, "")
  )

attendance_clean <- attendance_rows |>
  group_by(name) |>
  summarise(
    duration_seconds = sum(duration_seconds, na.rm = TRUE),
    minutes = duration_seconds / 60,
    email = {
      z <- email[email != "" & !is.na(email)]
      if (length(z)) z[[1]] else ""
    },
    regeneron = str_detect(
      email,
      regex("@regeneron\\.com$", ignore_case = TRUE)
    ),
    .groups = "drop"
  ) |>
  arrange(desc(minutes), name)

# ------------------------------------------------------------
# Summary metrics
# ------------------------------------------------------------

attendance_summary <- tibble(
  metric = c(
    "teams_report_participant_records",
    "unique_attendees",
    "median_minutes",
    "attended_60_minutes_or_more_n",
    "attended_60_minutes_or_more_pct",
    "regeneron_attendees"
  ),
  value = c(
    nrow(participants),
    nrow(attendance_clean),
    round(median(attendance_clean$minutes, na.rm = TRUE), 1),
    sum(attendance_clean$minutes >= 60, na.rm = TRUE),
    round(mean(attendance_clean$minutes >= 60, na.rm = TRUE) * 100),
    sum(attendance_clean$regeneron, na.rm = TRUE)
  )
)

# ------------------------------------------------------------
# Write outputs
# ------------------------------------------------------------

clean_file <- file.path(
  attendance_dir,
  "attendance_clean.csv"
)

summary_file <- file.path(
  attendance_dir,
  "attendance_summary.csv"
)

readr::write_csv(
  attendance_clean,
  clean_file
)

readr::write_csv(
  attendance_summary,
  summary_file
)

# ------------------------------------------------------------
# Console output
# ------------------------------------------------------------

message("")
message("C-POST attendance processing complete")
message("-------------------------------------")
message("Teams participant records: ", nrow(participants))
message(
  "Unique attendees after duplicate-name consolidation: ",
  nrow(attendance_clean)
)
message(
  "Median attendance: ",
  round(median(attendance_clean$minutes, na.rm = TRUE), 1),
  " minutes"
)
message(
  "Stayed >=60 minutes: ",
  round(mean(attendance_clean$minutes >= 60, na.rm = TRUE) * 100),
  "%"
)
message(
  "Regeneron attendees: ",
  sum(attendance_clean$regeneron, na.rm = TRUE)
)
message("")
message("Created:")
message(clean_file)
message(summary_file)
