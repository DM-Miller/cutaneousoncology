# SoCO Journal Club — November 24, 2025
# MATISSE — attendance processor
#
# Save as:
#   scripts/process_attendance.R
#
# Put the Teams export in:
#   files/meeting recap-attendance/
#
# Outputs:
#   files/meeting recap-attendance/attendance_clean.csv
#   files/meeting recap-attendance/attendance_summary.csv

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package 'readxl' is required for Excel attendance files.")
}

# =============================================================================
# 1. Resolve meeting directory
# =============================================================================

get_script_path <- function() {

  if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
    ctx <- tryCatch(
      rstudioapi::getSourceEditorContext(),
      error = function(e) NULL
    )

    if (!is.null(ctx) && !is.null(ctx$path) && nzchar(ctx$path)) {
      return(
        normalizePath(
          ctx$path,
          winslash = "/",
          mustWork = TRUE
        )
      )
    }
  }

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

  stop("Could not determine the script location.")
}

script_path <- get_script_path()
meeting_dir <- dirname(dirname(script_path))

attendance_dir <- file.path(
  meeting_dir,
  "files",
  "meeting recap-attendance"
)

dir.create(
  attendance_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

message("Meeting directory: ", meeting_dir)
message("Attendance directory: ", attendance_dir)

# =============================================================================
# 2. Locate attendance export
# =============================================================================

find_attendance_files <- function(path, recursive = FALSE) {

  if (!dir.exists(path)) return(character(0))

  x <- list.files(
    path,
    full.names = TRUE,
    recursive = recursive,
    all.files = FALSE,
    no.. = TRUE,
    include.dirs = FALSE
  )

  x <- x[
    tolower(tools::file_ext(x)) %in% c("csv", "xls", "xlsx")
  ]

  x[
    !tolower(basename(x)) %in%
      c("attendance_clean.csv", "attendance_summary.csv")
  ]
}

attendance_candidates <- find_attendance_files(
  attendance_dir,
  recursive = FALSE
)

if (length(attendance_candidates) == 0) {
  message(
    "No attendance file visible in the expected folder; ",
    "searching the meeting directory recursively..."
  )

  attendance_candidates <- find_attendance_files(
    meeting_dir,
    recursive = TRUE
  )
}

if (length(attendance_candidates) == 0) {
  stop(
    paste0(
      "No CSV/XLS/XLSX attendance export found under:\n",
      meeting_dir,
      "\n\nPlace the Teams attendance export in:\n",
      attendance_dir
    )
  )
}

if (length(attendance_candidates) > 1) {

  attendance_score <- (
    100 * str_detect(
      basename(attendance_candidates),
      regex("attendance", ignore_case = TRUE)
    ) +
    20 * str_detect(
      attendance_candidates,
      regex("meeting recap-attendance", ignore_case = TRUE)
    )
  )

  attendance_candidates <- attendance_candidates[
    order(
      attendance_score,
      file.info(attendance_candidates)$mtime,
      decreasing = TRUE,
      na.last = TRUE
    )
  ]
}

attendance_file <- attendance_candidates[1]
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
      "MD", "M\\.D", "DO", "PhD", "Ph\\.D", "MPH", "MBA",
      "MS", "MSc", "CNP", "NP", "PA-C", "PA",
      "RN", "BSN", "MSN", "FACS", "FAAD"
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
    str_replace_all(regex(credential_pattern, ignore_case = TRUE), "") |>
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

      str_squish(nm)
    }
  )
}

normalize_display_name <- function(x) {

  case_when(
    str_detect(str_to_lower(x), "^mariam$|mariam.*el-ashmawy|el-ashmawy.*mariam") ~ "Mariam El-Ashmawy",
    str_detect(str_to_lower(x), "david.*miller|miller.*david") ~ "David M. Miller",
    str_detect(str_to_lower(x), "howard.*kaufman|kaufman.*howard") ~ "Howard L. Kaufman",
    str_detect(str_to_lower(x), "ross.*merkin|merkin.*ross") ~ "Ross D. Merkin",
    str_detect(str_to_lower(x), "ryan.*sullivan|sullivan.*ryan") ~ "Ryan J. Sullivan",
    str_detect(str_to_lower(x), "sameer.*gupta|gupta.*sameer") ~ "Sameer Gupta",
    str_detect(str_to_lower(x), "alexandra.*sorrentino|sorrentino.*alexandra") ~ "Alex Sorrentino",
    str_detect(str_to_lower(x), "kevin.*emerick|emerick.*kevin") ~ "Kevin S. Emerick",
    str_detect(str_to_lower(x), "kenneth.*tsai|ken.*tsai|tsai.*kenneth|tsai.*ken") ~ "Ken Tsai",
    str_detect(str_to_lower(x), "sunandana.*chandra|chandra.*sunandana") ~ "Sunandana Chandra",
    str_detect(str_to_lower(x), "vishal.*patel|patel.*vishal") ~ "Vishal Patel",
    str_detect(str_to_lower(x), "frank.*worden|frances.*collichio") ~ x,
    TRUE ~ x
  )
}

first_last_key <- function(x) {

  map_chr(
    x,
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

# =============================================================================
# 4. Read Teams report
# =============================================================================

ext <- tolower(tools::file_ext(attendance_file))

if (ext %in% c("xls", "xlsx")) {

  raw <- readxl::read_excel(
    attendance_file,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  ) |>
    as_tibble(.name_repair = "minimal")

} else {

  # Teams "CSV" exports are often UTF-16LE and tab-delimited.
  con <- file(attendance_file, open = "r", encoding = "UTF-16LE")
  on.exit(close(con), add = TRUE)

  lines <- readLines(con, warn = FALSE)

  if (length(lines) > 0) {
    lines[1] <- sub("^\ufeff", "", lines[1])
  }

  split_lines <- strsplit(lines, "\t", fixed = TRUE)
  max_cols <- max(lengths(split_lines))

  padded <- lapply(
    split_lines,
    function(z) {
      length(z) <- max_cols
      z[is.na(z)] <- ""
      z
    }
  )

  raw <- as_tibble(
    do.call(rbind, padded),
    .name_repair = "minimal"
  )
}

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
# 5. Locate participant table
# =============================================================================

row_text <- apply(
  as.data.frame(raw),
  1,
  function(z) paste(z[z != ""], collapse = " | ")
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
  stop("Could not identify the Teams participant/activity sections.")
}

candidate_header_rows <- seq(
  participants_section + 1,
  activities_section - 1
)

expected_headers <- c(
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
  "Engagement: Unmute"
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

    sum(vals %in% expected_headers)
  }
)

header_row <- candidate_header_rows[
  which.max(header_scores)
]

if (length(header_row) == 0 || max(header_scores) < 2) {
  stop("Could not identify the Teams participant header row.")
}

header <- raw[header_row, , drop = FALSE] |>
  unlist(use.names = FALSE) |>
  as.character() |>
  str_squish()

empty_header <- is.na(header) | header == ""
header[empty_header] <- paste0("X", which(empty_header))
header <- make.unique(header)

participants_raw <- raw[
  seq(header_row + 1, activities_section - 1),
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

missing_required <- setdiff(required, names(participants_raw))

if (length(missing_required) > 0) {
  stop(
    paste0(
      "Missing expected Teams columns: ",
      paste(missing_required, collapse = ", ")
    )
  )
}

# =============================================================================
# 6. Clean + reconcile reconnects
# =============================================================================

participants <- participants_raw |>
  transmute(
    name_raw = as.character(Name),
    Name = clean_person_name(Name),
    duration_min = duration_to_minutes(`In-Meeting Duration`),
    camera_on = suppressWarnings(as.numeric(`Engagement: Camera On`)) |>
      replace_na(0),
    hand_raise = suppressWarnings(as.numeric(`Engagement: Raise Hands`)) |>
      replace_na(0),
    unmute = suppressWarnings(as.numeric(`Engagement: Unmute`)) |>
      replace_na(0)
  ) |>
  mutate(
    Name = normalize_display_name(Name),
    match_key = first_last_key(Name)
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
    any_unmute = unmute > 0
  ) |>
  arrange(Name)

# =============================================================================
# 7. Summary + outputs
# =============================================================================

headline <- people |>
  summarise(
    people = n(),
    median_minutes = median(duration_min, na.rm = TRUE),
    pct_60min_plus = mean(duration_min >= 60, na.rm = TRUE),
    pct_camera = mean(any_camera, na.rm = TRUE),
    pct_unmute = mean(any_unmute, na.rm = TRUE),
    pct_hand_raise = mean(any_hand_raise, na.rm = TRUE)
  )

clean_path <- file.path(attendance_dir, "attendance_clean.csv")
summary_path <- file.path(attendance_dir, "attendance_summary.csv")

readr::write_csv(people, clean_path)
readr::write_csv(headline, summary_path)

message("")
message("Attendance processing complete.")
message("People represented: ", headline$people)
message("Median minutes: ", round(headline$median_minutes, 1))
message("Stayed >=60 min: ", scales::percent(headline$pct_60min_plus, accuracy = 1))
message("Camera on: ", scales::percent(headline$pct_camera, accuracy = 1))
message("Unmuted: ", scales::percent(headline$pct_unmute, accuracy = 1))
message("Raised hand: ", scales::percent(headline$pct_hand_raise, accuracy = 1))
message("")
message("Wrote:")
message("  ", clean_path)
message("  ", summary_path)
