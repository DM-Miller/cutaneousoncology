# SoCO Community Highlights
# Attendance-over-time processor
# ------------------------------------------------------------
# Intended location:
# community_highlights/scripts/process_community_attendance.R
#
# Source directory:
# Attendance Over Time/
#
# Expected structure:
# Attendance Over Time/
#   <meeting folder>/
#     attendance_clean.csv
#     attendance_summary.csv
#     raw Teams attendance export...
#
# Outputs:
# community_highlights/files/community_attendance_long.csv
# community_highlights/files/community_member_summary.csv
# community_highlights/files/community_industry_summary.csv
# community_highlights/files/community_company_summary.csv
# community_highlights/files/community_meeting_summary.csv
#
# The script:
# 1. scans all meeting folders with attendance_clean.csv
# 2. classifies meetings as Journal Club or Research Forum
# 3. identifies industry colleagues using email-domain lookup
# 4. excludes industry colleagues from the academic/member leaderboard
# 5. calculates:
#      - Journal Club attendance %
#      - tracked-meeting attendance %
#      - total minutes
#      - normalized minutes (top academic participant = 100)
#
# IMPORTANT:
# The source files are an incomplete historical archive. The page should
# describe these as "tracked meetings" rather than all SoCO meetings ever held.

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(stringr)
})

# ------------------------------------------------------------
# Resolve project paths
# ------------------------------------------------------------

project_root <- here::here()
attendance_root <- file.path(project_root, "Attendance Over Time")
community_dir <- file.path(project_root, "community_highlights")
output_dir <- file.path(community_dir, "files")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(attendance_root)) {
  stop(
    "Could not find the attendance archive:\n",
    attendance_root
  )
}

# ------------------------------------------------------------
# Meeting metadata
#
# Explicitly classify the currently tracked folders shown in the archive.
# Anything new defaults to Journal Club unless it matches known
# Research Forum language. You can override this table at any time.
# ------------------------------------------------------------

meeting_metadata <- tribble(
  ~folder_pattern, ~meeting_type,
  "Open Inference Project", "Research Forum",
  "Bayesian Causal Analysis of Dose Intensity", "Research Forum",
  "Research Forum", "Research Forum"
)

classify_meeting <- function(folder_name) {
  
  hit <- meeting_metadata |>
    filter(
      str_detect(
        folder_name,
        regex(folder_pattern, ignore_case = TRUE)
      )
    )
  
  if (nrow(hit)) {
    return(hit$meeting_type[[1]])
  }
  
  "Journal Club"
}

extract_meeting_date <- function(folder_name) {
  
  # Folder examples:
  # July 11 2025 - c-Post
  # April 12 2026 - IGNYTE
  # March 9 2026 - Bayesian Causal Analysis...
  date_text <- str_extract(
    folder_name,
    "^[A-Za-z]+\\s+\\d{1,2}\\s+\\d{4}"
  )
  
  suppressWarnings(
    as.Date(
      date_text,
      format = "%B %d %Y"
    )
  )
}

# ------------------------------------------------------------
# Industry lookup
#
# Domain lookup is intentionally simple and editable.
# Add rows as new partners participate.
#
# A second optional file, industry_people_overrides.csv, can identify
# individuals when an attendance export lacks an email address.
# ------------------------------------------------------------

domain_lookup_file <- file.path(
  community_dir,
  "industry_domains.csv"
)

if (!file.exists(domain_lookup_file)) {
  
  default_domains <- tribble(
    ~domain, ~company,
    "regeneron.com", "Regeneron",
    "bms.com", "Bristol Myers Squibb"
  )
  
  readr::write_csv(
    default_domains,
    domain_lookup_file
  )
}

industry_domains <- readr::read_csv(
  domain_lookup_file,
  show_col_types = FALSE
) |>
  mutate(
    domain = str_to_lower(str_trim(domain)),
    company = str_trim(company)
  ) |>
  filter(
    domain != "",
    company != ""
  )

person_override_file <- file.path(
  community_dir,
  "industry_people_overrides.csv"
)

if (!file.exists(person_override_file)) {
  readr::write_csv(
    tibble(
      name = character(),
      company = character()
    ),
    person_override_file
  )
}

industry_people <- NULL  # loaded after identity helper definitions

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

clean_name_key <- function(x) {
  
  x <- as.character(x)
  x <- str_replace_all(x, "\\u00A0", " ")
  x <- str_squish(x)
  
  map_chr(x, function(nm) {
    
    if (is.na(nm) || nm == "") return("")
    
    # Remove trailing credentials if they survived prior meeting-level cleaning.
    nm2 <- nm |>
      str_replace(
        regex(
          ",?\\s*(MD|PhD|DO|MBBS|CNP|NP|PA|MS|MPH|MBA|PGY\\d+)(,?\\s*(MD|PhD|DO|MBBS|CNP|NP|PA|MS|MPH|MBA|PGY\\d+))*\\s*$",
          ignore_case = TRUE
        ),
        ""
      ) |>
      str_squish()
    
    # Handle "Last, First Middle" explicitly.
    if (str_detect(nm2, ",")) {
      
      parts <- str_split_fixed(nm2, ",", 2)
      
      last_name <- parts[1] |>
        str_squish() |>
        str_to_lower()
      
      given_part <- parts[2] |>
        str_squish()
      
      first_name <- given_part |>
        str_split("\\s+") |>
        pluck(1, 1) |>
        str_to_lower()
      
      key <- paste(first_name, last_name)
      
    } else {
      
      words <- nm2 |>
        str_replace_all("[^A-Za-z0-9' -]", " ") |>
        str_squish() |>
        str_split("\\s+") |>
        pluck(1)
      
      if (length(words) == 1) {
        key <- str_to_lower(words[1])
      } else {
        key <- paste(
          str_to_lower(words[1]),
          str_to_lower(words[length(words)])
        )
      }
    }
    
    key |>
      str_replace_all("[^a-z0-9]", "") |>
      str_trim()
  })
}

industry_people <- readr::read_csv(
  person_override_file,
  show_col_types = FALSE
) |>
  mutate(
    name_key = clean_name_key(name),
    company = str_trim(company)
  )

# ------------------------------------------------------------
# Embedded identity reconciliation
# ------------------------------------------------------------
#
# Keep all person-name reconciliation here so the analysis remains
# self-contained and auditable.
#
# observed_key:
#   current normalized identity key produced by clean_name_key()
#
# canonical_name:
#   preferred display name for the community page
#
# exclude:
#   TRUE for non-person / artifact identifiers that should not appear
#
# This table does two jobs:
#   1. merges clearly duplicated identities that currently have different keys
#   2. cleans malformed / reversed / credential-heavy display names that are
#      already grouped under one key
#
# Deliberately NOT merged here because identity is not certain from attendance
# alone:
#   - "Mariam" and "Mariam El-Ashmawy"

name_reconciliation <- tribble(
  ~observed_key,          ~canonical_name,             ~exclude,
  
  # Clear duplicate identities across different keys
  "adeadamson",           "Adewole S. Adamson",        FALSE,
  "adewoleadamson",       "Adewole S. Adamson",        FALSE,
  
  "andrewknight",         "Andrew D. Knight",          FALSE,
  "andyknight",           "Andrew D. Knight",          FALSE,
  
  "sameer",               "Sameer Gupta",              FALSE,
  "sameergupta",          "Sameer Gupta",              FALSE,
  "samirgupta",           "Sameer Gupta",              FALSE,
  
  "vernonsondak",         "Vern Sondak",               FALSE,
  "vernsondak",           "Vern Sondak",               FALSE,
  
  # Non-person / artifact
  "d237",                 NA_character_,               TRUE,
  
  # Preferred display names / malformed Teams strings
  "adewunmiadelaja",      "Adewunmi O. Adelaja",       FALSE,
  "ajaysharma",           "Ajay N. Sharma",            FALSE,
  "christinecimoch",      "Christine C. Cimoch",       FALSE,
  "davidmiller",          "David M. Miller",            FALSE,
  "elizabethbuchbinder",  "Elizabeth I. Buchbinder",   FALSE,
  "elizabethlilley",      "Elizabeth J. Lilley",       FALSE,
  "francescollichio",     "Frances Collichio",         FALSE,
  "howardkaufman",        "Howard L. Kaufman",         FALSE,
  "isaacbrownell",        "Isaac Brownell",            FALSE,
  "jenniferdesimone",     "Jennifer DeSimone",         FALSE,
  "jessicafewkes",        "Jessica L. Fewkes",         FALSE,
  "julianeczapla",        "Juliane Andrade Czapla",    FALSE,
  "kamanehmontazeri",     "Kamaneh Montazeri",         FALSE,
  "karamkhaddour",        "Karam Khaddour",            FALSE,
  "kevinemerick",         "Kevin S. Emerick",          FALSE,
  "kristarubin",          "Krista M. Rubin",           FALSE,
  "larisageskin",         "Larisa Geskin",             FALSE,
  "lauraferris",          "Laura Ferris",              FALSE,
  "laurenoloughlin",      "Lauren O'Loughlin",         FALSE,
  "meghanmooradian",      "Meghan J. Mooradian",       FALSE,
  "mehranyusuf",          "Mehran Behruj Yusuf",       FALSE,
  "minabakhtiar",         "Mina Bakhtiar",             FALSE,
  "mollyyancovitz",       "Molly Yancovitz",           FALSE,
  "nikhilkhushalani",     "Nikhil Khushalani",         FALSE,
  "reeddrews",            "Reed E. Drews",             FALSE,
  "rhodaalani",           "Rhoda Alani",               FALSE,
  "rossmerkin",           "Ross D. Merkin",            FALSE,
  "tatyanasharova",       "Tatyana Sharova",           FALSE,
  "vatchetchekmedyian",   "Vatche Tchekmedyian",      FALSE
) |>
  mutate(
    canonical_key = if_else(
      is.na(canonical_name),
      NA_character_,
      clean_name_key(canonical_name)
    )
  )

apply_name_reconciliation <- function(df) {
  
  df |>
    left_join(
      name_reconciliation,
      by = c("name_key" = "observed_key")
    ) |>
    mutate(
      exclude = replace_na(exclude, FALSE),
      
      name = if_else(
        !is.na(canonical_name),
        canonical_name,
        name
      ),
      
      name_key = if_else(
        !is.na(canonical_key),
        canonical_key,
        name_key
      )
    ) |>
    filter(!exclude) |>
    select(
      -canonical_name,
      -canonical_key,
      -exclude
    )
}


normalize_email <- function(x) {
  x |>
    replace_na("") |>
    as.character() |>
    str_replace_all("\\\\", "") |>
    str_to_lower() |>
    str_trim()
}

extract_domain <- function(email) {
  ifelse(
    str_detect(email, "@"),
    str_replace(email, "^.*@", ""),
    ""
  )
}

standardize_attendance <- function(path, folder_name) {
  
  x <- readr::read_csv(
    path,
    show_col_types = FALSE
  )
  
  nms <- names(x)
  
  # Name
  name_col <- case_when(
    "name" %in% nms ~ "name",
    "Name" %in% nms ~ "Name",
    "participant" %in% nms ~ "participant",
    "Participant" %in% nms ~ "Participant",
    TRUE ~ NA_character_
  )
  
  # Minutes
  minutes_col <- case_when(
    "minutes" %in% nms ~ "minutes",
    "Minutes" %in% nms ~ "Minutes",
    "duration_min" %in% nms ~ "duration_min",
    "attendance_minutes" %in% nms ~ "attendance_minutes",
    "duration_minutes" %in% nms ~ "duration_minutes",
    TRUE ~ NA_character_
  )
  
  # Email is useful but optional.
  email_col <- case_when(
    "email" %in% nms ~ "email",
    "Email" %in% nms ~ "Email",
    TRUE ~ NA_character_
  )
  
  if (is.na(name_col) || is.na(minutes_col)) {
    warning(
      "Skipping ", folder_name,
      ": attendance_clean.csv does not contain recognizable name/minutes columns. ",
      "Columns found: ",
      paste(nms, collapse = " | ")
    )
    return(NULL)
  }
  
  tibble(
    meeting_folder = folder_name,
    meeting_date = extract_meeting_date(folder_name),
    meeting_type = classify_meeting(folder_name),
    name = as.character(x[[name_col]]),
    minutes = suppressWarnings(as.numeric(x[[minutes_col]])),
    email = if (!is.na(email_col)) as.character(x[[email_col]]) else ""
  ) |>
    mutate(
      name = str_squish(name),
      name_key = clean_name_key(name),
      email = normalize_email(email),
      domain = extract_domain(email)
    ) |>
    filter(
      name != "",
      !is.na(minutes)
    )
}

# ------------------------------------------------------------
# Scan attendance archive
# ------------------------------------------------------------

meeting_folders <- list.dirs(
  attendance_root,
  recursive = FALSE,
  full.names = TRUE
)

clean_files <- file.path(
  meeting_folders,
  "attendance_clean.csv"
)

has_clean <- file.exists(clean_files)

meeting_folders <- meeting_folders[has_clean]
clean_files <- clean_files[has_clean]

if (!length(clean_files)) {
  stop(
    "No attendance_clean.csv files were found under:\n",
    attendance_root
  )
}

attendance_list <- map2(
  clean_files,
  basename(meeting_folders),
  standardize_attendance
)

attendance_long <- bind_rows(attendance_list)

if (!nrow(attendance_long)) {
  stop("Attendance files were found, but no usable attendance rows were produced.")
}

# Apply the embedded identity lookup before any cross-meeting aggregation.
attendance_long <- apply_name_reconciliation(attendance_long)

# ------------------------------------------------------------
# Collapse duplicates within a meeting
# ------------------------------------------------------------

attendance_long <- attendance_long |>
  group_by(
    meeting_folder,
    meeting_date,
    meeting_type,
    name_key
  ) |>
  summarise(
    name = first(name),
    minutes = sum(minutes, na.rm = TRUE),
    email = {
      z <- email[email != "" & !is.na(email)]
      if (length(z)) z[[1]] else ""
    },
    domain = {
      z <- domain[domain != "" & !is.na(domain)]
      if (length(z)) z[[1]] else ""
    },
    .groups = "drop"
  )

# ------------------------------------------------------------
# Identify industry
# ------------------------------------------------------------

attendance_long <- attendance_long |>
  left_join(
    industry_domains,
    by = "domain"
  ) |>
  left_join(
    industry_people |>
      select(
        name_key,
        company_override = company
      ),
    by = "name_key"
  ) |>
  mutate(
    company = coalesce(
      na_if(company_override, ""),
      na_if(company, "")
    ),
    audience = if_else(
      !is.na(company),
      "Industry",
      "Academic / clinical"
    )
  ) |>
  select(
    -company_override
  )

# ------------------------------------------------------------
# Denominators
# ------------------------------------------------------------

meeting_summary <- attendance_long |>
  distinct(
    meeting_folder,
    meeting_date,
    meeting_type
  ) |>
  arrange(
    meeting_date,
    meeting_folder
  )

n_tracked_meetings <- nrow(meeting_summary)

n_tracked_jc <- meeting_summary |>
  filter(meeting_type == "Journal Club") |>
  nrow()

n_tracked_rf <- meeting_summary |>
  filter(meeting_type == "Research Forum") |>
  nrow()

# ------------------------------------------------------------
# Academic / clinical participant summary
# ------------------------------------------------------------

academic <- attendance_long |>
  filter(audience == "Academic / clinical")

member_summary <- academic |>
  group_by(name_key) |>
  summarise(
    name = first(name),
    meetings_attended = n_distinct(meeting_folder),
    journal_clubs_attended = n_distinct(
      meeting_folder[meeting_type == "Journal Club"]
    ),
    research_forums_attended = n_distinct(
      meeting_folder[meeting_type == "Research Forum"]
    ),
    total_minutes = sum(minutes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    tracked_meeting_attendance_pct =
      if (n_tracked_meetings > 0) {
        100 * meetings_attended / n_tracked_meetings
      } else {
        rep(NA_real_, n())
      },
    
    journal_club_attendance_pct =
      if (n_tracked_jc > 0) {
        100 * journal_clubs_attended / n_tracked_jc
      } else {
        rep(NA_real_, n())
      },
    
    research_forum_attendance_pct =
      if (n_tracked_rf > 0) {
        100 * research_forums_attended / n_tracked_rf
      } else {
        rep(NA_real_, n())
      }
  )

max_minutes <- max(
  member_summary$total_minutes,
  na.rm = TRUE
)

member_summary <- member_summary |>
  mutate(
    normalized_minutes_pct =
      if (max_minutes > 0) {
        100 * total_minutes / max_minutes
      } else {
        rep(0, n())
      }
  ) |>
  arrange(
    desc(journal_club_attendance_pct),
    desc(normalized_minutes_pct),
    name
  ) |>
  mutate(
    rank = row_number()
  )

# ------------------------------------------------------------
# Industry recognition
# ------------------------------------------------------------

industry_summary <- attendance_long |>
  filter(audience == "Industry") |>
  group_by(
    name_key,
    company
  ) |>
  summarise(
    name = first(name),
    meetings_attended = n_distinct(meeting_folder),
    total_minutes = sum(minutes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(
    company,
    desc(meetings_attended),
    desc(total_minutes),
    name
  )

company_summary <- industry_summary |>
  group_by(company) |>
  summarise(
    unique_colleagues = n_distinct(name_key),
    total_person_minutes = sum(total_minutes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(unique_colleagues), company)

# ------------------------------------------------------------
# Meeting-level summary
# ------------------------------------------------------------

meeting_detail <- attendance_long |>
  group_by(
    meeting_folder,
    meeting_date,
    meeting_type
  ) |>
  summarise(
    unique_people = n_distinct(name_key),
    academic_people = n_distinct(
      name_key[audience == "Academic / clinical"]
    ),
    industry_people = n_distinct(
      name_key[audience == "Industry"]
    ),
    total_person_minutes = sum(minutes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(
    meeting_date,
    meeting_folder
  )

# ------------------------------------------------------------
# Write outputs
# ------------------------------------------------------------

readr::write_csv(
  attendance_long,
  file.path(output_dir, "community_attendance_long.csv")
)

readr::write_csv(
  member_summary,
  file.path(output_dir, "community_member_summary.csv")
)

readr::write_csv(
  industry_summary,
  file.path(output_dir, "community_industry_summary.csv")
)

readr::write_csv(
  company_summary,
  file.path(output_dir, "community_company_summary.csv")
)

readr::write_csv(
  meeting_detail,
  file.path(output_dir, "community_meeting_summary.csv")
)

# ------------------------------------------------------------
# Optional identity audit
# ------------------------------------------------------------
#
# Uncomment either block when you want to inspect every final member name
# or every observed name variant before/after reconciliation.
#
# member_summary |>
#   arrange(name) |>
#   select(
#     name_key,
#     name,
#     meetings_attended,
#     journal_clubs_attended,
#     research_forums_attended,
#     total_minutes
#   ) |>
#   print(n = Inf, width = Inf)
#
# attendance_long |>
#   distinct(name_key, name) |>
#   arrange(name_key, name) |>
#   print(n = Inf, width = Inf)

# ------------------------------------------------------------
# Console summary
# ------------------------------------------------------------

message("")
message("SoCO Community Highlights processing complete")
message("---------------------------------------------")
message("Tracked meetings: ", n_tracked_meetings)
message("  Journal Clubs: ", n_tracked_jc)
message("  Research Forums: ", n_tracked_rf)
message(
  "Unique academic / clinical participants: ",
  nrow(member_summary)
)
message(
  "Unique industry colleagues: ",
  nrow(industry_summary)
)
message(
  "Industry companies recognized: ",
  nrow(company_summary)
)
message("")
message("Top academic / clinical participant by tracked minutes:")
message(
  member_summary$name[[1]],
  " — ",
  round(member_summary$total_minutes[[1]]),
  " min; ",
  round(member_summary$journal_club_attendance_pct[[1]]),
  "% of tracked Journal Clubs"
)
message("")
message("Outputs written to:")
message(output_dir)
