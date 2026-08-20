# make_socojc_community_survey_dictionary.R
# Builds a streamlined REDCap data dictionary for SoCO Journal Club
# Purpose: characterize the Society of Cutaneous Oncology community
# Journal Club date: August 14, 2026

# ---- LOAD CONFIG ----
if (!file.exists("files/config/config.R")) {
  stop("config.R file not found. Please create it with REDCAP_URI and REDCAP_TOKEN variables.")
}

source("files/config/config.R")

# ---- LOAD PACKAGES ----
library(REDCapR)

# ---- OUTPUT ----
project_name <- "SoCO_Journal_Club_Survey"

out_dir <- "files/data dictionary"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

out_path <- file.path(
  out_dir,
  sprintf("%s_Community_DataDictionary_%s.csv", project_name, Sys.Date())
)

# ---- INPUTS ----
form_name <- "soco_jc_survey"
journal_club_date <- "August 14"

# ---- Helper: "1, A | 2, B | ..." ----
choices_str <- function(labels) {
  paste(paste0(seq_along(labels), ", ", labels), collapse = " | ")
}

############################################################
# Styled Descriptive Fields
############################################################

q_intro <- list(
  var = "survey_intro",
  type = "descriptive",
  label = paste0(
    '<div style="background: linear-gradient(135deg, #1A1F26, #2B2F33); ',
    'color: white; padding: 22px 24px; border-radius: 16px; ',
    'margin-bottom: 24px; box-shadow: 0 4px 14px rgba(0,0,0,0.18); ',
    'border-top: 6px solid #E25A4E;">',
    
    '<div style="font-size: 28px; font-weight: 800; line-height:1.15; ',
    'margin-bottom: 14px;">',
    'SoCO Journal Club<br>Community Survey',
    '</div>',
    
    '<div style="font-size: 15.5px; line-height: 1.4; font-weight:500; ',
    'color:#FFFFFF; background:rgba(255,255,255,0.08); ',
    'border-left:4px solid #2BB3A3; padding:10px 12px; ',
    'border-radius:8px; margin-bottom:14px;">',
    'A few brief questions to help us understand who is participating in the ',
    'Society of Cutaneous Oncology community.',
    '</div>',
    
    '<div style="display:inline-block; background:rgba(255,255,255,0.10); ',
    'border:1px solid rgba(255,255,255,0.22); border-radius:999px; ',
    'padding:6px 12px; font-size:15px; font-weight:700; ',
    'color:#FFFFFF;">',
    journal_club_date, ' Journal Club',
    '</div>',
    
    '</div>'
  )
)

q_background_header <- list(
  var = "background_header",
  type = "descriptive",
  label = paste0(
    '<div style="background:#F3F6FA; border-left:6px solid #2F80ED; ',
    'padding:14px 18px; border-radius:10px; margin:18px 0;">',
    '<strong>Participant Background</strong><br>',
    '<span style="font-weight:400;">',
    'These questions help us characterize the breadth and experience of the SoCO community.',
    '</span>',
    '</div>'
  )
)

############################################################
# SoCO Journal Club Community Survey Questions
############################################################

# -----------------------------
# Question 1 - Name
# -----------------------------
q1 <- list(
  var = "respondent_name",
  type = "text",
  label = "Please provide your name.",
  required = "y"
)

# -----------------------------
# Question 2 - Current professional role
# -----------------------------
q2 <- list(
  var = "professional_role",
  type = "radio",
  label = "Which of the following best describes your current professional role?",
  choices = c(
    "Student - medical, graduate, or other",
    "Postgraduate trainee - resident or fellow",
    "Postdoctoral researcher",
    "Clinical faculty / practicing clinician",
    "Research faculty / scientist",
    "Advanced practice provider",
    "Other"
  ),
  required = "y"
)

# -----------------------------
# Question 3 - Institution / organization
# -----------------------------
q3 <- list(
  var = "institution",
  type = "text",
  label = "What is your primary institution or organization?",
  required = "y"
)

# -----------------------------
# Question 4 - Time in SoCO community
# -----------------------------
q4 <- list(
  var = "soco_tenure",
  type = "radio",
  label = "How long have you been part of the Society of Cutaneous Oncology community?",
  choices = c(
    "This is my first SoCO meeting",
    "Less than 1 year",
    "1-2 years",
    "More than 2 years",
    "Since the beginning"
  ),
  required = "y"
)

############################################################
# Combine Questions
############################################################

questions <- list(
  list(q = q_intro, section = ""),
  list(q = q_background_header, section = ""),
  list(q = q1, section = ""),
  list(q = q2, section = ""),
  list(q = q3, section = ""),
  list(q = q4, section = "")
)

############################################################
# REDCap Data Dictionary Structure
############################################################

cols <- c(
  "Variable / Field Name",
  "Form Name",
  "Section Header",
  "Field Type",
  "Field Label",
  "Choices, Calculations, OR Slider Labels",
  "Field Note",
  "Text Validation Type OR Show Slider Number",
  "Text Validation Min",
  "Text Validation Max",
  "Identifier?",
  "Branching Logic (Show field only if...)",
  "Required Field?",
  "Custom Alignment",
  "Question Number (surveys only)",
  "Matrix Group Name",
  "Matrix Ranking?",
  "Field Annotation"
)

blank_row <- function() {
  x <- as.list(rep("", length(cols)))
  names(x) <- cols
  x
}

build_question_row <- function(q, form_name, section_header = "") {
  r <- blank_row()
  
  r[["Variable / Field Name"]] <- q$var
  r[["Form Name"]] <- form_name
  r[["Section Header"]] <- section_header
  r[["Field Type"]] <- q$type
  r[["Field Label"]] <- q$label
  
  if (!is.null(q$choices) && length(q$choices) > 0) {
    r[["Choices, Calculations, OR Slider Labels"]] <- choices_str(q$choices)
  }
  
  if (!is.null(q$branching)) {
    r[["Branching Logic (Show field only if...)"]] <- q$branching
  }
  
  if (!is.null(q$required)) {
    r[["Required Field?"]] <- q$required
  }
  
  r
}

############################################################
# Build Data Dictionary
############################################################

r_id <- blank_row()
r_id[["Variable / Field Name"]] <- "record_id"
r_id[["Form Name"]] <- form_name
r_id[["Field Type"]] <- "text"
r_id[["Field Label"]] <- "Record ID"

dd_list <- list(r_id)

for (i in seq_along(questions)) {
  dd_list[[length(dd_list) + 1]] <- build_question_row(
    questions[[i]]$q,
    form_name,
    section_header = questions[[i]]$section
  )
}

dd <- do.call(
  rbind,
  lapply(dd_list, function(x) {
    as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
  })
)

dd <- dd[, cols]

############################################################
# Write CSV
############################################################

write.csv(dd, out_path, row.names = FALSE, na = "")

cat("Wrote CSV:", out_path, "\\n")

############################################################
# Upload to REDCap
############################################################

cat("Uploading to REDCap...\\n")

upload_result <- REDCapR::redcap_metadata_write(
  ds = dd,
  redcap_uri = REDCAP_URI,
  token = REDCAP_TOKEN
)

if (upload_result$success) {
  cat("Successfully uploaded data dictionary to REDCap!\\n")
  cat("Records affected:", upload_result$records_affected_count, "\\n")
} else {
  warning("Upload failed. Check your token and REDCap permissions.\\n")
  cat("Error message:", upload_result$raw_text, "\\n")
}
