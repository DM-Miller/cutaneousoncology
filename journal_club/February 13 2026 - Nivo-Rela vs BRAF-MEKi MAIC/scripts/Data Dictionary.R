# make_socojc_survey_dictionary.R
# Builds a REDCap data dictionary for monthly journal club survey
# and uploads it to REDCap via API

# ---- LOAD CONFIG ----
if (!file.exists("files/config/config.R")) {
  stop("config.R file not found. Please create it with REDCAP_URI and REDCAP_TOKEN variables.")
}
source("files/config/config.R")

# ---- LOAD PACKAGES ----
library(REDCapR)

# ---- OUTPUT ----
project_name <- "SoCO JC Survey"
out_dir  <- "files/data dictionary"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
out_path <- file.path(out_dir, sprintf("%s_DataDictionary_%s.csv", project_name, Sys.Date()))

# ---- Helper: "1, A | 2, B | ..." ----
choices_str <- function(labels) {
  paste(paste0(seq_along(labels), ", ", labels), collapse = " | ")
}

# ---- INPUTS (edit only these each month) ----
form_name <- "soco_jc_survey"

# Journal club focus (edit monthly)
analysis_topic <- "MAIC / indirect comparisons"
supplement_name <- "Supplemental Methods"
disease_context <- "advanced melanoma"  # edit as desired

# -----------------------------
# Question 1 - Standard demographics
# -----------------------------
q1 <- list(
  var = "who_are_you",
  type = "radio",
  label = "Which of the following best describes you?",
  choices = c(
    "Medical Oncologist",
    "Medical Dermatologist",
    "Surgical Oncologist",
    "Mohs Surgeon",
    "Radiation Oncologist",
    "Non-Clinician Researcher",
    "Student/Trainee",
    "Advanced Practice Provider",
    "Other"
  )
)

# -----------------------------
# Question 2 - Practice focus / relevance
# -----------------------------
q2 <- list(
  var = "clinical_relevance",
  type = "radio",
  label = paste0("How clinically relevant is this topic (", analysis_topic, ") to your work?"),
  choices = c(
    "Highly relevant (I make or advise treatment decisions in this space)",
    "Moderately relevant (I interpret evidence but don’t routinely make the final decision)",
    "Somewhat relevant (I am interested but it rarely affects my day-to-day work)",
    "Not very relevant",
    "Not applicable"
  )
)

# -----------------------------
# Question 3 - Familiarity with MAIC / ITCs
# -----------------------------
q3 <- list(
  var = "maic_familiarity",
  type = "radio",
  label = "Before this journal club, how familiar were you with matching-adjusted indirect comparisons (MAICs) or indirect treatment comparisons (ITCs)?",
  choices = c(
    "Very familiar — I have helped design, analyze, or publish one",
    "Moderately familiar — I have read several and understand the methodology",
    "Somewhat familiar — I have read one or two but remain uncertain",
    "Minimally familiar — I have heard of them but not reviewed one closely",
    "Not familiar at all prior to this session"
  )
)

# -----------------------------
# Question 4 - Evidence hierarchy / perceived role
# -----------------------------
q4 <- list(
  var = "maic_evidence_role",
  type = "radio",
  label = "In your view, where do MAICs generally fall in the hierarchy of clinical evidence?",
  choices = c(
    "Often persuasive and clinically meaningful when done rigorously",
    "Useful but hypothesis-generating rather than practice-changing",
    "Occasionally informative but usually limited by residual confounding",
    "Rarely persuasive compared with randomized or real-world data",
    "I do not find MAICs helpful in most settings"
  )
)

# -----------------------------
# Question 5 - Read the supplemental methods?
# -----------------------------
q5 <- list(
  var = "read_supp_methods",
  type = "radio",
  label = paste0("Did you read the ", supplement_name, " document linked for this paper?"),
  choices = c(
    "I read the entire document",
    "I read selected parts of it",
    "I skimmed it",
    "I did not read it"
  )
)

# -----------------------------
# Question 6 - Usefulness of supplemental methods
# -----------------------------
q6 <- list(
  var = "supp_methods_usefulness",
  type = "radio",
  label = paste0("How useful did you find the ", supplement_name, " in helping you interpret the ", analysis_topic, " analysis?"),
  choices = c(
    "Very useful",
    "Somewhat useful",
    "Neutral",
    "Slightly useful",
    "Not useful",
    "I did not read it"
  )
)

# -----------------------------
# Question 7 - Pharma collaboration experience
# -----------------------------
q7 <- list(
  var = "industry_collab_experience",
  type = "radio",
  label = "Have you previously collaborated with industry on a clinical research manuscript or analysis (e.g., advisory-driven analysis, post-hoc study, MAIC/ITC, registry analysis, etc.)?",
  choices = c(
    "Yes — multiple times",
    "Yes — once or twice",
    "No, but I have been invited",
    "No — I have not had this experience"
  )
)

# -----------------------------
# Question 8 - Data access in industry collaborations
# -----------------------------
q8 <- list(
  var = "industry_data_access",
  type = "radio",
  label = "In your past industry-sponsored collaborations, how much access did you typically have to the underlying data?",
  choices = c(
    "Full access to all relevant patient-level data",
    "Partial access (e.g., summary tables, select analyses, limited extracts)",
    "Interpretation access without raw data (reviewing outputs only)",
    "Primarily advisory input without meaningful data access",
    "I have not participated in this type of work"
  )
)

# -----------------------------
# Question 9 - Perceived role of academic collaborators
# -----------------------------
q9 <- list(
  var = "perceived_academic_role",
  type = "radio",
  label = "In industry-sponsored analyses that are not traditional clinical trials (e.g., indirect comparisons, real-world analyses, or post-hoc modeling studies), how do you generally view the role of academic collaborators?",
  choices = c(
    "True scientific partners with shared ownership of the analysis",
    "Scientific advisors with meaningful influence but limited control",
    "Content experts brought in to contextualize pre-specified analyses",
    "Primarily to provide external credibility and clinical framing",
    "It varies widely by project",
    "I have not participated in this type of work"
  )
)

# -----------------------------
# Question 10 - Comfort with limited data access model
# -----------------------------
q10 <- list(
  var = "comfort_limited_access",
  type = "radio",
  label = "How comfortable are you with the typical model where academics may not have full data access in industry-sponsored analyses?",
  choices = c(
    "Very comfortable — this is a reasonable and transparent model",
    "Somewhat comfortable — acceptable with clear disclosure",
    "Neutral / depends on context",
    "Somewhat uncomfortable",
    "Very uncomfortable",
    "Not applicable / no opinion"
  )
)

# -----------------------------
# Question 11 - What would improve trust?
# -----------------------------
q11 <- list(
  var = "increase_trust_factors",
  type = "checkbox",
  label = "Which factors would most increase your confidence in industry–academic collaborative analyses like this? (Select all that apply)",
  choices = c(
    "Greater transparency around data access limitations",
    "Broader access to patient-level data for academic authors",
    "Pre-specified analysis plans with academic input",
    "Independent replication by non-industry groups",
    "Clearer articulation of limitations in the manuscript",
    "Journal or peer-review requirements for reproducibility artifacts (e.g., code, diagnostics)",
    "I am already comfortable with this model",
    "Other"
  )
)

# ---- REDCap 18 columns (exact names) ----
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

# ---- Helper to convert labels to REDCap-friendly codes ----
clean_for_redcap <- function(text) {
  text <- tolower(text)
  text <- gsub(">", "greater_than_", text)
  text <- gsub("/", "_", text)
  text <- gsub("-", "__", text)
  text <- gsub(",", "_1", text)
  text <- gsub("[^a-z0-9_]", "_", text)
  text <- gsub("_+", "_", text)
  text <- gsub("^_|_$", "", text)
  text
}

# ---- Build rows from question list ----
build_question_row <- function(q, form_name, section_header = "") {
  r <- blank_row()
  r[["Variable / Field Name"]] <- q$var
  r[["Form Name"]] <- form_name
  r[["Section Header"]] <- section_header
  r[["Field Type"]] <- q$type
  r[["Field Label"]] <- q$label
  
  if (!is.null(q$choices) && length(q$choices) > 0) {
    codes <- sapply(q$choices, clean_for_redcap)
    r[["Choices, Calculations, OR Slider Labels"]] <-
      paste(paste0(codes, ", ", q$choices), collapse = " | ")
  }
  
  r
}

# ---- Build record_id row ----
r_id <- blank_row()
r_id[["Variable / Field Name"]] <- "record_id"
r_id[["Form Name"]] <- form_name
r_id[["Field Type"]] <- "text"
r_id[["Field Label"]] <- "Record ID"

# ---- Collect all questions ----
questions <- list(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11)

# Build rows
dd_list <- list(r_id)
for (i in seq_along(questions)) {
  dd_list[[length(dd_list) + 1]] <- build_question_row(
    questions[[i]],
    form_name,
    section_header = paste("Question", i)
  )
}

# Create data.frame
dd <- do.call(rbind, lapply(dd_list, function(x) as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)))
dd <- dd[, cols]

# ---- Write CSV ----
write.csv(dd, out_path, row.names = FALSE, na = "")
cat("Wrote CSV:", out_path, "\n")

# ---- Upload to REDCap ----
cat("Uploading to REDCap...\n")
upload_result <- REDCapR::redcap_metadata_write(
  ds = dd,
  redcap_uri = REDCAP_URI,
  token = REDCAP_TOKEN
)

if (upload_result$success) {
  cat("Successfully uploaded data dictionary to REDCap!\n")
  cat("Records affected:", upload_result$records_affected_count, "\n")
} else {
  warning("Upload failed. Check your token and REDCap permissions.\n")
  cat("Error message:", upload_result$raw_text, "\n")
}

