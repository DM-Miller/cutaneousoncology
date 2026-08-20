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
analysis_topic <- "IGNYTE Study"
disease_name <- "melanoma"
############################################################
# IGNYTE Pre-JC Survey Questions
# Focus: RP1 + nivolumab, FDA approval debate, intratumoral therapy,
# single-arm evidence, estimands / ICH E9(R1)
############################################################



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
# Question 2 - Patient volume
# -----------------------------
q2 <- list(
  var = "patients_per_month",
  type = "radio",
  label = paste0("How many advanced ", disease_name, " patients do you see each month?"),
  choices = c(
    "1-2",
    "3-5",
    "6-10",
    "11-20",
    ">20",
    paste0("I am a clinician but I do not manage ", disease_name, " patients"),
    "I am not a clinician"
  )
)

# -----------------------------
# Question 3 - PD-1 refractory melanoma exposure
# -----------------------------
q3 <- list(
  var = "pd1_refractory_exposure",
  type = "radio",
  label = paste0("How often do you care for patients with PD-1 refractory advanced ", disease_name, "?"),
  choices = c(
    "Frequently",
    "Occasionally",
    "Rarely",
    "Never",
    paste0("I am a clinician but I do not manage ", disease_name, " patients"),
    "I am not a clinician"
  )
)

# -----------------------------
# Question 4 - Experience with intratumoral therapy
# -----------------------------
q4 <- list(
  var = "intratumoral_therapy_experience",
  type = "radio",
  label = paste0("Have you ever used an intratumoral therapy (for example, T-VEC) for ", disease_name, "?"),
  choices = c(
    "Yes",
    "No",
    "Familiar with these therapies but have not used one",
    paste0("I am a clinician but I do not manage ", disease_name, " patients"),
    "I am not a clinician"
  )
)

# -----------------------------
# Question 5 - Familiarity with oncolytic immunotherapy
# -----------------------------
q5 <- list(
  var = "oncolytic_familiarity",
  type = "radio",
  label = "Before this journal club, how familiar were you with oncolytic virus therapies such as T-VEC or RP1?",
  choices = c(
    "Very familiar",
    "Moderately familiar",
    "Somewhat familiar",
    "Minimally familiar",
    "Not familiar at all"
  )
)

# -----------------------------
# Question 6 - Familiarity with IGNYTE study
# -----------------------------
q6 <- list(
  var = "ignyte_familiarity",
  type = "radio",
  label = "Before this journal club, how familiar were you with the IGNYTE study of RP1 plus nivolumab in anti-PD-1 failed melanoma?",
  choices = c(
    "Very familiar",
    "Moderately familiar",
    "Somewhat familiar",
    "Minimally familiar",
    "Not familiar at all"
  )
)

# -----------------------------
# Question 7 - Persuasiveness of single-arm evidence
# -----------------------------
q7 <- list(
  var = "single_arm_persuasiveness",
  type = "radio",
  label = "In general, how persuasive do you find single-arm trials evaluating new therapies in PD-1 refractory melanoma?",
  choices = c(
    "Often sufficient for clinical decision-making",
    "Persuasive when results substantially exceed historical expectations",
    "Hypothesis-generating but rarely practice-changing",
    "Rarely persuasive without randomized data",
    "Not sure"
  )
)

# -----------------------------
# Question 8 - Meaningfulness of ORR in this setting
# -----------------------------
q8 <- list(
  var = "orr_clinical_meaningfulness",
  type = "radio",
  label = "In PD-1 refractory melanoma, how clinically meaningful do you consider objective response rate (ORR) as a primary endpoint?",
  choices = c(
    "Highly meaningful if responses are durable",
    "Moderately meaningful but insufficient alone",
    "Useful mainly as a signal-detection endpoint",
    "Limited clinical relevance without PFS or OS support",
    "Not sure"
  )
)

# -----------------------------
# Question 9 - Biologic plausibility of systemic effect
# -----------------------------
q9 <- list(
  var = "systemic_response_plausibility",
  type = "radio",
  label = "How biologically plausible do you find the idea that an intratumoral oncolytic therapy can induce meaningful systemic responses in uninjected lesions?",
  choices = c(
    "Highly plausible",
    "Plausible but still somewhat uncertain",
    "Possible but not strongly supported",
    "Unlikely to meaningfully affect visceral disease",
    "Not sure"
  )
)

# -----------------------------
# Question 10 - Weight given to injected lesion responses
# -----------------------------
q10 <- list(
  var = "injected_vs_systemic_inference",
  type = "radio",
  label = "When evaluating an intratumoral therapy, how much weight should responses in injected lesions carry when inferring systemic clinical benefit?",
  choices = c(
    "High weight",
    "Moderate weight if durability is shown",
    "Limited weight unless non-injected lesions also respond",
    "Minimal weight because of local treatment effects",
    "Not sure"
  )
)

# -----------------------------
# Question 11 - Should IGNYTE have supported approval?
# -----------------------------
q11 <- list(
  var = "ignyte_approval_view",
  type = "radio",
  label = "Based on your current understanding, should the IGNYTE study have been sufficient to support FDA approval of RP1 in PD-1 refractory melanoma?",
  choices = c(
    "Yes — evidence appears sufficient for accelerated approval",
    "Possibly — acceptable with post-marketing requirements",
    "Uncertain",
    "Probably not sufficient without randomized data",
    "Definitely not sufficient for accelerated approval"
  )
)

# -----------------------------
# Question 12 - Main limitation of IGNYTE
# -----------------------------
q12 <- list(
  var = "ignyte_main_limitation",
  type = "checkbox",
  label = "What do you see as the primary limitation(s) of the IGNYTE study?",
  choices = c(
    "Single-arm design",
    "Heterogeneous patient population",
    "Unclear contribution of RP1 versus nivolumab",
    "Interpretability of ORR endpoint",
    "Injected versus non-injected lesion issues",
    "Limited comparator context",
    "External validity concerns",
    "Sample size",
    "Other",
    "None the study was adequate"
  )
)

# -----------------------------
# Question 13 - Contribution of components
# -----------------------------
q13 <- list(
  var = "contribution_of_components",
  type = "radio",
  label = "How confidently can the treatment effect observed in IGNYTE be attributed specifically to RP1 rather than nivolumab alone?",
  choices = c(
    "Highly confident RP1 contributes meaningful activity",
    "Moderately confident RP1 contributes activity",
    "Uncertain contribution of RP1",
    "Likely primarily driven by nivolumab",
    "Cannot determine from available data"
  )
)

# -----------------------------
# Question 14 - Familiarity with ICH E9(R1)
# -----------------------------
q14 <- list(
  var = "ich_e9_familiarity",
  type = "radio",
  label = "Before this journal club, how familiar were you with the ICH E9(R1) estimand framework?",
  choices = c(
    "Very familiar — I have used it in protocol design, review, or analysis",
    "Moderately familiar",
    "Somewhat familiar — I have heard of estimands",
    "Minimally familiar",
    "Not familiar at all"
  )
)

# -----------------------------
# Question 15 - Prior use of ICH E9(R1)
# -----------------------------
q15 <- list(
  var = "ich_e9_prior_use",
  type = "radio",
  label = "Have you ever used ICH E9(R1) or estimand thinking when reviewing or designing a clinical trial protocol?",
  choices = c(
    "Yes — directly",
    "Yes — indirectly or conceptually",
    "No",
    "Not sure"
  )
)

# -----------------------------
# Question 16 - Estimand clarity in IGNYTE
# -----------------------------
q16 <- list(
  var = "ignyte_estimand_clarity",
  type = "radio",
  label = "How clearly do you think the IGNYTE study defines the treatment effect it aims to estimate?",
  choices = c(
    "Very clearly defined",
    "Mostly clear",
    "Somewhat unclear",
    "Substantially unclear",
    "Not sure"
  )
)

# -----------------------------
# Question 17 - Preferred confirmatory design
# -----------------------------
q17 <- list(
  var = "preferred_confirmatory_design",
  type = "radio",
  label = "Which confirmatory trial design would provide the most convincing evidence for RP1 in this setting?",
  choices = c(
    "Randomized against physician's choice therapy",
    "Randomized RP1 plus nivolumab versus nivolumab-based control",
    "External control using high-quality real-world data",
    "Single-arm study with stronger endpoint framework",
    "Other",
    "Not sure"
  )
)

# -----------------------------
# Question 18 - Evidence threshold for adoption
# -----------------------------
q18 <- list(
  var = "evidence_threshold_for_use",
  type = "radio",
  label = "What level of evidence would you require before using RP1 in routine practice for PD-1 refractory melanoma?",
  choices = c(
    "Current evidence is sufficient",
    "FDA approval would be sufficient",
    "Randomized phase II evidence would be required",
    "Phase III evidence would be required",
    "Guideline inclusion would be required",
    "Real-world validation would be required",
    "Not sure"
  )
)

# -----------------------------
# Question 19 - Practical barriers to adoption
# -----------------------------
q19 <- list(
  var = "implementation_barriers",
  type = "checkbox",
  label = "Which factors might limit adoption of intratumoral oncolytic immunotherapy in practice?",
  choices = c(
    "Need for injectable lesions",
    "Logistical complexity of intratumoral administration",
    "Limited comparative data versus existing options",
    "Reimbursement concerns",
    "Uncertainty about durability of benefit",
    "Toxicity concerns",
    "Lack of clinician familiarity",
    "Institutional workflow constraints",
    "Other"
  )
)

# -----------------------------
# Question 20 - Biggest uncertainty
# -----------------------------
q20 <- list(
  var = "key_uncertainty",
  type = "notes",
  label = "What is the single biggest uncertainty preventing clear interpretation of the IGNYTE study?"
)

############################################################
# Combine into question list
############################################################

ignyte_questions <- list(
  q1, q2, q3, q4, q5,
  q6, q7, q8, q9, q10,
  q11, q12, q13, q14, q15,
  q16, q17, q18, q19, q20
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
questions <- list(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14, q15, q16, q17, q18, q19, q20)

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

