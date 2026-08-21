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
disease_name <- "CSCC"  # Change monthly

# Question 1 - Standard demographics
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

# Question 2 - Patient volume
q2 <- list(
  var = "patients_per_month",
  type = "radio",
  label = paste0("How many high risk ", disease_name, " patients do you see each month?"),
  choices = c(
    "1-2",
    "3-5",
    "6-10",
    "11-20",
    ">20",
    paste0("I am a clinician but I do not treat ", disease_name),
    "I am not a clinician"
  )
)

# Question 3 - Experience with treatment
q3 <- list(
  var = "used_nivo_ipi",
  type = "radio",
  label = paste0("Have you ever used Nivo + Ipi in the preoperative setting for high risk resectable ", disease_name, "?"),
  choices = c(
    "Yes",
    "No",
    paste0("I am a clinician but I do not manage ", disease_name, " patients"),
    "I am not a clinician"
  )
)

# Question 4 - Preferred agent
q4 <- list(
  var = "preferred_preop_agent",
  type = "radio",
  label = paste0("What is your (or your team's) preferred preoperative agent for high risk resectable ", disease_name, "?"),
  choices = c(
    "Cemiplimab",
    "Pembrolizumab",
    "Nivolumab",
    "Nivo plus Ipi",
    "Other",
    paste0("I am a clinician but I do not manage ", disease_name, " patients"),
    "I am not a clinician"
  )
)

# Question 5 - Number of doses
q5 <- list(
  var = "doses_before_surgery",
  type = "radio",
  label = "How many doses do you (or your team) typically give prior to surgery if you choose a neoadjuvant plan?",
  choices = c(
    "1",
    "2",
    "3",
    "4",
    "Other",
    paste0("I am a clinician but I do not manage ", disease_name, " patients"),
    "I am not a clinician"
  )
)

# Question 6 - Perceived necessary number of doses
q6 <- list(
  var = "doses_needed_for_benefit",
  type = "radio",
  label = paste0(
    "In your clinical opinion, how many neoadjuvant doses are actually needed ",
    "to achieve most of the potential benefit for high risk resectable ",
    disease_name,
    ", even if this differs from what you typically do in practice?"
  ),
  choices = c(
    "1",
    "2",
    "3",
    "4",
    "Other",
    paste0("I am a clinician but I do not manage ", disease_name, " patients"),
    "I am not a clinician"
  )
)

# Question 7 - Surgery decision
q7 <- list(
  var = "surgery_decision",
  type = "radio",
  label = "If you embark on a neoadjuvant approach (i.e. start initial mgmt plan with neoadjuvant intent), does your team routinely take the patient to surgery or is that decision based on their response at a pre-surgical evaluation?",
  choices = c(
    "Routinely take to surgery",
    "Decision based on response at pre-surgical evaluation",
    paste0("I am a clinician but I do not manage ", disease_name, " patients"),
    "I am not a clinician"
  )
)

# Question 8 - Omit Surgery
q8 <- list(
  var = "indicators_omit_surgery",
  type = "checkbox",
  label = paste0(
    "Under what clinical circumstances would you feel comfortable ",
    "considering omission of surgery (with close observation +/- continuing ICI) after ",
    "frontline immunotherapy for high-risk resectable ", disease_name, "?"
  ),
  choices = c(
    "Meaningful improvement in patient-reported symptoms (e.g., pain)",
    "Visible decrease in tumor size on physical examination",
    "Complete clinical response on physical examination",
    "Partial response on CT or MRI",
    "Complete radiologic response on CT or MRI",
    "Substantial decrease in FDG-PET metabolic activity (e.g., ΔTLG50% reduction)",
    "Complete resolution of FDG-PET avidity",
    "Biopsy at ~4-8 weeks showing ≤50% viable tumor",
    "Biopsy at ~4-8 weeks showing ≤10% viable tumor",
    "Biopsy at ~4-8 weeks showing 0% viable tumor",
    "None — I would not omit surgery outside a clinical trial",
    paste0("I am a clinician but I do not manage ", disease_name, " patients"),
    "I am not a clinician"
  )
)

# Question 9 - Minimum response to omit surgery
q9 <- list(
  var = "minimum_response_omit_surgery",
  type = "radio",
  label = paste0(
    "If you were to consider omission of surgery (with close observation) after ",
    "frontline immunotherapy for high-risk resectable ", disease_name, ", ",
    "what is the *minimum level of response* you would require?"
  ),
  choices = c(
    "Meaningful symptomatic improvement alone",
    "Visible decrease in tumor size on physical examination",
    "Complete clinical response on physical examination",
    "Partial radiologic response (CT or MRI)",
    "Complete radiologic response (CT or MRI)",
    "Substantial reduction in FDG-PET metabolic activity (e.g., ΔTLG50% decrease)",
    "Complete resolution of FDG-PET avidity",
    "Biopsy at ~4-8 weeks showing ≤50% viable tumor",
    "Biopsy at ~4-8 weeks showing ≤10% viable tumor",
    "Biopsy at ~4-8 weeks showing 0% viable tumor",
    "I would not consider omitting surgery outside a clinical trial",
    paste0("I am a clinician but I do not manage ", disease_name, " patients"),
    "I am not a clinician"
  )
)

# Question 10 - Situations to Use Ipi/Nivo
q10 <- list(
  var = "situations_nivo_ipi",
  type = "checkbox",
  label = paste0(
    "In which of the following clinical situations would you consider using ",
    "frotline/pre-operative nivolumab plus ipilimumab (rather than PD-1 monotherapy) ",
    "for a patient with high-risk resectable", disease_name, " or a related squamous malignancy?"
  ),
  choices = c(
    # Ocular / periocular
    "Locally advanced conjunctival squamous cell carcinoma where surgical excision would require orbital exenteration",
    "Locally advanced periorbital *cutaneous* squamous cell carcinoma where surgery would require orbital exenteration",
    
    # Locally advanced / mutilating primary disease
    paste0("Locally advanced ", disease_name, 
           " at another critical site where surgery would be mutilating (e.g., major loss of function or disfigurement)"),
    paste0("Bulky regional nodal ", disease_name, 
           " (e.g., N2/N3) where cure is possible but surgery and/or radiation would carry very high morbidity"),
    
    # Prior PD-1 exposure
    paste0(disease_name, " with progression on prior anti–PD-1 monotherapy, where dual checkpoint blockade is being considered as salvage"),
    
    # Hematologic malignancy / CLL
    "Presence of an indolent hematologic malignancy such as CLL that does not currently require treatment",
    "Presence of a hematologic malignancy such as CLL that is currently receiving active systemic treatment",
    
    # Immunosuppression / complex hosts (optional but high-yield)
    paste0(disease_name, 
           " in a solid-organ transplant recipient after careful multidisciplinary discussion"),
    paste0(disease_name, 
           " in a patient on low-dose immunosuppression for autoimmune disease, after risk–benefit discussion"),
    
    # Safety / meta-options
    "None of the above — I would not use nivolumab plus ipilimumab in these scenarios",
    paste0("I am a clinician but I do not manage ", disease_name, " patients"),
    "I am not a clinician"
  )
)


# Question 11 - Barriers (checkbox)
q11 <- list(
  var = "barriers_nivo_ipi",
  type = "checkbox",
  label = "What barriers, if any, prevent you from using Nivo plus Ipi in the preoperative setting?",
  choices = c(
    "Concerns about toxicity or tolerability",
    "Cost or insurance coverage concerns",
    "Uncertainty about benefit versus risk",
    "Not FDA approved",
    "Not recommended by guidelines",
    "I have no concerns",
    paste0("I am a clinician but I do not manage ", disease_name, " patients"),
    "I am not a clinician"
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

# ---- Helper to convert underscores to REDCap-friendly format ----
# Helper to convert labels to REDCap-friendly codes
clean_for_redcap <- function(text) {
  text <- tolower(text)                     # Lowercase
  text <- gsub(">", "greater_than_", text)  # Handle > symbol
  text <- gsub("/", "_", text)              # Forward slash to underscore
  text <- gsub("-", "__", text)             # Hyphens to double underscore
  text <- gsub(",", "_1", text)             # Commas to _1
  text <- gsub("[^a-z0-9_]", "_", text)     # Any other special char to underscore
  text <- gsub("_+", "_", text)             # Multiple underscores to single
  text <- gsub("^_|_$", "", text)           # Remove leading/trailing underscores
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
    # Generate codes from labels
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
dd <- dd[, cols]  # enforce column order

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
