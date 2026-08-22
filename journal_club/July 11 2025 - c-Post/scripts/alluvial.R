library(tidyverse)
library(ggalluvial)
df <- open_recent_file(
  directory = file.path(
    files_dir,
    "Pre_JC_survey_processed"
  )
)



#-----------------------------------------------------
# You no longer need to pivot the barriers
# Create a count table directly
df_counts_mgmt <- df %>%
  group_by(
    who_are_you,
    patients_per_month,
    used_adjuvant_ici,
    mgmt_case_1,
    mgmt_case_2,
    neoadjuvant_adjuvant
  ) %>%
  summarise(n = n(), .groups = "drop")

df_counts_mgmt <- df_counts_mgmt %>%
  mutate(
    who_are_you = recode(
      who_are_you,
      "Advanced Practice Provider" = "APP",
      "Medical Dermatologist" = "Dermatologist",
      "Medical Oncologist" = "Med Onc",
      "Mohs Surgeon" = "Mohs Surgeon",
      "Surgical Oncologist" = "Surg Onc",
      "Non Clinician Researcher" = "Scientist",
      "Student Trainee" = "Trainee",
      "Radiation Oncologist" = "Rad Onc"
    ),
    who_are_you = replace_na(who_are_you, "Missing"),
    who_are_you = factor(
      who_are_you,
      levels = c(
        "Missing",
        "Other",
        "Scientist",
        "Trainee",
        "APP",
        "Rad Onc",
        "Dermatologist",
        "Mohs Surgeon",
        "Surg Onc",
        "Med Onc"
      ),
      ordered = TRUE
    )
  )

# Recode any remaining spelling variations if needed
df_counts_mgmt <- df_counts_mgmt %>%
  mutate(
    patients_per_month = recode(
      patients_per_month,
      "1-2" = "1-2",
      "3-5" = "3-5",
      "6-10" = "6-10",
      "11-20" = "11-20",
      "Greater Than 20" = ">20",
      "I Am A Clinician But I Do Not Treat Cscc" = "Do Not Tx CSCC",
      "I Am Not A Clinician" = "Not A Clinician"
    ),
    patients_per_month = replace_na(patients_per_month, "Missing"),
    # Convert to ordered factor
    patients_per_month = factor(
      patients_per_month,
      levels = c(
        "Missing",
        "Not A Clinician",
        "Do Not Tx CSCC",
        "1-2",
        "3-5",
        "6-10",
        "11-20",
        ">20"
      ),
      ordered = TRUE
    )
  )


df_counts_mgmt <- df_counts_mgmt %>%
  mutate(
    mgmt_case_1 = recode(
      mgmt_case_1,
      "Surgery Post Op Rt Adjuvant Anti Pd1" = "Surg + RT\n + Adj PD1",
      "Surgery Art" = "Surgery + ART",
      "Definitive Apd1" = "Definitive ICI",
      "Definitive Rt Alone" = "Definitive RT Alone",
      "Pre Operative Anti Pd1 Surgery Post Op Rt Based On Response" = "Neoadjuvant ICI\n + Surgery + Path Eval",
      "Start Ici Reassess" = "Start ICI\n Reassess for\n Surgery or RT",
      "Not Applicable Clinician" = "Do Not Tx CSCC",
      "I Am Not A Clinician" = "Not A Clinician"
    ),
    mgmt_case_1 = replace_na(mgmt_case_1, "Missing"),
    mgmt_case_1 = factor(
      mgmt_case_1,
      levels = c(
        "Missing",
        "Not A Clinician",
        "Do Not Tx CSCC",
        "Not Sure",
        "Other",
        "Definitive RT Alone",
        "Surgery + ART",
        "Definitive ICI",
        "Start ICI\n Reassess for\n Surgery or RT",
        "Surg + RT\n + Adj PD1",
        "Neoadjuvant ICI\n + Surgery + Path Eval"
      ),
      ordered = TRUE
    )
  )

df_counts_mgmt <- df_counts_mgmt %>%
  mutate(
    mgmt_case_2 = recode(
      mgmt_case_2,
      "Very Comfortable" = "Very Comfortable",
      "Comfortable" = "Comfortable",
      "Ambivalent" = "Ambivalent",
      "Uncomfortable" = "Uncomfortable",
      "Very Uncomfortable" = "Very Uncomfortable",
      "Not Applicable Clinician" = "Do Not Tx CSCC",
      "I Am Not A Clinician" = "Not A Clinician"
    ),
    mgmt_case_2 = replace_na(mgmt_case_2, "Missing")
  )


df_counts_mgmt$mgmt_case_2 <- factor(
  df_counts_mgmt$mgmt_case_2,
  levels = c(
    "Not A Clinician",
    "Missing",
    "Very Uncomfortable",
    "Uncomfortable",
    "Ambivalent",
    "Comfortable",
    "Very Comfortable",
    "Do Not Tx CSCC"
  )
)

df_counts_mgmt <- df_counts_mgmt %>%
  mutate(
    used_adjuvant_ici = recode(
      used_adjuvant_ici,
      "Not Applicable Clinician" = "Do Not Tx CSCC",
      "I Am Not A Clinician" = "Not A Clinician"
    ),
    used_adjuvant_ici = replace_na(used_adjuvant_ici, "Missing"),
    used_adjuvant_ici = factor(
      used_adjuvant_ici,
      levels = c(
        "Missing",
        "Not A Clinician",
        "Do Not Tx CSCC",
        "No",
        "Yes"
      )
    )
  )

df_counts_mgmt <- df_counts_mgmt %>%
  mutate(
    neoadjuvant_adjuvant = recode(
      neoadjuvant_adjuvant,
      "Neoadjuvant" = "Neoadjuvant",
      "Adjuvant" = "Adjuvant",
      "I Am Not A Clinician" = "Not A Clinician",
      "Not Applicable Clinician" = "Do Not Tx CSCC"
    ),
    neoadjuvant_adjuvant = replace_na(neoadjuvant_adjuvant, "Missing"),
    neoadjuvant_adjuvant = factor(
      neoadjuvant_adjuvant,
      levels = c(
        "Missing",
        "Not A Clinician",
        "Do Not Tx CSCC",
        "Adjuvant",
        "Neoadjuvant"
      ),
      ordered = TRUE
    )
  )

p <- ggplot(
  df_counts_mgmt,
  aes(
    axis1 = who_are_you,
    axis2 = patients_per_month,
    axis3 = used_adjuvant_ici,
    axis4 = mgmt_case_1,
    axis5 = mgmt_case_2,
    axis6 = neoadjuvant_adjuvant,
    y = n
  )
  
  ) +
  geom_alluvium(aes(fill = who_are_you), width = 1/12) +
#  geom_stratum(width = 1/12, fill = "grey90", color = "black") +
  geom_stratum(width = 0.7, fill = "grey90", color = "black") +
  geom_text(
    stat = "stratum", 
    aes(label = after_stat(stratum)), 
    size = 2.4) +
  scale_x_discrete(
    limits = c(
      "Role",
      "CSCC Volume",
      "Used Adjuvant ICI",
      "UnTx Parotid Met",
      "Resected Parotid Met",
      "Neo vs. Adjuvant"
    ),
    expand = c(.05, .05)
  ) +
  # Add this to get whole number breaks:
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 10),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Journal Club Pre-Test: Clinical Decision Flows",
    y = "Respondent Count",
    fill = "Role"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold")
    )
p
