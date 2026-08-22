library(tidyverse)

# --- Step 1: Load processed survey dataset
dt <- open_recent_file(
  directory = file.path(files_dir, "Pre_JC_survey_processed")
)

# --- Step 2: Define question title and subtitle
question_title <- "If adjuvant anti-PD1 therapy were FDA-approved for CSCC, what barriers would most limit your ability to recommend or use it in the next year?"
subtitle <- "(Select all that apply)"

# --- Step 3: Identify checkbox columns and coerce to numeric
barriers_columns <- grep("^barriers_adjuvant_ici___", names(dt), value = TRUE)
dt[barriers_columns] <- lapply(dt[barriers_columns], function(x) as.numeric(as.character(x)))

# --- Step 4: Filter to respondents who selected at least one barrier
dt_filtered <- dt %>%
  mutate(barrier_sum = rowSums(across(all_of(barriers_columns)), na.rm = TRUE)) %>%
  filter(barrier_sum > 0)

# --- Step 4a: Identify those with no barriers
not_answered <- dt %>%
  mutate(barrier_sum = rowSums(across(all_of(barriers_columns)), na.rm = TRUE)) %>%
  filter(barrier_sum == 0)

# --- Step 5: Store total number of respondents
respondent_total <- nrow(dt_filtered)

# --- Step 6: Pivot to long format
barriers_long <- dt_filtered %>%
  select(all_of(barriers_columns)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "barrier",
    values_to = "selected"
  ) %>%
  filter(selected == 1) %>%
  mutate(
    barrier = recode(
      barrier,
      "barriers_adjuvant_ici___toxicity_concerns" = "Toxicity concerns",
      "barriers_adjuvant_ici___prefer_neoadjuvant" = "Plan to use neoadjuvant over adjuvant",
      "barriers_adjuvant_ici___prefer_wait_for_recurrence" = "Prefer to wait for recurrence to avoid overtreatment",
      "barriers_adjuvant_ici___cost_coverage" = "Cost or insurance",
      "barriers_adjuvant_ici___equipoise_nnt_nnh" = "Uncertain benefit vs. risk",
      "barriers_adjuvant_ici___patient_preference" = "Patient preference",
      "barriers_adjuvant_ici___other" = "Other"
    )
  )


# --- Step 7: Define ordered factor levels
ordered_levels <- c(
  "Toxicity concerns",
  "Uncertain benefit vs. risk",
  "Plan to use neoadjuvant over adjuvant",
  "Prefer to wait for recurrence to avoid overtreatment",
  "Cost or insurance",
  "Patient preference",
  "Other"
)

# --- Step 8: Count and calculate percentages
plot_data <- barriers_long %>%
  count(barrier) %>%
  mutate(
    prop = round(n / respondent_total * 100),
    barrier = factor(barrier, levels = ordered_levels, ordered = TRUE)
  )

# --- Step 9: Create plot
plot <- ggplot(
  plot_data,
  aes(x = barrier, y = n)
) +
  geom_col(fill = "steelblue4") +
  geom_text(aes(label = paste0(prop, "%")), hjust = -0.1) +
  ggtitle(
    label = str_wrap(question_title, width = 52),
    subtitle = str_wrap(subtitle, width = 52)
  ) +
  xlab("") +
  ylab(paste0("Number of Respondents (n = ", respondent_total, ")")) +
  theme(
    plot.title = element_text(
      hjust = 0.5, face = "bold", size = 20,
      margin = margin(0, 205, 10, 0)
    ),
    plot.subtitle = element_text(
      hjust = 0.5, face = "bold", size = 18,
      margin = margin(0, 205, 10, 0)
    ),
    axis.title.x = element_text(face = "bold", size = 16, hjust = 0.1),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(),
    plot.margin = margin(0.2, 1, 0.2, 0, "cm")
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 45)) +
  scale_y_continuous(
    breaks = seq(0, max(plot_data$n + 1), by = 2),
    limits = c(0, max(plot_data$n + 1))
  ) +
  coord_flip()

# --- Step 10: Display plot
plot
