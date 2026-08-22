# Load necessary libraries
library(tidyverse)
library(here)

# Load Data
dt <- open_recent_file(
  directory = file.path(
    files_dir,
    "Pre_JC_survey_processed"
  )
)


# Define the question variable and plot title
question_var <- "ai_devices_skin_cancer"
question_title <- "Have you ever used, trialed, or recommended an AI-powered device (e.g., DermaSensor, MelaFind, Nevisense) for skin cancer detection? (choose the best answer)?"


# Clean specific entries
dt[[question_var]] <- as.character(dt[[question_var]])
dt[[question_var]][is.na(dt[[question_var]])] <- "not_answered"
dt[[question_var]][dt[[question_var]] == ""] <- "not_answered"

# Normalize values to lowercase with underscores
dt[[question_var]] <- tolower(dt[[question_var]])
dt[[question_var]] <- str_replace_all(dt[[question_var]], "\\s+", "_")


ai_device_labels <- c(
  "no_never_used_or_recommended"     = "No, I have never used or recommended one",
  "familiar_but_never_used"          = "I am familiar with these devices but have never used one clinically",
  "used_as_part_of_study_or_demo"    = "I have used one as part of a study, demonstration, or training",
  "use_in_clinical_practice"         = "I currently use or have recommended one in clinical practice",
  "i_am_not_a_clinician"             = "I am not a clinician",
  "not_answered"                     = "Not Answered"
)

# Recode to readable form
dt <- dt |> 
  mutate(!!question_var := recode(.data[[question_var]], !!!ai_device_labels))


# Define ordered levels
ordered_levels <- rev(c(
  "Not Answered",
  "I am not a clinician",
  "No, I have never used or recommended one",
  "I am familiar with these devices but have never used one clinically",
  "I have used one as part of a study, demonstration, or training",
  "I currently use or have recommended one in clinical practice"
))



# Prepare summary
response_summary <- dt |> 
  mutate(!!question_var := factor(.data[[question_var]], levels = ordered_levels, ordered = TRUE)) |> 
  count(!!sym(question_var), name = "n", .drop = FALSE) |> 
  mutate(prop = round(n / sum(n) * 100))



# Generate the plot
plot <- ggplot(
  response_summary,
  aes(x = .data[[question_var]], y = n)
) + 
  geom_col(fill = "steelblue4") + 
  geom_text(
    aes(label = paste0(prop, "%")),
    hjust = -0.1
  ) +
  ggtitle(str_wrap(question_title, 60)) +
  xlab("") +
  ylab(paste0("Number of Respondents (Total = ", sum(response_summary$n), ")")) +
  theme(
    plot.title = element_text(
      hjust = 0.5, face = "bold", size = 20,
      margin = margin(0, 250, 0, 0)),
    title = element_text(face = "bold", size = 18),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 16),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(),
    plot.margin = margin(0.2, 0, 0.2, 0, "cm")
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 45)) +
  scale_y_continuous(
#    breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
    breaks = seq(1, max(response_summary$n), by = 1),
    limits = c(0, max(response_summary$n + 0.5))
  ) +
  coord_flip()

# Print the plot
plot
