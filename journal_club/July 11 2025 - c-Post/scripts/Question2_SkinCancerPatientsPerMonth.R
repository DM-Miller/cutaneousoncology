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
question_var <- "patients_per_month"
question_title <- "How many skin cancer patients do you see per month?"
# Clean specific entries
dt[[question_var]] <- as.character(dt[[question_var]])

# Replace missing or malformed values
dt[[question_var]][is.na(dt[[question_var]])] <- "Not Answered"
dt[[question_var]][dt[[question_var]] == ""] <- "Not Answered"
dt[[question_var]][dt[[question_var]] == "Greater Than, 00"] <- "Greater than 100"

# Define the correct order of response categories
# Define the correct order of response categories, from least to most clinical engagement
ordered_levels <- rev(c(
  "Not Answered",
  "I Am Not A Clinician",
  "I Am A Clinician But I Do Not Treat Skin Cancer",
  "1-10",
  "11-20",
  "21-40",
  "41-60",
  "61-80",
  "81-100",
  "Greater than 100"
))


# Ensure factor levels are set before counting
response_summary <- dt |>
  drop_na(all_of(question_var)) |>
  mutate(!!question_var := factor(.data[[question_var]], levels = ordered_levels, ordered = TRUE)) |>
  count(.data[[question_var]], name = "n", .drop = FALSE) |>
  mutate(prop = round(n / sum(n) * 100))

# Generate the plot
patients_per_month_plot <- ggplot(
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
      margin = margin(0, 130, 0, 0)),
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
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_y_continuous(
#    breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
    breaks = seq(1, max(response_summary$n), by = 1),
    limits = c(0, max(response_summary$n + 0.5))
  ) +
  coord_flip()

# Print the plot
patients_per_month_plot
