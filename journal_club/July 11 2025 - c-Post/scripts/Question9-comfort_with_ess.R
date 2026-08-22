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
question_var <- "ess_understanding"
question_title <- "How comfortable are you with your understanding of elastic scattering spectroscopy (ESS)? (choose the best answer)"


# Clean specific entries
dt[[question_var]] <- as.character(dt[[question_var]])
dt[[question_var]][is.na(dt[[question_var]])] <- "not_answered"
dt[[question_var]][dt[[question_var]] == ""] <- "not_answered"

# Normalize values to lowercase with underscores
dt[[question_var]] <- tolower(dt[[question_var]])
dt[[question_var]] <- str_replace_all(dt[[question_var]], "\\s+", "_")


labels <- c(
  "no_understanding_just_a_device" = "I have no understanding – it seems like a device with a mysterious signal",
  "general_conceptual_understanding" = "I understand the general concept of light scattering and tissue structure",
  "familiar_with_spectral_signatures_and_classification" = "I'm familiar with how optical signatures are extracted and classified",
  "comfortable_but_not_expert_ess" = "I am comfortable with the physics and data interpretation of ESS but not an expert",
  "expert_ess" = "I have a technical understanding of ESS and could teach or troubleshoot the methodology",
  "not_answered" = "Not Answered"
)


dt <- dt |> 
  mutate(!!question_var := recode(.data[[question_var]], !!!labels))

ordered_levels <- rev(c(
  "Not Answered",
  "I have no understanding – it seems like a device with a mysterious signal",
  "I understand the general concept of light scattering and tissue structure",
  "I'm familiar with how optical signatures are extracted and classified",
  "I am comfortable with the physics and data interpretation of ESS but not an expert",
  "I have a technical understanding of ESS and could teach or troubleshoot the methodology"
))


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
