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
question_title <- "How many CSCC patients do you see per month?"

# Clean specific entries


# Define the correct order of response categories
plot_data <- dt %>%
  mutate(
    !!question_var := recode(
      .data[[question_var]],
      "1-2" = "1-2",
      "3-5" = "3-5",
      "6-10" = "6-10",
      "11-20" = "11-20",
      "Greater Than 20" = ">20",
      "I Am A Clinician But I Do Not Treat Cscc" = "Do Not Treat CSCC",
      "I Am Not A Clinician" = "Not A Clinician"
    ),
    !!question_var := replace_na(.data[[question_var]], "Missing"),
    !!question_var := factor(
      .data[[question_var]],
      levels = rev(c(
        "Missing",
        "Not A Clinician",
        "Do Not Treat CSCC",
        "1-2",
        "3-5",
        "6-10",
        "11-20",
        ">20"
      )),
      ordered = TRUE
    )
  ) %>%
  count(.data[[question_var]], name = "n", .drop = FALSE) %>%
  mutate(prop = round(n / sum(n) * 100))

# Generate the plot
plot <- 
  ggplot(plot_data, 
         aes(x = .data[[question_var]], y = n)) +
  geom_col(fill = "steelblue4") +
  geom_text(aes(label = paste0(prop, "%")), hjust = -0.1) +
  ggtitle(str_wrap(question_title, width = 60)) +
  xlab("") +
  ylab(paste0("Number of Respondents (Total = ", sum(plot_data$n), ")")) +
  theme(
    plot.title = element_text(
      hjust = 0.5, face = "bold", size = 22,
      margin = margin(0, 150, 0, 0)),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(),
    plot.margin = margin(0.2, 0, 0.2, 0, "cm")
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(
    breaks = seq(0, max(plot_data$n), by = 1),
    limits = c(0, max(plot_data$n + 1))
  ) +
  coord_flip()

plot
