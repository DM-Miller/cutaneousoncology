library(tidyverse)

# Load the processed survey dataset
dt <- open_recent_file(
  directory = file.path(
    files_dir,
    "Pre_JC_survey_processed"
  )
)

# Define the variable name for this question
question_var <- "mgmt_case_1"
question_title <- "71M ECOG1, h/o numerous NMSC, presents with 3 cm left parotid nodal met of CSCC. Not interested in a clinical trial."

subtitle <- "Which would you (or your MDC team) likely recommend?"

# Define excluded categories
excluded_responses <- c(
  "Do Not Treat CSCC",
  "I Am Not A Clinician",
  "Missing",
  "Not Sure"
)


# Total clinician count
clinician_total <- sum(plot_data$n)

# Define the correct order of response categories
plot_data <- dt %>%
  mutate(
    !!question_var := recode(
      .data[[question_var]],
      "Surgery Post Op Rt Adjuvant Anti Pd1" = "Surgery + Adj RT + Adj ICI",
      "Surgery Art" = "Surgery + Adj RT",
      "NeoAdj ICI + Post Op RT" = "NeoAdj ICI + RT",
      "Definitive Apd1" = "Definitive ICI",
      "Definitive Rt Alone" = "Definitive RT Alone",
      "Pre Operative Anti Pd1 Surgery Post Op Rt Based On Response" = "Neoadjuvant ICI + Surgery + Path Eval +/- RT",
      "Start Ici Reassess" = "Start ICI, Reassess for Surgery or RT",
      "Not Applicable Clinician" = "Do Not Treat CSCC",
      "I Am Not A Clinician" = "Not A Clinician"
    ),
    !!question_var := replace_na(.data[[question_var]], "Missing"),
    !!question_var := factor(
      .data[[question_var]],
      levels = rev(c(
        "Missing",
        "Not A Clinician",
        "Do Not Treat CSCC",
        "Not Sure",
        "Other",
        "Definitive RT Alone",
        "Surgery + Adj RT",
        "Definitive ICI",
        "Start ICI, Reassess for Surgery or RT",
        "Surgery + Adj RT + Adj ICI",
        "Neoadjuvant ICI + Surgery + Path Eval +/- RT"
      )),
      ordered = TRUE
    )
  ) %>%
#  filter(!.data[[question_var]] %in% excluded_responses) |>
  count(.data[[question_var]], name = "n", .drop = FALSE) %>%
  mutate(prop = round(n / sum(n) * 100))

# Generate the plot
plot <- 
  ggplot(plot_data, 
         aes(x = .data[[question_var]], y = n)) +
  geom_col(fill = "steelblue4") +
  geom_text(aes(label = paste0(prop, "%")), hjust = -0.1) +
  ggtitle(
    str_wrap(question_title, width = 50),
    subtitle = str_wrap(subtitle, width = 52)
    ) +
  xlab("") +
  ylab(paste0("Number of Respondents (Total = ", sum(plot_data$n), ")")) +
  theme(
    plot.title = element_text(
      hjust = 0.5, face = "bold", size = 22,
      margin = margin(0, 185, 0, 0)),
    plot.subtitle = element_text(
      hjust = 0.5, face = "bold", size = 18,
      margin = margin(10, 185, 10, 10)),
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
  scale_x_discrete(labels = function(x) str_wrap(x, width = 45)) +
  scale_y_continuous(
    breaks = seq(0, max(plot_data$n), by = 1),
    limits = c(0, max(plot_data$n + 1))
  ) +
  coord_flip()

plot


