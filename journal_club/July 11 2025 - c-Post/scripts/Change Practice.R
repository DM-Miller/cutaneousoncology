# Load the processed survey dataset
dt <- open_recent_file(
  directory = file.path(
    files_dir,
    "Post_JC_survey_processed"
  )
)

# Define variable and question title
question_var <- "change_practice_yn"
question_title <- "Will this paper change your practice?"

# Recode responses to match form labels
dt[[question_var]] <- recode(
  dt[[question_var]],
  "No Not Persuasive Enough" = "No – Not persuasive enough",
  "No My Practice Is Already Aligned" = "No – My practice is already aligned",
  "Topic Not Relevant To My Trade" = "Topic not relevant to my trade",
  "Yes" = "Yes"
)

# Replace NA or blank with "Not Answered"
dt[[question_var]][is.na(dt[[question_var]]) | dt[[question_var]] == ""] <- "Not Answered"

# Define ordered levels from the form
ordered_levels <- c(
  "Yes",
  "No – My practice is already aligned",
  "No – Not persuasive enough",
  "Topic not relevant to my trade",
  "Not Answered"
)

# Prepare plot data
plot_data <- dt |> 
  mutate(!!question_var := factor(.data[[question_var]], levels = ordered_levels, ordered = TRUE)) |> 
  count(.data[[question_var]], name = "n", .drop = FALSE) |> 
  mutate(prop = round(n / sum(n) * 100))

# Generate the plot
change_practice_plot <- ggplot(
  plot_data,
  aes(x = .data[[question_var]], y = n)
) +
  geom_col(fill = "steelblue4") +
  geom_text(aes(label = paste0(prop, "%")), hjust = -0.1) +
  ggtitle(str_wrap(question_title, width = 60)) +
  xlab("") +
  ylab(paste0("Number of Respondents (Total = ", sum(plot_data$n), ")")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20,
                              margin = margin(0, 130, 0, 0)),
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
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_y_continuous(
    breaks = seq(0, max(plot_data$n), by = 1),
    limits = c(0, max(plot_data$n + 1))
  ) +
  coord_flip()

# Print the plot
change_practice_plot
