# Load necessary libraries
library(tidyverse)

# Load Data
dt <- open_recent_file(
  directory = file.path(
    files_dir,
    "Pre_JC_survey_processed"
  )
)

# Step 1: Identify checkbox columns
ai_use_columns <- grep("^ai_use___", names(dt), value = TRUE)

# Step 2: Ensure all checkbox columns are numeric
dt[ai_use_columns] <- lapply(dt[ai_use_columns], function(x) as.numeric(as.character(x)))

# Step 3: Create 'answered' status based on sum across all barrier checkboxes
dt <- dt |> 
  mutate(ai_use___answered = rowSums(across(all_of(ai_use_columns))) > 0)


# Step 4: Pivot into long format
ai_use_long <- dt |> 
  dplyr::select(all_of(ai_use_columns)) |> 
  pivot_longer(cols = everything(), 
               names_to = "ai_use", 
               values_to = "selected") |> 
  dplyr::filter(selected == 1) |> 
  mutate(ai_use = str_replace(ai_use, "ai_use___", ""))

# Step 5: Add non-respondents ("Not Answered")
not_answered_n <- dt |> 
  dplyr::filter(ai_use___answered == 0) |> 
  summarise(n = n()) |> 
  mutate(
    ai_use = "not_answered",
    selected = 1
  )

# Step 6: Combine with actual responses
ai_use_long <- ai_use_long |> 
  bind_rows(not_answered_n |> select(ai_use, selected))

# Step 7: Apply human-readable labels

ai_use_labels <- c(
  "i_do_not_use_ai_and_am_unaware_of_its_role_in_my_work" = "I do not use AI and am unaware of its role in my work",
  "i_suspect_ai_tools_are_used_in_my_work_but_i_do_not_directly_engage_with_them" = "I suspect AI tools are used in my work but I do not directly engage with them",
  "i_occasionally_use_ai_tools_like_chatgpt_or_ambient_ai_tools" = "I occasionally use AI tools like ChatGPT or AI for clinical notes",
  "i_regularly_use_generative_ai_tools_like_chatgpt_daxcopilot_or_image_generators_in_my_work" = "I regularly use generative AI tools like ChatGPT, DAX Copilot, or image generators in my work",
  "i_work_with_structured_data_and_basic_machine_learning_models" = "I work with structured data and basic machine learning models",
  "i_am_developing_or_fine_tuning_deep_learning_models_using_libraries_such_as_pytorch_tensorflow_or_fastai" = "I am developing or fine-tuning deep learning models using libraries such as PyTorch, TensorFlow, or FastAI",
  "not_answered" = "Not Answered"
)



ai_use_long <- ai_use_long |> 
  mutate(ai_use = recode(ai_use, !!!ai_use_labels))

# Step 8: Define ordered levels (include non-response)
ordered_levels <- c(
  "Not Answered",
  "I do not use AI and am unaware of its role in my work",
  "I suspect AI tools are used in my work but I do not directly engage with them",
  "I occasionally use AI tools like ChatGPT or AI for clinical notes",
  "I regularly use generative AI tools like ChatGPT, DAX Copilot, or image generators in my work",
  "I work with structured data and basic machine learning models",
  "I am developing or fine-tuning deep learning models using libraries such as PyTorch, TensorFlow, or FastAI"
)

# Step 5: Count occurrences & normalize by total **filtered** responses
plot_data <- ai_use_long |> 
  count(ai_use) |> 
  mutate(prop = round(n / nrow(dt) * 100)) |>  # Normalize by filtered responses
  mutate(ai_use = factor(ai_use, levels = rev(ordered_levels), ordered = TRUE)) 

# Step 6: Create Plot
ai_use_plot <- plot_data |> 
  ggplot(
    aes(x = ai_use,
        y = n)
  ) + 
  geom_col(fill = "steelblue4") + 
  geom_text(
    aes(label = paste(prop, "%")),
    hjust = -0.1
  ) +
  ggtitle(str_wrap(
    "How do you currently use artificial intelligence (AI) tools in your work? (check all that apply)",
    55)
  ) +
  xlab("") +
  ylab(paste0(
    "Number of Respondents (Total = ", nrow(dt), ")")) +  # Use filtered total responses
  theme(
    plot.title = element_text(
      hjust = 0.5, face = "bold", 
      margin = margin(0, 300, 0, 0)),
    title = element_text(face = "bold", size = 18),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 16),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(),
    plot.margin = margin(0.2, 0, 0.2, 0, "cm")
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 60)) +
  scale_y_continuous(
    breaks = seq(0, max(plot_data$n + 1), by = 2), # Sets breaks at intervals of 2
    limits = c(0, max(plot_data$n + 1)) # Adjusts upper limit for spacing
  ) +
  coord_flip() 

# Display the plot
ai_use_plot
