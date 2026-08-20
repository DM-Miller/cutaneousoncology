# REDCap_Helpers.R
# Helper functions for processing REDCap survey data

# ===============================================================================
# DICTIONARY FUNCTIONS
# ===============================================================================

# Get path to most recent data dictionary file
get_latest_dictionary <- function(base_path = getwd(),
                                  dict_subdir = "files/data dictionary",
                                  pattern = "_DataDictionary_.*\\.csv$") {
  dd_dir <- file.path(base_path, dict_subdir)
  if (!dir.exists(dd_dir)) {
    stop("Dictionary directory not found: ", dd_dir)
  }
  dd_files <- list.files(dd_dir, pattern = pattern, full.names = TRUE)
  if (length(dd_files) == 0) {
    stop("No data dictionary files found in: ", dd_dir, " with pattern: ", pattern)
  }
  dd_files[which.max(file.mtime(dd_files))]
}

# Load and parse data dictionary to extract choice mappings
get_dictionary_choices <- function(dict_path = NULL, dict_dir = "files/data dictionary") {
  # Get most recent dictionary if path not provided
  if (is.null(dict_path)) {
    dict_path <- get_latest_dictionary(base_path = here::here(), dict_subdir = dict_dir)
  }
  
  # Read dictionary
  dict <- read.csv(dict_path, check.names = FALSE, stringsAsFactors = FALSE)
  
  # Column names
  col_var <- "Variable / Field Name"
  col_type <- "Field Type"
  col_choices <- "Choices, Calculations, OR Slider Labels"
  
  # Initialize result list
  choice_map <- list()
  
  # Loop through each row
  for (i in 1:nrow(dict)) {
    var_name <- dict[[col_var]][i]
    field_type <- dict[[col_type]][i]
    choices_str <- dict[[col_choices]][i]
    
    # Only process choice-based fields with choices defined
    if (!is.na(field_type) && 
        field_type %in% c("radio", "dropdown", "checkbox", "yesno") &&
        !is.na(choices_str) && nzchar(choices_str)) {
      
      # Split by pipe
      choice_parts <- strsplit(choices_str, " \\| ")[[1]]
      
      # Parse each choice into code and label
      codes <- character()
      labels <- character()
      
      for (choice in choice_parts) {
        # Split by first comma
        parts <- strsplit(choice, ", ", fixed = TRUE)[[1]]
        if (length(parts) >= 2) {
          codes <- c(codes, trimws(parts[1]))
          labels <- c(labels, trimws(paste(parts[-1], collapse = ", ")))
        }
      }
      
      # Store as named vector (code = label)
      names(labels) <- codes
      choice_map[[var_name]] <- labels
    }
  }
  
  return(choice_map)
}

# ===============================================================================
# DATA LOADING FUNCTIONS
# ===============================================================================

# Load REDCap data with fallback to saved file if API fails
load_or_fallback_redcap <- function(redcap_uri,
                                    token,
                                    files_dir,
                                    subfolder = "Pre_JC_survey_unprocessed",
                                    filename = "survey_results_pre_test.rds",
                                    timeout_seconds = 15) {
  # Full subdirectory path
  subdir_path <- file.path(files_dir, subfolder)
  
  # Helper to save data
  save_data <- function(data) {
    saveRDS(data, file = file.path(subdir_path, filename))
  }
  
  # Helper to open most recent file
  open_recent <- function() {
    open_recent_file(directory = subdir_path)
  }
  
  # Try to pull from REDCap
  result <- tryCatch(
    {
      message("Attempting to pull data from REDCap...")
      old_timeout <- getOption("timeout")
      options(timeout = timeout_seconds)
      
      redcapr_dt <- REDCapR::redcap_read_oneshot(
        redcap_uri = redcap_uri,
        token = token
      )
      
      options(timeout = old_timeout)
      dt1 <- redcapr_dt$data
      save_data(dt1)
      message("Successfully downloaded and saved REDCap data.")
      dt1
    },
    error = function(e) {
      message("Error occurred: ", conditionMessage(e))
      message("Falling back to loading most recent saved file.")
      open_recent()
    }
  )
  
  return(result)
}

# ===============================================================================
# DATA CLEANING FUNCTIONS
# ===============================================================================

# Clean radio/dropdown fields by replacing codes with labels
clean_radio_dropdown <- function(data, var_name, choice_map) {
  if (!var_name %in% names(data)) {
    warning("Variable '", var_name, "' not found in data")
    return(data)
  }
  
  labels <- choice_map[[var_name]]
  
  if (is.null(labels) || length(labels) == 0) {
    warning("No choice mapping found for variable '", var_name, "'")
    return(data)
  }
  
  # Replace codes with labels
  data[[var_name]] <- labels[data[[var_name]]]
  
  # Apply string transformations for display
  data[[var_name]] <- stringr::str_replace_all(data[[var_name]], "__", "-")
  data[[var_name]] <- stringr::str_replace_all(data[[var_name]], "_1", ",")
  data[[var_name]] <- stringr::str_replace_all(data[[var_name]], "_", " ")
  data[[var_name]] <- stringr::str_to_title(data[[var_name]])
  data[[var_name]] <- standardize_acronyms(data[[var_name]])
  
  return(data)
}

# Clean all radio/dropdown/yesno fields in dataset (skips checkboxes)
clean_all_survey_data <- function(data, choice_map = NULL, dict_path = NULL) {
  # Load choice_map if not provided
  if (is.null(choice_map)) {
    choice_map <- get_dictionary_choices(dict_path = dict_path)
  }
  
  # Get dictionary to identify field types
  if (is.null(dict_path)) {
    dict_path <- get_latest_dictionary(base_path = here::here(), dict_subdir = "files/data dictionary")
  }
  dict <- read.csv(dict_path, check.names = FALSE, stringsAsFactors = FALSE)
  
  # Process each variable
  for (var_name in names(choice_map)) {
    field_type <- dict[dict[["Variable / Field Name"]] == var_name, "Field Type"]
    
    if (length(field_type) == 0) next
    
    # Only clean radio/dropdown/yesno (skip checkbox)
    if (field_type %in% c("radio", "dropdown", "yesno")) {
      data <- clean_radio_dropdown(data, var_name, choice_map)
    }
  }
  
  return(data)
}

# ===============================================================================
# LABEL STANDARDIZATION
# ===============================================================================

standardize_acronyms <- function(x) {
  
  x %>%
    stringr::str_replace_all("\\bRp1\\b", "RP1") %>%
    stringr::str_replace_all("\\bOrr\\b", "ORR") %>%
    stringr::str_replace_all("\\bOr\\b", "OR") %>%
    stringr::str_replace_all("\\bOs\\b", "OS") %>%
    stringr::str_replace_all("\\bPfs\\b", "PFS") %>%
    stringr::str_replace_all("\\bHr\\b", "HR") %>%
    stringr::str_replace_all("\\bCi\\b", "CI") %>%
    stringr::str_replace_all("\\bFda\\b", "FDA") %>%
    stringr::str_replace_all("\\bNccn\\b", "NCCN") %>%
    stringr::str_replace_all("\\bPd-1\\b", "PD-1") %>%
    stringr::str_replace_all("\\bPd-L1\\b", "PD-L1") %>%
    stringr::str_replace_all("\\bCtla-4\\b", "CTLA-4")
}

# ===============================================================================
# CHECKBOX PROCESSING FUNCTIONS
# ===============================================================================

# Prepare single checkbox variable for plotting (pivot to long format)
prepare_checkbox_for_plot <- function(data, var_name, choice_map = NULL, dict_path = NULL) {
  # Load choice_map if not provided
  if (is.null(choice_map)) {
    choice_map <- get_dictionary_choices(dict_path = dict_path)
  }
  
  labels <- choice_map[[var_name]]
  
  if (is.null(labels) || length(labels) == 0) {
    stop("No choice mapping found for variable '", var_name, "'")
  }
  
  # Find checkbox columns
  checkbox_cols <- grep(paste0("^", var_name, "___"), names(data), value = TRUE)
  
  if (length(checkbox_cols) == 0) {
    stop("No checkbox columns found for variable '", var_name, "'")
  }
  
  # Coerce to numeric
  data[checkbox_cols] <- lapply(data[checkbox_cols], function(x) as.numeric(as.character(x)))
  
  # --- NEW: FULL LIST OF OPTIONS ---
  full_choices <- tibble::tibble(
    label_raw = labels,
    code = names(labels)
  )
  
  # --- OBSERVED DATA IN LONG FORMAT ---
  long_data <- data %>%
    dplyr::mutate(.respondent_id = dplyr::row_number()) %>%
    tidyr::pivot_longer(
      cols = all_of(checkbox_cols),
      names_to = "option",
      values_to = "selected"
    ) %>%
    dplyr::mutate(
      code = sub(paste0("^", var_name, "___"), "", option)
    )
  
  # --- COUNT SELECTED OPTIONS ---
  observed_counts <- long_data %>%
    dplyr::filter(selected == 1) %>%
    dplyr::count(code, name = "n_selected")
  
  # --- MERGE FULL LIST WITH OBSERVED ---
  plot_data <- full_choices %>%
    dplyr::left_join(observed_counts, by = "code") %>%
    dplyr::mutate(
      n_selected = dplyr::coalesce(n_selected, 0),
      label = label_raw,
      label = stringr::str_replace_all(label, "__", "-"),
      label = stringr::str_replace_all(label, "_1", ","),
      label = stringr::str_replace_all(label, "_", " "),
      label = stringr::str_to_title(label),
      label = standardize_acronyms(label)
    ) %>%
    dplyr::select(code, label, n_selected)
  
  # Add respondents (needed for denominator)
  respondents <- long_data %>%
    dplyr::distinct(.respondent_id)
  
  plot_data$.respondent_total <- nrow(respondents)
  
  return(plot_data)
}


# Prepare all checkbox variables for plotting
prepare_all_checkboxes <- function(data, choice_map = NULL, dict_path = NULL) {
  # Load choice_map if not provided
  if (is.null(choice_map)) {
    choice_map <- get_dictionary_choices(dict_path = dict_path)
  }
  
  # Get dictionary to identify checkbox variables
  if (is.null(dict_path)) {
    dict_path <- get_latest_dictionary(base_path = here::here(), dict_subdir = "files/data dictionary")
  }
  dict <- read.csv(dict_path, check.names = FALSE, stringsAsFactors = FALSE)
  
  # Find all checkbox variables
  checkbox_vars <- dict %>%
    filter(`Field Type` == "checkbox") %>%
    pull(`Variable / Field Name`)
  
  # Process each checkbox variable
  checkbox_data_list <- list()
  for (var_name in checkbox_vars) {
    checkbox_data_list[[var_name]] <- prepare_checkbox_for_plot(data, var_name, choice_map)
  }
  
  return(checkbox_data_list)
}



# Create question number mapping from dictionary
# Create question number mapping from dictionary with field types
create_question_map <- function(dict_path = NULL) {
  # Get dictionary
  if (is.null(dict_path)) {
    dict_path <- get_latest_dictionary(base_path = here::here(), 
                                       dict_subdir = "files/data dictionary")
  }
  dict <- read.csv(dict_path, check.names = FALSE, stringsAsFactors = FALSE)
  
  # Get all variables except record_id
  dict_filtered <- dict %>%
    filter(`Variable / Field Name` != "record_id")
  
  # Create list with variable name and field type
  question_map <- list()
  for (i in 1:nrow(dict_filtered)) {
    q_num <- paste0("q", i)
    question_map[[q_num]] <- list(
      var = dict_filtered[["Variable / Field Name"]][i],
      type = dict_filtered[["Field Type"]][i]
    )
  }
  
  return(question_map)
}

# Plot radio/dropdown question with customization options
plot_radio_dropdown <- function(data, var_name, dict_path = NULL, 
                                custom_order = NULL, subtitle = NULL,
                                title_width = 52, subtitle_width = 52,
                                x_label_width = 45, y_breaks = 2,
                                title_size = 20, title_margin = margin(0, 205, 10, 0)) {
  # Get dictionary path if needed
  if (is.null(dict_path)) {
    dict_path <- get_latest_dictionary(
      base_path  = here::here(), 
      dict_subdir = "files/data dictionary"
    )
  }
  
  # Read dictionary
  dict <- read.csv(dict_path, check.names = FALSE, stringsAsFactors = FALSE)
  
  # Get question title
  question_title <- dict[dict[["Variable / Field Name"]] == var_name, "Field Label"]
  if (length(question_title) == 0) {
    stop("Variable '", var_name, "' not found in dictionary")
  }
  
  # --- NEW: get full set of labels for this variable from choice map ---
  choice_map <- get_dictionary_choices(dict_path = dict_path)
  labels_raw <- choice_map[[var_name]]
  
  if (is.null(labels_raw) || length(labels_raw) == 0) {
    stop("No choice mapping found for variable '", var_name, "'")
  }
  
  # Apply same transformations as clean_radio_dropdown so labels match dt_clean
  labels_clean <- labels_raw |>
    stringr::str_replace_all("__", "-") |>
    stringr::str_replace_all("_1", ",") |>
    stringr::str_replace_all("_",  " ") |>
    stringr::str_to_title() |>
    standardize_acronyms()
  
  # Full set of possible options as a tibble
  # Full set of possible options as a tibble (drop NA / blank labels)
  full_options <- tibble::tibble(
    !!var_name := labels_clean
  ) %>%
    dplyr::filter(
      !is.na(.data[[var_name]]),
      .data[[var_name]] != ""
    )
  
  
  # --- Observed data (only non-NA responses) ---
  obs_data <- data %>%
    dplyr::filter(!is.na(.data[[var_name]])) %>%
    dplyr::count(.data[[var_name]], name = "n")
  
  # Number of respondents who answered this question
  respondent_total <- if (nrow(obs_data) > 0) sum(obs_data$n) else 0
  
  # --- Merge full option set with observed counts (fill missing with 0) ---
  plot_data <- full_options %>%
    dplyr::left_join(
      obs_data,
      by = setNames(var_name, var_name)
    ) %>%
    dplyr::mutate(
      n    = dplyr::coalesce(n, 0L),
      prop = if (respondent_total > 0) round(n / respondent_total * 100) else 0
    )
  
  # Apply custom order if provided
  if (!is.null(custom_order)) {
    plot_data <- plot_data %>%
      dplyr::mutate(
        !!var_name := factor(.data[[var_name]],
                             levels = rev(custom_order),
                             ordered = TRUE)
      )
  } else {
    plot_data <- plot_data %>%
      dplyr::mutate(
        !!var_name := factor(.data[[var_name]],
                             levels = rev(.data[[var_name]]))
      )
  }
  
  plot_data <- plot_data %>%
    dplyr::filter(!is.na(.data[[var_name]]))
  
  # Create plot
  plot <- ggplot(plot_data, aes(x = .data[[var_name]], y = n)) +
    geom_col(fill = "steelblue4") +
    geom_text(aes(label = paste0(prop, "%")), hjust = -0.1) +
    ggtitle(
      label    = str_wrap(question_title, width = title_width),
      subtitle = if (!is.null(subtitle)) str_wrap(subtitle, width = subtitle_width) else NULL
    ) +
    xlab("") +
    ylab(paste0("Number of Respondents (n = ", respondent_total, ")")) +
    theme(
      plot.title    = element_text(hjust = 0.5, face = "bold", size = title_size,
                                   margin = title_margin),
      plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 18,
                                   margin = margin(0, 205, 10, 0)),
      axis.title.x  = element_text(face = "bold", size = 16, hjust = 0.1),
      axis.text.x   = element_text(face = "bold", size = 14),
      axis.title.y  = element_text(face = "bold", size = 16),
      axis.text.y   = element_text(face = "bold", size = 16),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background  = element_blank(),
      axis.line         = element_line(),
      plot.margin       = margin(0.2, 1, 0.2, 0, "cm")
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = x_label_width)) +
    scale_y_continuous(
      breaks = seq(0, max(plot_data$n + 1), by = y_breaks),
      limits = c(0, max(plot_data$n + 1))
    ) +
    coord_flip()
  
  return(plot)
}



prepare_checkbox_for_plot <- function(data, var_name, choice_map = NULL, dict_path = NULL) {
  # Load choice_map if not provided
  if (is.null(choice_map)) {
    choice_map <- get_dictionary_choices(dict_path = dict_path)
  }
  
  labels <- choice_map[[var_name]]
  
  if (is.null(labels) || length(labels) == 0) {
    stop("No choice mapping found for variable '", var_name, "'")
  }
  
  # Find checkbox columns
  checkbox_cols <- grep(paste0("^", var_name, "___"), names(data), value = TRUE)
  
  if (length(checkbox_cols) == 0) {
    stop("No checkbox columns found for variable '", var_name, "'")
  }
  
  # Coerce to numeric
  data[checkbox_cols] <- lapply(data[checkbox_cols], function(x) as.numeric(as.character(x)))
  
  # --- NEW: FULL LIST OF OPTIONS ---
  full_choices <- tibble::tibble(
    label_raw = labels,
    code = names(labels)
  )
  
  # --- OBSERVED DATA IN LONG FORMAT ---
  long_data <- data %>%
    dplyr::mutate(.respondent_id = dplyr::row_number()) %>%
    tidyr::pivot_longer(
      cols = all_of(checkbox_cols),
      names_to = "option",
      values_to = "selected"
    ) %>%
    dplyr::mutate(
      code = sub(paste0("^", var_name, "___"), "", option)
    )
  
  # --- COUNT SELECTED OPTIONS ---
  observed_counts <- long_data %>%
    dplyr::filter(selected == 1) %>%
    dplyr::count(code, name = "n_selected")
  
  # --- MERGE FULL LIST WITH OBSERVED ---
  plot_data <- full_choices %>%
    dplyr::left_join(observed_counts, by = "code") %>%
    dplyr::mutate(
      n_selected = dplyr::coalesce(n_selected, 0),
      label = label_raw,
      label = stringr::str_replace_all(label, "__", "-"),
      label = stringr::str_replace_all(label, "_1", ","),
      label = stringr::str_replace_all(label, "_", " "),
      label = stringr::str_to_title(label),
      label = standardize_acronyms(label)
    ) %>%
    dplyr::select(code, label, n_selected)
  
  # Add respondents (needed for denominator)
  respondents <- long_data %>%
    dplyr::distinct(.respondent_id)
  
  plot_data$.respondent_total <- nrow(respondents)
  
  return(plot_data)
}

plot_checkbox <- function(checkbox_data, var_name, dict_path = NULL, 
                          custom_order = NULL, subtitle = "(Select all that apply)",
                          title_width = 52, subtitle_width = 52,
                          x_label_width = 45, y_breaks = 2,
                          title_size = 20, title_margin = margin(0, 205, 10, 0)) {
  
  if (is.null(dict_path)) {
    dict_path <- get_latest_dictionary(
      base_path = here::here(), 
      dict_subdir = "files/data dictionary"
    )
  }
  
  dict <- read.csv(dict_path, check.names = FALSE, stringsAsFactors = FALSE)
  
  question_title <- dict[dict[["Variable / Field Name"]] == var_name, "Field Label"]
  
  if (length(question_title) == 0) {
    stop("Variable not found: ", var_name)
  }
  
  respondent_total <- checkbox_data$.respondent_total[1]
  
  plot_data <- checkbox_data %>%
    dplyr::mutate(
      label = standardize_acronyms(label),
      prop = if (respondent_total > 0) round(n_selected / respondent_total * 100) else 0
    )
  
  # Custom order with guard against silent NA levels
  if (!is.null(custom_order)) {
    
    custom_order <- standardize_acronyms(custom_order)
    
    unmatched_labels <- setdiff(unique(plot_data$label), custom_order)
    
    if (length(unmatched_labels) > 0) {
      stop(
        "These labels are present in the data but missing from custom_order: ",
        paste(unmatched_labels, collapse = " | ")
      )
    }
    
    plot_data <- plot_data %>%
      dplyr::mutate(
        label = factor(label, levels = rev(custom_order), ordered = TRUE)
      )
    
  } else {
    plot_data <- plot_data %>%
      dplyr::mutate(
        label = factor(label, levels = rev(unique(label)))
      )
  }
  
  ggplot(plot_data, aes(x = label, y = n_selected)) +
    geom_col(fill = "steelblue4") +
    geom_text(aes(label = paste0(prop, "%")), hjust = -0.1) +
    ggtitle(
      label = str_wrap(question_title, width = title_width),
      subtitle = str_wrap(subtitle, width = subtitle_width)
    ) +
    xlab("") +
    ylab(paste0("Number of Respondents (n = ", respondent_total, ")")) +
    theme(
      plot.title = element_text(
        hjust = 0.5, face = "bold", size = title_size,
        margin = title_margin
      ),
      plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 18),
      axis.title = element_text(face = "bold", size = 16),
      axis.text = element_text(face = "bold", size = 14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line()
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = x_label_width)) +
    scale_y_continuous(
      breaks = seq(0, max(plot_data$n_selected) + 1, by = y_breaks),
      limits = c(0, max(plot_data$n_selected) + 1)
    ) +
    coord_flip()
}


# Universal plotting function with customization options
plot_question <- function(question_info, data_clean, checkbox_data_list, 
                          custom_order = NULL, subtitle = NULL, dict_path = NULL,
                          title_width = 52, subtitle_width = 52,
                          x_label_width = 45, y_breaks = 2,
                          title_size = 20, title_margin = margin(0, 205, 10, 0)) {
  
  var_name <- question_info$var
  field_type <- question_info$type
  
  # Route to appropriate plotting function based on field type
  if (field_type %in% c("radio", "dropdown", "yesno")) {
    plot_radio_dropdown(data_clean, var_name, dict_path, custom_order, subtitle,
                        title_width, subtitle_width, x_label_width, y_breaks,
                        title_size, title_margin)
  } else if (field_type == "checkbox") {
    # Use default subtitle for checkboxes if not provided
    if (is.null(subtitle)) subtitle <- "(Select all that apply)"
    plot_checkbox(checkbox_data_list[[var_name]], var_name, dict_path, custom_order, subtitle,
                  title_width, subtitle_width, x_label_width, y_breaks,
                  title_size, title_margin)
  } else {
    stop("Unsupported field type: ", field_type)
  }
}
