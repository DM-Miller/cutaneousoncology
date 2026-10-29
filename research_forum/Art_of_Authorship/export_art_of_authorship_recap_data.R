# Art of Authorship: export existing aggregate results for the recap.
# Run from the usual SoCO RStudio project: source(file.choose())
# Select this R file. Upload the resulting ZIP, or the CSV files it contains.
# This script reads existing outputs. It does not refresh or modify Netlify.

export_art_of_authorship_recap_data <- function(
    start_dir = getwd(), summary_path = NULL, output_dir = NULL) {

  ancestors <- function(path) {
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    result <- path
    repeat {
      parent <- dirname(path)
      if (identical(parent, path)) break
      result <- c(result, parent)
      path <- parent
    }
    result
  }

  if (is.null(summary_path)) {
    roots <- ancestors(start_dir)
    relative_paths <- c(
      "pre_event_survey/outputs/visualizations/data/question_summaries.csv",
      "outputs/visualizations/data/question_summaries.csv",
      "question_summaries.csv",
      paste0("workflows/workshops/events/2026-10-26-Art_of_Authorship/",
             "pre_event_survey/outputs/visualizations/data/question_summaries.csv")
    )
    candidates <- unlist(lapply(roots, function(root) {
      file.path(root, relative_paths)
    }), use.names = FALSE)
    candidates <- candidates[file.exists(candidates)]
    candidates <- unique(normalizePath(candidates, winslash = "/", mustWork = TRUE))
    if (length(candidates) != 1L) {
      stop(
        if (!length(candidates)) "Could not find question_summaries.csv. " else
          "Found more than one possible question_summaries.csv. ",
        "Run this from your usual SoCO project, or call ",
        "export_art_of_authorship_recap_data(summary_path = ",
        "\"/full/path/to/question_summaries.csv\").",
        call. = FALSE
      )
    }
    summary_path <- candidates[[1L]]
  }

  summary_path <- normalizePath(summary_path, winslash = "/", mustWork = TRUE)
  results <- read.csv(
    summary_path, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = "character", fileEncoding = "UTF-8-BOM"
  )
  if (!nrow(results) || !ncol(results)) {
    stop("The question summary is empty; nothing was exported.", call. = FALSE)
  }
  normalized_names <- tolower(gsub("[^a-z0-9]+", "_", tolower(names(results))))
  private_fields <- c(
    "email", "email_address", "browser_id", "person_id", "contact_id",
    "respondent_id", "submission_id", "submission_number", "netlify_created_at"
  )
  if (any(normalized_names %in% private_fields)) {
    stop("This looks like respondent-level data. Please select the aggregate question_summaries.csv instead.", call. = FALSE)
  }
  values <- unlist(results, use.names = FALSE)
  if (any(grepl("[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]{2,}", values), na.rm = TRUE)) {
    stop("The selected file contains email addresses. No export was made.", call. = FALSE)
  }

  event_root <- sub("/pre_event_survey/.*$", "", summary_path)
  if (identical(event_root, summary_path)) event_root <- dirname(summary_path)
  if (is.null(output_dir)) {
    output_dir <- file.path(
      event_root, "presentation", "recap_data_export",
      format(Sys.time(), "%Y%m%d_%H%M%S")
    )
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

  summary_output <- file.path(output_dir, "art_of_authorship_question_summaries.csv")
  if (file.exists(summary_output)) {
    stop("The export file already exists. Choose a new output_dir; existing files were not overwritten.", call. = FALSE)
  }
  if (!file.copy(summary_path, summary_output, overwrite = FALSE)) {
    stop("Could not copy the aggregate summary.", call. = FALSE)
  }
  stopifnot(identical(unname(tools::md5sum(summary_path)), unname(tools::md5sum(summary_output))))

  # Where available, include the exact wording and option order already
  # embedded in the current presentation. This is supplementary metadata;
  # it does not limit which questions are copied in the summary above.
  catalog_note <- "No embedded presentation question catalog was available."
  catalog_n <- 0L
  presentation_candidates <- unique(c(
    file.path(event_root, "presentation", "Art_of_Authorship_Workshop.qmd"),
    file.path(start_dir, "Art_of_Authorship_Workshop.qmd")
  ))
  presentation_candidates <- presentation_candidates[file.exists(presentation_candidates)]
  if (length(presentation_candidates) && requireNamespace("jsonlite", quietly = TRUE)) {
    catalog_result <- tryCatch({
      presentation <- paste(readLines(presentation_candidates[[1L]], warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      marker <- 'aoa_question_json <- r"---('
      start <- regexpr(marker, presentation, fixed = TRUE)[[1L]]
      if (start < 1L) stop("The presentation has no embedded question catalog.")
      remaining <- substring(presentation, start + nchar(marker))
      end <- regexpr(')---"', remaining, fixed = TRUE)[[1L]]
      if (end < 1L) stop("Could not locate the end of the embedded catalog.")
      questions <- jsonlite::fromJSON(substr(remaining, 1L, end - 1L), simplifyVector = FALSE)
      scalar <- function(x) if (is.null(x) || !length(x)) "" else as.character(x[[1L]])
      catalog <- list()
      for (i in seq_along(questions)) {
        question <- questions[[i]]
        options <- question$options
        if (is.null(options) || !length(options)) options <- list(list())
        for (j in seq_along(options)) {
          option <- options[[j]]
          catalog[[length(catalog) + 1L]] <- data.frame(
            question_id = names(questions)[[i]], question_order = i,
            question_type = scalar(question$type),
            question_text = scalar(question$prompt),
            scenario = scalar(question$scenario),
            perspective = scalar(question$branch),
            option_order = j, option_value = scalar(option$value),
            option_label = scalar(option$label),
            display_label = scalar(option$display),
            metadata_source = "current presentation", stringsAsFactors = FALSE
          )
        }
      }
      catalog <- do.call(rbind, catalog)
      write.csv(catalog, file.path(output_dir, "art_of_authorship_question_catalog.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
      length(questions)
    }, error = function(e) {
      catalog_note <<- paste("Supplementary catalog:", conditionMessage(e))
      NA_integer_
    })
    if (!is.na(catalog_result)) {
      catalog_n <- catalog_result
      catalog_note <- paste(catalog_n, "question descriptions were recovered from the presentation. The aggregate summary may contain additional questions.")
    }
  }

  file_info <- file.info(summary_path)
  question_column <- intersect(c("question_id", "question", "field", "item_id"), names(results))
  question_count <- if (length(question_column)) {
    length(unique(results[[question_column[[1L]]]][!is.na(results[[question_column[[1L]]]]) & nzchar(results[[question_column[[1L]]]])]))
  } else NA_integer_
  manifest <- data.frame(
    file = basename(summary_output),
    source_file = basename(summary_path),
    source_modified = format(file_info$mtime, "%Y-%m-%d %H:%M:%S %Z"),
    exported_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    rows = nrow(results), columns = ncol(results),
    distinct_question_ids_if_identifiable = question_count,
    source_md5 = unname(tools::md5sum(summary_path)),
    export_md5 = unname(tools::md5sum(summary_output)),
    data_handling = "Existing aggregate output copied unchanged; no refresh or recount", stringsAsFactors = FALSE
  )
  write.csv(manifest, file.path(output_dir, "export_manifest.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
  writeLines(c(
    "ART OF AUTHORSHIP RECAP DATA EXPORT", "",
    "The aggregate question-summary CSV is an exact copy of the existing pipeline output.",
    "All rows and columns were retained. Counts and percentages were not recalculated.",
    "This script did not refresh data, contact Netlify, or alter its source files.",
    "The source modification time is recorded in export_manifest.csv.", "",
    catalog_note, "",
    "Summary columns:", paste(names(results), collapse = ", "), "",
    "Upload the ZIP, or upload the question summaries and supplementary catalog CSVs.",
    "No respondent-level export is required for rebuilding the recap's survey panels."
  ), file.path(output_dir, "READ_ME.txt"), useBytes = TRUE)

  archive <- file.path(output_dir, "Art_of_Authorship_Recap_Data.zip")
  if (nzchar(Sys.which("zip"))) {
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(output_dir)
    zip_status <- tryCatch(utils::zip(basename(archive), files = c(
      "art_of_authorship_question_summaries.csv",
      if (file.exists("art_of_authorship_question_catalog.csv")) "art_of_authorship_question_catalog.csv",
      "export_manifest.csv", "READ_ME.txt"
    ), flags = "-q"), error = function(e) 1L)
    if (!is.null(zip_status) && zip_status != 0L) message("The CSV export succeeded; ZIP creation did not complete.")
  }
  message("\nAggregate export complete: ", nrow(results), " rows, ", ncol(results), " columns.")
  message(catalog_note)
  message("Output folder: ", output_dir)
  message("Upload: ", if (file.exists(archive)) archive else summary_output)
  invisible(list(directory = output_dir, summary = summary_output,
                 catalog_questions = catalog_n,
                 archive = if (file.exists(archive)) archive else NA_character_))
}

aoa_recap_export <- export_art_of_authorship_recap_data()
