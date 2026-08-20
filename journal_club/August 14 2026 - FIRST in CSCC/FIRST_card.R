# FIRST Journal Club card generator
# ------------------------------------------------------------
# This script ALWAYS writes card.svg into the SAME folder
# that contains this script, regardless of R's working directory.
#
# Expected structure:
#
# journal_club/
# └── August 14 2026 - FIRST in CSCC/
#     ├── index.qmd
#     ├── FIRST_card.R
#     ├── card.svg        <- generated here
#     └── files/
#
# index.qmd should contain:
# image: card.svg


# ------------------------------------------------------------
# Find this script's own directory
# ------------------------------------------------------------

get_script_dir <- function() {
  
  # Best option when sourced from RStudio
  if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
    ctx <- tryCatch(
      rstudioapi::getSourceEditorContext(),
      error = function(e) NULL
    )
    
    if (!is.null(ctx) &&
        !is.null(ctx$path) &&
        nzchar(ctx$path)) {
      return(dirname(normalizePath(ctx$path, winslash = "/", mustWork = TRUE)))
    }
  }
  
  # Option when run with Rscript FIRST_card.R
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  
  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg[1])
    return(dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)))
  }
  
  # Fallback
  message(
    "Could not determine script location automatically; ",
    "using current working directory."
  )
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}


script_dir <- get_script_dir()
output_file <- file.path(script_dir, "card.svg")


# ------------------------------------------------------------
# Card content
# ------------------------------------------------------------

title_lines <- c(
  "Frontline immunotherapy with response-guided subsequent",
  "treatment (FIRST) in cutaneous squamous cell carcinoma:",
  "a Bayesian causal analysis of dose intensity, early benefit,",
  "and treatment de-escalation"
)


# ------------------------------------------------------------
# Build SVG
# ------------------------------------------------------------

svg <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n',
  '<svg xmlns="http://www.w3.org/2000/svg" ',
  'width="1200" height="630" viewBox="0 0 1200 630">\n',
  
  '  <defs>\n',
  '    <linearGradient id="bg" x1="0" y1="0" x2="1" y2="1">\n',
  '      <stop offset="0%" stop-color="#F3F8F8"/>\n',
  '      <stop offset="100%" stop-color="#FFFFFF"/>\n',
  '    </linearGradient>\n',
  
  '    <filter id="shadow" x="-10%" y="-10%" width="120%" height="120%">\n',
  '      <feDropShadow dx="0" dy="10" stdDeviation="16" ',
  'flood-color="#082D4D" flood-opacity="0.10"/>\n',
  '    </filter>\n',
  '  </defs>\n',
  
  '  <rect x="30" y="30" width="1140" height="570" rx="28" ',
  'fill="url(#bg)" filter="url(#shadow)"/>\n',
  
  '  <rect x="30" y="30" width="8" height="570" rx="4" fill="#28A9A2"/>\n',
  
  '  <text x="92" y="102" fill="#28A9A2" ',
  'font-family="Arial, Helvetica, sans-serif" ',
  'font-size="24" font-weight="700" letter-spacing="4">',
  'PRIMARY ARTICLE · JOURNAL FOR IMMUNOTHERAPY OF CANCER',
  '</text>\n',
  
  paste0(
    sprintf(
      paste0(
        '  <text x="92" y="%d" fill="#082D4D" ',
        'font-family="Arial, Helvetica, sans-serif" ',
        'font-size="44" font-weight="700">%s</text>\n'
      ),
      c(185, 240, 295, 350),
      title_lines
    ),
    collapse = ""
  ),
  
  '  <text x="92" y="435" fill="#082D4D" ',
  'font-family="Arial, Helvetica, sans-serif" ',
  'font-size="24" font-weight="700">Miller DM, et al.</text>\n',
  
  '  <text x="292" y="435" fill="#52616D" ',
  'font-family="Georgia, Times New Roman, serif" ',
  'font-size="23" font-style="italic">J Immunother Cancer.</text>\n',
  
  '  <text x="505" y="435" fill="#52616D" ',
  'font-family="Arial, Helvetica, sans-serif" ',
  'font-size="23">2026;14(7):e015029.</text>\n',
  
  '  <text x="92" y="480" fill="#31424E" ',
  'font-family="Arial, Helvetica, sans-serif" ',
  'font-size="21">doi: 10.1136/jitc-2026-015029</text>\n',
  
  '  <rect x="92" y="520" width="116" height="6" rx="3" fill="#28A9A2"/>\n',
  '  <rect x="208" y="520" width="52" height="6" rx="3" fill="#E25A4E"/>\n',
  
  '</svg>\n'
)


# ------------------------------------------------------------
# Write beside this script
# ------------------------------------------------------------

writeLines(svg, output_file, useBytes = TRUE)

message("")
message("Created Journal Club card:")
message(output_file)
message("")
