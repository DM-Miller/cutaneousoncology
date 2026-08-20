# FIRST Journal Club card generator — corrected thumbnail version
# Writes card.svg beside this script.

get_script_dir <- function() {
  if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
    ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
    if (!is.null(ctx) && !is.null(ctx$path) && nzchar(ctx$path)) {
      return(dirname(normalizePath(ctx$path, winslash = "/", mustWork = TRUE)))
    }
  }
  
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  
  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg[1])
    return(dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)))
  }
  
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

script_dir <- get_script_dir()
output_file <- file.path(script_dir, "card.svg")

title_lines <- c(
  "Frontline immunotherapy with",
  "response-guided subsequent treatment",
  "(FIRST) in cutaneous squamous cell carcinoma:",
  "a Bayesian causal analysis of dose intensity,",
  "early benefit, and treatment de-escalation"
)

ys <- c(205, 258, 311, 364, 417)

title_svg <- paste0(
  sprintf(
    paste0(
      '  <text x="108" y="%d" fill="#082D4D" ',
      'font-family="Arial, Helvetica, sans-serif" ',
      'font-size="41" font-weight="700">%s</text>\n'
    ),
    ys,
    title_lines
  ),
  collapse = ""
)

svg <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>\n',
  '<svg xmlns="http://www.w3.org/2000/svg" width="1200" height="700" viewBox="0 0 1200 700">\n',
  '  <defs>\n',
  '    <linearGradient id="bg" x1="0" y1="0" x2="1" y2="1">\n',
  '      <stop offset="0%" stop-color="#F3F8F8"/>\n',
  '      <stop offset="100%" stop-color="#FFFFFF"/>\n',
  '    </linearGradient>\n',
  '    <filter id="shadow" x="-12%" y="-12%" width="124%" height="124%">\n',
  '      <feDropShadow dx="0" dy="12" stdDeviation="18" flood-color="#082D4D" flood-opacity="0.10"/>\n',
  '    </filter>\n',
  '  </defs>\n',
  '  <rect x="42" y="42" width="1116" height="616" rx="28" fill="url(#bg)" filter="url(#shadow)"/>\n',
  '  <rect x="42" y="42" width="9" height="616" rx="4.5" fill="#28A9A2"/>\n',
  '  <text x="108" y="118" fill="#28A9A2" font-family="Arial, Helvetica, sans-serif" font-size="22" font-weight="700" letter-spacing="3.2">PRIMARY ARTICLE · JOURNAL FOR IMMUNOTHERAPY OF CANCER</text>\n',
  title_svg,
  '  <text x="108" y="505" fill="#082D4D" font-family="Arial, Helvetica, sans-serif" font-size="23" font-weight="700">Miller DM, et al.</text>\n',
  '  <text x="300" y="505" fill="#52616D" font-family="Georgia, Times New Roman, serif" font-size="22" font-style="italic">J Immunother Cancer.</text>\n',
  '  <text x="520" y="505" fill="#52616D" font-family="Arial, Helvetica, sans-serif" font-size="22">2026;14(7):e015029.</text>\n',
  '  <text x="108" y="552" fill="#31424E" font-family="Arial, Helvetica, sans-serif" font-size="20">doi: 10.1136/jitc-2026-015029</text>\n',
  '  <rect x="108" y="596" width="108" height="6" rx="3" fill="#28A9A2"/>\n',
  '  <rect x="216" y="596" width="48" height="6" rx="3" fill="#E25A4E"/>\n',
  '</svg>\n'
)

writeLines(svg, output_file, useBytes = TRUE)

message("Created: ", output_file)
