##############################################################
# Validate generated outputs against reported thesis results  #
##############################################################

source("Code/00_Project_Setup.R")

checks <- c(
  "Output/Tables/main_model_2014_hs_cleaned.tex" =
    "Exposure \\(BOS years\\) x High Intensity &  &  &  & 0\\.010\\*",
  "Output/Tables/main_model_2014_hs_cleaned.tex" =
    "Num\\.Obs\\. & 2088 & 2088 & 2088 & 2088",
  "Output/Tables/main_model_2014_years_cleaned.tex" =
    "Exposure \\(BOS years\\) x High Intensity &  &  &  & -0\\.006",
  "Output/Tables/robustness_sensitivity_hs.tex" =
    "Exposure \\$\\\\times\\$ Higher-Intensity Indicator & 0\\.010\\* & -0\\.005 & 0\\.005 & 0\\.007",
  "Output/Tables/hte_main_hs.tex" =
    "BOS Years x High Intensity x Female & 0\\.001"
)

for (i in seq_along(checks)) {
  output_path <- names(checks)[i]
  if (!file.exists(output_path)) {
    stop("Missing expected generated output: ", output_path)
  }

  output_text <- paste(readLines(output_path, warn = FALSE), collapse = "\n")
  if (!grepl(checks[[i]], output_text)) {
    stop("Reported-results check failed for: ", output_path)
  }
}

message("Validated main result, secondary outcome, sensitivity, and heterogeneity outputs.")
