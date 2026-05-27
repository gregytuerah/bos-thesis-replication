##############################
# Main thesis pipeline
##############################

# Replication pipeline for the thesis tables and manuscript figures.

source("Code/00_Project_Setup.R")

scripts <- c(
  "Code/01_Identification.R",
  "Code/01b_Controls.R",
  "Code/02_Balance.R",
  "Code/03_Regression.R",
  "Code/04_Manuscript_Figures.R",
  "Code/HTE-Robustness.R",
  "Code/99_Validate_Reported_Results.R"
)

for (script in scripts) {
  message("\nRunning: ", script)
  source(script, echo = FALSE)
}

message("\nMain thesis replication pipeline completed.")
