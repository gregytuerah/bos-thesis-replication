##############################
# Main thesis pipeline
##############################

# Core non-ML replication pipeline for the thesis tables.
# Writing-sample and poster assets are generated separately.

source("Code/00_Project_Setup.R")

scripts <- c(
  "Code/01_Identification.R",
  "Code/01b_Controls.R",
  "Code/02_Balance.R",
  "Code/03_Regression.R",
  "Code/HTE-Robustness.R",
  "Code/99_Validate_Reported_Results.R"
)

for (script in scripts) {
  message("\nRunning: ", script)
  source(script, echo = FALSE)
}

message("\nMain non-ML thesis replication pipeline completed.")
