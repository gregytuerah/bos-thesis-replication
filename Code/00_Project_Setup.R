##############################################################
# Project setup for the public replication package            #
##############################################################

# Scripts are intended to be run from the repository root.
if (!file.exists(file.path("Code", "run_main_thesis_pipeline.R"))) {
  stop(
    "Run this script from the repository root, for example: ",
    "Rscript Code/run_main_thesis_pipeline.R"
  )
}

dir.create(file.path("Data", "Processed"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("Output", "Tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("Output", "Figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(
  file.path("Output", "Drafts", "Thesis_Longer_Exposure__Better_Outcomes"),
  recursive = TRUE,
  showWarnings = FALSE
)
