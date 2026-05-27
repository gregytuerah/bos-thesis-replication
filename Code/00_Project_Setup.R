##############################################################
# Project setup for the public replication package            #
##############################################################

# Scripts are intended to be run from the repository root.
if (!file.exists(file.path("Code", "01_Identification.R"))) {
  stop(
    "Run the replication scripts from the repository root, for example: ",
    "Rscript Code/01_Identification.R"
  )
}

dir.create(file.path("Data", "Processed"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("Output", "Tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("Output", "Figures"), recursive = TRUE, showWarnings = FALSE)
