##############################################################
# Project: Master's thesis - Main regression script          #
# Author: Gregy                                              #
# Date: March 2026                                           #
#                                                            #
# NOTE: Run 01_Identification.R and 01b_Controls.R first.    #
##############################################################

suppressPackageStartupMessages({
  library(pacman)
  pacman::p_load(haven, dplyr, fixest, modelsummary, kableExtra)
})

source("Code/00_Project_Setup.R")

output_tables_dir <- "Output/Tables"
draft_dir <- "Paper/source"

dir.create(output_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(draft_dir, recursive = TRUE, showWarnings = FALSE)

##############################
# Helper creation
##############################

# This helper is only for table formatting. Keeping the table formatting here
# lets the regression block below stay easier to read.
write_clean_latex_table <- function(models,
                                    filepaths,
                                    title = NULL,
                                    label = NULL,
                                    coef_map = NULL,
                                    gof_map = NULL,
                                    add_rows = NULL,
                                    notes = NULL,
                                    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01)) {
  tbl_df <- modelsummary(
    models,
    stars = stars,
    coef_map = coef_map,
    gof_map = gof_map,
    add_rows = add_rows,
    output = "data.frame"
  )

  model_cols <- setdiff(names(tbl_df), c("part", "term", "statistic"))
  rendered_rows <- list()
  row_idx <- 1L

  # Formatting loop only: put each estimate and standard error on separate rows.
  for (term_i in unique(tbl_df$term[tbl_df$part == "estimates"])) {
    term_block <- tbl_df[tbl_df$part == "estimates" & tbl_df$term == term_i, , drop = FALSE]

    estimate_row <- data.frame(term = term_i, check.names = FALSE)
    for (col_i in model_cols) {
      estimate_cell <- term_block[term_block$statistic == "estimate", col_i, drop = TRUE]
      estimate_row[[col_i]] <- if (length(estimate_cell) == 0) "" else estimate_cell[1]
    }
    rendered_rows[[row_idx]] <- estimate_row
    row_idx <- row_idx + 1L

    stderr_row <- data.frame(term = "", check.names = FALSE)
    has_stderr <- FALSE
    for (col_i in model_cols) {
      stderr_cell <- term_block[term_block$statistic == "std.error", col_i, drop = TRUE]
      stderr_row[[col_i]] <- if (length(stderr_cell) == 0) "" else stderr_cell[1]
      has_stderr <- has_stderr || (length(stderr_cell) > 0 && nzchar(stderr_row[[col_i]]))
    }
    if (has_stderr) {
      rendered_rows[[row_idx]] <- stderr_row
      row_idx <- row_idx + 1L
    }
  }

  tail_rows <- tbl_df[tbl_df$part != "estimates", c("term", model_cols), drop = FALSE]
  names(tail_rows)[1] <- "term"
  final_df <- bind_rows(rendered_rows, list(tail_rows))

  caption_text <- title
  if (!is.null(label) && nzchar(label)) {
    caption_text <- paste0(title, " \\label{", label, "}")
  }

  latex_tbl <- kableExtra::kbl(
    final_df,
    format = "latex",
    booktabs = TRUE,
    longtable = FALSE,
    linesep = "",
    escape = FALSE,
    align = paste0("l", strrep("c", length(model_cols))),
    col.names = c("", model_cols),
    caption = caption_text
  ) %>%
    kableExtra::kable_styling(
      latex_options = "hold_position",
      full_width = FALSE,
      position = "center"
    )

  if (!is.null(notes) && length(notes) > 0) {
    latex_tbl <- latex_tbl %>%
      kableExtra::footnote(
        general = paste(notes, collapse = " "),
        general_title = "Notes: ",
        threeparttable = TRUE,
        footnote_as_chunk = TRUE,
        escape = FALSE
      )
  }

  latex_txt <- as.character(latex_tbl)
  latex_txt <- gsub("\n\\\\centering\n\\\\begin\\{threeparttable\\}", "\n\\\\begin{threeparttable}", latex_txt)
  latex_txt <- gsub("\\\\begin\\{tabular\\}\\[t\\]\\{l", "\\\\footnotesize\n\\\\resizebox{\\\\linewidth}{!}{%\n\\\\begin{tabular}[t]{@{}l", latex_txt)
  latex_txt <- gsub("\\\\end\\{tabular\\}", "\\\\end{tabular}\n}", latex_txt)
  latex_txt <- gsub("\\}\\{l(@?\\{\\})?cccc\\}", "}{@{}lcccc@{}}", latex_txt)
  latex_txt <- gsub("[ \t]+(?=\n)", "", latex_txt, perl = TRUE)

  # The same table needs to exist in both the canonical output folder and the draft folder.
  writeLines(latex_txt, filepaths[1])
  if (length(filepaths) > 1) {
    writeLines(latex_txt, filepaths[2])
  }
}

##############################
# Data loading and merging
##############################

required_inputs <- c(
  "Data/Processed/est_with_controls.rds",
  "Data/Raw/More on IFLS/cf07_all_dta/schl.dta",
  "Data/Raw/14_bk_sc1.dta"
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop("Missing required input(s): ", paste(missing_inputs, collapse = ", "))
}

est <- readRDS("Data/Processed/est_with_controls.rds") %>%
  mutate(across(where(haven::is.labelled), haven::zap_labels))

# The 2007 school file gives the pre-outcome province-level BOS intensity proxy.
school_2007 <- read_dta("Data/Raw/More on IFLS/cf07_all_dta/schl.dta") %>%
  transmute(
    province = as.numeric(lk010707),
    local_bop_amt = as.numeric(b76d)
  ) %>%
  mutate(
    local_bop_amt = if_else(
      local_bop_amt %in% c(99997, 99998, 99999, 999999999997, 999999999998, 999999999999),
      NA_real_, local_bop_amt
    ),
    local_bop_pos = as.integer(!is.na(local_bop_amt) & local_bop_amt > 0),
    geo_id = if_else(!is.na(province), paste0("P", as.integer(province)), NA_character_)
  ) %>%
  filter(!is.na(geo_id))

intensity_2007 <- school_2007 %>%
  group_by(geo_id) %>%
  summarise(
    n_school_2007 = n(),
    share_local_bop_pos_2007 = mean(local_bop_pos, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(
  intensity_2007,
  file.path(output_tables_dir, "main_model_intensity_2007_province.csv"),
  row.names = FALSE
)

threshold_2007 <- median(intensity_2007$share_local_bop_pos_2007, na.rm = TRUE)

# The main regressions use the 2014 child cross-section merged to 2007 intensity.
hh_loc_2014 <- read_dta("Data/Raw/14_bk_sc1.dta") %>%
  transmute(
    true_hh = hhid14,
    province_loc = as.numeric(sc01_14_14)
  ) %>%
  distinct(true_hh, .keep_all = TRUE)

est_2014 <- est %>%
  filter(year == 2014) %>%
  left_join(hh_loc_2014, by = "true_hh") %>%
  mutate(
    province_merge = dplyr::coalesce(as.numeric(province), province_loc),
    geo_id = if_else(!is.na(province_merge), paste0("P", as.integer(province_merge)), NA_character_)
  ) %>%
  left_join(intensity_2007, by = "geo_id") %>%
  filter(!is.na(share_local_bop_pos_2007)) %>%
  mutate(
    years_educ = as.numeric(years_educ),
    BOS_years = as.numeric(BOS_years),
    attain_hs = if_else(!is.na(years_educ), as.integer(years_educ >= 10), NA_integer_),
    high_intensity_2007 = as.integer(share_local_bop_pos_2007 > threshold_2007),
    female = as.integer(sex == 3)
  )

# Split by outcome so the sample is explicit for each table.
est_2014_hs <- est_2014 %>% filter(!is.na(attain_hs))
est_2014_years <- est_2014 %>% filter(!is.na(years_educ))

##############################
# Main analysis
##############################

# Main HS-attainment models.
m_hs_clean_1 <- feols(
  attain_hs ~ treated + treated:share_local_bop_pos_2007 + female | hh_origin,
  data = est_2014_hs,
  cluster = ~hh_origin
)

m_hs_clean_2 <- feols(
  attain_hs ~ treated + treated:high_intensity_2007 + female | hh_origin,
  data = est_2014_hs,
  cluster = ~hh_origin
)

m_hs_clean_3 <- feols(
  attain_hs ~ BOS_years + BOS_years:share_local_bop_pos_2007 + female | hh_origin,
  data = est_2014_hs,
  cluster = ~hh_origin
)

m_hs_clean_4 <- feols(
  attain_hs ~ BOS_years + BOS_years:high_intensity_2007 + female | hh_origin,
  data = est_2014_hs,
  cluster = ~hh_origin
)

# Secondary years-of-education models.
m_yr_clean_1 <- feols(
  years_educ ~ treated + treated:share_local_bop_pos_2007 + female | hh_origin,
  data = est_2014_years,
  cluster = ~hh_origin
)

m_yr_clean_2 <- feols(
  years_educ ~ treated + treated:high_intensity_2007 + female | hh_origin,
  data = est_2014_years,
  cluster = ~hh_origin
)

m_yr_clean_3 <- feols(
  years_educ ~ BOS_years + BOS_years:share_local_bop_pos_2007 + female | hh_origin,
  data = est_2014_years,
  cluster = ~hh_origin
)

m_yr_clean_4 <- feols(
  years_educ ~ BOS_years + BOS_years:high_intensity_2007 + female | hh_origin,
  data = est_2014_years,
  cluster = ~hh_origin
)

##############################
# Table construction
##############################

# The two outcome tables use the same four empirical specifications.
model_names <- c(
  "(1) Bin Exp x Cont Int",
  "(2) Bin Exp x Bin Int",
  "(3) BOS Years x Cont Int",
  "(4) BOS Years x Bin Int"
)

# From my advisor: "Readable labels keep the table focused on interpretation, not variable names."
coef_map <- c(
  "treated" = "More Exposure (younger cohort)",
  "treated:share_local_bop_pos_2007" = "More Exposure x BOS Intensity (2007 share)",
  "share_local_bop_pos_2007:treated" = "More Exposure x BOS Intensity (2007 share)",
  "treated:high_intensity_2007" = "More Exposure x High Intensity",
  "high_intensity_2007:treated" = "More Exposure x High Intensity",
  "BOS_years" = "Exposure (BOS years)",
  "BOS_years:share_local_bop_pos_2007" = "Exposure (BOS years) x BOS Intensity (2007 share)",
  "share_local_bop_pos_2007:BOS_years" = "Exposure (BOS years) x BOS Intensity (2007 share)",
  "BOS_years:high_intensity_2007" = "Exposure (BOS years) x High Intensity",
  "high_intensity_2007:BOS_years" = "Exposure (BOS years) x High Intensity",
  "female" = "Female"
)

# Models shown in the HS-attainment table.
hs_models <- list(
  "(1) Bin Exp x Cont Int" = m_hs_clean_1,
  "(2) Bin Exp x Bin Int" = m_hs_clean_2,
  "(3) BOS Years x Cont Int" = m_hs_clean_3,
  "(4) BOS Years x Bin Int" = m_hs_clean_4
)

years_models <- list(
  "(1) Bin Exp x Cont Int" = m_yr_clean_1,
  "(2) Bin Exp x Bin Int" = m_yr_clean_2,
  "(3) BOS Years x Cont Int" = m_yr_clean_3,
  "(4) BOS Years x Bin Int" = m_yr_clean_4
)

# Extra rows document design choices that are not coefficients.
model_info_rows <- data.frame(
  term = c("Household FE", "Controls", "SE Cluster"),
  "(1) Bin Exp x Cont Int" = c("Yes", "Female only", "Household"),
  "(2) Bin Exp x Bin Int" = c("Yes", "Female only", "Household"),
  "(3) BOS Years x Cont Int" = c("Yes", "Female only", "Household"),
  "(4) BOS Years x Bin Int" = c("Yes", "Female only", "Household"),
  check.names = FALSE
)

# Control means should match the sample actually used after singleton removal.
hs_removed_obs <- m_hs_clean_1$obs_selection$obsRemoved
if (is.null(hs_removed_obs) || length(hs_removed_obs) == 0) {
  hs_control_sample <- est_2014_hs
} else {
  hs_control_sample <- est_2014_hs[setdiff(seq_len(nrow(est_2014_hs)), abs(hs_removed_obs)), , drop = FALSE]
}

years_removed_obs <- m_yr_clean_1$obs_selection$obsRemoved
if (is.null(years_removed_obs) || length(years_removed_obs) == 0) {
  years_control_sample <- est_2014_years
} else {
  years_control_sample <- est_2014_years[setdiff(seq_len(nrow(est_2014_years)), abs(years_removed_obs)), , drop = FALSE]
}

hs_control_mean <- mean(hs_control_sample$attain_hs[hs_control_sample$treated == 0], na.rm = TRUE)
years_control_mean <- mean(years_control_sample$years_educ[years_control_sample$treated == 0], na.rm = TRUE)

hs_add_rows <- bind_rows(
  data.frame(
    term = "Control Mean (treated = 0)",
    "(1) Bin Exp x Cont Int" = sprintf("%.3f", hs_control_mean),
    "(2) Bin Exp x Bin Int" = sprintf("%.3f", hs_control_mean),
    "(3) BOS Years x Cont Int" = sprintf("%.3f", hs_control_mean),
    "(4) BOS Years x Bin Int" = sprintf("%.3f", hs_control_mean),
    check.names = FALSE
  ),
  model_info_rows
)

years_add_rows <- bind_rows(
  data.frame(
    term = "Control Mean (treated = 0)",
    "(1) Bin Exp x Cont Int" = sprintf("%.3f", years_control_mean),
    "(2) Bin Exp x Bin Int" = sprintf("%.3f", years_control_mean),
    "(3) BOS Years x Cont Int" = sprintf("%.3f", years_control_mean),
    "(4) BOS Years x Bin Int" = sprintf("%.3f", years_control_mean),
    check.names = FALSE
  ),
  model_info_rows
)

# Notes for the main HS-attainment table.
hs_notes <- list(
  "Cleaned HS models: all columns use household fixed effects and female as the only control.",
  "Binary intensity = 2007 median split. Continuous intensity = raw province-level 2007 BOS intensity share.",
  "Exposure in columns (3)-(4) is raw BOS years (not z-score).",
  "Control mean reports the lower-exposure cohort mean within the 2014 estimation sample.",
  "All standard errors are cluster-robust at household level."
)

# Notes for the secondary years-of-education table.
years_notes <- list(
  "Cleaned years-of-education models: all columns use household fixed effects and female as the only control.",
  "Binary intensity = 2007 median split. Continuous intensity = raw province-level 2007 BOS intensity share.",
  "Exposure in columns (3)-(4) is raw BOS years (not z-score).",
  "Control mean reports the lower-exposure cohort mean within the 2014 estimation sample.",
  "All standard errors are cluster-robust at household level."
)

##############################
# Export tables
##############################

# Main HS-attainment table.
write_clean_latex_table(
  models = hs_models,
  filepaths = c(
    file.path(output_tables_dir, "main_model_2014_hs_cleaned.tex"),
    file.path(draft_dir, "main_model_2014_hs_cleaned.tex")
  ),
  title = "Main Model: Effect on HS Attainment",
  label = "tab:main_hs",
  coef_map = coef_map,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  add_rows = hs_add_rows,
  notes = hs_notes
)

# Secondary years-of-education table.
write_clean_latex_table(
  models = years_models,
  filepaths = c(
    file.path(output_tables_dir, "main_model_2014_years_cleaned.tex"),
    file.path(draft_dir, "main_model_2014_years_cleaned.tex")
  ),
  title = "Main Model: Effect on Years of Education",
  label = "tab:main_years",
  coef_map = coef_map,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  add_rows = years_add_rows,
  notes = years_notes
)
