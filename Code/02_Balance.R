##############################################################
# Project: Master's thesis - Balance Table                  #
# Author: Gregy                                              #
# Date: February 2026                                        #
#                                                            #
# NOTE: Run 01_Identification.R and 01b_Controls.R first.   #
##############################################################

suppressPackageStartupMessages({
  library(pacman)
  pacman::p_load(haven, dplyr, tidyr)
})

source("Code/00_Project_Setup.R")

output_tables_dir <- "Output/Tables"
draft_dir <- "Paper/source"

dir.create(output_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(draft_dir, recursive = TRUE, showWarnings = FALSE)

required_inputs <- c(
  "Data/Processed/est_with_controls.rds",
  "Data/Raw/More on IFLS/cf07_all_dta/schl.dta",
  "Data/Raw/14_bk_sc1.dta"
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop("Missing required input(s): ", paste(missing_inputs, collapse = ", "))
}

est_with_controls <- readRDS("Data/Processed/est_with_controls.rds") %>%
  mutate(across(where(haven::is.labelled), haven::zap_labels))

##############################
# BOS intensity construction
##############################

# The balance table uses the same 2007 BOS intensity proxy as the regressions.
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
    share_local_bop_pos_2007 = mean(local_bop_pos, na.rm = TRUE),
    .groups = "drop"
  )

threshold_2007 <- median(intensity_2007$share_local_bop_pos_2007, na.rm = TRUE)

##############################
# Main analysis data
##############################

# 2014 household location is the fallback when province is missing.
# This merge affects the design-variable rows, so it should stay aligned with the main model.
hh_loc_2014 <- read_dta("Data/Raw/14_bk_sc1.dta") %>%
  transmute(
    true_hh = hhid14,
    province_loc = as.numeric(sc01_14_14)
  ) %>%
  distinct(true_hh, .keep_all = TRUE)

# I restrict the balance table to the same 2014 analytic sample used before
# household fixed effects drop singleton observations.
analytic_2014_ids <- est_with_controls %>%
  filter(year == 2014) %>%
  left_join(hh_loc_2014, by = "true_hh") %>%
  mutate(
    province_merge = dplyr::coalesce(as.numeric(province), province_loc),
    geo_id = if_else(!is.na(province_merge), paste0("P", as.integer(province_merge)), NA_character_)
  ) %>%
  left_join(intensity_2007, by = "geo_id") %>%
  mutate(
    years_educ = as.numeric(years_educ),
    attain_hs = if_else(!is.na(years_educ), as.integer(years_educ >= 10), NA_integer_)
  ) %>%
  filter(!is.na(attain_hs), !is.na(share_local_bop_pos_2007)) %>%
  distinct(pidlink)

# The balance table uses 2007 baseline covariates for the 2014 analytic children.
balance_data <- est_with_controls %>%
  filter(year == 2007) %>%
  semi_join(analytic_2014_ids, by = "pidlink") %>%
  left_join(
    est_with_controls %>%
      filter(year == 2014) %>%
      select(pidlink, true_hh, province) %>%
      distinct(pidlink, .keep_all = TRUE),
    by = "pidlink",
    suffix = c("", "_2014")
  ) %>%
  left_join(hh_loc_2014, by = "true_hh") %>%
  mutate(
    province_merge = dplyr::coalesce(as.numeric(province_2014), province_loc),
    geo_id = if_else(!is.na(province_merge), paste0("P", as.integer(province_merge)), NA_character_)
  ) %>%
  left_join(intensity_2007, by = "geo_id") %>%
  transmute(
    treated,
    female = as.integer(sex == 3),
    urban,
    lnpce,
    hh_size,
    female_head,
    max_parent_educ,
    share_local_bop_pos_2007,
    high_intensity_2007 = as.integer(share_local_bop_pos_2007 > threshold_2007)
  )

balance_group_counts <- balance_data %>%
  count(treated, name = "n")

lower_exposure_n <- balance_group_counts$n[balance_group_counts$treated == 0]
higher_exposure_n <- balance_group_counts$n[balance_group_counts$treated == 1]

##############################
# Balance calculation
##############################

# I keep this small helper because every balance row needs the same mean,
# difference, p-value, and normalized-difference calculation.
calc_balance <- function(data, var_name, var_label, panel_label) {
  t_test_data <- data %>%
    select(treated, value = all_of(var_name)) %>%
    filter(!is.na(treated), !is.na(value))

  p_value <- t.test(value ~ treated, data = t_test_data)$p.value

  data %>%
    group_by(treated) %>%
    summarise(
      mean = mean(.data[[var_name]], na.rm = TRUE),
      sd = sd(.data[[var_name]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = treated, values_from = c(mean, sd)) %>%
    transmute(
      panel = panel_label,
      variable = var_label,
      mean_0,
      mean_1,
      diff = mean_1 - mean_0,
      p_value = p_value,
      norm_diff = diff / sqrt((sd_0^2 + sd_1^2) / 2)
    )
}

# Calculate balance rows variable by variable.
balance_rows <- bind_rows(
  calc_balance(balance_data, "female", "Female", "Panel A. Baseline covariates"),
  calc_balance(balance_data, "urban", "Urban", "Panel A. Baseline covariates"),
  calc_balance(balance_data, "lnpce", "Log per-capita exp.", "Panel A. Baseline covariates"),
  calc_balance(balance_data, "hh_size", "HH size", "Panel A. Baseline covariates"),
  calc_balance(balance_data, "female_head", "Female-headed HH", "Panel A. Baseline covariates"),
  calc_balance(balance_data, "max_parent_educ", "Max parent educ.", "Panel A. Baseline covariates"),
  calc_balance(balance_data, "share_local_bop_pos_2007", "BOS intensity share", "Panel B. Design variables"),
  calc_balance(balance_data, "high_intensity_2007", "High intensity", "Panel B. Design variables")
)

write.csv(
  balance_rows %>% mutate(across(c(mean_0, mean_1, diff, p_value, norm_diff), ~ round(.x, 3))),
  file.path(output_tables_dir, "table1_balance.csv"),
  row.names = FALSE
)

##############################
# Table construction
##############################

# I format the rows after the calculations so the CSV keeps the unrounded numbers.
format_rows <- function(df) {
  apply(
    df %>% mutate(across(c(mean_0, mean_1, diff, p_value, norm_diff), ~ sprintf("%.3f", .x))),
    1,
    function(row) paste(row[["variable"]], row[["mean_0"]], row[["mean_1"]], row[["diff"]], row[["p_value"]], row[["norm_diff"]], sep = " & ")
  )
}

panel_a_lines <- paste0(format_rows(balance_rows %>% filter(panel == "Panel A. Baseline covariates")), "\\\\")
panel_b_lines <- paste0(format_rows(balance_rows %>% filter(panel == "Panel B. Design variables")), "\\\\")

# Final balance table used as Table 1 in the thesis.
balance_tex <- c(
  "\\begin{table}[!htbp]",
  "\\caption{Balance by Birth Cohort: Lower-Exposure (1988--1992) vs. Higher-Exposure (1994--1997)}",
  "\\label{tab:balance}",
  "\\centering",
  "\\vspace{-0.5em}",
  "\\begin{threeparttable}",
  "\\small",
  "\\setlength{\\tabcolsep}{3pt}",
  "\\renewcommand{\\arraystretch}{0.95}",
  "\\begin{tabular}[t]{@{}lrrrrr@{}}",
  "\\toprule",
  "Variable & Lower exposure & Higher exposure & Diff. & p-value & Norm. diff.\\\\",
  "\\midrule",
  paste0("Children (N) & ", lower_exposure_n, " & ", higher_exposure_n, " &  &  & \\\\"),
  "\\addlinespace",
  "\\multicolumn{6}{l}{\\textit{Panel A. Baseline covariates}}\\\\",
  panel_a_lines,
  "\\addlinespace",
  "\\multicolumn{6}{l}{\\textit{Panel B. Design variables}}\\\\",
  panel_b_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\vspace{-0.4em}",
  "\\begin{tablenotes}[para]",
  "\\item \\textit{Note: }",
  paste0(
    "\\item The lower-exposure group includes children born in 1988--1992; the higher-exposure group includes children born in 1994--1997. ",
    "The table is restricted to children in the 2014 analytic sample before household fixed-effects singleton observations are dropped. ",
    "Panel A reports predetermined child and household covariates measured in 2007. ",
    "Panel B reports design variables based on the 2007 province-level BOS intensity proxy. ",
    "BOS intensity is the province share of sampled IFLS schools reporting positive BOS-related local operational funding in 2007. ",
    "High intensity equals one for provinces above the 2007 median of this proxy. ",
    "p-values come from two-sided t-tests of equality in group means. ",
    "Age is omitted because treatment status is mechanically defined by birth year."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(balance_tex, file.path(output_tables_dir, "table1_balance.tex"))
writeLines(balance_tex, file.path(draft_dir, "table1_balance.tex"))

message("Saved Output/Tables/table1_balance.csv")
message("Saved Output/Tables/table1_balance.tex")
message("Saved draft copy of table1_balance.tex")
