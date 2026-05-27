##############################################################
# Project: Master's thesis - Control Variable Construction  #
# Author: Gregy                                              #
# Date: February 2026                                        #
#                                                            #
# NOTE: Run 01_Identification.R first to create the data.   #
##############################################################

suppressPackageStartupMessages({
  library(pacman)
  pacman::p_load(haven, dplyr)
})

source("Code/00_Project_Setup.R")

required_inputs <- c(
  "Data/Processed/est_within_balanced.rds",
  "Data/Processed/panel_full.rds",
  "Data/Raw/07_bk_sc.dta",
  "Data/Raw/14_bk_sc1.dta",
  "Data/Raw/More on IFLS/pce-1993-1997_2000-2007/pce07nom.dta",
  "Data/Raw/More on IFLS/hh14_all_dta/b1_ks1.dta",
  "Data/Raw/More on IFLS/hh14_all_dta/b1_ks2.dta",
  "Data/Raw/More on IFLS/hh14_all_dta/b1_ks3.dta",
  "Data/Raw/More on IFLS/hh14_all_dta/b2_kr.dta",
  "Data/Raw/More on IFLS/hh14_all_dta/b1_ks0.dta",
  "Data/Raw/14_bk_ar1.dta"
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop("Missing required input(s): ", paste(missing_inputs, collapse = ", "),
       ". Run 01_Identification.R first.")
}

est_within_balanced <- readRDS("Data/Processed/est_within_balanced.rds")
panel <- readRDS("Data/Processed/panel_full.rds")

##############################
# Helper creation
##############################

# This helper keeps the one-household-per-row files from duplicating records after merges.
read_first_distinct <- function(path, cols, key) {
  read_dta(path) %>%
    transmute(!!!cols) %>%
    distinct({{ key }}, .keep_all = TRUE)
}

##############################
# Household location controls
##############################

# Urban/rural status comes from the IFLS community files.
sc_07 <- read_first_distinct(
  "Data/Raw/07_bk_sc.dta",
  rlang::exprs(
    hhid07 = hhid07,
    urban_07 = as.integer(sc05 == 1)
  ),
  hhid07
)

sc_14 <- read_first_distinct(
  "Data/Raw/14_bk_sc1.dta",
  rlang::exprs(
    hhid14 = hhid14,
    urban_14 = as.integer(sc05 == 1)
  ),
  hhid14
)

province_07 <- read_first_distinct(
  "Data/Raw/07_bk_sc.dta",
  rlang::exprs(
    hhid07 = hhid07,
    province = sc010707
  ),
  hhid07
)

##############################
# Household consumption controls
##############################

# The 2007 IFLS PCE file is already cleaned, so I use it directly.
pce_07 <- read_dta("Data/Raw/More on IFLS/pce-1993-1997_2000-2007/pce07nom.dta") %>%
  transmute(
    hhid07,
    pce_07 = pce,
    lnpce_07 = lnpce
  ) %>%
  distinct(hhid07, .keep_all = TRUE)

# The 2014 PCE file is not used here, so I rebuild monthly PCE from its components.
food_14 <- read_dta("Data/Raw/More on IFLS/hh14_all_dta/b1_ks1.dta") %>%
  mutate(
    ks02 = ifelse(ks02 >= 999995, NA, ks02),
    ks03 = ifelse(ks03 >= 999995, NA, ks03)
  ) %>%
  group_by(hhid14) %>%
  summarise(
    food_purchased = sum(ks02, na.rm = TRUE) * 52 / 12,
    food_own = sum(ks03, na.rm = TRUE) * 52 / 12,
    .groups = "drop"
  )

nonfood_monthly_14 <- read_dta("Data/Raw/More on IFLS/hh14_all_dta/b1_ks2.dta") %>%
  mutate(ks06 = ifelse(ks06 >= 99999995, NA, ks06)) %>%
  group_by(hhid14) %>%
  summarise(nonfood_monthly = sum(ks06, na.rm = TRUE), .groups = "drop")

nonfood_yearly_14 <- read_dta("Data/Raw/More on IFLS/hh14_all_dta/b1_ks3.dta") %>%
  mutate(
    ks08 = ifelse(ks08 >= 98989998, NA, ks08),
    ks09a = ifelse(ks09a >= 98998998, NA, ks09a)
  ) %>%
  group_by(hhid14) %>%
  summarise(
    nonfood_yearly = (sum(ks08, na.rm = TRUE) + sum(ks09a, na.rm = TRUE)) / 12,
    .groups = "drop"
  )

housing_14 <- read_dta("Data/Raw/More on IFLS/hh14_all_dta/b2_kr.dta") %>%
  mutate(
    kr04 = case_when(
      kr04ax == 2 ~ kr04a,
      kr04ax == 1 ~ kr04a / 12,
      TRUE ~ NA_real_
    ),
    kr05 = case_when(
      kr05ax == 2 ~ kr05a,
      kr05ax == 1 ~ kr05a / 12,
      TRUE ~ NA_real_
    ),
    housing = dplyr::coalesce(kr04, kr05)
  ) %>%
  group_by(hhid14) %>%
  summarise(housing = first(housing), .groups = "drop")

educ_14 <- read_dta("Data/Raw/More on IFLS/hh14_all_dta/b1_ks0.dta") %>%
  mutate(
    ks10aa = ifelse(ks10aa >= 98998998, NA, ks10aa),
    ks11aa = ifelse(ks11aa >= 98998998, NA, ks11aa),
    ks12aa = ifelse(ks12aa >= 98998998, NA, ks12aa)
  ) %>%
  group_by(hhid14) %>%
  summarise(
    educ = (first(ks10aa) + first(ks11aa) + first(ks12aa)) / 12,
    .groups = "drop"
  )

hh_size_14 <- read_dta("Data/Raw/14_bk_ar1.dta") %>%
  filter(ar01a %in% c(1, 2, 5, 11)) %>%
  group_by(hhid14) %>%
  summarise(hhsize_14 = n(), .groups = "drop")

pce_14 <- food_14 %>%
  left_join(nonfood_monthly_14, by = "hhid14") %>%
  left_join(nonfood_yearly_14, by = "hhid14") %>%
  left_join(housing_14, by = "hhid14") %>%
  left_join(educ_14, by = "hhid14") %>%
  left_join(hh_size_14, by = "hhid14") %>%
  mutate(
    xfood = food_purchased + food_own,
    xnonfood = nonfood_monthly + nonfood_yearly + coalesce(housing, 0) + coalesce(educ, 0),
    hhexp = xfood + xnonfood,
    pce_14 = hhexp / hhsize_14,
    lnpce_14 = log(pce_14)
  ) %>%
  select(hhid14, pce_14, lnpce_14) %>%
  filter(!is.na(pce_14), pce_14 > 0)

##############################
# Parent education controls
##############################

# Parent education requires mapping child-reported parent line numbers to parent pidlinks.
roster_2007 <- panel %>%
  filter(year == 2007, resident == TRUE, !is.na(hh_origin), !is.na(line_no), !is.na(pidlink)) %>%
  transmute(
    hh_origin,
    line_no = as.numeric(line_no),
    pidlink = as.character(pidlink)
  ) %>%
  distinct(hh_origin, line_no, .keep_all = TRUE)

child_parent_pid <- panel %>%
  filter(year == 2007, child_in_2007 == 1, !is.na(hh_origin)) %>%
  transmute(
    child_pid = as.character(pidlink),
    hh_origin,
    father_line = as.numeric(father_id),
    mother_line = as.numeric(mother_id)
  ) %>%
  distinct() %>%
  left_join(
    roster_2007 %>% rename(father_line = line_no, father_pid = pidlink),
    by = c("hh_origin", "father_line")
  ) %>%
  left_join(
    roster_2007 %>% rename(mother_line = line_no, mother_pid = pidlink),
    by = c("hh_origin", "mother_line")
  )

# Parent education sometimes shifts slightly across waves, mostly from reporting noise.
# Small differences are smoothed with the median; larger inconsistencies use the latest report.
parent_educ_resolved <- panel %>%
  filter(year %in% c(2000, 2007, 2014), !is.na(pidlink)) %>%
  transmute(
    pidlink = as.character(pidlink),
    year = as.numeric(year),
    years_educ = as.numeric(years_educ)
  ) %>%
  distinct(pidlink, year, .keep_all = TRUE) %>%
  group_by(pidlink) %>%
  arrange(year, .by_group = TRUE) %>%
  summarise(
    n_non_missing = sum(!is.na(years_educ)),
    educ_range = ifelse(
      n_non_missing > 0,
      max(years_educ, na.rm = TRUE) - min(years_educ, na.rm = TRUE),
      NA_real_
    ),
    latest_educ = {
      idx <- which(!is.na(years_educ))
      if (length(idx) == 0) NA_real_ else years_educ[idx[which.max(year[idx])]]
    },
    median_educ = ifelse(n_non_missing > 0, median(years_educ, na.rm = TRUE), NA_real_),
    inconsistent = as.integer(!is.na(educ_range) & educ_range > 1),
    parent_educ = ifelse(inconsistent == 1, latest_educ, median_educ),
    .groups = "drop"
  )

# Collapse father/mother education to the origin-household level for HTE and balance checks.
parent_educ_final <- child_parent_pid %>%
  left_join(
    parent_educ_resolved %>%
      select(father_pid = pidlink, father_educ = parent_educ, father_inconsistent = inconsistent),
    by = "father_pid"
  ) %>%
  left_join(
    parent_educ_resolved %>%
      select(mother_pid = pidlink, mother_educ = parent_educ, mother_inconsistent = inconsistent),
    by = "mother_pid"
  ) %>%
  group_by(hh_origin) %>%
  summarise(
    father_educ = ifelse(all(is.na(father_educ)), NA_real_, first(father_educ[!is.na(father_educ)])),
    mother_educ = ifelse(all(is.na(mother_educ)), NA_real_, first(mother_educ[!is.na(mother_educ)])),
    any_parent_inconsistent = as.integer(any(father_inconsistent == 1 | mother_inconsistent == 1, na.rm = TRUE)),
    max_parent_educ = pmax(father_educ, mother_educ, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    max_parent_educ = ifelse(is.infinite(max_parent_educ), NA_real_, max_parent_educ),
    any_parent_inconsistent = ifelse(is.na(any_parent_inconsistent), 0L, any_parent_inconsistent)
  )

##############################
# Household-level baseline controls
##############################

# Baseline household characteristics are anchored to the 2007 origin household.
female_head <- panel %>%
  filter(year == 2007, status == 1, !is.na(hh_origin)) %>%
  transmute(hh_origin, female_head = as.integer(sex == 3)) %>%
  distinct(hh_origin, .keep_all = TRUE)

hh_size <- panel %>%
  filter(resident == TRUE) %>%
  group_by(hh_origin, year) %>%
  summarise(hh_size = n(), .groups = "drop")

##############################
# Main merge
##############################

# This map connects each child-wave record to the wave-specific household ID.
hh_mapping <- est_within_balanced %>%
  select(pidlink, hh_origin, true_hh, year) %>%
  distinct()

# Controls are merged into the balanced within-family estimation panel.
est_with_controls <- est_within_balanced %>%
  left_join(
    hh_mapping %>%
      filter(year == 2007) %>%
      select(pidlink, true_hh) %>%
      left_join(sc_07, by = c("true_hh" = "hhid07")) %>%
      select(pidlink, urban_07),
    by = "pidlink"
  ) %>%
  left_join(
    hh_mapping %>%
      filter(year == 2014) %>%
      select(pidlink, true_hh) %>%
      left_join(sc_14, by = c("true_hh" = "hhid14")) %>%
      select(pidlink, urban_14),
    by = "pidlink"
  ) %>%
  mutate(
    urban = case_when(
      year == 2007 ~ urban_07,
      year == 2014 ~ urban_14,
      TRUE ~ NA_integer_
    )
  ) %>%
  left_join(
    hh_mapping %>%
      filter(year == 2007) %>%
      select(pidlink, true_hh) %>%
      left_join(pce_07, by = c("true_hh" = "hhid07")),
    by = "pidlink"
  ) %>%
  left_join(
    hh_mapping %>%
      filter(year == 2014) %>%
      select(pidlink, true_hh) %>%
      left_join(pce_14, by = c("true_hh" = "hhid14")),
    by = "pidlink"
  ) %>%
  mutate(
    pce = case_when(
      year == 2007 ~ pce_07,
      year == 2014 ~ pce_14,
      TRUE ~ NA_real_
    ),
    lnpce = case_when(
      year == 2007 ~ lnpce_07,
      year == 2014 ~ lnpce_14,
      TRUE ~ NA_real_
    )
  ) %>%
  left_join(
    hh_mapping %>%
      filter(year == 2007) %>%
      select(pidlink, true_hh) %>%
      left_join(province_07, by = c("true_hh" = "hhid07")) %>%
      select(pidlink, province),
    by = "pidlink"
  ) %>%
  left_join(hh_size, by = c("hh_origin", "year")) %>%
  left_join(parent_educ_final, by = "hh_origin") %>%
  left_join(female_head, by = "hh_origin")

##############################
# Descriptive variables
##############################

# Baseline terciles are only for descriptive and heterogeneity variables.
baseline_terciles <- est_with_controls %>%
  filter(year == 2007) %>%
  select(hh_origin, pce_07, urban_07, max_parent_educ) %>%
  distinct(hh_origin, .keep_all = TRUE) %>%
  mutate(
    pce_tercile = ntile(pce_07, 3),
    poor_baseline = as.integer(pce_tercile == 1),
    middle_baseline = as.integer(pce_tercile == 2),
    rich_baseline = as.integer(pce_tercile == 3),
    parent_educ_tercile = ntile(max_parent_educ, 3),
    low_parent_educ = as.integer(parent_educ_tercile == 1),
    high_parent_educ = as.integer(parent_educ_tercile == 3),
    urban_baseline = urban_07
  ) %>%
  select(
    hh_origin, pce_tercile, poor_baseline, middle_baseline, rich_baseline,
    parent_educ_tercile, low_parent_educ, high_parent_educ, urban_baseline
  )

est_with_controls <- est_with_controls %>%
  left_join(baseline_terciles, by = "hh_origin")

saveRDS(est_with_controls, "Data/Processed/est_with_controls.rds")

message("Saved Data/Processed/est_with_controls.rds")
message("Next: Run 02_Balance.R, 03_Regression.R, and HTE-Robustness.R")
