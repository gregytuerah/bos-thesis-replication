##############################################################
# Project: Master's thesis: Identification                   #
# Author: Gregy                                              #
# Date: January 2026                                         #
##############################################################

# IDENTIFICATION STRATEGY (FINAL):
# - BOS program: 2005-2014 (grades 1-9, ages 6-15)
# - Variation: CUMULATIVE YEARS of BOS exposure by birth cohort
# - Control: Born 1988-1992 (0-3 years BOS, ages 22-26 in 2014)
# - Treatment: Born 1994-1997 (5-8 years BOS, ages 17-20 in 2014)
# - Gap: Born 1993 excluded (transition year, 4 years BOS)
# - Method: HOUSEHOLD FIXED EFFECTS (primary)

##############################################################
# SETUP
##############################################################

suppressPackageStartupMessages({
  library(pacman)
  pacman::p_load(haven, dplyr, knitr, kableExtra)
})

source("Code/00_Project_Setup.R")

##############################################################
# HELPERS
##############################################################

# Same IFLS education coding appears in each wave, so this conversion is shared.
# The output is completed schooling years from education level and completed grade.
derive_education_outcomes <- function(educ, educ_grade) {
  educ_num <- suppressWarnings(as.numeric(educ))
  grade_num <- suppressWarnings(as.numeric(educ_grade))
  grade_base <- case_when(
    is.na(grade_num) ~ NA_real_,
    grade_num %in% c(96, 98, 99) ~ NA_real_,
    grade_num == 0 ~ 0,
    grade_num >= 1 & grade_num <= 6 ~ grade_num,
    grade_num == 7 ~ 7,
    TRUE ~ NA_real_
  )

  grade_elem <- case_when(
    is.na(grade_base) ~ NA_real_,
    grade_base == 7 ~ 6,
    TRUE ~ pmin(grade_base, 6)
  )

  grade_jhs <- case_when(
    is.na(grade_base) ~ NA_real_,
    grade_base == 7 ~ 3,
    TRUE ~ pmin(grade_base, 3)
  )

  grade_shs <- case_when(
    is.na(grade_base) ~ NA_real_,
    grade_base == 7 ~ 3,
    TRUE ~ pmin(grade_base, 3)
  )

  grade_tert <- case_when(
    is.na(grade_base) ~ NA_real_,
    grade_base == 7 ~ 4,
    TRUE ~ grade_base
  )

  years_educ <- case_when(
    educ_num %in% c(1, 90) ~ 0,
    educ_num %in% c(2, 72) ~ grade_elem,
    educ_num %in% c(3, 4, 73) ~ 6 + grade_jhs,
    educ_num %in% c(5, 6, 74) ~ 9 + grade_shs,
    educ_num %in% c(11) ~ grade_elem,                      # Paket A
    educ_num %in% c(12) ~ 6 + grade_jhs,                   # Paket B
    educ_num %in% c(15) ~ 9 + grade_shs,                   # Paket C
    educ_num %in% c(60) ~ 12 + pmin(grade_tert, 3),        # D1-D3
    educ_num %in% c(61, 13) ~ 12 + pmin(grade_tert, 4),    # S1/Open Univ
    educ_num %in% c(62) ~ 16 + pmin(grade_tert, 2),        # S2
    educ_num %in% c(63) ~ 18 + pmin(grade_tert, 3),        # S3
    educ_num %in% c(14, 17, 95) ~ 12 + pmin(grade_tert, 8),
    educ_num %in% c(98, 99) ~ NA_real_,
    TRUE ~ NA_real_
  )

  hs_continue <- if_else(!is.na(years_educ), as.integer(years_educ >= 10), NA_integer_)

  tibble(
    years_educ = years_educ,
    hs_continue = hs_continue
  )
}

##############################
# Load and clean raw data
##############################

message("STEP 1: Loading raw data")

# Household member rosters for each IFLS wave.
df_00 <- read_dta("Data/Raw/00_bk_ar1.dta")
df_07 <- read_dta("Data/Raw/07_bk_ar1.dta")
df_14 <- read_dta("Data/Raw/14_bk_ar1.dta")

# Each roster wave needs the same column structure before stacking.
clean_wave <- function(df, year, hhid_var, line_var) {
  df %>%
    transmute(
      pidlink,
      hhid       = !!sym(hhid_var),
      line_no    = suppressWarnings(as.numeric(!!sym(line_var))),      # line number within household roster
      year       = year,

      # Residence and relationship
      ar01a      = ar01a,      # residence status
      ar01b      = ar01b,      # tracking status
      status     = ar02b,      # relationship to HH head

      # Demographics
      age_reported        = ar09,       # current age (reported in wave)
      sex        = ar07,       # sex
      birth_year_reported = ar08yr,     # reported birth year (wave-specific)
      birth_month= ar08mth,    # birth month

      # Education
      educ       = ar16,       # highest education level
      educ_grade = ar17,       # highest grade completed
      still_school = ar18c,    # still in school?

      # Family structure
      father_id  = ar10,       # father's ID
      mother_id  = ar11,       # mother's ID
      spouse_id  = ar14,       # spouse's line number

      # Additional covariates
      marital    = ar13,       # marital status
      religion   = ar15,       # religion
      work_lastyear = ar15a    # worked last year
    )
}

df00_clean <- clean_wave(df_00, 2000, "hhid00", "ar00b")
df07_clean <- clean_wave(df_07, 2007, "hhid07", "ar00")
df14_clean <- clean_wave(df_14, 2014, "hhid14", "ar00id")

# Stack all waves after the roster variables have the same names.
panel_raw <- bind_rows(df00_clean, df07_clean, df14_clean) %>%
  arrange(pidlink, year)

message("Raw panel observations: ", nrow(panel_raw))
message("Unique individuals: ", n_distinct(panel_raw$pidlink))

##############################
# Identify resident household
##############################

message("STEP 2: Identifying residents")

panel <- panel_raw %>%
  mutate(is_resident_row = ar01a %in% c(1, 2, 5, 11)) %>%
  group_by(pidlink, year) %>%
  arrange(desc(is_resident_row), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(
    panel_raw %>%
      group_by(pidlink, year) %>%
      summarise(resident = any(ar01a %in% c(1, 2, 5, 11), na.rm = TRUE),
                .groups = "drop"),
    by = c("pidlink", "year")
  ) %>%
  transmute(
    pidlink,
    year,
    resident,
    true_hh = hhid,
    status,
    ar01a,
    line_no,
    ar01b,
    age_reported,
    sex,
    birth_year_reported,
    birth_month,
    educ,
    educ_grade,
    still_school,
    father_id,
    mother_id,
    spouse_id,
    marital,
    religion,
    work_lastyear
  ) %>%
  mutate(
    age_reported = na_if(age_reported, 998),
    birth_year_reported = na_if(birth_year_reported, 9998)
  )

message("After identifying residents: ", nrow(panel))

##############################
# Identify children
##############################

message("STEP 3: Identifying children")

panel <- panel %>%
  group_by(pidlink) %>%
  mutate(
    child_in_2007 = as.integer(any(year == 2007 & status %in% c(3, 4), na.rm = TRUE)),

    # I keep this stricter child definition only as a diagnostic.
    ever_non_child = any(!status %in% c(3, 4), na.rm = TRUE),
    always_child   = as.integer(!ever_non_child & any(status %in% c(3, 4), na.rm = TRUE))
  ) %>%
  ungroup()

message(
  "'Always children' (status 3/4 only): ",
  n_distinct(panel$pidlink[panel$always_child == 1]),
  " individuals"
)

##############################
# Clean birth year
##############################

message("STEP 4: Cleaning birth year")

panel <- panel %>%
  mutate(birth_year_from_age = year - age_reported)

birth_year_pid <- panel %>%
  group_by(pidlink) %>%
  summarise(
    by_rep_min = suppressWarnings(min(birth_year_reported, na.rm = TRUE)),
    by_rep_max = suppressWarnings(max(birth_year_reported, na.rm = TRUE)),
    by_rep_median = suppressWarnings(median(birth_year_reported, na.rm = TRUE)),
    by_latest = dplyr::last(birth_year_reported[!is.na(birth_year_reported)], default = NA_real_),
    by_from_age = suppressWarnings(median(birth_year_from_age, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    across(c(by_rep_min, by_rep_max, by_rep_median, by_latest, by_from_age),
           ~ ifelse(is.infinite(.) | is.nan(.), NA_real_, .)),
    birth_year = case_when(
      !is.na(by_latest) & !is.na(by_rep_min) & !is.na(by_rep_max) &
        (by_rep_max - by_rep_min <= 1) ~ round(by_rep_median),
      !is.na(by_latest) ~ round(by_latest),
      !is.na(by_from_age) ~ round(by_from_age),
      TRUE ~ NA_real_
    ),
    birth_year = as.integer(birth_year)
  ) %>%
  select(pidlink, birth_year)

panel <- panel %>%
  left_join(birth_year_pid, by = "pidlink") %>%
  mutate(
    age = if_else(!is.na(birth_year), year - birth_year, age_reported),
    age_minus_reported = if_else(!is.na(age_reported), age - age_reported, NA_real_)
  ) %>%
  select(-birth_year_from_age)

message(
  "Birth year range: ",
  min(panel$birth_year, na.rm = TRUE),
  " to ",
  max(panel$birth_year, na.rm = TRUE)
)

message("Age consistency check (constructed age - reported age)")
panel %>%
  filter(year %in% c(2007, 2014), !is.na(age_minus_reported)) %>%
  summarise(
    N = n(),
    MeanDiff = mean(age_minus_reported, na.rm = TRUE),
    PctAbsGt1 = 100 * mean(abs(age_minus_reported) > 1, na.rm = TRUE)
  ) %>%
  kable(digits = 2) %>%
  print()

##############################
# Calculate BOS exposure
##############################

message("STEP 5: Calculating BOS exposure")

panel <- panel %>%
  mutate(
    # Diagnostic age snapshots used only to construct cumulative potential BOS
    # exposure windows relative to the 2005 policy rollout.
    age_2005 = 2005 - birth_year,
    age_2014 = 2014 - birth_year,
    age_2007 = 2007 - birth_year,

    # Cumulative potential BOS exposure during basic-school ages (approx. ages
    # 6-15) observed at each survey wave. This is policy-timing-based exposure,
    # not self-reported BOS receipt.
    BOS_years = case_when(
      is.na(birth_year) ~ NA_real_,
      year == 2000 ~ 0,
      year == 2007 ~ {
        start_year <- pmax(2005, birth_year + 6)
        end_year <- pmin(2007, birth_year + 15)
        pmax(0, end_year - start_year + 1)
      },
      year == 2014 ~ {
        start_year <- pmax(2005, birth_year + 6)
        end_year <- pmin(2014, birth_year + 15)
        pmax(0, end_year - start_year + 1)
      },
      TRUE ~ NA_real_
    )
  ) %>%
  select(-age_2005, -age_2014, -age_2007)

message(
  "BOS_years range: ",
  min(panel$BOS_years, na.rm = TRUE),
  " to ",
  max(panel$BOS_years, na.rm = TRUE)
)

# Check whether the BOS exposure variable behaves as expected by birth year.
message("BOS exposure by birth year (2014)")
verification <- panel %>%
  filter(always_child == 1, year == 2014, !is.na(birth_year)) %>%
  group_by(birth_year) %>%
  summarise(
    n = n(),
    mean_BOS = mean(BOS_years, na.rm = TRUE),
    age_2005 = first(2005 - birth_year),
    age_2014 = first(2014 - birth_year),
    .groups = "drop"
  ) %>%
  filter(birth_year >= 1988 & birth_year <= 1998)

verification %>%
  kable(
    digits = 1,
    col.names = c("Birth Year", "N", "Mean BOS", "Age 2005", "Age 2014")
  ) %>%
  print()

##############################
# Define cohort groups
##############################

message("STEP 6: Defining cohort groups")

# Lower- and higher-exposure cohorts are defined from birth year.
panel <- panel %>%
  mutate(
    # Cohort labels used in the baseline within-family exposure design.
    cohort_group = case_when(
      birth_year >= 1988 & birth_year <= 1992 ~ "Low Exposure (1988-1992)",
      birth_year >= 1994 & birth_year <= 1997 ~ "High Exposure (1994-1997)",
      TRUE ~ "Other"
    ),

    # Binary cohort indicators used in the panel DiD / exposure design.
    low_exposure  = as.integer(birth_year >= 1988 & birth_year <= 1992),
    high_exposure = as.integer(birth_year >= 1994 & birth_year <= 1997),

    # Baseline treatment indicator for regressions:
    # treated = 1 for the higher-exposure cohort (1994-1997).
    treated = high_exposure,

    # Post-period indicator for the two-wave panel regression design.
    # The baseline specification compares 2007 (pre) vs 2014 (post).
    post = as.integer(year == 2014)
  )

message("Sample by cohort group (2014)")
panel %>%
  filter(always_child == 1, year == 2014) %>%
  count(cohort_group) %>%
  kable(caption = "Sample by Cohort Group (2014)") %>%
  print()

##############################
# Family-of-origin household
##############################
# hh_origin is the earliest observed resident household for each person.
# This keeps siblings grouped even if they move out later.

panel <- panel %>%
  group_by(pidlink) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    hh_origin = first(true_hh[resident == TRUE])
  ) %>%
  ungroup()

# If someone was never observed as resident, I fall back to the first true_hh.
panel <- panel %>%
  group_by(pidlink) %>%
  mutate(
    hh_origin = ifelse(is.na(hh_origin), dplyr::first(true_hh), hh_origin)
  ) %>%
  ungroup()

##############################
# Birth order
##############################

birth_order_permanent <- panel %>%
  filter(child_in_2007 == 1, !is.na(birth_year)) %>%     # use child_in_2007 sample
  group_by(hh_origin, pidlink, birth_year) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(hh_origin) %>%
  arrange(birth_year, .by_group = TRUE) %>%
  mutate(birth_order = row_number()) %>%
  ungroup() %>%
  select(pidlink, hh_origin, birth_order)

panel <- panel %>%
  select(-any_of("birth_order")) %>%   # won't error if missing
  left_join(birth_order_permanent, by = c("pidlink", "hh_origin"))


##############################
# Years of education
##############################

message("STEP 7: Calculating years of education")

panel <- panel %>%
  mutate(
    educ = as.numeric(educ),
    educ_grade = as.numeric(educ_grade)
  )

educ_outcomes <- derive_education_outcomes(panel$educ, panel$educ_grade)
panel <- bind_cols(panel, educ_outcomes)

# Preserve the raw education outcome before panel-consistency cleaning.
panel <- panel %>%
  mutate(years_educ_raw = years_educ)

# Clean years_educ for panel consistency:
# 1) Non-decreasing between 2007 and 2014 for same individual
# 2) Cap 2014 gain by observed age gap (max one grade per year)
# 3) Soft age-feasibility cap (years <= age - 4)
panel <- panel %>%
  group_by(pidlink) %>%
  mutate(
    y2007 = first(years_educ_raw[year == 2007], default = NA_real_),
    y2014 = first(years_educ_raw[year == 2014], default = NA_real_),
    a2007 = first(age[year == 2007], default = NA_real_),
    a2014 = first(age[year == 2014], default = NA_real_),
    max_gain = if_else(!is.na(a2007) & !is.na(a2014), pmax(a2014 - a2007, 0), NA_real_),
    y2014_clean = case_when(
      !is.na(y2007) & !is.na(y2014) & !is.na(max_gain) ~ pmin(pmax(y2014, y2007), y2007 + max_gain),
      !is.na(y2007) & !is.na(y2014) ~ pmax(y2014, y2007),
      TRUE ~ y2014
    ),
    years_educ = case_when(
      year == 2014 & !is.na(y2014_clean) ~ y2014_clean,
      TRUE ~ years_educ_raw
    ),
    years_educ = if_else(
      !is.na(years_educ) & !is.na(age),
      pmin(years_educ, pmax(age - 4, 0)),
      years_educ
    )
  ) %>%
  ungroup() %>%
  select(-y2007, -y2014, -a2007, -a2014, -max_gain, -y2014_clean)

# Additional binary outcome and mechanism variables for later checks.
panel <- panel %>%
  mutate(
    in_school = case_when(
      still_school == 1 ~ 1L,
      still_school == 3 ~ 0L,
      TRUE ~ NA_integer_
    ),
    worked_lastyear_bin = case_when(
      work_lastyear == 1 ~ 1L,
      work_lastyear == 3 ~ 0L,
      TRUE ~ NA_integer_
    ),
    married_bin = case_when(
      marital %in% c(2, 3, 4, 5) ~ 1L,
      marital == 1 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

message(
  "Years of education range: ",
  min(panel$years_educ, na.rm = TRUE),
  " to ",
  max(panel$years_educ, na.rm = TRUE),
  " years"
)

message("Years-education cleaning diagnostics (2007->2014)")
panel %>%
  filter(year %in% c(2007, 2014)) %>%
  group_by(pidlink) %>%
  summarise(
    y07_raw = first(years_educ_raw[year == 2007], default = NA_real_),
    y14_raw = first(years_educ_raw[year == 2014], default = NA_real_),
    y07 = first(years_educ[year == 2007], default = NA_real_),
    y14 = first(years_educ[year == 2014], default = NA_real_),
    .groups = "drop"
  ) %>%
  summarise(
    N = n(),
    RawDecline = sum(!is.na(y14_raw) & !is.na(y07_raw) & y14_raw < y07_raw),
    CleanDecline = sum(!is.na(y14) & !is.na(y07) & y14 < y07),
    AnyAdjusted = sum((!is.na(y14_raw) & !is.na(y14)) & (y14_raw != y14))
  ) %>%
  kable() %>%
  print()

message("HS continuation (reached senior high or above)")
panel %>%
  filter(year %in% c(2007, 2014), !is.na(hs_continue)) %>%
  group_by(year) %>%
  summarise(
    N = n(),
    Share_HS = mean(hs_continue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Share_HS = round(100 * Share_HS, 1)) %>%
  kable(col.names = c("Year", "N", "Share HS (%)")) %>%
  print()

# Check the older low-exposure cohort after years_educ is created.
message("Born-1988 education check after years_educ is created")
panel %>%
  filter(always_child == 1, year == 2014, birth_year == 1988) %>%
  summarise(
    N = n(),
    Mean_BOS = mean(BOS_years, na.rm = TRUE),
    Mean_Educ = mean(years_educ, na.rm = TRUE),
    Mean_Age = mean(age, na.rm = TRUE),
    Still_School_Pct = mean(still_school == 1, na.rm = TRUE) * 100
  ) %>%
  kable(digits = 2) %>%
  print()

##############################
# Households with both cohorts
##############################

# Define has_both using the 2007 roster to avoid conditioning on 2014 attrition.
hh_structure_origin <- panel %>%
  filter(child_in_2007 == 1, year == 2007) %>%
  group_by(hh_origin) %>%
  summarise(
    has_low  = any(low_exposure == 1, na.rm = TRUE),
    has_high = any(high_exposure == 1, na.rm = TRUE),
    n_children = n(),
    .groups = "drop"
  ) %>%
  mutate(has_both = has_low & has_high)

panel <- panel %>%
  left_join(hh_structure_origin %>% select(hh_origin, has_both),
            by = "hh_origin")

##############################
# Analysis samples
##############################

# Diagnostic sample 1: full two-wave child-in-2007 cohort.
sample_full <- panel %>%
  filter(
    child_in_2007 == 1,
    cohort_group %in% c("Low Exposure (1988-1992)", "High Exposure (1994-1997)"),
    !is.na(BOS_years),
    !is.na(years_educ),
    year %in% c(2007, 2014)
  )

# Diagnostic sample 2: households with both lower- and higher-exposure cohorts.
sample_within_hh <- panel %>%
  filter(
    child_in_2007 == 1,
    has_both == TRUE,
    cohort_group %in% c("Low Exposure (1988-1992)", "High Exposure (1994-1997)"),
    !is.na(BOS_years),
    !is.na(years_educ),
    year %in% c(2007, 2014)
  )

# Diagnostic sample 3: children observed in both 2007 and 2014.
sample_balanced <- sample_full %>%
  group_by(pidlink) %>%
  filter(n_distinct(year) == 2) %>%
  ungroup()

##############################
# Estimation datasets
##############################

# Main estimation data used by the controls, balance, and regression scripts.
est <- panel %>%
  filter(
    child_in_2007 == 1,
    cohort_group %in% c("Low Exposure (1988-1992)", "High Exposure (1994-1997)"),
    year %in% c(2007, 2014),
    !is.na(years_educ),
    !is.na(BOS_years),
    !is.na(hh_origin)
  )

# "Both cohorts" is defined from the 2007 roster, consistent with has_both above.
hh_both_est <- est %>%
  filter(year == 2007) %>%
  group_by(hh_origin) %>%
  summarise(
    has_low  = any(low_exposure == 1),
    has_high = any(high_exposure == 1),
    .groups = "drop"
  ) %>%
  filter(has_low & has_high) %>%
  select(hh_origin)

est_within <- est %>%
  semi_join(hh_both_est, by = "hh_origin")

est_within_balanced <- est_within %>%
  group_by(pidlink) %>%
  filter(n_distinct(year) == 2) %>%
  ungroup()

##############################
# Save processed data
##############################

message("Saving data for regression analysis")

# Create the output directory if it does not exist.
if (!dir.exists("Data/Processed")) {
  dir.create("Data/Processed", recursive = TRUE)
}

# Save only the datasets used by downstream scripts.
saveRDS(panel, "Data/Processed/panel_full.rds")
saveRDS(est_within_balanced, "Data/Processed/est_within_balanced.rds")

message("Saved Data/Processed/panel_full.rds")
message("Saved Data/Processed/est_within_balanced.rds")
message("Next: Run 01b_Controls.R")
