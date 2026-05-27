##############################################################
# Project: Master's thesis - Writing sample excerpt outputs  #
# Author: Gregy                                              #
# Date: April 2026                                           #
##############################################################

suppressPackageStartupMessages({
  library(pacman)
  pacman::p_load(haven, fixest, modelsummary, dplyr, kableExtra)
})

source("Code/00_Project_Setup.R")

if (!dir.exists("Output/Tables")) dir.create("Output/Tables", recursive = TRUE)
if (!dir.exists("Output/Drafts")) dir.create("Output/Drafts", recursive = TRUE)

##############################
# Helper creation
##############################

# This helper formats the compact table used in the writing sample.
write_clean_latex_table <- function(models,
                                    filepath,
                                    title = NULL,
                                    label = NULL,
                                    coef_map = NULL,
                                    gof_map = NULL,
                                    add_rows = NULL,
                                    notes = NULL,
                                    stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01)) {
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
  estimate_terms <- unique(tbl_df$term[tbl_df$part == "estimates"])
  for (term_i in estimate_terms) {
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
    ) %>%
    kableExtra::footnote(
      general = paste(notes, collapse = " "),
      general_title = "Notes: ",
      threeparttable = TRUE,
      footnote_as_chunk = TRUE,
      escape = FALSE
    )

  latex_txt <- as.character(latex_tbl)
  latex_txt <- gsub(
    "\n\\\\centering\n\\\\begin\\{threeparttable\\}",
    "\n\\\\begin{threeparttable}",
    latex_txt
  )
  latex_txt <- gsub(
    "\\\\begin\\{tabular\\}\\[t\\]\\{l",
    "\\\\footnotesize\n\\\\resizebox{\\\\linewidth}{!}{%\n\\\\begin{tabular}[t]{@{}l",
    latex_txt
  )
  latex_txt <- gsub(
    "\\}\\{l(@?\\{\\})?cc\\}",
    "}{@{}lcc@{}}",
    latex_txt
  )
  latex_txt <- gsub(
    "\\\\end\\{tabular\\}\n\\\\begin\\{tablenotes\\}",
    "\\\\end{tabular}\n}\n\\\\begin{tablenotes}",
    latex_txt
  )
  latex_txt <- gsub(
    "\\\\begin\\{tablenotes\\}\\[para\\]\n\\\\item \\\\textit\\{Notes: \\} \n\\\\item ",
    "\\\\begin{tablenotes}[flushleft]\n\\\\footnotesize\n\\\\item \\\\textit{Notes:} ",
    latex_txt
  )
  latex_txt <- gsub(
    "\\\\begin\\{table\\}\\[!h\\]",
    "\\\\begin{table}[!htbp]",
    latex_txt
  )

  writeLines(latex_txt, filepath)
}

##############################
# Data loading and merging
##############################

# The writing sample uses the same cleaned thesis data and HS-attainment outcome.
est <- readRDS("Data/Processed/est_with_controls.rds") %>%
  mutate(across(where(haven::is.labelled), haven::zap_labels)) %>%
  mutate(
    years_educ = as.numeric(years_educ),
    attain_hs = if_else(!is.na(years_educ), as.integer(years_educ >= 10), NA_integer_),
    female = as.integer(sex == 3)
  )

# The 2007 school file keeps the intensity measure predetermined relative to 2014 outcomes.
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
    local_bop_pos = as.numeric(!is.na(local_bop_amt) & local_bop_amt > 0),
    geo_id = if_else(!is.na(province), paste0("P", as.integer(province)), NA_character_)
  ) %>%
  filter(!is.na(geo_id))

intensity_2007 <- school_2007 %>%
  group_by(geo_id) %>%
  summarize(
    share_local_bop_pos_2007 = mean(local_bop_pos, na.rm = TRUE),
    .groups = "drop"
  )

threshold_2007 <- median(intensity_2007$share_local_bop_pos_2007, na.rm = TRUE)

hh_loc_2014 <- read_dta("Data/Raw/14_bk_sc1.dta") %>%
  transmute(
    true_hh = hhid14,
    province_loc = as.numeric(sc01_14_14)
  ) %>%
  distinct(true_hh, .keep_all = TRUE)

# Merge the 2014 child sample to province intensity for the writing-sample table.
est_2014_hs <- est %>%
  filter(year == 2014) %>%
  left_join(hh_loc_2014, by = "true_hh") %>%
  mutate(
    province_merge = dplyr::coalesce(as.numeric(province), province_loc),
    geo_id = if_else(!is.na(province_merge), paste0("P", as.integer(province_merge)), NA_character_)
  ) %>%
  left_join(intensity_2007, by = "geo_id") %>%
  filter(!is.na(share_local_bop_pos_2007)) %>%
  mutate(
    BOS_years = as.numeric(BOS_years),
    high_intensity_2007 = as.integer(share_local_bop_pos_2007 > threshold_2007)
  ) %>%
  filter(!is.na(attain_hs))

##############################
# Main analysis
##############################

# Column 1 keeps the simpler binary exposure x binary intensity specification.
m_ws_1 <- feols(
  attain_hs ~ treated + treated:high_intensity_2007 + female | hh_origin,
  data = est_2014_hs,
  cluster = ~hh_origin
)

# Column 2 uses the preferred BOS-years x binary intensity specification.
m_ws_2 <- feols(
  attain_hs ~ BOS_years + BOS_years:high_intensity_2007 + female | hh_origin,
  data = est_2014_hs,
  cluster = ~hh_origin
)

##############################
# Table construction
##############################

# These are the two columns shown in the writing sample.
ws_models <- list(
  "(1) Bin Exp x Bin Int" = m_ws_1,
  "(2) BOS Years x Bin Int" = m_ws_2
)

# Readable labels keep the table focused on interpretation, not variable names.
ws_coef_map <- c(
  "treated" = "More Exposure (younger cohort)",
  "treated:high_intensity_2007" = "More Exposure x High Intensity",
  "high_intensity_2007:treated" = "More Exposure x High Intensity",
  "BOS_years" = "Exposure (BOS years)",
  "BOS_years:high_intensity_2007" = "Exposure (BOS years) x High Intensity",
  "high_intensity_2007:BOS_years" = "Exposure (BOS years) x High Intensity",
  "female" = "Female"
)

# Extra rows document the sample, fixed effects, controls, and clustered SEs.
ws_add_rows <- data.frame(
  term = c("Control Mean (treated = 0)", "Household FE", "Controls", "SE Cluster"),
  "(1) Bin Exp x Bin Int" = c(
    sprintf("%.3f", mean(est_2014_hs$attain_hs[est_2014_hs$treated == 0], na.rm = TRUE)),
    "Yes", "Female only", "Household"
  ),
  "(2) BOS Years x Bin Int" = c(
    sprintf("%.3f", mean(est_2014_hs$attain_hs[est_2014_hs$treated == 0], na.rm = TRUE)),
    "Yes", "Female only", "Household"
  ),
  check.names = FALSE
)

# Notes shown under the writing-sample table.
ws_notes <- list(
  "Outcome = senior-high attainment (1 if completed years of schooling are at least 10).",
  "Exposure is defined as higher-exposure cohort membership or potential BOS-exposed years. High intensity equals 1 for provinces above the 2007 median of the BOS-funding proxy.",
  "All specifications include household fixed effects and a female control. Standard errors are clustered at the household level."
)

##############################
# Export outputs
##############################

# Compact regression table.
write_clean_latex_table(
  models = ws_models,
  filepath = "Output/Tables/writing_sample_main_hs.tex",
  title = "Main Result: BOS Exposure and Senior-High Attainment",
  label = "tab:writing_sample_main_hs",
  coef_map = ws_coef_map,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  add_rows = ws_add_rows,
  notes = ws_notes
)

# Plain-text backup of the writing-sample prose.
excerpt_text <- c(
  "Longer Exposure, Better Outcomes? Evidence from Indonesia's BOS Program",
  "",
  "Education is one of the most important channels through which individuals accumulate human capital and improve their long-run economic opportunities. A large literature, especially in developing-country settings, links schooling to higher earnings and better labor-market outcomes, which is why education policy remains a central part of development strategy (Psacharopoulos and Patrinos, 2018; Glewwe and Muralidharan, 2016). Yet the relationship between education policy and educational attainment is mediated by the family. Even when schooling is formally subsidized, households still face direct and indirect costs of keeping children in school, including transportation, learning materials, and the opportunity cost of children's time (Glewwe and Muralidharan, 2016; Duflo, Dupas, and Kremer, 2021). In this context, the key policy question is not only whether subsidies raise schooling on average, but also whether they change how educational opportunities are distributed within the household.",
  "",
  "This intra-household margin matters because parents often allocate scarce resources across children of different ages and schooling stages. Family-investment models emphasize that children's educational outcomes depend partly on parental resources and borrowing constraints (Becker and Tomes, 1979; Loury, 1981). A related empirical literature shows that siblings within the same family can experience different educational outcomes because of birth order, family size, and resource allocation (Black, Devereux, and Salvanes, 2005). In such a setting, a broad schooling subsidy may affect not only whether a household invests in education, but also which child benefits more when budgets remain tight.",
  "",
  "Indonesia provides a useful setting in which to study this question. The country has a long history of large-scale education interventions, and prior work has shown that public investment in schooling can substantially affect educational attainment and later-life outcomes. Most notably, Duflo (2001) shows that the INPRES primary school construction program increased years of education and wages for children exposed at primary-school age. Beginning in the mid-2000s, however, the policy focus shifted away from school construction and toward reducing the recurring cost of schooling. A major intervention in this period was the Bantuan Operasional Sekolah (BOS) program, introduced in 2005, which provides operational grants to schools and is intended to reduce the cost burden of basic education for households.",
  "",
  "Existing evidence suggests that BOS improved educational outcomes and relaxed household budget constraints. Studies using Indonesian data find positive effects on student performance, household educational investment, and continuation to senior secondary schooling, especially among poorer households (Sulistyaningrum, 2016; Sari and Tanaka, 2019; Kartasasmita and Sulistyaningrum, 2021). However, this literature largely evaluates BOS through comparisons across households, schools, or students. Much less is known about whether these gains are distributed differently across children within the same family. If the subsidy reduces schooling costs but does not eliminate them entirely, then younger and older siblings may benefit differently depending on the timing and length of their exposure.",
  "",
  "This paper asks a simple question: within the same family, do children with greater cumulative exposure to Indonesia's BOS program attain more schooling and have a higher probability of reaching senior high school than their siblings with lower exposure? To answer this, I use data from the Indonesia Family Life Survey (IFLS) and exploit cohort-based differences in BOS exposure across siblings within origin households. Children from younger cohorts were potentially exposed to BOS for more school-age years than their older siblings because of their age at the time of the program's 2005 rollout. I combine this within-household cohort variation with cross-province variation in a BOS-related school-funding intensity proxy constructed from the IFLS school-facility data, following the broader cohort-exposure by regional-intensity logic of Duflo (2001).",
  "",
  "The empirical design is a 2014 cross-sectional household fixed-effects model. Household fixed effects absorb all time-invariant characteristics shared by siblings within the same family, including persistent socioeconomic background, parental preferences, and other household-level factors that could confound comparisons across households (Angrist and Pischke, 2009). The main outcome is a senior-high attainment indicator, defined as completing at least 10 years of schooling. I focus on this outcome because it captures progression beyond basic education and is less mechanically tied to age than completed years of schooling. The analysis sample contains 2,112 children after merging the child-level data to the province-level intensity proxy; the household fixed-effects estimator drops a small number of singleton observations, leaving 2,088 observations in the main regression sample.",
  "",
  "I measure treatment along two dimensions. The first is BOS exposure, captured either by a binary indicator for belonging to the higher-exposure cohort or by a continuous measure equal to the number of potential BOS-exposed school years between ages 6 and 15. The second is regional BOS intensity, measured using the 2007 share of sampled schools in a province reporting positive BOS-related local operational funding. In the preferred specification, I interact the continuous exposure measure with a binary indicator for provinces above the median of the 2007 intensity distribution. This specification provides the clearest interpretation of how an additional year of potential BOS exposure varies with the local funding environment.",
  "",
  "Table 1 reports the main results for senior-high attainment. Across specifications, the interaction between BOS exposure and regional intensity is positive. In the preferred specification, one additional year of potential BOS exposure is associated with a 0.010 increase in the probability of senior-high attainment in high-intensity provinces. Relative to the control-group mean of 0.618, this corresponds to an increase of about 1.6 percent of the baseline attainment rate. The simpler binary-exposure specification points in the same direction, although the estimate is less precise.",
  "",
  "These results suggest a modest but positive relationship between greater BOS exposure and progression to senior high school in stronger local funding environments. At the same time, the estimates are not uniformly precise, and I therefore interpret them cautiously. The evidence is consistent with the view that BOS may have supported educational progression at the senior-high margin, but it is not strong enough to sustain a definitive causal claim across all specifications. Still, the analysis highlights an important point for policy evaluation: a broad subsidy may operate not only across households, but also within them, by shifting how educational opportunities are distributed among siblings."
)

writeLines(excerpt_text, "Output/Drafts/writing_sample_excerpt.txt")

message("Saved Output/Tables/writing_sample_main_hs.tex")
message("Saved Output/Drafts/writing_sample_excerpt.txt")
