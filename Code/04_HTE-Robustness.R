##############################################################
# Project: Master's thesis - HTE and robustness tables       #
# Author: Gregy                                              #
# Date: April 2026                                           #
# This script is to show the heterogeneous treatment effect  #
# and robustness/sensitivity analysis for my master's thesis #
##############################################################

suppressPackageStartupMessages({
  library(pacman)
  pacman::p_load(haven, dplyr, fixest, modelsummary, kableExtra, ggplot2)
})

source("Code/00_Project_Setup.R")

output_tables_dir <- "Output/Tables"
output_figures_dir <- "Output/Figures"
draft_dir <- "Paper/source"

dir.create(output_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(draft_dir, recursive = TRUE, showWarnings = FALSE)

##############################
# Helper creation
##############################

# This helper handles the HTE table layout so the model definitions stay readable later.
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
  latex_txt <- gsub("\\}\\{l(@?\\{\\})?cc\\}", "}{@{}lcc@{}}", latex_txt)

  if (!is.null(notes) && length(notes) > 0) {
    note_text <- paste(notes, collapse = " ")
    note_block <- paste0(
      "\\\\vspace{0.5em}\n",
      "\\\\parbox{0.95\\\\linewidth}{\\\\footnotesize \\\\textit{Notes:} ",
      note_text,
      "}"
    )
    latex_txt <- sub(
      "\\\\begin\\{tablenotes\\}\\[para\\][\\s\\S]*?\\\\end\\{tablenotes\\}",
      note_block,
      latex_txt,
      perl = TRUE
    )
  }

  # The same table needs to exist in both the canonical output folder and the draft folder.
  writeLines(latex_txt, filepaths[1])
  if (length(filepaths) > 1) {
    writeLines(latex_txt, filepaths[2])
  }
}

# This helper is for the robustness tables, which need a more manual layout.
write_robustness_table <- function(models,
                                   filepaths,
                                   caption,
                                   label,
                                   outcome_note,
                                   cohort_note = "Column (3) restricts the sample to birth cohorts 1988--1995 to reduce concern that the youngest cohorts had insufficient time to complete senior high school by 2014.") {
  extract_cell <- function(model, term) {
    ct <- as.data.frame(coeftable(model))
    ct$term <- rownames(ct)
    idx <- match(term, ct$term)
    if (is.na(idx)) {
      return(c("", ""))
    }

    estimate <- ct$Estimate[idx]
    stderr <- ct$`Std. Error`[idx]
    p_value <- ct$`Pr(>|t|)`[idx]
    stars <- if (is.na(p_value)) {
      ""
    } else if (p_value < 0.01) {
      "***"
    } else if (p_value < 0.05) {
      "**"
    } else if (p_value < 0.1) {
      "*"
    } else {
      ""
    }

    c(sprintf("%.3f%s", estimate, stars), sprintf("(%.3f)", stderr))
  }

  # I read the same selected coefficients from each robustness model.
  model_cols <- names(models)
  baseline_term <- "BOS_years"
  female_term <- "female"

  interaction_terms <- c(
    "BOS_years:high_intensity_2007",
    "BOS_years:high_intensity_top_tercile",
    "high_intensity_2007:BOS_years",
    "high_intensity_top_tercile:BOS_years"
  )

  get_interaction <- function(model) {
    ct_terms <- rownames(coeftable(model))
    interaction_terms[interaction_terms %in% ct_terms][1]
  }

  lines <- c(
    "\\begin{table}[H]",
    "\\centering",
    paste0("\\caption{", caption, "\\label{", label, "}}"),
    "\\footnotesize",
    "\\resizebox{\\linewidth}{!}{%",
    "\\begin{tabular}{@{}lcccc@{}}",
    "\\toprule",
    paste0("& ", paste(model_cols, collapse = " & "), " \\\\"),
    "\\midrule"
  )

  add_term_block <- function(label, extractor) {
    estimates <- c()
    ses <- c()
    for (nm in model_cols) {
      vals <- extractor(models[[nm]])
      estimates <- c(estimates, vals[1])
      ses <- c(ses, vals[2])
    }
    c(
      paste0(label, " & ", paste(estimates, collapse = " & "), " \\\\"),
      paste0("& ", paste(ses, collapse = " & "), " \\\\")
    )
  }

  lines <- c(
    lines,
    add_term_block("Exposure (BOS years)", function(model) extract_cell(model, baseline_term)),
    add_term_block("Exposure $\\times$ Higher-Intensity Indicator", function(model) {
      term <- get_interaction(model)
      if (is.na(term) || length(term) == 0) c("", "") else extract_cell(model, term)
    }),
    add_term_block("Female", function(model) extract_cell(model, female_term))
  )

  obs_row <- paste(
    sapply(models, function(model) format(nobs(model), big.mark = ",", scientific = FALSE)),
    collapse = " & "
  )

  lines <- c(
    lines,
    paste0("Observations & ", obs_row, " \\\\"),
    "Household FE & Yes & Yes & Yes & Yes \\\\",
    "Controls & Female only & Female only & Female only & Female only \\\\",
    "\\bottomrule",
    "\\end{tabular}",
    "}",
    "",
    "\\vspace{0.5em}",
    paste0(
      "\\parbox{0.95\\linewidth}{\\footnotesize Notes: All columns use the preferred 2014 household fixed-effects specification for ", outcome_note, ", ",
      "with standard errors clustered at the household level. Column (2) restricts the sample to birth cohorts 1990--1992 and 1994--1995, excluding 1993 and the outer cohorts. ",
      cohort_note, " ",
      "Columns (1)--(3) define high intensity using the province-level 2007 median split of the BOS intensity proxy. ",
      "Column (4) instead defines high intensity as the top tercile of the 2007 province intensity distribution. * $p<0.10$, ** $p<0.05$, *** $p<0.01$.}"
    ),
    "\\end{table}"
  )

  # Write the same table to the canonical output folder and the thesis draft folder.
  writeLines(lines, filepaths[1])
  if (length(filepaths) > 1) {
    writeLines(lines, filepaths[2])
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

# Rebuild the 2007 intensity proxy here so the HTE and robustness tables can run on their own.
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
top_tercile_threshold <- as.numeric(quantile(
  intensity_2007$share_local_bop_pos_2007,
  probs = 2 / 3,
  na.rm = TRUE,
  type = 7
))

# The HTE sample starts from the same 2014 child cross-section as the preferred model.
hh_loc_2014 <- read_dta("Data/Raw/14_bk_sc1.dta") %>%
  transmute(
    true_hh = hhid14,
    province_loc = as.numeric(sc01_14_14)
  ) %>%
  distinct(true_hh, .keep_all = TRUE)

est_2014_hs <- est %>%
  filter(year == 2014) %>%
  left_join(hh_loc_2014, by = "true_hh") %>%
  mutate(
    province_merge = dplyr::coalesce(as.numeric(province), province_loc),
    geo_id = if_else(!is.na(province_merge), paste0("P", as.integer(province_merge)), NA_character_)
  ) %>%
  left_join(intensity_2007, by = "geo_id") %>%
  mutate(
    years_educ = as.numeric(years_educ),
    attain_hs = if_else(!is.na(years_educ), as.integer(years_educ >= 10), NA_integer_),
    BOS_years = as.numeric(BOS_years),
    female = as.integer(sex == 3),
    high_intensity_2007 = as.integer(share_local_bop_pos_2007 > threshold_2007),
    high_intensity_top_tercile = as.integer(share_local_bop_pos_2007 >= top_tercile_threshold),
    parent_hs = if_else(!is.na(max_parent_educ), as.integer(max_parent_educ >= 10), NA_integer_),
    female_head = as.integer(female_head)
  ) %>%
  filter(!is.na(attain_hs), !is.na(share_local_bop_pos_2007))

##############################
# Heterogeneity analysis
##############################

# HTE model 1: child gender.
female_hte <- feols(
  attain_hs ~ BOS_years + BOS_years:high_intensity_2007 + female +
    BOS_years:female + BOS_years:high_intensity_2007:female | hh_origin,
  data = est_2014_hs,
  cluster = ~hh_origin
)

# HTE model 2: parental senior-high attainment.
parent_hte <- feols(
  attain_hs ~ BOS_years + BOS_years:high_intensity_2007 +
    BOS_years:parent_hs + BOS_years:high_intensity_2007:parent_hs + female | hh_origin,
  data = est_2014_hs %>% filter(!is.na(parent_hs)),
  cluster = ~hh_origin
)

# Exploratory appendix HTE: female-headed origin household.
female_head_hte <- feols(
  attain_hs ~ BOS_years + BOS_years:high_intensity_2007 +
    BOS_years:female_head + BOS_years:high_intensity_2007:female_head + female | hh_origin,
  data = est_2014_hs %>% filter(!is.na(female_head)),
  cluster = ~hh_origin
)

##############################
# HTE figure
##############################

# I use the same HTE regressions to plot subgroup-specific BOS-years x high-intensity slopes.
get_term_name <- function(model, possible_terms) {
  coef_names <- names(coef(model))
  found <- possible_terms[possible_terms %in% coef_names]
  if (length(found) == 0) {
    stop("None of these coefficient names were found: ", paste(possible_terms, collapse = ", "))
  }
  found[1]
}

get_marginal_effect <- function(model, base_terms, extra_terms = NULL) {
  coef_names <- names(coef(model))
  base_term <- get_term_name(model, base_terms)
  extra_term <- if (is.null(extra_terms)) NA_character_ else get_term_name(model, extra_terms)

  weights <- rep(0, length(coef_names))
  names(weights) <- coef_names
  weights[base_term] <- 1
  if (!is.na(extra_term)) {
    weights[extra_term] <- 1
  }

  estimate <- sum(weights * coef(model))
  vcov_matrix <- vcov(model)
  std_error <- as.numeric(sqrt(t(weights) %*% vcov_matrix %*% weights))

  data.frame(
    estimate = estimate,
    std_error = std_error,
    conf_low = estimate - 1.96 * std_error,
    conf_high = estimate + 1.96 * std_error
  )
}

base_intensity_terms <- c(
  "BOS_years:high_intensity_2007",
  "high_intensity_2007:BOS_years"
)

female_triple_terms <- c(
  "BOS_years:high_intensity_2007:female",
  "BOS_years:female:high_intensity_2007",
  "high_intensity_2007:BOS_years:female",
  "high_intensity_2007:female:BOS_years",
  "female:BOS_years:high_intensity_2007",
  "female:high_intensity_2007:BOS_years"
)

parent_triple_terms <- c(
  "BOS_years:high_intensity_2007:parent_hs",
  "BOS_years:parent_hs:high_intensity_2007",
  "high_intensity_2007:BOS_years:parent_hs",
  "high_intensity_2007:parent_hs:BOS_years",
  "parent_hs:BOS_years:high_intensity_2007",
  "parent_hs:high_intensity_2007:BOS_years"
)

gender_plot_df <- bind_rows(
  get_marginal_effect(female_hte, base_intensity_terms) %>%
    mutate(panel = "Child gender", group = "Boys", color_key = "Boys"),
  get_marginal_effect(female_hte, base_intensity_terms, female_triple_terms) %>%
    mutate(panel = "Child gender", group = "Girls", color_key = "Girls")
)

parent_plot_df <- bind_rows(
  get_marginal_effect(parent_hte, base_intensity_terms) %>%
    mutate(panel = "Parents' education", group = "Lower-parent\neducation", color_key = "Lower-parent education"),
  get_marginal_effect(parent_hte, base_intensity_terms, parent_triple_terms) %>%
    mutate(panel = "Parents' education", group = "Higher-parent\neducation", color_key = "Higher-parent education")
)

hte_plot_df <- bind_rows(parent_plot_df, gender_plot_df) %>%
  mutate(
    panel = factor(panel, levels = c("Parents' education", "Child gender")),
    group = factor(group, levels = c("Higher-parent\neducation", "Lower-parent\neducation", "Girls", "Boys"))
  )

hte_combined_plot <- ggplot(hte_plot_df, aes(x = estimate, y = group, color = color_key)) +
  geom_vline(xintercept = 0, color = "grey45", linewidth = 0.5) +
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high), orientation = "y", width = 0, linewidth = 0.9) +
  geom_point(size = 3.2) +
  facet_wrap(~panel, scales = "free_y", nrow = 1) +
  scale_color_manual(
    values = c(
      "Lower-parent education" = "#5d6a71",
      "Higher-parent education" = "#cc8528",
      "Boys" = "#4a78bd",
      "Girls" = "#d74d5f"
    )
  ) +
  scale_x_continuous(limits = c(-0.01, 0.03), breaks = c(-0.01, 0, 0.01, 0.02, 0.03)) +
  labs(
    x = "Estimated BOS years x high-intensity effect",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 13),
    axis.text = element_text(color = "grey25"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 8))
  )

ggsave(
  file.path(output_figures_dir, "heterogeneity_combined_thesis.png"),
  hte_combined_plot,
  width = 8.8,
  height = 4.2,
  dpi = 300
)

ggsave(
  file.path(draft_dir, "heterogeneity_combined_thesis.png"),
  hte_combined_plot,
  width = 8.8,
  height = 4.2,
  dpi = 300
)

##############################
# HTE table construction
##############################

# Both HTE models are shown in one table for easier comparison.
hte_models <- list(
  "(1) Female HTE" = female_hte,
  "(2) Parent HS HTE" = parent_hte
)

# Interaction order can differ internally, so I list both orderings for clean labels.
hte_coef_map <- c(
  "BOS_years:high_intensity_2007" = "BOS Years x High Intensity",
  "BOS_years:female" = "BOS Years x Female",
  "female:BOS_years" = "BOS Years x Female",
  "BOS_years:high_intensity_2007:female" = "BOS Years x High Intensity x Female",
  "high_intensity_2007:BOS_years:female" = "BOS Years x High Intensity x Female",
  "female:BOS_years:high_intensity_2007" = "BOS Years x High Intensity x Female",
  "BOS_years:parent_hs" = "BOS Years x Parent HS",
  "parent_hs:BOS_years" = "BOS Years x Parent HS",
  "BOS_years:high_intensity_2007:parent_hs" = "BOS Years x High Intensity x Parent HS",
  "high_intensity_2007:BOS_years:parent_hs" = "BOS Years x High Intensity x Parent HS",
  "parent_hs:BOS_years:high_intensity_2007" = "BOS Years x High Intensity x Parent HS",
  "female" = "Female"
)

# Extra rows make clear what each heterogeneity column tests.
hte_add_rows <- data.frame(
  term = c("Outcome", "Household FE", "Heterogeneity Terms", "SE Cluster"),
  "(1) Female HTE" = c("HS attainment", "Yes", "Female interactions", "Household"),
  "(2) Parent HS HTE" = c("HS attainment", "Yes", "Parent HS interactions", "Household"),
  check.names = FALSE
)

# Notes for the HTE table.
hte_notes <- list(
  "All columns use the preferred main specification: BOS years x binary high-intensity indicator, with household fixed effects.",
  "Parent HS = 1 if the maximum observed parental education is at least 10 completed years.",
  "The coefficient on the triple interaction reports whether the BOS-years x high-intensity effect differs for girls or for children from higher-parent-education households."
)

hte_paths <- c(
  file.path(output_tables_dir, "hte_main_hs.tex"),
  file.path(draft_dir, "hte_main_hs.tex")
)

# Export the HTE table.
write_clean_latex_table(
  models = hte_models,
  filepaths = hte_paths,
  title = "Heterogeneous Treatment Effects: HS Attainment",
  label = "tab:hte_main_hs",
  coef_map = hte_coef_map,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  add_rows = hte_add_rows,
  notes = hte_notes
)

##############################
# Appendix HTE table
##############################

female_head_hte_models <- list(
  "(1) Female-Headed HH HTE" = female_head_hte
)

female_head_coef_map <- c(
  "BOS_years:high_intensity_2007" = "BOS Years x High Intensity",
  "BOS_years:female_head" = "BOS Years x Female-Headed HH",
  "female_head:BOS_years" = "BOS Years x Female-Headed HH",
  "BOS_years:high_intensity_2007:female_head" = "BOS Years x High Intensity x Female-Headed HH",
  "high_intensity_2007:BOS_years:female_head" = "BOS Years x High Intensity x Female-Headed HH",
  "female_head:BOS_years:high_intensity_2007" = "BOS Years x High Intensity x Female-Headed HH",
  "female" = "Female"
)

female_head_add_rows <- data.frame(
  term = c("Outcome", "Household FE", "Heterogeneity Terms", "SE Cluster"),
  "(1) Female-Headed HH HTE" = c("HS attainment", "Yes", "Female-headed household interactions", "Household"),
  check.names = FALSE
)

female_head_notes <- list(
  "This appendix table reports an exploratory heterogeneity check using the preferred main specification.",
  "Female-headed household is measured at the origin-household level; its standalone effect is absorbed by household fixed effects.",
  "The triple interaction reports whether the BOS-years x high-intensity relationship differs for children from female-headed households."
)

female_head_paths <- c(
  file.path(output_tables_dir, "appendix_female_head_hte.tex"),
  file.path(draft_dir, "appendix_female_head_hte.tex")
)

write_clean_latex_table(
  models = female_head_hte_models,
  filepaths = female_head_paths,
  title = "Appendix: Heterogeneity by Female-Headed Household",
  label = "tab:appendix_female_head_hte",
  coef_map = female_head_coef_map,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  add_rows = female_head_add_rows,
  notes = female_head_notes
)

##############################
# Robustness checks
##############################

# Robustness checks for the preferred HS-attainment specification.
robustness_models <- list(
  "(1) Baseline" = feols(
    attain_hs ~ BOS_years + BOS_years:high_intensity_2007 + female | hh_origin,
    data = est_2014_hs,
    cluster = ~hh_origin
  ),
  "(2) Narrow Window" = feols(
    attain_hs ~ BOS_years + BOS_years:high_intensity_2007 + female | hh_origin,
    data = est_2014_hs %>%
      filter((birth_year >= 1990 & birth_year <= 1992) | (birth_year >= 1994 & birth_year <= 1995)),
    cluster = ~hh_origin
  ),
  "(3) Older Cohorts Only" = feols(
    attain_hs ~ BOS_years + BOS_years:high_intensity_2007 + female | hh_origin,
    data = est_2014_hs %>% filter(birth_year <= 1995),
    cluster = ~hh_origin
  ),
  "(4) Top-Tercile Intensity" = feols(
    attain_hs ~ BOS_years + BOS_years:high_intensity_top_tercile + female | hh_origin,
    data = est_2014_hs,
    cluster = ~hh_origin
  )
)

robustness_paths <- c(
  file.path(output_tables_dir, "robustness_sensitivity_hs.tex"),
  file.path(draft_dir, "robustness_sensitivity_hs.tex")
)

# Export the robustness table.
write_robustness_table(
  robustness_models,
  robustness_paths,
  caption = "Sensitivity Checks for Senior-High Attainment",
  label = "tab:robust_hs",
  outcome_note = "the senior-high attainment indicator"
)

# The same sensitivity checks for completed years of schooling are included in the appendix.
robustness_years_models <- list(
  "(1) Baseline" = feols(
    years_educ ~ BOS_years + BOS_years:high_intensity_2007 + female | hh_origin,
    data = est_2014_hs,
    cluster = ~hh_origin
  ),
  "(2) Narrow Window" = feols(
    years_educ ~ BOS_years + BOS_years:high_intensity_2007 + female | hh_origin,
    data = est_2014_hs %>%
      filter((birth_year >= 1990 & birth_year <= 1992) | (birth_year >= 1994 & birth_year <= 1995)),
    cluster = ~hh_origin
  ),
  "(3) Older Cohorts Only" = feols(
    years_educ ~ BOS_years + BOS_years:high_intensity_2007 + female | hh_origin,
    data = est_2014_hs %>% filter(birth_year <= 1995),
    cluster = ~hh_origin
  ),
  "(4) Top-Tercile Intensity" = feols(
    years_educ ~ BOS_years + BOS_years:high_intensity_top_tercile + female | hh_origin,
    data = est_2014_hs,
    cluster = ~hh_origin
  )
)

robustness_years_paths <- c(
  file.path(output_tables_dir, "appendix_robustness_sensitivity_years.tex"),
  file.path(draft_dir, "appendix_robustness_sensitivity_years.tex")
)

write_robustness_table(
  robustness_years_models,
  robustness_years_paths,
  caption = "Appendix: Sensitivity Checks for Completed Years of Schooling",
  label = "tab:appendix_robust_years",
  outcome_note = "completed years of schooling",
  cohort_note = "Column (3) restricts the sample to birth cohorts 1988--1995 to reduce concern that the youngest cohorts had less time to complete schooling by 2014."
)
