##############################################################
# Project: Master's thesis - Poster assets                   #
# Author: Gregy                                              #
# Date: April 2026                                           #
##############################################################

suppressPackageStartupMessages({
  library(pacman)
  pacman::p_load(
    haven, dplyr, fixest, ggplot2, sf,
    rnaturalearth, rnaturalearthhires, ggrepel,
    patchwork, stringr
  )
})

source("Code/00_Project_Setup.R")

poster_asset_dir <- 'Output/Poster/assets'
draft_dir <- 'Paper/source'

dir.create(poster_asset_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(draft_dir, recursive = TRUE, showWarnings = FALSE)

##############################
# Province origin map
##############################

est <- readRDS('Data/Processed/est_with_controls.rds') %>%
  mutate(across(where(haven::is.labelled), haven::zap_labels))

# The poster map shows where the 2014 origin-household sample comes from.
prov_counts <- est %>%
  filter(year == 2014, !is.na(province), !is.na(hh_origin)) %>%
  distinct(hh_origin, province) %>%
  count(province, sort = TRUE)

code_map <- tibble::tribble(
  ~province, ~province_name,
  12, 'North Sumatra',
  13, 'West Sumatra',
  14, 'Riau',
  16, 'South Sumatra',
  18, 'Lampung',
  19, 'Bangka Belitung',
  31, 'Jakarta Raya',
  32, 'West Java',
  33, 'Central Java',
  34, 'Yogyakarta',
  35, 'East Java',
  36, 'Banten',
  51, 'Bali',
  52, 'West Nusa Tenggara',
  63, 'South Kalimantan',
  64, 'East Kalimantan',
  73, 'South Sulawesi',
  76, 'West Sulawesi'
)

prov_counts <- prov_counts %>% left_join(code_map, by = 'province')

# Natural Earth gives the province polygons used for the Indonesia map.
indo <- ne_states(country = 'indonesia', returnclass = 'sf') %>%
  st_transform(4326)

name_crosswalk <- tibble::tribble(
  ~province_name, ~ne_name,
  'North Sumatra', 'Sumatera Utara',
  'West Sumatra', 'Sumatera Barat',
  'Riau', 'Riau',
  'South Sumatra', 'Sumatera Selatan',
  'Lampung', 'Lampung',
  'Bangka Belitung', 'Bangka-Belitung',
  'Jakarta Raya', 'Jakarta Raya',
  'West Java', 'Jawa Barat',
  'Central Java', 'Jawa Tengah',
  'Yogyakarta', 'Yogyakarta',
  'East Java', 'Jawa Timur',
  'Banten', 'Banten',
  'Bali', 'Bali',
  'West Nusa Tenggara', 'Nusa Tenggara Barat',
  'South Kalimantan', 'Kalimantan Selatan',
  'East Kalimantan', 'Kalimantan Timur',
  'South Sulawesi', 'Sulawesi Selatan',
  'West Sulawesi', 'Sulawesi Barat'
)

# IFLS province names need a crosswalk before joining to the map polygons.
sample_map <- indo %>%
  left_join(name_crosswalk, by = c('name' = 'ne_name')) %>%
  left_join(prov_counts, by = 'province_name') %>%
  mutate(in_sample = !is.na(n), n = if_else(is.na(n), 0L, n))

# Only the largest sample-origin provinces are labelled to avoid clutter.
centroids <- st_point_on_surface(sample_map %>% filter(in_sample)) %>%
  cbind(st_coordinates(.)) %>%
  st_drop_geometry() %>%
  mutate(label = if_else(n >= 55, province_name, NA_character_))

# Darker provinces contribute more origin households to the analytic sample.
map_plot <- ggplot() +
  geom_sf(data = sample_map, aes(fill = n), color = 'white', linewidth = 0.32) +
  scale_fill_gradient(
    low = '#edd9d4',
    high = '#800000',
    name = 'Origin\nhouseholds'
  ) +
  geom_text_repel(
    data = centroids %>% filter(!is.na(label)),
    aes(X, Y, label = label),
    family = 'Arial',
    size = 3.2,
    color = '#333333',
    box.padding = 0.22,
    point.padding = 0.1,
    min.segment.length = 0,
    seed = 42,
    segment.color = '#666666',
    segment.size = 0.22
  ) +
  coord_sf(xlim = c(94, 142), ylim = c(-11, 7)) +
  theme_void(base_family = 'Arial') +
  theme(
    legend.position = c(0.90, 0.80),
    legend.title = element_text(size = 10.5, face = 'bold'),
    legend.text = element_text(size = 9.5),
    plot.background = element_rect(fill = 'white', color = NA),
    panel.background = element_rect(fill = 'white', color = NA)
  )

ggsave(
  file.path(poster_asset_dir, 'indonesia_sample_origin_province_map_r.png'),
  map_plot,
  width = 10.5, height = 6.4, dpi = 320, bg = 'white'
)

# The draft folder needs the same map because the LaTeX file reads from there.
invisible(file.copy(
  file.path(poster_asset_dir, 'indonesia_sample_origin_province_map_r.png'),
  file.path(draft_dir, 'indonesia_sample_origin_province_map_r.png'),
  overwrite = TRUE
))

##############################
# HTE data preparation
##############################

# The poster HTE figures use the same 2014 HS-attainment outcome as the thesis.
est_hs <- est %>%
  mutate(
    years_educ = as.numeric(years_educ),
    attain_hs = if_else(!is.na(years_educ), as.integer(years_educ >= 10), NA_integer_),
    female = as.integer(sex == 3)
  )

# Rebuilding the intensity proxy keeps the poster assets reproducible from this script alone.
school_2007 <- read_dta('Data/Raw/More on IFLS/cf07_all_dta/schl.dta') %>%
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
    geo_id = if_else(!is.na(province), paste0('P', as.integer(province)), NA_character_)
  ) %>%
  filter(!is.na(geo_id))

intensity_2007 <- school_2007 %>%
  group_by(geo_id) %>%
  summarize(share_local_bop_pos_2007 = mean(local_bop_pos, na.rm = TRUE), .groups = 'drop')

threshold_2007 <- median(intensity_2007$share_local_bop_pos_2007, na.rm = TRUE)

##############################
# BOS intensity map
##############################

# This map shows the province-level BOS implementation proxy used in the thesis.
intensity_map <- intensity_2007 %>%
  mutate(province = as.integer(stringr::str_remove(geo_id, '^P'))) %>%
  left_join(code_map, by = 'province') %>%
  left_join(name_crosswalk, by = 'province_name')

bos_intensity_map <- indo %>%
  left_join(intensity_map, by = c('name' = 'ne_name')) %>%
  mutate(
    in_ifls_intensity = !is.na(share_local_bop_pos_2007),
    high_intensity_2007 = if_else(
      !is.na(share_local_bop_pos_2007),
      share_local_bop_pos_2007 > threshold_2007,
      NA
    )
  )

intensity_centroids <- st_point_on_surface(bos_intensity_map %>% filter(in_ifls_intensity)) %>%
  cbind(st_coordinates(.)) %>%
  st_drop_geometry() %>%
  mutate(label = if_else(!is.na(province_name), province_name, NA_character_))

intensity_plot <- ggplot() +
  geom_sf(
    data = bos_intensity_map,
    aes(fill = share_local_bop_pos_2007),
    color = 'white',
    linewidth = 0.32
  ) +
  scale_fill_gradient(
    low = '#edf3f7',
    high = '#305a9b',
    na.value = '#f2eee9',
    labels = scales::label_percent(accuracy = 1),
    name = 'Schools reporting\npositive BOS-related\nfunding'
  ) +
  geom_text_repel(
    data = intensity_centroids %>% filter(!is.na(label)),
    aes(X, Y, label = label),
    family = 'Arial',
    size = 3.0,
    color = '#333333',
    box.padding = 0.20,
    point.padding = 0.08,
    min.segment.length = 0,
    seed = 42,
    segment.color = '#666666',
    segment.size = 0.20
  ) +
  coord_sf(xlim = c(94, 142), ylim = c(-11, 7)) +
  theme_void(base_family = 'Arial') +
  theme(
    legend.position = c(0.88, 0.80),
    legend.title = element_text(size = 9.8, face = 'bold'),
    legend.text = element_text(size = 8.8),
    plot.background = element_rect(fill = 'white', color = NA),
    panel.background = element_rect(fill = 'white', color = NA)
  )

ggsave(
  file.path(poster_asset_dir, 'indonesia_bos_intensity_2007_map_r.png'),
  intensity_plot,
  width = 10.5, height = 6.4, dpi = 320, bg = 'white'
)

invisible(file.copy(
  file.path(poster_asset_dir, 'indonesia_bos_intensity_2007_map_r.png'),
  file.path(draft_dir, 'indonesia_bos_intensity_2007_map_r.png'),
  overwrite = TRUE
))

hh_loc_2014 <- read_dta('Data/Raw/14_bk_sc1.dta') %>%
  transmute(true_hh = hhid14, province_loc = as.numeric(sc01_14_14)) %>%
  distinct(true_hh, .keep_all = TRUE)

# Child records are merged to province intensity before estimating subgroup slopes.
est_2014_hs <- est_hs %>%
  filter(year == 2014) %>%
  left_join(hh_loc_2014, by = 'true_hh') %>%
  mutate(
    province_merge = dplyr::coalesce(as.numeric(province), province_loc),
    geo_id = if_else(!is.na(province_merge), paste0('P', as.integer(province_merge)), NA_character_)
  ) %>%
  left_join(intensity_2007, by = 'geo_id') %>%
  mutate(
    BOS_years = as.numeric(BOS_years),
    high_intensity_2007 = as.integer(share_local_bop_pos_2007 > threshold_2007),
    parent_hs = if_else(!is.na(max_parent_educ), as.integer(max_parent_educ >= 10), NA_integer_)
  ) %>%
  filter(!is.na(attain_hs), !is.na(high_intensity_2007))

##############################
# HTE models
##############################

# Gender HTE model.
female_hte <- feols(
  attain_hs ~ BOS_years + BOS_years:high_intensity_2007 + female +
    BOS_years:female + BOS_years:high_intensity_2007:female | hh_origin,
  data = est_2014_hs,
  cluster = ~hh_origin
)

# Parent-education HTE model.
parent_hte <- feols(
  attain_hs ~ BOS_years + BOS_years:high_intensity_2007 +
    BOS_years:parent_hs + BOS_years:high_intensity_2007:parent_hs + female | hh_origin,
  data = est_2014_hs %>% filter(!is.na(parent_hs)),
  cluster = ~hh_origin
)

cf <- coef(female_hte)
vc <- vcov(female_hte)
b_names <- names(cf)

##############################
# Gender HTE figure
##############################

# This helper calculates the subgroup slopes shown in the gender plot.
make_L <- function(weights) {
  L <- rep(0, length(cf))
  names(L) <- b_names
  for (nm in names(weights)) {
    if (nm %in% b_names) L[nm] <- weights[[nm]]
  }
  L
}

Lb <- make_L(c('BOS_years:high_intensity_2007' = 1))
Lg <- make_L(c('BOS_years:high_intensity_2007' = 1,
               'BOS_years:high_intensity_2007:female' = 1))

# Poster figure for estimated BOS effects by child gender.
plot_df <- tibble::tibble(
  group = c('Boys', 'Girls'),
  estimate = c(sum(Lb * cf), sum(Lg * cf)),
  se = c(
    sqrt(as.numeric(t(Lb) %*% vc %*% Lb)),
    sqrt(as.numeric(t(Lg) %*% vc %*% Lg))
  )
) %>%
  mutate(
    lo = estimate - 1.96 * se,
    hi = estimate + 1.96 * se,
    label = sprintf('%.3f', estimate)
  )

gender_plot <- ggplot(plot_df, aes(x = group, y = estimate, color = group)) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = '#666666', linetype = 'dashed') +
  geom_linerange(aes(ymin = lo, ymax = hi), linewidth = 1.5, show.legend = FALSE) +
  geom_point(size = 6, show.legend = FALSE) +
  geom_text(aes(label = label), nudge_y = 0.0014, size = 5.2, color = '#333333', fontface = 'bold', show.legend = FALSE) +
  scale_color_manual(values = c('Boys' = '#1D8A84', 'Girls' = '#800000')) +
  labs(
    title = 'Estimated BOS effect by child gender',
    subtitle = 'Preferred BOS-years × high-intensity specification with 95% confidence intervals',
    x = NULL,
    y = 'Effect on probability of senior-high attainment'
  ) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1), expand = expansion(mult = c(0.08, 0.12))) +
  theme_minimal(base_family = 'Arial') +
  theme(
    plot.title = element_text(size = 19, face = 'bold', color = '#800000'),
    plot.subtitle = element_text(size = 11.5, color = '#555555'),
    axis.title.y = element_text(size = 12.5, face = 'bold'),
    axis.text = element_text(size = 12.5, face = 'bold'),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = '#E5DDD8'),
    plot.margin = margin(10, 15, 10, 10)
  )

ggsave(
  'Output/Poster/assets/gender_hte_poster.png',
  gender_plot,
  width = 8.2, height = 5.5, dpi = 320, bg = 'white'
)

##############################
# Combined HTE figure
##############################

# This helper calculates subgroup slopes for the combined HTE figure.
lincom_effect <- function(model, weights) {
  b <- coef(model)
  V <- vcov(model)
  L <- rep(0, length(b))
  names(L) <- names(b)
  for (nm in names(weights)) {
    if (nm %in% names(b)) L[nm] <- weights[[nm]]
  }
  est <- sum(L * b)
  se <- sqrt(as.numeric(t(L) %*% V %*% L))
  tibble::tibble(
    estimate = est,
    se = se,
    lo = est - 1.96 * se,
    hi = est + 1.96 * se
  )
}

# Child gender appears on the right side of the combined HTE figure.
gender_combined <- bind_rows(
  lincom_effect(
    female_hte,
    c('BOS_years:high_intensity_2007' = 1)
  ) %>% mutate(group = 'Boys', panel = 'Child gender'),
  lincom_effect(
    female_hte,
    c(
      'BOS_years:high_intensity_2007' = 1,
      'BOS_years:high_intensity_2007:female' = 1
    )
  ) %>% mutate(group = 'Girls', panel = 'Child gender')
)

# Parents' education appears on the left side of the combined HTE figure.
parent_combined <- bind_rows(
  lincom_effect(
    parent_hte,
    c('BOS_years:high_intensity_2007' = 1)
  ) %>% mutate(group = 'Lower-parent\neducation', panel = "Parents' education"),
  lincom_effect(
    parent_hte,
    c(
      'BOS_years:high_intensity_2007' = 1,
      'BOS_years:high_intensity_2007:parent_hs' = 1
    )
  ) %>% mutate(group = 'Higher-parent\neducation', panel = "Parents' education")
)

# Both HTE dimensions are combined into one poster-ready figure.
combined_plot_df <- bind_rows(parent_combined, gender_combined) %>%
  mutate(
    panel = factor(panel, levels = c("Parents' education", "Child gender")),
    group = factor(
      group,
      levels = c('Higher-parent\neducation', 'Lower-parent\neducation', 'Girls', 'Boys')
    ),
    color_key = case_when(
      group == 'Boys' ~ 'boys',
      group == 'Girls' ~ 'girls',
      group == 'Lower-parent\neducation' ~ 'low_parent',
      TRUE ~ 'high_parent'
    )
  )

heterogeneity_plot <- ggplot(combined_plot_df, aes(x = estimate, y = group, color = color_key)) +
  geom_vline(xintercept = 0, linewidth = 0.5, color = '#666666') +
  geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 1.5, show.legend = FALSE) +
  geom_point(size = 5, show.legend = FALSE) +
  facet_wrap(~panel, ncol = 2, scales = 'free_y') +
  scale_color_manual(
    values = c(
      boys = '#4B74B7',
      girls = '#D24D57',
      low_parent = '#5B6770',
      high_parent = '#C4822E'
    )
  ) +
  labs(
    x = 'Estimated BOS years × high-intensity effect',
    y = NULL
  ) +
  theme_minimal(base_family = 'Arial') +
  theme(
    strip.text = element_text(size = 15, face = 'bold'),
    axis.title.x = element_text(size = 12.5, face = 'bold'),
    axis.text = element_text(size = 12.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = '#E5DDD8'),
    panel.grid.major.x = element_line(color = '#F0ECE8'),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(
  'Output/Poster/assets/heterogeneity_combined_poster.png',
  heterogeneity_plot,
  width = 10.5, height = 4.8, dpi = 320, bg = 'white'
)

##############################
# Contribution and implications visual
##############################

# Visual block 1 shows how the study changes the unit of comparison.
compare_plot <- ggplot() +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 6), clip = 'off') +
  annotate('rect', xmin = 0, xmax = 10, ymin = 5.1, ymax = 6, fill = '#800000', color = '#800000') +
  annotate('text', x = 5, y = 5.55, label = 'WHAT THIS STUDY CHANGES', family = 'Arial',
           fontface = 'bold', color = 'white', size = 7) +
  annotate('rect', xmin = 0.5, xmax = 3.8, ymin = 1.4, ymax = 4.5, fill = '#F8ECE8', color = '#D7D1C8', linewidth = 0.4) +
  annotate('rect', xmin = 6.2, xmax = 9.5, ymin = 1.4, ymax = 4.5, fill = '#EEF5F8', color = '#D7D1C8', linewidth = 0.4) +
  annotate('segment', x = 4.35, xend = 5.75, y = 2.95, yend = 2.95, linewidth = 1.2, color = '#C4822E',
           arrow = grid::arrow(length = grid::unit(0.22, 'inches'), type = 'closed')) +
  annotate('text', x = 2.15, y = 4.08, label = 'Most BOS studies compare', family = 'Arial',
           fontface = 'bold', color = '#800000', size = 5.6) +
  annotate('text', x = 7.85, y = 4.08, label = 'This study compares', family = 'Arial',
           fontface = 'bold', color = '#800000', size = 5.6) +
  annotate('text', x = 2.15, y = 2.75, label = 'Schools\nDistricts\nHouseholds', family = 'Arial',
           color = '#222222', size = 6.2, lineheight = 1.3) +
  annotate('text', x = 7.85, y = 2.75,
           label = 'Siblings within the same family\nDifferent cumulative BOS exposure\nShared household background',
           family = 'Arial', color = '#222222', size = 5.4, lineheight = 1.2) +
  annotate('rect', xmin = 2.55, xmax = 7.45, ymin = 0.12, ymax = 1.0,
           fill = '#FFF6E6', color = '#B7A28C', linewidth = 0.35) +
  annotate('text', x = 5, y = 0.56,
           label = stringr::str_wrap('This shifts BOS evaluation to the within-family margin.', width = 48),
           family = 'Arial', fontface = 'bold', size = 4.9, color = '#222222') +
  theme_void()

# Visual block 2 summarizes the policy implications.
policy_plot <- ggplot() +
  coord_cartesian(xlim = c(0, 12), ylim = c(0, 4.2), clip = 'off') +
  annotate('rect', xmin = 0, xmax = 12, ymin = 3.3, ymax = 4.2, fill = '#800000', color = '#800000') +
  annotate('text', x = 6, y = 3.75, label = 'POLICY IMPLICATIONS', family = 'Arial',
           fontface = 'bold', color = 'white', size = 7) +
  annotate('rect', xmin = 0.25, xmax = 3.85, ymin = 0.2, ymax = 2.85, fill = '#FFF8EA', color = '#D7D1C8', linewidth = 0.4) +
  annotate('rect', xmin = 4.2, xmax = 7.8, ymin = 0.2, ymax = 2.85, fill = '#F8ECE8', color = '#D7D1C8', linewidth = 0.4) +
  annotate('rect', xmin = 8.15, xmax = 11.75, ymin = 0.2, ymax = 2.85, fill = '#EEF5F8', color = '#D7D1C8', linewidth = 0.4) +
  annotate('text', x = 2.05, y = 2.35, label = 'Clearest margin', family = 'Arial',
           fontface = 'bold', color = '#800000', size = 5.3) +
  annotate('text', x = 6.0, y = 2.35, label = 'Why gains can differ', family = 'Arial',
           fontface = 'bold', color = '#800000', size = 5.3) +
  annotate('text', x = 9.95, y = 2.35, label = 'What better data would do', family = 'Arial',
           fontface = 'bold', color = '#800000', size = 5.1) +
  annotate('text', x = 2.05, y = 1.3, label = stringr::str_wrap('Senior-high progression', width = 18),
           family = 'Arial', fontface = 'bold', color = '#222222', size = 6.1, lineheight = 1.15) +
  annotate('text', x = 6.0, y = 1.28,
           label = stringr::str_wrap('Exposure and local intensity vary across siblings and provinces', width = 23),
           family = 'Arial', color = '#222222', size = 5.1, lineheight = 1.15) +
  annotate('text', x = 9.95, y = 1.28,
           label = stringr::str_wrap('Administrative allocation records would sharpen implementation measurement', width = 24),
           family = 'Arial', color = '#222222', size = 5.0, lineheight = 1.15) +
  theme_void()

# Visual block 3 summarizes the next steps.
next_plot <- ggplot() +
  coord_cartesian(xlim = c(0, 12), ylim = c(0, 4.1), clip = 'off') +
  annotate('rect', xmin = 0, xmax = 12, ymin = 3.2, ymax = 4.1, fill = '#800000', color = '#800000') +
  annotate('text', x = 6, y = 3.65, label = 'NEXT STEPS', family = 'Arial',
           fontface = 'bold', color = 'white', size = 7) +
  annotate('segment', x = 3.25, xend = 4.5, y = 1.72, yend = 1.72, linewidth = 0.9, color = '#B7A28C',
           arrow = grid::arrow(length = grid::unit(0.14, 'inches'), type = 'closed')) +
  annotate('segment', x = 7.25, xend = 8.5, y = 1.72, yend = 1.72, linewidth = 0.9, color = '#B7A28C',
           arrow = grid::arrow(length = grid::unit(0.14, 'inches'), type = 'closed')) +
  annotate('rect', xmin = 0.6, xmax = 3.1, ymin = 0.55, ymax = 2.85, fill = '#EEF5F8', color = '#D7D1C8', linewidth = 0.4) +
  annotate('rect', xmin = 4.75, xmax = 7.25, ymin = 0.55, ymax = 2.85, fill = '#FFF8EA', color = '#D7D1C8', linewidth = 0.4) +
  annotate('rect', xmin = 8.9, xmax = 11.4, ymin = 0.55, ymax = 2.85, fill = '#F8ECE8', color = '#D7D1C8', linewidth = 0.4) +
  annotate('text', x = c(1.0, 5.15, 9.3), y = 2.52, label = c('1', '2', '3'),
           family = 'Arial', fontface = 'bold', size = 5.6, color = '#800000') +
  annotate('text', x = c(1.85, 6.0, 10.15), y = 2.05,
           label = c('Sharpen\nmeasurement', 'Follow outcomes\nlonger', 'Study household\nallocation'),
           family = 'Arial', fontface = 'bold', size = 4.95, color = '#222222', lineheight = 1.05) +
  annotate('text', x = c(1.85, 6.0, 10.15), y = 1.0,
           label = c(
             stringr::str_wrap('Administrative BOS funding records', width = 16),
             stringr::str_wrap('Further education and labor-market outcomes', width = 18),
             stringr::str_wrap('Which siblings benefit most when costs fall?', width = 17)
           ),
           family = 'Arial', size = 4.55, color = '#555555', lineheight = 1.1) +
  theme_void()

contrib_visual <- compare_plot / policy_plot / next_plot +
  patchwork::plot_layout(heights = c(1.15, 1.0, 0.95))

ggsave(
  'Output/Poster/assets/contribution_implications_visual.png',
  contrib_visual,
  width = 11.2, height = 10.8, dpi = 320, bg = 'white'
)

message('Saved Output/Poster/assets/indonesia_sample_origin_province_map_r.png')
message('Saved draft copy of indonesia_sample_origin_province_map_r.png')
message('Saved Output/Poster/assets/indonesia_bos_intensity_2007_map_r.png')
message('Saved draft copy of indonesia_bos_intensity_2007_map_r.png')
message('Saved Output/Poster/assets/gender_hte_poster.png')
message('Saved Output/Poster/assets/heterogeneity_combined_poster.png')
message('Saved Output/Poster/assets/contribution_implications_visual.png')
