##############################################################
# Project: Master's thesis - Manuscript maps                 #
# Author: Gregy Tuerah                                       #
# Date: April 2026                                           #
##############################################################

suppressPackageStartupMessages({
  library(pacman)
  pacman::p_load(
    haven, dplyr, ggplot2, sf, rnaturalearth,
    rnaturalearthhires, ggrepel, stringr, scales
  )
})

source("Code/00_Project_Setup.R")

output_figures_dir <- "Output/Figures"
paper_source_dir <- "Paper/source"

dir.create(output_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(paper_source_dir, recursive = TRUE, showWarnings = FALSE)

required_inputs <- c(
  "Data/Processed/est_with_controls.rds",
  "Data/Raw/More on IFLS/cf07_all_dta/schl.dta"
)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop(
    "Missing required input(s): ", paste(missing_inputs, collapse = ", "),
    ". Run the construction scripts first."
  )
}

save_manuscript_figure <- function(filename, plot) {
  ggsave(
    file.path(output_figures_dir, filename),
    plot,
    width = 10.5,
    height = 6.4,
    dpi = 320,
    bg = "white"
  )
  ggsave(
    file.path(paper_source_dir, filename),
    plot,
    width = 10.5,
    height = 6.4,
    dpi = 320,
    bg = "white"
  )
}

province_codes <- tibble::tribble(
  ~province, ~province_name,
  12, "North Sumatra",
  13, "West Sumatra",
  14, "Riau",
  16, "South Sumatra",
  18, "Lampung",
  19, "Bangka Belitung",
  31, "Jakarta Raya",
  32, "West Java",
  33, "Central Java",
  34, "Yogyakarta",
  35, "East Java",
  36, "Banten",
  51, "Bali",
  52, "West Nusa Tenggara",
  63, "South Kalimantan",
  64, "East Kalimantan",
  73, "South Sulawesi",
  76, "West Sulawesi"
)

name_crosswalk <- tibble::tribble(
  ~province_name, ~ne_name,
  "North Sumatra", "Sumatera Utara",
  "West Sumatra", "Sumatera Barat",
  "Riau", "Riau",
  "South Sumatra", "Sumatera Selatan",
  "Lampung", "Lampung",
  "Bangka Belitung", "Bangka-Belitung",
  "Jakarta Raya", "Jakarta Raya",
  "West Java", "Jawa Barat",
  "Central Java", "Jawa Tengah",
  "Yogyakarta", "Yogyakarta",
  "East Java", "Jawa Timur",
  "Banten", "Banten",
  "Bali", "Bali",
  "West Nusa Tenggara", "Nusa Tenggara Barat",
  "South Kalimantan", "Kalimantan Selatan",
  "East Kalimantan", "Kalimantan Timur",
  "South Sulawesi", "Sulawesi Selatan",
  "West Sulawesi", "Sulawesi Barat"
)

indonesia_states <- ne_states(country = "indonesia", returnclass = "sf") %>%
  st_transform(4326)

##############################
# Analytic sample origin map
##############################

est <- readRDS("Data/Processed/est_with_controls.rds") %>%
  mutate(across(where(haven::is.labelled), haven::zap_labels))

province_counts <- est %>%
  filter(year == 2014, !is.na(province), !is.na(hh_origin)) %>%
  distinct(hh_origin, province) %>%
  count(province, sort = TRUE) %>%
  left_join(province_codes, by = "province")

sample_map <- indonesia_states %>%
  left_join(name_crosswalk, by = c("name" = "ne_name")) %>%
  left_join(province_counts, by = "province_name") %>%
  mutate(in_sample = !is.na(n), n = if_else(is.na(n), 0L, n))

sample_centroids <- suppressWarnings(st_point_on_surface(sample_map %>% filter(in_sample))) %>%
  cbind(st_coordinates(.)) %>%
  st_drop_geometry() %>%
  mutate(label = if_else(n >= 55, province_name, NA_character_))

sample_plot <- ggplot() +
  geom_sf(data = sample_map, aes(fill = n), color = "white", linewidth = 0.32) +
  scale_fill_gradient(
    low = "#edd9d4",
    high = "#800000",
    name = "Origin\nhouseholds"
  ) +
  geom_text_repel(
    data = sample_centroids %>% filter(!is.na(label)),
    aes(X, Y, label = label),
    family = "Arial",
    size = 3.2,
    color = "#333333",
    box.padding = 0.22,
    point.padding = 0.1,
    min.segment.length = 0,
    seed = 42,
    segment.color = "#666666",
    segment.size = 0.22
  ) +
  coord_sf(xlim = c(94, 142), ylim = c(-11, 7)) +
  theme_void(base_family = "Arial") +
  theme(
    legend.position = c(0.90, 0.80),
    legend.title = element_text(size = 10.5, face = "bold"),
    legend.text = element_text(size = 9.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

save_manuscript_figure("indonesia_sample_origin_province_map_r.png", sample_plot)

##############################
# BOS implementation map
##############################

school_2007 <- read_dta("Data/Raw/More on IFLS/cf07_all_dta/schl.dta") %>%
  transmute(
    province = as.numeric(lk010707),
    local_bop_amt = as.numeric(b76d)
  ) %>%
  mutate(
    local_bop_amt = if_else(
      local_bop_amt %in% c(99997, 99998, 99999, 999999999997, 999999999998, 999999999999),
      NA_real_,
      local_bop_amt
    ),
    local_bop_pos = as.numeric(!is.na(local_bop_amt) & local_bop_amt > 0),
    geo_id = if_else(!is.na(province), paste0("P", as.integer(province)), NA_character_)
  ) %>%
  filter(!is.na(geo_id))

intensity_2007 <- school_2007 %>%
  group_by(geo_id) %>%
  summarize(share_local_bop_pos_2007 = mean(local_bop_pos, na.rm = TRUE), .groups = "drop")

intensity_map <- intensity_2007 %>%
  mutate(province = as.integer(stringr::str_remove(geo_id, "^P"))) %>%
  left_join(province_codes, by = "province") %>%
  left_join(name_crosswalk, by = "province_name")

bos_intensity_map <- indonesia_states %>%
  left_join(intensity_map, by = c("name" = "ne_name"))

intensity_centroids <- suppressWarnings(st_point_on_surface(
  bos_intensity_map %>% filter(!is.na(share_local_bop_pos_2007))
)) %>%
  cbind(st_coordinates(.)) %>%
  st_drop_geometry() %>%
  mutate(label = if_else(!is.na(province_name), province_name, NA_character_))

intensity_plot <- ggplot() +
  geom_sf(
    data = bos_intensity_map,
    aes(fill = share_local_bop_pos_2007),
    color = "white",
    linewidth = 0.32
  ) +
  scale_fill_gradient(
    low = "#edf3f7",
    high = "#305a9b",
    na.value = "#f2eee9",
    labels = scales::label_percent(accuracy = 1),
    name = "Schools reporting\npositive BOS-related\nfunding"
  ) +
  geom_text_repel(
    data = intensity_centroids %>% filter(!is.na(label)),
    aes(X, Y, label = label),
    family = "Arial",
    size = 3.0,
    color = "#333333",
    box.padding = 0.20,
    point.padding = 0.08,
    min.segment.length = 0,
    seed = 42,
    segment.color = "#666666",
    segment.size = 0.20
  ) +
  coord_sf(xlim = c(94, 142), ylim = c(-11, 7)) +
  theme_void(base_family = "Arial") +
  theme(
    legend.position = c(0.88, 0.80),
    legend.title = element_text(size = 9.8, face = "bold"),
    legend.text = element_text(size = 8.8),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

save_manuscript_figure("indonesia_bos_intensity_2007_map_r.png", intensity_plot)

message("Saved manuscript map figures to Output/Figures/ and Paper/source/.")
