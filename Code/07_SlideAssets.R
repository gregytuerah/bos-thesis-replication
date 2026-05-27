##############################################################
# Project: Master's thesis - Slide Assets                    #
# Author: Gregy                                              #
# Date: May 2026                                             #
##############################################################

suppressPackageStartupMessages({
  library(pacman)
  pacman::p_load(ggplot2, dplyr, tidyr, stringr, sf, rnaturalearth, rnaturalearthhires, ggrepel)
})

source("Code/00_Project_Setup.R")

output_dir <- "Output/Slides/assets"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

##############################
# BOS exposure illustration
##############################

# I use two representative siblings to make the identification idea visual.
# Older sibling: born in 1991, school-age window ages 6-15 is 1997-2006.
# Younger sibling: born in 1995, school-age window ages 6-15 is 2001-2010.
# BOS starts in 2005, so the younger sibling accumulates more exposed school years.

bos_start <- 2005

older_birth_year <- 1991
younger_birth_year <- 1995

older_years <- tibble(
  sibling = "Older sibling\nborn 1991",
  year = (older_birth_year + 6):(older_birth_year + 15),
  age = 6:15
)

younger_years <- tibble(
  sibling = "Younger sibling\nborn 1995",
  year = (younger_birth_year + 6):(younger_birth_year + 15),
  age = 6:15
)

exposure_data <- bind_rows(older_years, younger_years) %>%
  mutate(
    exposed = year >= bos_start,
    exposure_label = if_else(exposed, "Potential BOS-exposed school year", "Pre-BOS school year"),
    sibling = factor(sibling, levels = c("Older sibling\nborn 1991", "Younger sibling\nborn 1995")),
    y_position = if_else(sibling == "Younger sibling\nborn 1995", 2, 1)
  )

exposure_counts <- exposure_data %>%
  group_by(sibling) %>%
  summarise(
    bos_years = sum(exposed),
    y_position = first(y_position),
    .groups = "drop"
  )

exposure_plot <- ggplot(exposure_data, aes(x = year, y = y_position)) +
  geom_tile(aes(fill = exposure_label), width = 0.92, height = 0.50, color = "white", linewidth = 0.7) +
  geom_vline(xintercept = bos_start - 0.5, color = "#8B1E2D", linewidth = 1.2, linetype = "dashed") +
  geom_text(
    data = exposure_data %>% filter(age %in% c(6, 10, 15)),
    aes(x = year, y = y_position + 0.36, label = paste0("age ", age)),
    size = 3.2,
    color = "#303030",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = exposure_counts,
    aes(x = 2010.45, y = y_position, label = paste0(bos_years, " BOS years")),
    inherit.aes = FALSE,
    hjust = 0,
    fontface = "bold",
    size = 5.2,
    color = "#8B1E2D"
  ) +
  annotate(
    "label",
    x = bos_start - 0.5,
    y = 2.62,
    label = "BOS begins\nin 2005",
    fill = "#8B1E2D",
    color = "white",
    fontface = "bold",
    size = 4.4
  ) +
  scale_fill_manual(
    values = c(
      "Pre-BOS school year" = "#D9D4CE",
      "Potential BOS-exposed school year" = "#8B1E2D"
    )
  ) +
  scale_x_continuous(
    breaks = seq(1997, 2010, by = 1),
    limits = c(1996.5, 2013.0),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = c(1, 2),
    labels = c("Older sibling\nborn 1991", "Younger sibling\nborn 1995"),
    limits = c(0.55, 2.8),
    expand = c(0, 0)
  ) +
  labs(
    title = "Same family, different exposure to BOS",
    subtitle = "School-age years after the 2005 rollout vary by birth cohort",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 24, color = "#1D1D1D"),
    plot.subtitle = element_text(size = 15, color = "#555555", margin = margin(b = 18)),
    axis.text.x = element_text(size = 11, color = "#444444"),
    axis.text.y = element_text(size = 15, color = "#222222", face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#EFEFEF", linewidth = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    plot.margin = margin(15, 90, 15, 15)
  )

ggsave(
  filename = file.path(output_dir, "bos_exposure_sibling_timeline.png"),
  plot = exposure_plot,
  width = 12,
  height = 5.4,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, "bos_exposure_sibling_timeline.pdf"),
  plot = exposure_plot,
  width = 12,
  height = 5.4
)

# A compact version works better when the slide already has a title and bullets.
exposure_plot_compact <- exposure_plot +
  labs(title = NULL, subtitle = NULL) +
  theme(
    axis.text.x = element_text(size = 10, color = "#444444"),
    axis.text.y = element_text(size = 13, color = "#222222", face = "bold"),
    legend.text = element_text(size = 11),
    plot.margin = margin(5, 80, 5, 10)
  )

ggsave(
  filename = file.path(output_dir, "bos_exposure_sibling_timeline_compact.png"),
  plot = exposure_plot_compact,
  width = 11,
  height = 3.8,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, "bos_exposure_sibling_timeline_compact.pdf"),
  plot = exposure_plot_compact,
  width = 11,
  height = 3.8
)

message("Saved Output/Slides/assets/bos_exposure_sibling_timeline.png")
message("Saved Output/Slides/assets/bos_exposure_sibling_timeline.pdf")
message("Saved Output/Slides/assets/bos_exposure_sibling_timeline_compact.png")
message("Saved Output/Slides/assets/bos_exposure_sibling_timeline_compact.pdf")

##############################
# Pre-BOS education timeline
##############################

# This timeline is for the background slide. I keep the text short because
# the slide should explain the policy sequence visually, not through paragraphs.

timeline_data <- tibble(
  year = c(1973, 1984, 1994, 2001, 2005),
  label = c(
    "INPRES school\nconstruction",
    "6-year primary\ncompulsory education",
    "9-year basic\ncompulsory education",
    "Education\ndecentralization",
    "BOS launched"
  ),
  note = c(
    "Access expansion",
    "Primary completion",
    "Junior-secondary\nprogression",
    "Local finance and\nimplementation capacity",
    "School grants to\nreduce cost burdens"
  ),
  y = c(1.18, 0.82, 1.18, 0.82, 1.18)
) %>%
  mutate(
    period = case_when(
      year < 1994 ~ "Access expansion",
      year < 2005 ~ "Progression and governance",
      TRUE ~ "Cost reduction through BOS"
    )
  )

timeline_plot <- ggplot(timeline_data, aes(x = year, y = 1)) +
  annotate("rect", xmin = 1972, xmax = 1993.5, ymin = 0.55, ymax = 1.45, fill = "#F4EEE8", alpha = 0.95) +
  annotate("rect", xmin = 1993.5, xmax = 2004.5, ymin = 0.55, ymax = 1.45, fill = "#E9F1F5", alpha = 0.95) +
  annotate("rect", xmin = 2004.5, xmax = 2006, ymin = 0.55, ymax = 1.45, fill = "#F7E8EA", alpha = 0.95) +
  annotate("segment", x = 1973, xend = 2005, y = 1, yend = 1, color = "#2F6F8F", linewidth = 2.1) +
  geom_point(aes(color = period), size = 7) +
  geom_text(aes(y = y, label = year), fontface = "bold", size = 5.2, color = "#1D1D1D") +
  geom_text(
    aes(y = if_else(y > 1, y + 0.18, y - 0.18), label = label),
    fontface = "bold",
    size = 4.4,
    lineheight = 0.93,
    color = "#1D1D1D"
  ) +
  geom_text(
    aes(y = if_else(y > 1, y + 0.42, y - 0.42), label = note),
    size = 3.7,
    lineheight = 0.93,
    color = "#4A4A4A"
  ) +
  annotate(
    "text",
    x = 1982.5,
    y = 1.53,
    label = "Building access",
    color = "#8B1E2D",
    fontface = "bold",
    size = 4.6
  ) +
  annotate(
    "text",
    x = 1999,
    y = 1.53,
    label = "Progression and governance",
    color = "#2F6F8F",
    fontface = "bold",
    size = 4.6
  ) +
  annotate(
    "text",
    x = 2005.2,
    y = 1.53,
    label = "Cost relief",
    color = "#8B1E2D",
    fontface = "bold",
    size = 4.6,
    hjust = 0.5
  ) +
  scale_color_manual(
    values = c(
      "Access expansion" = "#8B1E2D",
      "Progression and governance" = "#2F6F8F",
      "Cost reduction through BOS" = "#8B1E2D"
    )
  ) +
  scale_x_continuous(limits = c(1971.5, 2006.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.34, 1.65), expand = c(0, 0)) +
  labs(
    title = "Indonesia's education policy shifted from access to progression",
    subtitle = "BOS emerged after major gains in school supply, when cost burdens still limited continuation",
    x = NULL,
    y = NULL
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 22, color = "#1D1D1D", hjust = 0.5),
    plot.subtitle = element_text(size = 13.5, color = "#555555", hjust = 0.5, margin = margin(b = 10)),
    legend.position = "none",
    plot.margin = margin(12, 18, 12, 18)
  )

ggsave(
  filename = file.path(output_dir, "pre_bos_policy_timeline.png"),
  plot = timeline_plot,
  width = 12,
  height = 5.6,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, "pre_bos_policy_timeline.pdf"),
  plot = timeline_plot,
  width = 12,
  height = 5.6
)

message("Saved Output/Slides/assets/pre_bos_policy_timeline.png")
message("Saved Output/Slides/assets/pre_bos_policy_timeline.pdf")

# A cleaner slide version without a title, designed to sit under the sentence
# on the "Before BOS" background slide.
timeline_cards <- tibble(
  position = 1:5,
  year = c("1973", "1984", "1994", "2001", "2005"),
  event = c(
    "INPRES school\nconstruction",
    "6-year primary\ncompulsory education",
    "9-year basic\ncompulsory education",
    "Education\ndecentralization",
    "BOS launched"
  ),
  takeaway = c(
    "Build access",
    "Primary completion",
    "Junior-secondary\nprogression",
    "Local finance and\nimplementation capacity",
    "Cost relief through\nschool grants"
  ),
  color_group = c("access", "access", "progression", "progression", "bos"),
  y_card = c(1.30, 0.72, 1.30, 0.72, 1.30)
)

timeline_cards_plot <- ggplot(timeline_cards, aes(x = position, y = 1)) +
  annotate("segment", x = 1, xend = 5, y = 1, yend = 1, color = "#2F6F8F", linewidth = 2.3) +
  geom_point(aes(fill = color_group), shape = 21, color = "white", stroke = 1.2, size = 8) +
  geom_label(
    aes(y = 1.47, label = paste0(year, "\n", event)),
    fill = "white",
    color = "#1D1D1D",
    linewidth = 0.45,
    label.r = unit(0.12, "lines"),
    label.padding = unit(0.25, "lines"),
    fontface = "bold",
    size = 4.0,
    lineheight = 0.92
  ) +
  geom_text(
    aes(y = 0.60, label = takeaway, color = color_group),
    fontface = "bold",
    size = 3.7,
    lineheight = 0.92
  ) +
  annotate(
    "text",
    x = 1.5,
    y = 1.88,
    label = "ACCESS EXPANSION",
    color = "#8B1E2D",
    fontface = "bold",
    size = 4.5
  ) +
  annotate(
    "text",
    x = 3.5,
    y = 1.88,
    label = "PROGRESSION + GOVERNANCE",
    color = "#2F6F8F",
    fontface = "bold",
    size = 4.5
  ) +
  annotate(
    "text",
    x = 5,
    y = 1.88,
    label = "COST RELIEF",
    color = "#8B1E2D",
    fontface = "bold",
    size = 4.5
  ) +
  scale_fill_manual(values = c("access" = "#8B1E2D", "progression" = "#2F6F8F", "bos" = "#8B1E2D")) +
  scale_color_manual(values = c("access" = "#8B1E2D", "progression" = "#2F6F8F", "bos" = "#8B1E2D")) +
  scale_x_continuous(limits = c(0.55, 5.45), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.36, 1.98), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_void(base_size = 14) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 30, 20, 30)
  )

ggsave(
  filename = file.path(output_dir, "pre_bos_policy_timeline_compact.png"),
  plot = timeline_cards_plot,
  width = 12,
  height = 4.3,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(output_dir, "pre_bos_policy_timeline_compact.pdf"),
  plot = timeline_cards_plot,
  width = 12,
  height = 4.3,
  bg = "white"
)

message("Saved Output/Slides/assets/pre_bos_policy_timeline_compact.png")
message("Saved Output/Slides/assets/pre_bos_policy_timeline_compact.pdf")

##############################
# Indonesia context map
##############################

# I use this map at the start of the presentation to orient the audience.
# The point is not to show the thesis sample yet, but to make clear that
# Indonesia is geographically large and administratively spread out.

indonesia_states <- ne_states(country = "indonesia", returnclass = "sf") %>%
  st_transform(4326) %>%
  mutate(
    highlight_group = case_when(
      name == "Jakarta Raya" ~ "DKI Jakarta",
      name %in% c("Banten", "Jawa Barat", "Jawa Tengah", "Yogyakarta", "Jawa Timur") ~ "Java",
      TRUE ~ "Other provinces"
    )
  )

jakarta_point <- tibble(
  city = "Jakarta",
  longitude = 106.8456,
  latitude = -6.2088,
  label = "Jakarta\ncapital area"
)

context_labels <- tibble(
  label = c("JAVA", "SUMATRA", "KALIMANTAN", "SULAWESI", "PAPUA"),
  longitude = c(110.0, 101.5, 114.0, 121.0, 137.0),
  latitude = c(-7.8, 0.6, 0.4, -2.0, -4.0)
)

indonesia_context_map <- ggplot() +
  geom_sf(
    data = indonesia_states,
    aes(fill = highlight_group),
    color = "white",
    linewidth = 0.35
  ) +
  geom_point(
    data = jakarta_point,
    aes(x = longitude, y = latitude),
    color = "#FFD23F",
    fill = "#8B1E2D",
    shape = 21,
    size = 5.0,
    stroke = 1.5
  ) +
  geom_label_repel(
    data = jakarta_point,
    aes(x = longitude, y = latitude, label = label),
    fontface = "bold",
    size = 4.4,
    color = "#8B1E2D",
    fill = "white",
    linewidth = 0.35,
    label.r = unit(0.10, "lines"),
    box.padding = 0.7,
    point.padding = 0.6,
    nudge_x = 7.5,
    nudge_y = 3.0,
    segment.color = "#8B1E2D",
    segment.size = 0.55,
    seed = 42
  ) +
  geom_text(
    data = context_labels,
    aes(x = longitude, y = latitude, label = label),
    fontface = "bold",
    size = 4.2,
    color = "#5C5C5C",
    alpha = 0.75
  ) +
  annotate(
    "label",
    x = 106.3,
    y = -10.0,
    label = "Java: administrative and population center",
    fontface = "bold",
    size = 4.2,
    fill = "#F6EFE8",
    color = "#8B1E2D",
    linewidth = 0.25,
    label.r = unit(0.16, "lines")
  ) +
  scale_fill_manual(
    values = c(
      "Other provinces" = "#E9DDD9",
      "Java" = "#C8892B",
      "DKI Jakarta" = "#8B1E2D"
    ),
    breaks = c("DKI Jakarta", "Java", "Other provinces")
  ) +
  coord_sf(xlim = c(94, 142), ylim = c(-11.5, 7), expand = FALSE) +
  guides(fill = "none") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(8, 10, 8, 10)
  )

ggsave(
  filename = file.path(output_dir, "indonesia_context_jakarta_map.png"),
  plot = indonesia_context_map,
  width = 12,
  height = 6.3,
  dpi = 320,
  bg = "white"
)

ggsave(
  filename = file.path(output_dir, "indonesia_context_jakarta_map.pdf"),
  plot = indonesia_context_map,
  width = 12,
  height = 6.3,
  bg = "white"
)

message("Saved Output/Slides/assets/indonesia_context_jakarta_map.png")
message("Saved Output/Slides/assets/indonesia_context_jakarta_map.pdf")
