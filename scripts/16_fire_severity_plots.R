# ─────────────────────────────────────────────────────────────────────
# 16_fire_severity_plots.R — an all-fires pooled spread, a per-fire
# "coverage" check (do the inside sites span the fire's full severity
# range?), and then one combined summary per fire: a severity map
# (sampled pixels + perimeter + inside/outside sites) next to the spread
# (whole-fire violin vs. inside/outside box+jitter), for dNBR and RBR.
#
# This does NOT write any PNGs -- it just prints each plot to the active
# graphics device in turn (pooled dNBR/RBR, then coverage dNBR/RBR, then
# all 22 fires one by one; in RStudio each one lands in the Plots pane,
# flip through them with its back/forward arrows).
#
# Reads:
#   output/fire_severity/fire_severity_pixels.csv   (from 15_fire_severity_gee.py)
#   output/fire_severity/site_severity.csv          (from 15_fire_severity_gee.py)
#   output/fire_boundaries_confirmed_shp/fire_boundaries_confirmed.shp
#   output/recent/recent_sites_coordinates.csv       (for site lat/lon)
#   output/historical/historical_sites_coordinates.csv
#
# Writes:
#   output/fire_severity/severity_spread_summary.csv
#   output/fire_severity/severity_coverage_summary.csv
# ─────────────────────────────────────────────────────────────────────

source(here::here("scripts", "00_utils.R"))

library(ggplot2)
library(tidyr)
library(readr)
library(patchwork)

FIRE_SEVERITY_DIR <- here("output", "fire_severity")

METRICS <- c(dnbr = "dNBR", rbr = "RBR")

GROUP_LEVELS <- c("Whole fire", "Inside sites", "Outside sites")
GROUP_COLORS <- c(
  "Whole fire"    = "#B8B3A2",
  "Inside sites"  = "#A3452F",
  "Outside sites" = "#4E6A3E"
)


# Data ----

pixels <- read_csv(here(FIRE_SEVERITY_DIR, "fire_severity_pixels.csv"), show_col_types = FALSE)

site_catalog_recent <- read_csv(here("output", "recent", "recent_sites_coordinates.csv"), show_col_types = FALSE)
site_catalog_historical <- read_csv(here("output", "historical", "historical_sites_coordinates.csv"), show_col_types = FALSE)
site_catalog <- bind_rows(site_catalog_recent, site_catalog_historical) %>%
  select(site_id, lat, lon)

# Which era each fire belongs to, from which coordinates CSV its sites came from.
fire_era <- bind_rows(
  distinct(site_catalog_recent, fire_label) %>% mutate(era = "Recent"),
  distinct(site_catalog_historical, fire_label) %>% mutate(era = "Historical")
)

sites <- read_csv(here(FIRE_SEVERITY_DIR, "site_severity.csv"), show_col_types = FALSE) %>%
  mutate(
    location = case_when(
      str_detect(site_id, "_inside_") ~ "Inside",
      str_detect(site_id, "_outside_") ~ "Outside",
      TRUE ~ "Other"
    )
  ) %>%
  left_join(site_catalog, by = "site_id")

# Fire perimeters, flattened to plain lon/lat -- fine even for multi-part/
# holed perimeters (several here are) since, with no fill, each ring just
# draws as its own closed outline loop.
perimeters <- st_read(here("output", "fire_boundaries_confirmed_shp", "fire_boundaries_confirmed.shp"), quiet = TRUE)
perimeter_coords <- st_coordinates(perimeters) %>%
  as_tibble() %>%
  mutate(
    fire_label = paste0(norm_label(perimeters$fire_name[L3]), "_", perimeters$year[L3]),
    ring_id = paste(L3, L2, L1, sep = "_")
  ) %>%
  rename(lon = X, lat = Y)

pixels_long <- pixels %>%
  select(fire_label, sensor, dnbr, rbr) %>%
  pivot_longer(c(dnbr, rbr), names_to = "metric", values_to = "value") %>%
  mutate(group = "Whole fire")

sites_long <- sites %>%
  filter(location %in% c("Inside", "Outside")) %>%
  select(fire_label, sensor, location, dnbr, rbr) %>%
  pivot_longer(c(dnbr, rbr), names_to = "metric", values_to = "value") %>%
  mutate(group = paste(location, "sites"))

combined <- bind_rows(pixels_long, sites_long) %>%
  mutate(group = factor(group, levels = GROUP_LEVELS)) %>%
  left_join(fire_era, by = "fire_label")


# Summary table (methods-section numbers to go with the plots) ----

severity_spread_summary <- combined %>%
  mutate(metric_label = factor(METRICS[metric], levels = unname(METRICS))) %>%
  group_by(fire_label, sensor, metric_label, group) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(metric_label, fire_label, sensor, group)

write_csv(severity_spread_summary, here(FIRE_SEVERITY_DIR, "severity_spread_summary.csv"))


# Coverage: for each fire/sensor/metric, where do the inside sites fall on
# the whole-fire pixel distribution's own percentile scale? This is the
# actual "do my sites represent the range" check -- a box that visually
# overlaps the violin can still cluster near the median; this measures it
# directly instead of relying on the eye.
coverage_raw <- combined %>%
  filter(group %in% c("Whole fire", "Inside sites")) %>%
  group_by(fire_label, era, sensor, metric) %>%
  group_modify(~ {
    whole_vals <- .x$value[.x$group == "Whole fire"]
    site_vals  <- .x$value[.x$group == "Inside sites"]
    if (length(whole_vals) < 2 || length(site_vals) < 1) return(tibble())
    f <- ecdf(whole_vals)
    tibble(site_pct = f(site_vals) * 100)
  }) %>%
  ungroup()

coverage_summary <- coverage_raw %>%
  group_by(fire_label, era, sensor, metric) %>%
  summarise(
    n_sites = n(),
    min_pct = min(site_pct),
    max_pct = max(site_pct),
    range_pct = max_pct - min_pct,
    .groups = "drop"
  ) %>%
  mutate(metric_label = factor(METRICS[metric], levels = unname(METRICS))) %>%
  arrange(metric_label, fire_label, sensor)

write_csv(coverage_summary, here(FIRE_SEVERITY_DIR, "severity_coverage_summary.csv"))


# Plot helpers ----
# "Whole fire" (thousands of pixels) is drawn as a violin -- the shape of a
# dense distribution matters more than individual points. Site groups (a
# handful of plots each) are drawn as box + jittered points -- with so few
# values, the actual points are the honest representation; a violin would
# imply a smooth density that a dozen points can't support.
plot_spread <- function(data, facet_formula = NULL, ncol = NULL) {
  whole <- filter(data, group == "Whole fire")
  site_pts <- filter(data, group != "Whole fire")

  p <- ggplot(data, aes(x = group, y = value, fill = group)) +
    geom_violin(data = whole, color = NA, alpha = 0.85, trim = TRUE, linewidth = 0) +
    geom_boxplot(data = site_pts, width = 0.32, outlier.shape = NA, alpha = 0.9) +
    geom_jitter(data = site_pts, width = 0.08, height = 0, size = 1.2, alpha = 0.55,
                color = "#1F2622") +
    scale_fill_manual(values = GROUP_COLORS, guide = "none") +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 8),
      axis.text.x = element_text(angle = 20, hjust = 1)
    )
  if (!is.null(facet_formula)) p <- p + facet_wrap(facet_formula, ncol = ncol, scales = "free_y")
  p
}

# A severity map for ONE fire (all its available sensors faceted side by
# side if more than one) -- sampled pixels colored by severity, the
# perimeter outline, and sites overlaid (circle = inside, triangle = outside).
plot_fire_map <- function(fire_label_val, metric_key) {
  px <- pixels %>%
    filter(fire_label == fire_label_val, !is.na(.data[[metric_key]])) %>%
    select(sensor, lon, lat, value = all_of(metric_key))
  perim <- filter(perimeter_coords, fire_label == fire_label_val)
  site_pts <- sites %>%
    filter(fire_label == fire_label_val, location %in% c("Inside", "Outside"))

  n_sensors <- n_distinct(px$sensor)

  p <- ggplot() +
    geom_point(data = px, aes(lon, lat, color = value), size = 0.7, alpha = 0.85) +
    geom_polygon(data = perim, aes(lon, lat, group = ring_id),
                 fill = NA, color = "black", linewidth = 0.4) +
    scale_color_distiller(palette = "RdYlGn", direction = -1, name = METRICS[[metric_key]]) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

  if (nrow(site_pts) > 0) {
    p <- p + geom_point(data = site_pts, aes(lon, lat, shape = location),
                         size = 2, stroke = 0.8, color = "black", fill = "white") +
      scale_shape_manual(values = c(Inside = 21, Outside = 24), name = NULL)
  }
  if (n_sensors > 1) p <- p + facet_wrap(~sensor, scales = "free")
  p
}

# One fire's full summary: dNBR map + dNBR spread, RBR map + RBR spread.
plot_fire_summary <- function(fire_label_val) {
  d <- filter(combined, fire_label == fire_label_val)
  n_sensors <- n_distinct(d$sensor)

  dnbr_map <- plot_fire_map(fire_label_val, "dnbr")
  rbr_map <- plot_fire_map(fire_label_val, "rbr")
  dnbr_spread <- plot_spread(filter(d, metric == "dnbr"),
                              if (n_sensors > 1) ~sensor else NULL, ncol = n_sensors) +
    labs(title = "dNBR spread")
  rbr_spread <- plot_spread(filter(d, metric == "rbr"),
                             if (n_sensors > 1) ~sensor else NULL, ncol = n_sensors) +
    labs(title = "RBR spread")

  # wrap_plots() (an explicit function call) rather than the +//operator
  # composition syntax -- the operator form dispatches on ggplot2's own S7
  # method table and is sensitive to exact ggplot2/patchwork version
  # combinations; wrap_plots() is the more robust, version-stable API.
  wrap_plots(list(dnbr_map, dnbr_spread, rbr_map, rbr_spread), ncol = 2) +
    plot_annotation(title = fire_label_val,
                     subtitle = "Severity map (left) vs. whole-fire / inside / outside spread (right)")
}

# Fires pooled within one era: whole-fire pixel sample (every fire in that
# era) vs. inside/outside sites (every site in that era), one metric at a
# time, faceted by sensor.
plot_pooled <- function(metric_key, era_val) {
  d <- filter(combined, metric == metric_key, era == era_val)
  n_sensors <- n_distinct(d$sensor)
  n_fires <- n_distinct(d$fire_label)
  plot_spread(d, if (n_sensors > 1) ~sensor else NULL, ncol = n_sensors) +
    labs(title = paste0(era_val, " fires combined -- ", METRICS[[metric_key]]),
         subtitle = paste0("Whole-fire pixel sample (", n_fires, " ", tolower(era_val),
                            " fires) vs. inside/outside sampling sites"))
}

# Coverage: for each fire, the percentile range (0-100, relative to that
# fire's OWN whole-fire pixel distribution) spanned by its inside sites.
# A short bar sitting away from 0/100 means the sites cluster around
# typical severity and miss the extremes; a bar stretching close to 0-100
# means the sites capture most of the range the fire actually produced.
plot_coverage <- function(metric_key, era_val) {
  d <- coverage_summary %>% filter(metric == metric_key, era == era_val)

  # "Total" row: every inside site's percentile rank is already on the same
  # 0-100 scale (relative to its OWN fire), so pooling them across fires
  # within this era/sensor is a valid, direct summary -- not an average of
  # averages, but the min/max over every site pooled.
  total_row <- coverage_raw %>%
    filter(metric == metric_key, era == era_val) %>%
    group_by(sensor) %>%
    summarise(
      n_sites = n(),
      min_pct = min(site_pct),
      max_pct = max(site_pct),
      range_pct = max_pct - min_pct,
      .groups = "drop"
    ) %>%
    mutate(fire_label = "Total")

  d <- bind_rows(d, total_row) %>%
    mutate(
      fire_label = factor(fire_label, levels = c(sort(unique(d$fire_label)), "Total")),
      is_total = fire_label == "Total"
    )

  ggplot(d, aes(y = fire_label)) +
    geom_vline(xintercept = c(0, 100), linetype = "dashed", color = "grey70") +
    geom_hline(yintercept = n_distinct(d$fire_label) - 0.5, color = "grey50") +
    geom_segment(aes(x = min_pct, xend = max_pct, yend = fire_label, color = range_pct,
                      linewidth = is_total)) +
    geom_point(aes(x = min_pct, color = range_pct, size = is_total)) +
    geom_point(aes(x = max_pct, color = range_pct, size = is_total)) +
    scale_linewidth_manual(values = c(`FALSE` = 1.6, `TRUE` = 3), guide = "none") +
    scale_size_manual(values = c(`FALSE` = 1.8, `TRUE` = 3), guide = "none") +
    scale_color_distiller(palette = "RdYlGn", direction = 1, limits = c(0, 100),
                           name = "Range\ncovered") +
    scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
    facet_wrap(~sensor) +
    labs(x = "Percentile of whole-fire pixel distribution", y = NULL,
         title = paste0(era_val, " fires -- ", METRICS[[metric_key]],
                        " -- percentile range spanned by inside sites"),
         subtitle = "Bar = severity range (0-100%) each fire's sites actually sampled. Total = all sites in this era, pooled.") +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(), legend.position = "right")
}


# Overview first: pooled spread across all fires, then the coverage check,
# then step through every fire one at a time -- display only, no files. ----

for (era_val in c("Recent", "Historical")) {
  message("showing: ", tolower(era_val), " fires pooled, dNBR")
  print(plot_pooled("dnbr", era_val))
  message("showing: ", tolower(era_val), " fires pooled, RBR")
  print(plot_pooled("rbr", era_val))
}

for (era_val in c("Recent", "Historical")) {
  message("showing: ", tolower(era_val), " coverage, dNBR")
  print(plot_coverage("dnbr", era_val))
  message("showing: ", tolower(era_val), " coverage, RBR")
  print(plot_coverage("rbr", era_val))
}

for (fire_label_val in sort(unique(combined$fire_label))) {
  message("showing ", fire_label_val)
  print(plot_fire_summary(fire_label_val))
}

message("\nDone -- stepped through all ", n_distinct(combined$fire_label), " fires.",
        " Summary table at ", here(FIRE_SEVERITY_DIR, "severity_spread_summary.csv"))
