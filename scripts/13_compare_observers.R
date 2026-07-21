# ─────────────────────────────────────────────────────────────────────
# 13_compare_observers.R — Compare independent Collect Earth reads
# ─────────────────────────────────────────────────────────────────────
# Input: output/collect_earth/collected_data/mayazomer_*.csv
#        output/collect_earth/collected_data/lucialayritz_*.csv
# ─────────────────────────────────────────────────────────────────────

source(here::here("scripts", "00_utils.R"))
library(tidyverse)

collected_dir <- here("output", "collect_earth", "collected_data")

maya_file <- paste0("mayazomer_RECENTFIRES_collectedData_",
                    "earthjt_fire_recovery_on_180726_120818_CSV.csv")

maya_data <- read.csv(file.path(collected_dir, maya_file),
                      na.strings = c("", "NA"), stringsAsFactors = FALSE) %>%
  filter(!(actively_saved_on_month == 7 &
             actively_saved_on_day %in% c(6, 7, 8))) %>%
  mutate(
    user = "Maya",
    plot_location = case_when(
      stringr::str_detect(id, "_inside_") ~ "Inside",
      stringr::str_detect(id, "_outside_") ~ "Outside",
      TRUE ~ NA_character_
    ),
    mortality_percentage = if_else(jt_pre_fire == 0, NA_real_,
                                   (jt_pre_fire - jt_post_fire) / jt_pre_fire * 100)
  )

lucia_data <- read.csv(file.path(collected_dir,
                                 "lucialayritz_collectedData_earthjt_fire_recovery_on_20260720.csv"),
                       na.strings = c("", "NA"), stringsAsFactors = FALSE) %>%
  mutate(
    user = "Lucia",
    plot_location = case_when(
      stringr::str_detect(id, "_inside_") ~ "Inside",
      stringr::str_detect(id, "_outside_") ~ "Outside",
      TRUE ~ NA_character_
    ),
    mortality_percentage = if_else(jt_pre_fire == 0, NA_real_,
                                   (jt_pre_fire - jt_post_fire) / jt_pre_fire * 100)
  )

# Shared observer data ----

shared_ids <- intersect(maya_data$id, lucia_data$id)

maya_selection <- maya_data %>%
  filter(id %in% shared_ids) %>%
  select(id, fire_name, user, plot_location, jt_pre_fire, jt_post_fire,
         mortality_percentage, pre_confidence, post_confidence,
         pre_vegetation_cover, post_vegetation_cover, plot_unsuitable, comments)

lucia_selection <- lucia_data %>%
  filter(id %in% shared_ids) %>%
  select(id, fire_name, user, plot_location, jt_pre_fire, jt_post_fire,
         mortality_percentage, pre_confidence, post_confidence,
         pre_vegetation_cover, post_vegetation_cover, plot_unsuitable, comments)

selection <- bind_rows(maya_selection, lucia_selection) %>%
  mutate(
    plot_location = factor(plot_location, levels = c("Inside", "Outside")),
    user = factor(user, levels = c("Maya", "Lucia"))
  ) %>%
  arrange(fire_name, plot_location, id, user)

# Observer comparison plots ----

observer_colors <- c("Maya" = "#D55E00", "Lucia" = "#0072B2")

observer_theme <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey45", fill = NA,
                                linewidth = 0.7),
    panel.spacing.y = grid::unit(0.6, "lines"),
    strip.background = element_rect(fill = "grey90", color = "grey45",
                                    linewidth = 0.7),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 40, hjust = 1),
    axis.title = element_text(face = "bold")
  )

pre_fire_plot <- ggplot(selection,
                        aes(x = fire_name, y = jt_pre_fire, fill = user, color = user)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.6,
               outlier.shape = NA, alpha = 0.2, linewidth = 0.7) +
  geom_point(position = position_jitterdodge(jitter.width = 0.18,
                                             dodge.width = 0.75), shape = 21, fill = "white", stroke = 0.8,
             size = 2.2, show.legend = FALSE) +
  facet_grid(plot_location ~ ., scales = "free_y") +
  scale_fill_manual(values = observer_colors) +
  scale_color_manual(values = observer_colors) +
  scale_x_discrete(labels = stringr::str_to_title) +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.08))) +
  labs(x = "Fire", y = "Pre-fire Joshua tree count", fill = "Observer",
       color = "Observer") +
  observer_theme

post_fire_plot <- ggplot(selection,
                         aes(x = fire_name, y = jt_post_fire, fill = user, color = user)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.6,
               outlier.shape = NA, alpha = 0.2, linewidth = 0.7) +
  geom_point(position = position_jitterdodge(jitter.width = 0.18,
                                             dodge.width = 0.75), shape = 21, fill = "white", stroke = 0.8,
             size = 2.2, show.legend = FALSE) +
  facet_grid(plot_location ~ ., scales = "free_y") +
  scale_fill_manual(values = observer_colors) +
  scale_color_manual(values = observer_colors) +
  scale_x_discrete(labels = stringr::str_to_title) +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.08))) +
  labs(x = "Fire", y = "Post-fire Joshua tree count", fill = "Observer",
       color = "Observer") +
  observer_theme

mortality_plot <- ggplot(selection,
                         aes(x = fire_name, y = mortality_percentage, fill = user, color = user)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.6,
               outlier.shape = NA, alpha = 0.2, linewidth = 0.7) +
  geom_point(position = position_jitterdodge(jitter.width = 0.18,
                                             dodge.width = 0.75), shape = 21, fill = "white", stroke = 0.8,
             size = 2.2, show.legend = FALSE) +
  facet_grid(plot_location ~ .) +
  scale_fill_manual(values = observer_colors) +
  scale_color_manual(values = observer_colors) +
  scale_x_discrete(labels = stringr::str_to_title) +
  scale_y_continuous(breaks = seq(0, 100, 20),
                     expand = expansion(mult = c(0.01, 0.04))) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Fire", y = "Mortality (%)", fill = "Observer",
       color = "Observer") +
  observer_theme

pre_fire_plot
post_fire_plot
mortality_plot

# Shared-site inside versus outside plot ----

location_colors <- c("Inside" = "#D55E00", "Outside" = "#0072B2")

pre_fire_location_plot <- selection %>%
  filter(!is.na(plot_location)) %>%
  ggplot(aes(x = fire_name, y = jt_pre_fire, fill = plot_location,
             color = plot_location)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.6,
               outlier.shape = NA, alpha = 0.25, linewidth = 0.7) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15,
                                             dodge.width = 0.75), shape = 21, fill = "white", size = 2,
             stroke = 0.8, show.legend = FALSE) +
  facet_grid(user ~ .) +
  scale_fill_manual(values = location_colors) +
  scale_color_manual(values = location_colors) +
  scale_x_discrete(labels = stringr::str_to_title) +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.08))) +
  labs(x = "Fire", y = "Pre-fire Joshua tree count",
       fill = "Plot location", color = "Plot location") +
  observer_theme

pre_fire_location_plot

# Separate proxy datasets ----

maya_proxy_data <- maya_data %>%
  filter(!is.na(jt_pre_fire), !is.na(plot_location)) %>%
  mutate(plot_location = relevel(factor(plot_location), ref = "Outside"))

lucia_proxy_data <- lucia_data %>%
  filter(!is.na(jt_pre_fire), !is.na(plot_location)) %>%
  mutate(plot_location = relevel(factor(plot_location), ref = "Outside"))

# Exploratory fire-level wilcox tests ----

maya_pre_fire_tests <- maya_proxy_data %>%
  group_by(fire_name) %>%
  filter(n_distinct(plot_location) == 2) %>%
  summarise(
    inside_sites = sum(plot_location == "Inside"),
    outside_sites = sum(plot_location == "Outside"),
    median_inside = median(jt_pre_fire[plot_location == "Inside"]),
    median_outside = median(jt_pre_fire[plot_location == "Outside"]),
    median_difference = median_inside - median_outside,
    p_value = if_else(inside_sites >= 2 & outside_sites >= 2,
                      wilcox.test(jt_pre_fire[plot_location == "Inside"],
                                  jt_pre_fire[plot_location == "Outside"], exact = FALSE)$p.value,
                      NA_real_),
    .groups = "drop"
  ) %>%
  mutate(p_adjusted = p.adjust(p_value, method = "BH"),
         significant = p_adjusted < 0.05)

lucia_pre_fire_tests <- lucia_proxy_data %>%
  group_by(fire_name) %>%
  filter(n_distinct(plot_location) == 2) %>%
  summarise(
    inside_sites = sum(plot_location == "Inside"),
    outside_sites = sum(plot_location == "Outside"),
    median_inside = median(jt_pre_fire[plot_location == "Inside"]),
    median_outside = median(jt_pre_fire[plot_location == "Outside"]),
    median_difference = median_inside - median_outside,
    p_value = if_else(inside_sites >= 2 & outside_sites >= 2,
                      wilcox.test(jt_pre_fire[plot_location == "Inside"],
                                  jt_pre_fire[plot_location == "Outside"], exact = FALSE)$p.value,
                      NA_real_),
    .groups = "drop"
  ) %>%
  mutate(p_adjusted = p.adjust(p_value, method = "BH"),
         significant = p_adjusted < 0.05)

maya_pre_fire_tests
lucia_pre_fire_tests

# Proxy mixed-effects models ----

maya_proxy_model <- glmmTMB::glmmTMB(
  jt_pre_fire ~ plot_location + (1 | fire_name),
  family = glmmTMB::nbinom2,
  data = maya_proxy_data
)

lucia_proxy_model <- glmmTMB::glmmTMB(
  jt_pre_fire ~ plot_location + (1 | fire_name),
  family = poisson,
  data = lucia_proxy_data
)

summary(maya_proxy_model)
summary(lucia_proxy_model)

maya_diagnostics <- DHARMa::simulateResiduals(maya_proxy_model)
DHARMa::testDispersion(maya_diagnostics)
plot(maya_diagnostics)

lucia_diagnostics <- DHARMa::simulateResiduals(lucia_proxy_model)
DHARMa::testDispersion(lucia_diagnostics)
plot(lucia_diagnostics)

maya_location_effect <- broom.mixed::tidy(maya_proxy_model,
                                          effects = "fixed", conf.int = TRUE) %>%
  filter(term == "plot_locationInside") %>%
  transmute(inside_outside_ratio = exp(estimate),
            lower_95 = exp(conf.low), upper_95 = exp(conf.high),
            p_value = p.value)

lucia_location_effect <- broom.mixed::tidy(lucia_proxy_model,
                                           effects = "fixed", conf.int = TRUE) %>%
  filter(term == "plot_locationInside") %>%
  transmute(inside_outside_ratio = exp(estimate),
            lower_95 = exp(conf.low), upper_95 = exp(conf.high),
            p_value = p.value)

maya_location_effect
lucia_location_effect


# Fire-level inside-outside tests ----

maya_fire_data <- maya_proxy_data %>%
  group_by(fire_name) %>%
  filter(n_distinct(plot_location) == 2) %>%
  ungroup()

lucia_fire_data <- lucia_proxy_data %>%
  group_by(fire_name) %>%
  filter(n_distinct(plot_location) == 2) %>%
  ungroup()

maya_fire_model <- glmmTMB::glmmTMB(
  jt_pre_fire ~ fire_name * plot_location,
  family = glmmTMB::nbinom2,
  data = maya_fire_data
)

lucia_fire_model <- glmmTMB::glmmTMB(
  jt_pre_fire ~ fire_name * plot_location,
  family = poisson,
  data = lucia_fire_data
)


maya_fire_tests <- emmeans::emmeans(
  maya_fire_model, ~plot_location | fire_name
) %>%
  emmeans::contrast(method = "revpairwise") %>%
  summary(type = "response", infer = TRUE, adjust = "none") %>%
  as.data.frame() %>%
  mutate(
    p_adjusted = p.adjust(p.value, method = "BH"),
    significant = p_adjusted < 0.05
  )

lucia_fire_tests <- emmeans::emmeans(
  lucia_fire_model, ~plot_location | fire_name
) %>%
  emmeans::contrast(method = "revpairwise") %>%
  summary(type = "response", infer = TRUE, adjust = "none") %>%
  as.data.frame() %>%
  mutate(
    p_adjusted = p.adjust(p.value, method = "BH"),
    significant = p_adjusted < 0.05
  )
maya_fire_tests
lucia_fire_tests

#plot model results
# Maya proxy model plot ----

maya_model_predictions <- emmeans::emmeans(
  maya_proxy_model, ~plot_location, type = "response"
) %>%
  as.data.frame()

maya_proxy_plot <- ggplot(maya_proxy_data,
                          aes(x = plot_location, y = jt_pre_fire)) +
  geom_jitter(width = 0.12, alpha = 0.35, size = 1.8) +
  geom_errorbar(data = maya_model_predictions,
                aes(x = plot_location, ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.12, linewidth = 0.8, inherit.aes = FALSE) +
  geom_point(data = maya_model_predictions,
             aes(x = plot_location, y = response), shape = 21, fill = "white",
             size = 3.5, stroke = 1, inherit.aes = FALSE) +
  labs(x = "Plot location", y = "Pre-fire Joshua tree count") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

maya_proxy_plot

# Mortality agreement summaries ----

agreement_data <- selection %>%
  mutate(
    user = stringr::str_to_lower(as.character(user)),
    plot_unsuitable = stringr::str_to_lower(
      stringr::str_trim(as.character(plot_unsuitable))
    ),
    plot_unsuitable = na_if(plot_unsuitable, "")
  ) %>%
  select(id, fire_name, plot_location, user, jt_pre_fire, jt_post_fire,
         mortality_percentage, plot_unsuitable,
         pre_vegetation_cover, post_vegetation_cover) %>%
  tidyr::pivot_wider(names_from = user,
                     values_from = c(jt_pre_fire, jt_post_fire, mortality_percentage,
                                     plot_unsuitable, pre_vegetation_cover,
                                     post_vegetation_cover), names_sep = "_") %>%
  mutate(mortality_abs_diff = abs(mortality_percentage_maya -
                                    mortality_percentage_lucia))

mortality_by_location <- agreement_data %>%
  filter(!is.na(mortality_abs_diff)) %>%
  group_by(plot_location) %>%
  summarise(
    sites_compared = n(),
    pct_within_10_percentage_points =
      round(mean(mortality_abs_diff <= 10) * 100, 1),
    median_difference_percentage_points =
      round(median(mortality_abs_diff), 1),
    difference_range_percentage_points = sprintf("%.1f–%.1f",
                                                 min(mortality_abs_diff), max(mortality_abs_diff)),
    .groups = "drop"
  )

mortality_by_fire <- agreement_data %>%
  filter(!is.na(mortality_abs_diff)) %>%
  group_by(fire_name, plot_location) %>%
  summarise(
    sites_compared = n(),
    pct_within_10_percentage_points =
      round(mean(mortality_abs_diff <= 10) * 100, 1),
    median_difference_percentage_points =
      round(median(mortality_abs_diff), 1),
    difference_range_percentage_points = sprintf("%.1f–%.1f",
                                                 min(mortality_abs_diff), max(mortality_abs_diff)),
    .groups = "drop"
  ) %>%
  arrange(desc(median_difference_percentage_points))

mortality_by_location
mortality_by_fire

# Sites for review ----

mortality_difference_cutoff <- 10

sites_to_inspect <- agreement_data %>%
  filter(mortality_abs_diff > mortality_difference_cutoff) %>%
  arrange(desc(mortality_abs_diff)) %>%
  select(id, fire_name, plot_location, mortality_percentage_maya,
         mortality_percentage_lucia, mortality_abs_diff)

unsuitable_sites <- agreement_data %>%
  filter(coalesce(plot_unsuitable_maya == "unsuitable", FALSE) |
           coalesce(plot_unsuitable_lucia == "unsuitable", FALSE)) %>%
  select(id, fire_name, plot_location, plot_unsuitable_maya,
         plot_unsuitable_lucia)

sites_to_inspect
unsuitable_sites

# Vegetation cover agreement summaries ----

#ordinal distance between classes: 0 = same class, 1 = adjacent
#(low/medium or medium/high), 2 = opposite ends (low/high)
vegetation_levels <- c("low", "medium", "high")

vegetation_agreement_data <- agreement_data %>%
  mutate(
    pre_vegetation_diff = abs(match(pre_vegetation_cover_maya, vegetation_levels) -
                                match(pre_vegetation_cover_lucia, vegetation_levels)),
    post_vegetation_diff = abs(match(post_vegetation_cover_maya, vegetation_levels) -
                                 match(post_vegetation_cover_lucia, vegetation_levels))
  )

vegetation_by_location <- vegetation_agreement_data %>%
  group_by(plot_location) %>%
  summarise(
    sites_compared_pre = sum(!is.na(pre_vegetation_diff)),
    pct_agree_pre = round(mean(pre_vegetation_diff == 0, na.rm = TRUE) * 100, 1),
    sites_compared_post = sum(!is.na(post_vegetation_diff)),
    pct_agree_post = round(mean(post_vegetation_diff == 0, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

vegetation_by_fire <- vegetation_agreement_data %>%
  group_by(fire_name, plot_location) %>%
  summarise(
    sites_compared_pre = sum(!is.na(pre_vegetation_diff)),
    pct_agree_pre = round(mean(pre_vegetation_diff == 0, na.rm = TRUE) * 100, 1),
    sites_compared_post = sum(!is.na(post_vegetation_diff)),
    pct_agree_post = round(mean(post_vegetation_diff == 0, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(pct_agree_pre)

vegetation_by_location
vegetation_by_fire

# Sites for review (vegetation cover) ----

vegetation_sites_to_inspect <- vegetation_agreement_data %>%
  filter(pre_vegetation_diff > 0 | post_vegetation_diff > 0) %>%
  arrange(desc(pmax(pre_vegetation_diff, post_vegetation_diff, na.rm = TRUE))) %>%
  select(id, fire_name, plot_location,
         pre_vegetation_cover_maya, pre_vegetation_cover_lucia,
         post_vegetation_cover_maya, post_vegetation_cover_lucia)

vegetation_sites_to_inspect
