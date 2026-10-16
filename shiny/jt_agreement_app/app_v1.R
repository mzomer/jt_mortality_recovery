# -----------------------------------------------------------------------
# Joshua tree counts: observer agreement app (v1)
# Two students independently pin and complete the Collect Earth survey for
# each plot. The app compares those two records directly: counts must agree
# within tolerance, suitability and vegetation cover must match exactly.
# Agreeing plots are Accepted (final counts = rounded mean); both marking a
# plot unsuitable Excludes it; a missing field is Incomplete; anything else
# that disagrees goes to Team review.
#
# Tabs: Overview | Summary plots | Low confidence | Team review | Data checks | How it works
#
# Packages: shiny, bslib, DT, dplyr, tidyr, ggplot2, stringr, purrr, here,
#           googledrive (only if reading from Google Drive)
# Run:      shiny::runApp("shiny/jt_agreement_app", port = ..., ...) or open app_v1.R
#           and run it directly -- this file is independent of app.R.
# -----------------------------------------------------------------------

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)
library(here)

options(shiny.maxRequestSize = 30 * 1024^2)

`%||%` <- function(x, y) if (is.null(x)) y else x

# Where the exports live ----
# Exports are read from data/from_drive, filled by running
# scripts/15_sync_drive_exports.R by hand. To read Drive live instead, set
DRIVE_FOLDER <- "1xC1P6iVUxQd2cq3tSNHYuaojeWyqxvGL" #(it takes priority).

options(gargle_oauth_email = TRUE)  # reuse the saved Google sign-in, don't ask

DATA_FOLDER <- ''

# Collect Earth plot number ----
# IMPORTANT: update ACTIVE_SURVEY whenever the survey observers are actively
# counting against changes -- it must match
# output/collect_earth/placemark_<ACTIVE_SURVEY>.csv from whichever
# scripts/12*_collect_earth_export*.R was last run for the live survey.
ACTIVE_SURVEY <- "test"
PLOT_ORDER_PATH <- here("output", "collect_earth", paste0("placemark_", ACTIVE_SURVEY, ".csv"))

# Low confidence ----
# A plot is flagged when at least this many observers marked low confidence
# for the same count (pre- or post-fire).
LOW_CONF_MIN <- 2

drive_poll_ms <- 60000   # how often to re-check Google Drive
folder_poll_ms <- 15000  # how often to re-check a local folder

export_pattern <- "collectedData.*\\.csv$"
# Exports whose file name starts with "resolved" hold the team's resolutions
# of Team review plots, not a third observer's counts
resolved_pattern <- "^resolved"
is_resolved_file <- function(name) grepl(resolved_pattern, basename(name), ignore.case = TRUE)

empty_files <- function() {
  data.frame(name = character(0), datapath = character(0),
             mtime = as.POSIXct(character(0)))
}

empty_folder <- function() list(exports = empty_files())

list_folder_files <- function(folder) {
  folder <- path.expand(folder)
  if (!dir.exists(folder)) stop("Folder not found: ", folder)
  paths <- list.files(folder, pattern = export_pattern, ignore.case = TRUE,
                      full.names = TRUE, recursive = TRUE)
  exports <- if (length(paths) == 0) empty_files() else
    data.frame(name = basename(paths), datapath = paths,
               mtime = file.info(paths)$mtime)
  list(exports = exports)
}

# Google Drive: list the folder, download new or changed files to a cache.
# A separate cache dir from app.R's, so the two apps never contend over the
# same files while both are running.
drive_cache <- file.path(tempdir(), "jt_agreement_v1_drive_cache")

drive_list_raw <- function(folder) {
  found <- googledrive::drive_ls(googledrive::as_id(folder), recursive = TRUE)
  data.frame(
    id = as.character(found$id),
    name = found$name,
    modified = vapply(found$drive_resource,
                      function(r) r$modifiedTime %||% NA_character_,
                      character(1)),
    mime = vapply(found$drive_resource,
                  function(r) r$mimeType %||% NA_character_, character(1))
  )
}

drive_fetch <- function(id, path, type = NULL) {
  googledrive::drive_download(googledrive::as_id(id), path = path,
                              type = type, overwrite = TRUE)
}

cache_path <- function(id, modified, suffix = "") {
  file.path(drive_cache, paste0(id, "_", gsub("[^0-9]", "", modified),
                                suffix, ".csv"))
}

list_drive_files <- function(folder) {
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("Install the googledrive package: install.packages(\"googledrive\")")
  }
  found <- drive_list_raw(folder)
  dir.create(drive_cache, showWarnings = FALSE, recursive = TRUE)

  ex <- found[grepl(export_pattern, found$name, ignore.case = TRUE), ]
  exports <- empty_files()
  if (nrow(ex) > 0) {
    paths <- cache_path(ex$id, ex$modified)
    for (i in seq_len(nrow(ex))) {
      if (!file.exists(paths[i]) || is.na(ex$modified[i])) {
        drive_fetch(ex$id[i], paths[i])
      }
    }
    exports <- data.frame(
      name = ex$name, datapath = paths,
      mtime = as.POSIXct(ex$modified, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  list(exports = exports)
}

folder_source <- function() {
  if (nzchar(DRIVE_FOLDER)) "drive" else if (nzchar(DATA_FOLDER)) "local" else "none"
}

# Status categories, in display order ----
status_levels <- c("Not started", "In progress", "Incomplete", "Excluded",
                   "Accepted", "Team review", "Resolved")
status_help <- c(
  "Not started" = "Opened in Collect Earth, but neither observer has saved counts yet",
  "In progress" = "Counted by one of the two observers so far",
  "Incomplete"  = "Both observers have saved, but a required field is blank in at least one entry",
  "Excluded"    = "Both observers marked the plot unsuitable",
  "Accepted"    = "Both entries agree on suitability, vegetation cover, and counts",
  "Team review" = "Both entries are complete, but disagree on suitability, vegetation cover, or counts",
  "Resolved"    = "Was in Team review; the disagreeing fields were settled in a resolved export"
)
status_colors <- c(
  "Not started" = "#B8B3A2",
  "In progress" = "#C9A55A",
  "Incomplete"  = "#7C8DA6",
  "Excluded"    = "#6F7773",
  "Accepted"    = "#4E6A3E",
  "Team review" = "#A3452F",
  "Resolved"    = "#5E5A8C"
)
status_text <- c("Not started" = "#2B2B2B", "In progress" = "#2B2B2B",
                 "Incomplete" = "#FFFFFF", "Excluded" = "#FFFFFF",
                 "Accepted" = "#FFFFFF", "Team review" = "#FFFFFF",
                 "Resolved" = "#FFFFFF")

issue_labels <- c(
  f_suit     = "Suitability disagreement",
  f_missing  = "Missing count",
  f_vmissing = "Missing vegetation cover",
  f_pre      = "Pre-fire count disagreement",
  f_post     = "Post-fire count disagreement",
  f_vpre     = "Pre vegetation cover mismatch",
  f_vpost    = "Post vegetation cover mismatch",
  f_extra    = "More than 2 observers"
)
# Only these ever appear on a Team review plot (an incomplete/missing-field
# plot never reaches Team review -- it's Incomplete instead). Used to build
# the Issue filter on the Team review tab.
team_review_cols <- c("f_suit", "f_pre", "f_post", "f_vpre", "f_vpost", "f_extra")
veg_levels <- c("low", "medium", "high")

check_labels <- c(
  post_gt_pre   = "Post-fire count higher than pre-fire (inside plot)",
  count_missing = "Pre- or post-fire count missing",
  veg_missing   = "Vegetation cover missing",
  veg_bad       = "Unrecognised vegetation cover class",
  conf_missing  = "Confidence missing",
  unsuit_counts = "Marked unsuitable but has counts",
  not_saved     = "Opened but nothing saved",
  res_unused    = "Resolution for a plot not in Team review"
)

# Fire display order, matching the master sampling datasheet's row order.
read_survey_fire_order <- function(path) {
  if (!file.exists(path)) return(character(0))
  labels <- unique(read.csv(path, stringsAsFactors = FALSE)$fire_label)
  toupper(gsub("_", " ", sub("_[0-9]{4}$", "", labels)))
}
SURVEY_FIRE_ORDER <- unique(c(
  read_survey_fire_order(here("output", "recent", "recent_sites_coordinates.csv")),
  read_survey_fire_order(here("output", "historical", "historical_sites_coordinates.csv"))
))

# Chart look, shared by every results plot ----
viz_theme <- function(base_size = 14, vertical = FALSE) {
  grid <- element_line(color = "#e1e0d9", linewidth = 0.4)
  theme_minimal(base_size = base_size) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid.major.y = if (vertical) grid else element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = if (vertical) element_blank() else grid,
      axis.text.x = if (vertical) element_text(angle = 45, hjust = 1) else element_text(),
      axis.ticks = element_blank(),
      axis.text = element_text(color = "#52514e"),
      strip.text = element_text(face = "bold", color = "#0b0b0b"),
      strip.background = element_blank(),
      plot.background = element_rect(fill = "#fcfcfb", color = NA),
      panel.background = element_rect(fill = "#fcfcfb", color = NA)
    )
}

# Reading and cleaning ----

optional_cols <- c("operator", "actively_saved", "actively_saved_on_year",
                   "actively_saved_on_month", "actively_saved_on_day",
                   "fire_name", "pre_confidence", "pre_vegetation_cover",
                   "post_confidence", "post_vegetation_cover", "comments",
                   "plot_unsuitable", "location_x", "location_y")

read_ce_file <- function(path, filename, file_time = Sys.time()) {
  raw <- read.csv(path, na.strings = c("", "NA"), stringsAsFactors = FALSE,
                  check.names = FALSE, colClasses = "character")
  missing <- setdiff(c("id", "jt_pre_fire", "jt_post_fire"), names(raw))
  if (length(missing) > 0) {
    stop(sprintf("%s is missing required column(s): %s", filename,
                 paste(missing, collapse = ", ")))
  }
  for (col in setdiff(optional_cols, names(raw))) raw[[col]] <- NA_character_

  # Observer = the file's most common Collect Earth `operator` (else the
  # filename prefix). Assigned per file, not per row.
  ops <- sort(table(raw$operator), decreasing = TRUE)
  observer_name <- if (length(ops) > 0) names(ops)[1] else
    str_extract(filename, "^[^_]+")
  raw %>%
    mutate(other_operator = if_else(!is.na(operator) & operator != observer_name,
                                    operator, NA_character_),
           observer = observer_name,
           source_file = filename,
           source_path = path,
           file_time = file_time,
           row_in_file = row_number())
}

# One row per export file: who it belongs to, its latest save date, and
# whether it's that observer's newest file.
summarise_files <- function(raw) {
  raw %>%
    mutate(saved_date = as.Date(ISOdate(as.integer(actively_saved_on_year),
                                        as.integer(actively_saved_on_month),
                                        as.integer(actively_saved_on_day)))) %>%
    group_by(observer, source_path, source_file, file_time) %>%
    summarise(
      rows = n(),
      newest_save = if (all(is.na(saved_date))) as.Date(NA) else
        max(saved_date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(observer) %>%
    arrange(desc(newest_save), desc(file_time), desc(source_file),
            .by_group = TRUE) %>%
    mutate(is_newest = row_number() == 1) %>%
    ungroup()
}

# Keep one record per observer x plot: the latest actively-saved version
clean_reads <- function(raw) {
  raw %>%
    mutate(
      actively_saved = coalesce(as.logical(actively_saved), FALSE),
      saved_date = as.Date(ISOdate(as.integer(actively_saved_on_year),
                                   as.integer(actively_saved_on_month),
                                   as.integer(actively_saved_on_day)))
    ) %>%
    group_by(observer, id) %>%
    arrange(desc(actively_saved), desc(saved_date), desc(file_time),
            desc(row_in_file),
            .by_group = TRUE) %>%
    mutate(n_versions = n()) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      fire_from_id = str_match(id, "^(.+?)_(\\d{4})_")[, 2] %>%
        str_replace_all("_", " ") %>% str_to_upper(),
      fire_name = coalesce(fire_from_id, str_to_upper(fire_name)),
      fire_year = as.integer(str_match(id, "_(\\d{4})_")[, 2]),
      plot_location = case_when(
        str_detect(id, "_inside_") ~ "Inside",
        str_detect(id, "_outside_") ~ "Outside",
        TRUE ~ "Other"
      ),
      unsuitable = coalesce(str_detect(str_to_lower(plot_unsuitable), "unsuit"),
                            FALSE),
      jt_pre_fire = suppressWarnings(as.numeric(jt_pre_fire)),
      jt_post_fire = suppressWarnings(as.numeric(jt_post_fire)),
      mortality = if_else(plot_location == "Inside" & !unsuitable &
                            !is.na(jt_pre_fire) & jt_pre_fire > 0 &
                            !is.na(jt_post_fire),
                          (jt_pre_fire - jt_post_fire) / jt_pre_fire * 100,
                          NA_real_),
      pre_veg = str_to_lower(str_trim(pre_vegetation_cover)),
      post_veg = str_to_lower(str_trim(post_vegetation_cover)),
      pre_confidence = str_to_lower(str_trim(pre_confidence)),
      post_confidence = str_to_lower(str_trim(post_confidence)),
      completed = unsuitable | !is.na(jt_pre_fire) | !is.na(jt_post_fire) |
        !is.na(pre_veg) | !is.na(post_veg)
    ) %>%
    select(-fire_from_id)
}

read_plot_order <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) {
    return(tibble(id = character(0), plot_number = integer(0)))
  }
  read.csv(path, stringsAsFactors = FALSE) %>%
    transmute(id = as.character(id), plot_number = row_number())
}

# Low confidence per plot ----
low_confidence_by_plot <- function(reads) {
  reads %>%
    filter(completed, !unsuitable) %>%
    group_by(id) %>%
    summarise(
      n_rated = n(),
      pre_low = sum(pre_confidence %in% "low"),
      post_low = sum(post_confidence %in% "low"),
      .groups = "drop"
    ) %>%
    mutate(
      low_conf_label = paste0(
        if_else(pre_low >= LOW_CONF_MIN, paste0("pre ", pre_low, "/", n_rated), ""),
        if_else(pre_low >= LOW_CONF_MIN & post_low >= LOW_CONF_MIN, ", ", ""),
        if_else(post_low >= LOW_CONF_MIN, paste0("post ", post_low, "/", n_rated), "")
      ),
      low_conf_period = case_when(
        pre_low >= LOW_CONF_MIN & post_low >= LOW_CONF_MIN ~ "Both",
        pre_low >= LOW_CONF_MIN ~ "Pre-fire",
        post_low >= LOW_CONF_MIN ~ "Post-fire",
        TRUE ~ NA_character_
      )
    )
}

# Median, rounded half up (with exactly 2 values this is the mean, rounded
# half up rather than R's round-to-even default -- standard for count data).
median_count <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else floor(median(x) + 0.5)
}

# Agreement rule: counts agree if the absolute difference is no more than
# the allowed difference for their mean: abs_tol trees and/or rel_tol% of the
# mean. rule = "either" allows the larger of the two (as in
# 14_build_consensus.R), "both" the smaller. A tolerance set to NA is not
# used; with neither, counts must match exactly. With exactly two values,
# mean and median are the same number, so this is a direct simplification of
# the three-observer spread/median rule, not a new metric.
allowed_difference <- function(m, abs_tol, rel_tol, rule = "either") {
  tols <- c(abs_tol, rel_tol / 100 * m)
  tols <- tols[!is.na(tols)]
  if (length(tols) == 0) return(0)
  if (rule == "both") min(tols) else max(tols)
}

counts_agree <- function(a, b, abs_tol, rel_tol, rule = "either") {
  !is.na(a) & !is.na(b) &
    abs(a - b) <= allowed_difference(mean(c(a, b)), abs_tol, rel_tol, rule)
}

# Plain-language version of the count rule, for the sidebar and About tab
rule_description <- function(abs_tol, rel_tol, rule) {
  abs_txt <- paste0(format(abs_tol), " tree(s)")
  rel_txt <- paste0(format(rel_tol), "% of their average")
  if (is.na(abs_tol) && is.na(rel_tol)) return("0 trees: counts must match exactly")
  if (is.na(rel_tol)) return(abs_txt)
  if (is.na(abs_tol)) return(rel_txt)
  if (rule == "both") paste(abs_txt, "and", rel_txt, "(both must hold)")
  else paste(abs_txt, "or", rel_txt, "(whichever is larger)")
}

# Plot status and consensus values ----
# Exactly two independent, complete entries are compared directly -- no
# primary/fourth-reviewer distinction, no escalation stage, no persisted
# review ledger. See issue_labels/team_review_cols above for the possible
# disagreement reasons.
plot_status <- function(reads, abs_tol, rel_tol, rule = "either") {
  all_plots <- reads %>% distinct(id, fire_name, fire_year, plot_location)

  empty_row <- function(n_obs, observers, rule_status, issues = "") {
    tibble(
      n_obs = n_obs, observers = observers, rule_status = rule_status,
      issues = issues,
      f_suit = FALSE, f_missing = FALSE, f_vmissing = FALSE,
      f_pre = FALSE, f_post = FALSE, f_vpre = FALSE, f_vpost = FALSE,
      cons_pre = NA_real_, cons_post = NA_real_,
      cons_pre_veg = NA_character_, cons_post_veg = NA_character_,
      cons_mortality = NA_real_
    )
  }

  judged <- reads %>%
    group_by(id) %>%
    group_modify(~ {
      d <- .x[.x$completed, ]
      n_obs <- nrow(d)

      if (n_obs == 0) return(empty_row(0L, "", "Not started"))
      if (n_obs == 1) return(empty_row(1L, d$observer[1], "In progress"))
      if (n_obs > 2) {
        return(empty_row(n_obs, paste(sort(d$observer), collapse = ", "),
                         "Team review", issue_labels[["f_extra"]]) %>%
                 mutate(f_pre = TRUE, f_post = TRUE, f_vpre = TRUE, f_vpost = TRUE))
      }

      d <- d[order(d$observer), ]
      observers_txt <- paste(d$observer, collapse = ", ")
      entry_complete <- d$unsuitable |
        (!is.na(d$jt_pre_fire) & !is.na(d$jt_post_fire) &
           !is.na(d$pre_veg) & !is.na(d$post_veg))

      if (!all(entry_complete)) {
        missing_count <- any(!d$unsuitable & (is.na(d$jt_pre_fire) | is.na(d$jt_post_fire)))
        missing_veg <- any(!d$unsuitable & (is.na(d$pre_veg) | is.na(d$post_veg)))
        issues <- paste(
          c(if (missing_count) issue_labels[["f_missing"]],
            if (missing_veg) issue_labels[["f_vmissing"]]),
          collapse = "; "
        )
        return(empty_row(2L, observers_txt, "Incomplete", issues) %>%
                 mutate(f_missing = missing_count, f_vmissing = missing_veg))
      }

      if (all(d$unsuitable)) {
        return(empty_row(2L, observers_txt, "Excluded"))
      }

      if (d$unsuitable[1] != d$unsuitable[2]) {
        return(empty_row(2L, observers_txt, "Team review", issue_labels[["f_suit"]]) %>%
                 mutate(f_suit = TRUE))
      }

      pre_ok <- counts_agree(d$jt_pre_fire[1], d$jt_pre_fire[2], abs_tol, rel_tol, rule)
      post_ok <- counts_agree(d$jt_post_fire[1], d$jt_post_fire[2], abs_tol, rel_tol, rule)
      vpre_ok <- identical(d$pre_veg[1], d$pre_veg[2])
      vpost_ok <- identical(d$post_veg[1], d$post_veg[2])
      all_ok <- pre_ok && post_ok && vpre_ok && vpost_ok

      issues <- paste(
        c(if (!pre_ok) issue_labels[["f_pre"]],
          if (!post_ok) issue_labels[["f_post"]],
          if (!vpre_ok) issue_labels[["f_vpre"]],
          if (!vpost_ok) issue_labels[["f_vpost"]]),
        collapse = "; "
      )

      pre_val <- if (all_ok) median_count(d$jt_pre_fire) else NA_real_
      post_val <- if (all_ok) median_count(d$jt_post_fire) else NA_real_
      pre_veg_val <- if (all_ok) d$pre_veg[1] else NA_character_
      post_veg_val <- if (all_ok) d$post_veg[1] else NA_character_
      mortality_val <- if (!is.na(pre_val) && pre_val > 0 && !is.na(post_val) &&
                            d$plot_location[1] == "Inside") {
        (pre_val - post_val) / pre_val * 100
      } else {
        NA_real_
      }

      # NOTE: the RHS of each mutate() below must not reuse the target
      # column's own name (e.g. `cons_pre = cons_pre`) -- empty_row() already
      # creates a `cons_pre` column, so dplyr's data masking would resolve
      # the RHS to that existing (NA) column instead of the local variable
      # computed just above, silently discarding it.
      empty_row(2L, observers_txt, if (all_ok) "Accepted" else "Team review", issues) %>%
        mutate(f_pre = !pre_ok, f_post = !post_ok, f_vpre = !vpre_ok, f_vpost = !vpost_ok,
               cons_pre = pre_val, cons_post = post_val,
               cons_pre_veg = pre_veg_val, cons_post_veg = post_veg_val,
               cons_mortality = mortality_val)
    }) %>%
    ungroup()

  all_plots %>%
    left_join(judged, by = "id") %>%
    mutate(
      n_obs = coalesce(n_obs, 0L),
      rule_status = coalesce(rule_status, "Not started"),
      issues = coalesce(issues, ""),
      across(c(f_suit, f_missing, f_vmissing, f_pre, f_post, f_vpre, f_vpost),
             ~ coalesce(.x, FALSE)),
      status = factor(rule_status, levels = status_levels),
      fully_counted = n_obs >= 2
    )
}

# Resolutions ----
# A Team review plot with a saved entry in a resolved export becomes
# Resolved. The resolver only fills in the fields that disagreed: those come
# from the resolution, the rest from the two observers' agreed values. A
# disagreeing field the resolver left blank stays NA and is listed in
# res_missing (highlighted in the app, but the plot is still Resolved). With
# a suitability or >2-observer disagreement, every field needs resolving,
# unless the resolver marks the plot unsuitable.
res_field_labels <- c(pre = "pre-fire count", post = "post-fire count",
                      vpre = "pre vegetation cover", vpost = "post vegetation cover")

apply_resolutions <- function(st, reads, res) {
  if (is.null(res) || nrow(res) == 0) {
    res <- tibble(id = character(0), observer = character(0), unsuitable = logical(0),
                  jt_pre_fire = numeric(0), jt_post_fire = numeric(0),
                  pre_veg = character(0), post_veg = character(0),
                  comments = character(0))
  }
  r <- res %>%
    transmute(id, res_by = observer, res_unsuit = unsuitable,
              res_pre = jt_pre_fire, res_post = jt_post_fire,
              res_vpre = pre_veg, res_vpost = post_veg, res_comments = comments)
  agreed <- reads %>%
    filter(completed, !unsuitable) %>%
    group_by(id) %>%
    summarise(obs_pre = median_count(jt_pre_fire), obs_post = median_count(jt_post_fire),
              obs_vpre = first(na.omit(pre_veg)), obs_vpost = first(na.omit(post_veg)),
              .groups = "drop")
  missing_text <- function(m) {
    vapply(seq_len(nrow(m)), function(i) paste(res_field_labels[m[i, ]], collapse = ", "),
           character(1))
  }

  st %>%
    left_join(r, by = "id") %>%
    left_join(agreed, by = "id") %>%
    mutate(
      resolved = rule_status == "Team review" & !is.na(res_by),
      res_unsuit = coalesce(res_unsuit, FALSE),
      keep = resolved & !res_unsuit,
      res_need_pre = keep & (f_pre | f_suit),
      res_need_post = keep & (f_post | f_suit),
      res_need_vpre = keep & (f_vpre | f_suit),
      res_need_vpost = keep & (f_vpost | f_suit),
      res_missing = missing_text(cbind(
        res_need_pre & is.na(res_pre), res_need_post & is.na(res_post),
        res_need_vpre & is.na(res_vpre), res_need_vpost & is.na(res_vpost))),
      cons_pre = if_else(resolved, if_else(keep, if_else(res_need_pre, res_pre, obs_pre),
                                           NA_real_), cons_pre),
      cons_post = if_else(resolved, if_else(keep, if_else(res_need_post, res_post, obs_post),
                                            NA_real_), cons_post),
      cons_pre_veg = if_else(resolved, if_else(keep, if_else(res_need_vpre, res_vpre, obs_vpre),
                                               NA_character_), cons_pre_veg),
      cons_post_veg = if_else(resolved, if_else(keep, if_else(res_need_vpost, res_vpost, obs_vpost),
                                                NA_character_), cons_post_veg),
      cons_mortality = if_else(
        resolved,
        if_else(keep & plot_location == "Inside" & !is.na(cons_pre) & cons_pre > 0 &
                  !is.na(cons_post), (cons_pre - cons_post) / cons_pre * 100, NA_real_),
        cons_mortality),
      rule_status = if_else(resolved, "Resolved", rule_status),
      status = factor(rule_status, levels = status_levels),
      # A resolution only applies to a Team review plot; elsewhere drop it
      # (it is listed under Data checks instead)
      across(c(res_by, res_pre, res_post, res_vpre, res_vpost, res_comments),
             ~ replace(.x, !resolved, NA)),
      res_unsuit = resolved & res_unsuit
    ) %>%
    select(-keep, -resolved, -starts_with("obs_"))
}

# Each observer's record joined to its plot's flags (for shading)
observer_flags <- function(reads, st) {
  reads %>%
    filter(completed) %>%
    inner_join(select(st, id, status, rule_status, issues, f_suit, f_missing,
                      f_vmissing, f_pre, f_post, f_vpre, f_vpost),
               by = "id")
}

# Per-observer values for the expandable detail rows ----
fmt_count <- function(x) ifelse(is.na(x), "–", format(x, trim = TRUE))

with_conf <- function(value, conf) {
  ifelse(is.na(conf), value,
         paste0(value, " <span class='conf'>conf. ",
                htmltools::htmlEscape(conf), "</span>"))
}

# One row per observer, one block per plot; the fields in question shaded
conflict_long <- function(flags, st, ids) {
  esc <- htmltools::htmlEscape
  meta <- st %>%
    filter(id %in% ids) %>%
    transmute(id, fire_name, meta = paste0(
      esc(plot_location),
      if_else(nzchar(issues), paste0(" · ", esc(issues)), "")
    ))
  flags %>%
    filter(id %in% ids) %>%
    left_join(select(meta, id, meta), by = "id") %>%
    arrange(fire_name, id, observer) %>%
    group_by(id) %>%
    mutate(first_row = row_number() == 1) %>%
    ungroup() %>%
    transmute(
      Plot = ifelse(first_row,
                    paste0("<b>", esc(id), "</b><div class='plot-meta'>", meta, "</div>"),
                    ""),
      Observer = esc(observer),
      `Pre-fire` = with_conf(fmt_count(jt_pre_fire), pre_confidence),
      `Post-fire` = with_conf(fmt_count(jt_post_fire), post_confidence),
      Mortality = ifelse(is.na(mortality), "–", sprintf("%.0f%%", mortality)),
      `Veg pre` = coalesce(pre_veg, "–"),
      `Veg post` = coalesce(post_veg, "–"),
      Suitable = ifelse(unsuitable, "no", "yes"),
      Comments = if_else(is.na(comments), "",
                         paste0("<div class='cmt'>", esc(comments), "</div>")),
      .id = id,
      .first = first_row,
      # An observer who marked the plot unsuitable never contributes to the
      # count/veg checks -- their values are excluded, shaded grey ("muted")
      # rather than shown as agreeing or disputed.
      .pre = (f_pre & rule_status != "Incomplete" | (f_missing & is.na(jt_pre_fire))) & !unsuitable,
      .post = (f_post & rule_status != "Incomplete" | (f_missing & is.na(jt_post_fire))) & !unsuitable,
      .vpre = (f_vpre & rule_status != "Incomplete" | (f_vmissing & is.na(pre_veg))) & !unsuitable,
      .vpost = (f_vpost & rule_status != "Incomplete" | (f_vmissing & is.na(post_veg))) & !unsuitable,
      .suit = f_suit %in% TRUE,
      .excluded = unsuitable
    )
}

plot_summary_line <- function(s) {
  fmt <- function(x) if (is.na(x)) "–" else format(x)
  line <- if (s$status == "Accepted") {
    sprintf("Accepted: pre-fire %s, post-fire %s, vegetation %s → %s%s",
            fmt(s$cons_pre), fmt(s$cons_post), fmt(s$cons_pre_veg), fmt(s$cons_post_veg),
            if (is.na(s$cons_mortality)) "" else
              sprintf(", mortality %.0f%%", s$cons_mortality))
  } else if (s$status == "Incomplete") {
    paste0("Incomplete: ", s$issues)
  } else if (s$status == "Team review") {
    paste0("Team review: ", s$issues)
  } else if (s$status == "Resolved" && s$res_unsuit) {
    sprintf("Resolved by %s: plot marked unsuitable (was in Team review: %s)",
            s$res_by, s$issues)
  } else if (s$status == "Resolved") {
    sprintf("Resolved by %s: pre-fire %s, post-fire %s, vegetation %s → %s%s%s (was in Team review: %s)",
            s$res_by, fmt(s$cons_pre), fmt(s$cons_post), fmt(s$cons_pre_veg),
            fmt(s$cons_post_veg),
            if (is.na(s$cons_mortality)) "" else sprintf(", mortality %.0f%%", s$cons_mortality),
            if (nzchar(s$res_missing)) paste0(". Still missing: ", s$res_missing) else "",
            s$issues)
  } else if (s$status == "Excluded") {
    "Excluded: both observers marked the plot unsuitable"
  } else {
    as.character(s$status)
  }
  paste0("<p class='detail-summary'>", htmltools::htmlEscape(line), "</p>")
}

plot_details_all <- function(flags, st) {
  d <- conflict_long(flags, st, st$id)
  cell_td <- function(value, flag, excluded = FALSE) {
    excluded <- rep_len(excluded, length(value))
    flag <- rep_len(flag, length(value))
    cls <- ifelse(excluded, "muted", ifelse(flag %in% TRUE, "shade", NA))
    paste0("<td", ifelse(is.na(cls), "", paste0(" class='", cls, "'")), ">", value, "</td>")
  }
  d$row_html <- paste0(
    "<tr><td>", d$Observer, "</td>",
    cell_td(d$`Pre-fire`, d$.pre, d$.excluded), cell_td(d$`Post-fire`, d$.post, d$.excluded),
    cell_td(d$Mortality, FALSE, d$.excluded),
    cell_td(d$`Veg pre`, d$.vpre, d$.excluded), cell_td(d$`Veg post`, d$.vpost, d$.excluded),
    cell_td(d$Suitable, d$.suit), "<td>", d$Comments, "</td></tr>")
  rows_by_plot <- tapply(d$row_html, d$.id, paste, collapse = "")

  # The resolution as an extra row under the observers' entries
  res_st <- st[st$status %in% "Resolved", ]
  for (i in seq_len(nrow(res_st))) {
    s <- res_st[i, ]
    res_td <- function(value, need) {
      if (need && is.na(value)) return("<td class='res-missing'>missing</td>")
      paste0("<td>", if (is.na(value)) "–" else htmltools::htmlEscape(format(value)), "</td>")
    }
    rows_by_plot[s$id] <- paste0(
      coalesce(rows_by_plot[s$id], ""),
      "<tr class='res-row'><td><b>Resolved</b> (", htmltools::htmlEscape(s$res_by), ")</td>",
      res_td(s$res_pre, s$res_need_pre), res_td(s$res_post, s$res_need_post),
      "<td>", if (is.na(s$cons_mortality)) "–" else sprintf("%.0f%%", s$cons_mortality), "</td>",
      res_td(s$res_vpre, s$res_need_vpre), res_td(s$res_vpost, s$res_need_vpost),
      "<td>", if (s$res_unsuit) "no" else if (s$f_suit) "yes" else "–", "</td>",
      "<td>", if (is.na(s$res_comments)) "" else
        paste0("<div class='cmt'>", htmltools::htmlEscape(s$res_comments), "</div>"),
      "</td></tr>")
  }

  header <- paste0("<table class='table table-sm detail-table'><thead><tr>",
                   "<th>Observer</th><th>Pre-fire</th><th>Post-fire</th>",
                   "<th>Mortality</th><th>Veg pre</th><th>Veg post</th>",
                   "<th>Suitable</th><th>Comments</th></tr></thead><tbody>")
  out <- vapply(seq_len(nrow(st)), function(i) {
    s <- st[i, ]
    rows <- rows_by_plot[s$id]
    paste0(plot_summary_line(s),
           if (is.na(rows)) "<p class='text-muted mb-0'>No observer has saved counts for this plot yet.</p>"
           else paste0(header, rows, "</tbody></table>"))
  }, character(1))
  setNames(out, st$id)
}

# A datatable where clicking a row expands the plot's details underneath.
expandable_datatable <- function(d, details, filter = "none",
                                 raw_html_cols = character(0), dt_options = list(),
                                 key_col = "Plot", table_class = "display") {
  d$.detail <- unname(details[d[[key_col]]])
  detail_col <- match(".detail", names(d))
  no_escape <- unique(c(match(raw_html_cols, names(d)), detail_col))
  dt_options$columnDefs <- c(dt_options$columnDefs,
                             list(list(visible = FALSE, targets = detail_col - 1)))
  datatable(
    d, fillContainer = FALSE, rownames = FALSE, filter = filter,
    escape = -no_escape, selection = "none",
    class = table_class,
    callback = JS(sprintf(
      "table.on('click', 'tbody tr', function(e) {
         if ($(e.target).is('input.plot-tick')) return;
         var tr = $(this);
         var row = table.row(tr);
         if (!row.data()) return;
         if (row.child.isShown()) {
           row.child.hide();
           tr.removeClass('shown');
         } else {
           table.rows('.shown').every(function() {
             this.child.hide();
             $(this.node()).removeClass('shown');
           });
           row.child(row.data()[%d], 'expanded-detail').show();
           tr.addClass('shown');
         }
       });", detail_col - 1)),
    options = dt_options
  )
}

# Data checks: individual entries worth a second look ----
# Both observers are symmetric now -- no primary/fourth-reviewer distinction,
# so every completed, suitable entry is held to the same requirements.
data_checks <- function(reads) {
  suitable <- reads %>% filter(completed, !unsuitable)

  bind_rows(
    suitable %>% filter(plot_location == "Inside", !is.na(jt_post_fire),
                        !is.na(jt_pre_fire), jt_post_fire > jt_pre_fire) %>%
      mutate(check = check_labels[["post_gt_pre"]]),
    suitable %>% filter(is.na(jt_pre_fire) | is.na(jt_post_fire)) %>%
      mutate(check = check_labels[["count_missing"]]),
    suitable %>% filter(is.na(pre_veg) | is.na(post_veg)) %>%
      mutate(check = check_labels[["veg_missing"]]),
    suitable %>% filter(!pre_veg %in% c(veg_levels, NA) | !post_veg %in% c(veg_levels, NA)) %>%
      mutate(check = check_labels[["veg_bad"]]),
    suitable %>% filter(is.na(pre_confidence) | is.na(post_confidence)) %>%
      mutate(check = check_labels[["conf_missing"]]),
    reads %>% filter(unsuitable, !is.na(jt_pre_fire) | !is.na(jt_post_fire)) %>%
      mutate(check = check_labels[["unsuit_counts"]]),
    reads %>% filter(!completed) %>%
      mutate(check = check_labels[["not_saved"]])
  ) %>%
    select(check, observer, id, fire_name, plot_location, jt_pre_fire,
           pre_confidence, jt_post_fire, post_confidence, pre_veg, post_veg,
           comments) %>%
    arrange(check, observer, id)
}

# "How it works" page ----
about_template <- '
### What the app does

Two observers independently pin and complete the Collect Earth survey for
every plot -- counting trees in pre- and post-fire imagery, assigning a
vegetation cover class, and flagging unsuitable plots. The app compares the
two records directly and builds the consensus dataset for plots that agree.

### Plot status

| Status | Meaning |
|---|---|
| **Not started** | Opened in Collect Earth, but neither observer has saved yet |
| **In progress** | Counted by one of the two observers so far |
| **Incomplete** | Both have saved, but a required field is blank in at least one entry |
| **Excluded** | Both observers marked the plot unsuitable |
| **Accepted** | Suitability, vegetation cover, and counts all agree |
| **Team review** | Both entries are complete, but disagree on something -- see Issues |
| **Resolved** | Was in Team review; the disagreeing fields were settled in a resolved export |

### Agreement rule

- **Counts** (pre- and post-fire): the absolute difference between the two
  observers is no more than **{RULE}**. {EXAMPLE}
- **Vegetation cover** (pre and post) and **suitability** must match
  **exactly** between the two observers -- no majority vote, since there are
  only two of them.
- Agreeing on everything -> **Accepted**; final pre- and post-fire counts
  are the **rounded average** of the two observers\' counts (rounded half up,
  so counts stay whole trees).
- Any disagreement on suitability, vegetation cover, or counts ->
  **Team review** -- resolved by a human, not automatically.
- **Resolutions** come from exports whose file name starts with
  `resolved`. The resolver fills in only the fields that disagreed. A Team
  review plot with a saved resolution becomes **Resolved**: its final values
  are the resolver\'s for the disagreeing fields and the observers\' agreed
  values for the rest. A disagreeing field left blank is highlighted as
  missing. A resolution for a plot that is not in Team review is ignored and
  listed under Data checks.

### Which data are used

{SOURCE} For each observer, {NEWEST} The most recent saved version is used
if a plot was saved more than once.

### Low confidence

Flagged when at least {LOWMIN} observers marked low confidence for the same
period. Can mean poor imagery or a hard-to-count plot -- check the comments.
'

about_markdown <- function(abs_tol, rel_tol, rule, newest_only, source) {
  source_text <- switch(
    source,
    drive = "Exports are read from the shared Google Drive folder, re-checked every minute.",
    local = "Exports are read from the local folder set in `DATA_FOLDER`.",
    "No shared folder is set, so only uploaded files are used."
  )
  newest_text <- if (isTRUE(newest_only)) {
    paste("only their newest export is used (the one with the latest save",
          "date inside it).")
  } else {
    "all of their exports are merged."
  }
  tol_at <- function(m) format(round(allowed_difference(m, abs_tol, rel_tol, rule), 1))
  example_text <- paste0("For example, the allowed difference is ", tol_at(5),
                         " for plots averaging 5 trees and ", tol_at(30),
                         " for an average of 30.")
  md <- about_template
  fill <- c("{SOURCE}" = source_text, "{NEWEST}" = newest_text,
            "{EXAMPLE}" = example_text,
            "{RULE}" = rule_description(abs_tol, rel_tol, rule),
            "{LOWMIN}" = format(LOW_CONF_MIN))
  for (key in names(fill)) md <- gsub(key, fill[[key]], md, fixed = TRUE)
  md
}


# UI ----

theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF", fg = "#1F2622",
  primary = "#4E6A3E", secondary = "#C9A55A",
  base_font = font_google("Source Sans 3", local = FALSE),
  heading_font = font_google("Source Serif 4", local = FALSE),
  "border-radius" = "4px"
)

app_css <- "
.status-bar { display: flex; border-radius: 4px; overflow: hidden; }
.status-seg { flex: 1; padding: 20px 8px; text-align: center; cursor: pointer;
              font-size: 1.1rem; user-select: none; }
.status-seg:hover { filter: brightness(1.07); }
.status-seg.active { box-shadow: inset 0 -5px 0 rgba(255,255,255,.9); }
.status-seg .n { display: inline-block; min-width: 2.4em; margin-left: .45em;
                 padding: 1px 8px; border: 1.5px solid currentColor;
                 border-radius: 999px; font-variant-numeric: tabular-nums; }
.status-note { color: #5f6660; font-size: .92rem; margin-top: .6rem; }
.conf { color: #7b817c; font-size: .85em; }
table.dataTable td { vertical-align: top; }
table.dataTable th { word-break: normal; overflow-wrap: normal; }
table.dataTable > tbody > tr { cursor: pointer; }
.about { max-width: 46rem; padding: .5rem 0 2rem; line-height: 1.55; }
.about h3 { margin-top: 1.8rem; font-size: 1.3rem; }
.about table { margin: .6rem 0 1rem; }
.about td, .about th { padding: .35rem .8rem .35rem 0; vertical-align: top;
                       border-bottom: 1px solid #e3e5e2; }
.plot-meta { color: #5f6660; font-size: .85em; margin-top: 2px; }
.summary-line { margin: .8rem 0 0; }
.cmt { min-width: 340px; white-space: normal; }
#plot_table table.dataTable.display tbody tr:hover > *,
#plot_table table.dataTable.hover tbody tr:hover > * {
  box-shadow: none !important;
}
#plot_table tbody > tr:hover {
  outline: none !important;
  box-shadow: none !important;
}
table.dataTable > tbody > tr.shown { box-shadow: inset 3px 0 0 #4E6A3E; }
table.dataTable > tbody > tr.child > td { background: #FAFAF8; padding: .8rem 1.2rem; cursor: default; }
#team_review_table table.dataTable.display tbody tr:hover > *,
#team_review_table table.dataTable.hover tbody tr:hover > * {
  box-shadow: none !important;
}
#team_review_table tbody > tr:hover {
  outline: none !important;
  box-shadow: none !important;
}
.detail-summary { margin: 0 0 .6rem; }
.detail-table { margin: 0; }
.detail-table td, .detail-table th { padding: .3rem .7rem .3rem 0; vertical-align: top; }
.detail-table td.shade { background-color: #F4DED6; }
.detail-table td.muted { color: #AEA99C; }
input.plot-tick { width: 1.05rem; height: 1.05rem; cursor: pointer; }
.detail-table tr.res-row td { border-top: 2px solid #5E5A8C; }
.detail-table td.res-missing { background-color: #F3DFA2; }
.res-missing { background-color: #F3DFA2; padding: 1px 6px; border-radius: 3px; }
"

# Overview ticks: report each checkbox change to the server
tick_js <- "
$(document).on('change', 'input.plot-tick', function() {
  Shiny.setInputValue('plot_tick', {id: $(this).data('id'), checked: this.checked},
                      {priority: 'event'});
});
"

ui <- page_navbar(
  title = "Joshua tree counts · observer agreement (v1, 2 reviewers)",
  theme = theme,
  fillable = FALSE,
  header = tags$head(tags$style(HTML(app_css)), tags$script(HTML(tick_js))),
  sidebar = sidebar(
    width = 310,
    uiOutput("folder_status"),
    fileInput("files", "Add exports by upload (optional)",
              multiple = TRUE, accept = ".csv"),
    helpText("Uploads are added for this session only."),
    checkboxInput("newest_only", "Use only each observer's newest file",
                  value = TRUE),
    tags$hr(),
    tags$strong("Agreement rule"),
    checkboxInput("use_abs", "Use tree tolerance", value = TRUE),
    conditionalPanel("input.use_abs",
      numericInput("abs_tol", "Tolerance: trees", value = 2, min = 0, step = 1)),
    checkboxInput("use_rel", "Use % tolerance", value = TRUE),
    conditionalPanel("input.use_rel",
      numericInput("rel_tol", "Tolerance: % of average", value = 20, min = 0, step = 5)),
    conditionalPanel("input.use_abs && input.use_rel",
      radioButtons("tol_rule", "Counts agree if within",
                   choices = c("either tolerance (looser)" = "either",
                               "both tolerances (stricter)" = "both"),
                   selected = "either")),
    uiOutput("tolerance_example"),
    helpText("Untick a tolerance to leave it out; with neither, counts must",
             "match exactly. Vegetation cover and suitability must always",
             "match exactly. 14_build_consensus.R uses \"either\"."),
    helpText("Any disagreement goes to Team review -- there is no fourth",
             "reviewer or automatic escalation in this version."),
    tags$hr(),
    selectInput("fire_filter", "Fires", choices = NULL, multiple = TRUE),
    helpText("Leave empty to include all fires.")
  ),

  nav_panel(
    "Overview",
    uiOutput("status_bar"),
    uiOutput("status_note"),
    card(
      card_header(textOutput("plot_table_title", inline = TRUE)),
      helpText("Click a row to show each observer's counts for that plot.",
               "Tick the box to add a plot to your selection."),
      div(class = "d-flex flex-wrap align-items-center gap-3 mb-2",
          checkboxInput("ticked_only", "Show ticked plots only", width = "auto"),
          textOutput("ticked_count", inline = TRUE),
          actionLink("clear_ticks", "Clear ticks"),
          downloadButton("dl_ticked", "Download ticked (CSV)", class = "btn-sm")),
      DTOutput("plot_table", fill = FALSE)
    ),
    card(card_header("Observers"),
         helpText("Which export file is used for each observer, and how many",
                  "plots they have completed."),
         DTOutput("observer_files", fill = FALSE))
  ),

  nav_panel(
    "Summary plots",
    card(card_header("Plot status by fire"),
         plotOutput("status_by_fire_plot", height = "500px"),
         uiOutput("conflict_rate_text")),
    card(card_header("Mortality so far, by fire (inside plots)"),
         plotOutput("mortality_plot", height = "500px"),
         helpText("Pooled mortality: total trees that died ÷ total",
                  "pre-fire trees, over inside plots whose pre- and post-fire",
                  "consensus counts are resolved.")),
    card(card_header("Observer A vs. observer B"),
         plotOutput("pairwise_plot", height = "420px"),
         helpText("One panel each for pre-fire and post-fire counts. Points",
                  "on the dashed line match exactly; color shows whether",
                  "that plot was Accepted or sent to Team review."))
  ),

  nav_panel(
    "Low confidence",
    card(
      card_header("Plots with shared low confidence, by fire"),
      helpText("Plots where at least", LOW_CONF_MIN, "observers marked low",
               "confidence for the same count. Poor imagery is one possible",
               "reason; check the comments to see why."),
      uiOutput("low_confidence_summary"),
      plotOutput("low_confidence_plot", height = "460px")
    ),
    div(class = "d-flex justify-content-between align-items-center mb-2",
        helpText(class = "mb-0", "Click a row to show each observer's counts and comments."),
        downloadButton("dl_low_conf", "Download CSV (with coordinates)",
                       class = "btn-sm")),
    accordion(
      open = FALSE,
      accordion_panel(
        textOutput("low_pre_title", inline = TRUE), value = "low_pre",
        DTOutput("low_confidence_pre_table", fill = FALSE)
      ),
      accordion_panel(
        textOutput("low_post_title", inline = TRUE), value = "low_post",
        DTOutput("low_confidence_post_table", fill = FALSE)
      )
    )
  ),

  nav_panel(
    "Team review",
    card(
      card_header("Plots needing team review"),
      helpText("Both observers completed the plot but disagreed on",
               "suitability, vegetation cover, or counts. Resolved outside",
               "this app -- there is no fourth-reviewer or joint-review",
               "step in this version."),
      layout_columns(
        col_widths = c(4),
        selectInput("issue_filter", "Issue",
                    choices = c("All", unname(issue_labels[team_review_cols])))
      ),
      p("Click a row to see both observers' entries underneath",
        "(disagreeing fields shaded red)."),
      DTOutput("team_review_table", fill = FALSE)
    ),
    card(
      card_header(textOutput("resolved_title", inline = TRUE)),
      helpText("Team review plots settled in a resolved export (file name",
               "starting with \"resolved\"). Bold values were set by the",
               "resolver; the others are the observers' agreed values.",
               "Highlighted: a disagreeing field the resolver left blank.",
               "Click a row to see the observers' entries and the resolution."),
      uiOutput("resolved_files"),
      DTOutput("resolved_table", fill = FALSE)
    )
  ),

  nav_panel(
    "Data checks",
    card(card_header("Data checks: entries worth a second look"),
         helpText("Individual entries to fix in Collect Earth, e.g. missing",
                  "values or a post-fire count above the pre-fire count.",
                  "Click a column header to sort (e.g. by Observer) or use",
                  "the search boxes to filter."),
         layout_columns(
           col_widths = c(4),
           selectInput("check_filter", "Issue", choices = c("All", unname(check_labels)))
         ),
         DTOutput("checks_table", fill = FALSE)),
    card(
      card_header("Downloads"),
      p(tags$strong("Consensus data:"), "a live snapshot, one row per plot,",
        "with the consensus counts, vegetation cover, mortality and",
        "low-confidence flags so far. This is for checking progress, not",
        "necessarily the file to analyze once Team review plots remain."),
      downloadButton("dl_consensus", "Consensus data"),
      p(class = "mt-3", tags$strong("Observer counts:"),
        "one row per observer per plot (their own, independent values)."),
      downloadButton("dl_reads", "Observer counts"),
      p(class = "mt-3", tags$strong("Plot status:"),
        "every plot with its status, issues and consensus details."),
      downloadButton("dl_status", "Plot status")
    )
  ),

  nav_panel(
    "How it works",
    div(class = "about", uiOutput("about_page"))
  )
)

# Server ----

server <- function(input, output, session) {

  folder_state <- reactiveVal(empty_folder())
  folder_error <- reactiveVal(NULL)
  folder_checked <- reactiveVal(NULL)

  observe({
    input$refresh_folder
    src <- folder_source()
    if (src == "none") return()
    invalidateLater(if (src == "drive") drive_poll_ms else folder_poll_ms)
    result <- tryCatch({
      if (src == "drive") list_drive_files(DRIVE_FOLDER)
      else list_folder_files(DATA_FOLDER)
    }, error = function(e) e)
    if (inherits(result, "error")) {
      msg <- gsub("\033\\[[0-9;]*m", "", conditionMessage(result))
      msg <- gsub("\\s+", " ", msg)
      if (grepl("insufficient authentication scopes|insufficientPermissions", msg)) {
        msg <- paste("R is signed in to Google without Drive access. Stop the app,",
                     "run unlink(\"~/Library/Caches/gargle\", recursive = TRUE)",
                     "then googledrive::drive_auth() and allow Drive access.")
      } else if (grepl("File not found|404", msg)) {
        msg <- paste("Drive folder not found. Check the DRIVE_FOLDER link and that",
                     "this Google account can open the folder.")
      }
      folder_error(msg)
    } else {
      folder_error(NULL)
      folder_state(result)
    }
    folder_checked(Sys.time())
  })

  folder_files <- reactive(folder_state()$exports)

  all_files <- reactive({
    uploaded <- if (is.null(input$files)) NULL else
      data.frame(name = input$files$name, datapath = input$files$datapath,
                 mtime = Sys.time())
    rbind(folder_files(), uploaded)
  })

  has_data <- reactive(nrow(all_files()) > 0)

  plot_order <- reactiveFileReader(60000, session, PLOT_ORDER_PATH, read_plot_order)

  output$folder_status <- renderUI({
    src <- folder_source()
    if (src == "none") {
      return(helpText("No shared folder set. To read a Google Drive folder",
                      "automatically, set DRIVE_FOLDER at the top of app_v1.R."))
    }
    label <- if (src == "drive") "Google Drive folder" else
      paste("Folder:", basename(DATA_FOLDER))
    f <- folder_files()
    div(class = "small mb-3",
        tags$strong(label), tags$br(),
        if (!is.null(folder_error())) {
          div(class = "text-danger", "Couldn't read it: ", folder_error())
        } else if (is.null(folder_checked())) {
          span(class = "text-muted", "Checking…")
        } else {
          paste0(nrow(f), " export file", if (nrow(f) == 1) "" else "s",
                 if (nrow(f) > 0) paste0(", newest ",
                                         format(max(f$mtime), "%d %b %H:%M",
                                                tz = Sys.timezone())))
        },
        tags$br(),
        span(class = "text-muted",
             sprintf("Re-checked every %s. ",
                     if (src == "drive") "minute" else "15 seconds")),
        actionLink("refresh_folder", "Check now"))
  })

  raw_all <- reactive({
    files <- all_files()
    req(nrow(files) > 0)
    raw <- pmap_dfr(files, function(name, datapath, mtime) {
      tryCatch(read_ce_file(datapath, name, mtime), error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 15)
        NULL
      })
    })
    validate(need(nrow(raw) > 0, "No readable Collect Earth files found."))
    raw
  })

  # Observer exports vs. resolved exports (see resolved_pattern)
  raw_obs <- reactive(filter(raw_all(), !is_resolved_file(source_file)))

  file_info <- reactive({
    summarise_files(raw_obs()) %>%
      mutate(used = is_newest | !isTRUE(input$newest_only))
  })

  reads_all <- reactive({
    used_paths <- file_info()$source_path[file_info()$used]
    clean_reads(filter(raw_obs(), source_path %in% used_paths))
  })

  # Every resolved export is used; per plot, the latest saved resolution
  resolved_all <- reactive({
    r <- filter(raw_all(), is_resolved_file(source_file))
    if (nrow(r) == 0) return(NULL)
    clean_reads(r) %>%
      filter(completed) %>%
      group_by(id) %>%
      arrange(desc(saved_date), desc(file_time), .by_group = TRUE) %>%
      slice(1) %>%
      ungroup()
  })

  resolved <- reactive({
    r <- resolved_all()
    if (!is.null(r) && length(input$fire_filter) > 0) r <- filter(r, fire_name %in% input$fire_filter)
    r
  })

  observeEvent(reads_all(), {
    fires <- sort(unique(reads_all()$fire_name))
    updateSelectInput(session, "fire_filter", choices = fires,
                      selected = intersect(input$fire_filter, fires))
  })

  reads <- reactive({
    r <- reads_all()
    if (length(input$fire_filter) > 0) r <- filter(r, fire_name %in% input$fire_filter)
    r
  })

  # A tolerance that is ticked off is NA, i.e. not used
  settings <- reactive({
    list(abs_tol = if (isFALSE(input$use_abs)) NA_real_ else
           max(0, input$abs_tol %||% 2, na.rm = TRUE),
         rel_tol = if (isFALSE(input$use_rel)) NA_real_ else
           max(0, input$rel_tol %||% 20, na.rm = TRUE),
         rule = input$tol_rule %||% "either")
  })

  low_conf <- reactive(low_confidence_by_plot(reads()))

  status <- reactive({
    s <- settings()
    plot_status(reads(), s$abs_tol, s$rel_tol, s$rule) %>%
      apply_resolutions(reads(), resolved()) %>%
      left_join(select(low_conf(), id, pre_low, post_low, n_rated,
                       low_conf_label, low_conf_period), by = "id") %>%
      mutate(low_conf_label = coalesce(low_conf_label, ""))
  })

  flags <- reactive(observer_flags(reads(), status()))

  details <- reactive(plot_details_all(flags(), status()))

  fire_order <- reactive({
    present <- unique(status()$fire_name)
    ordered <- SURVEY_FIRE_ORDER[SURVEY_FIRE_ORDER %in% present]
    rev(c(ordered, sort(setdiff(present, ordered))))
  })

  # Overview ----
  selected_status <- reactiveVal(NULL)
  observeEvent(input$status_click, {
    if (identical(selected_status(), input$status_click)) {
      selected_status(NULL)
    } else {
      selected_status(input$status_click)
    }
  })

  output$status_bar <- renderUI({
    if (!has_data()) {
      return(card(card_body(
        h4("No exports yet"),
        p("Observers save their Collect Earth exports into the shared folder",
          "(or upload them in the sidebar). The app then compares every plot",
          "counted by both observers.")
      )))
    }
    counts <- table(status()$status)
    segs <- map(status_levels, function(s) {
      tags$div(
        class = paste("status-seg", if (identical(selected_status(), s)) "active"),
        style = sprintf("background:%s;color:%s;", status_colors[[s]],
                        status_text[[s]]),
        title = status_help[[s]],
        onclick = sprintf(
          "Shiny.setInputValue('status_click', '%s', {priority: 'event'})", s),
        str_to_lower(s), tags$span(class = "n", counts[[s]])
      )
    })
    tags$div(class = "status-bar", segs)
  })

  output$status_note <- renderUI({
    req(has_data())
    full <- status() %>% filter(fully_counted)
    n_full <- nrow(full)
    n_review <- sum(full$rule_status == "Team review")
    txt <- if (n_full == 0) "No plots have been counted by both observers yet." else
      sprintf("%d plots counted by both observers: %d (%.0f%%) need team review.",
              n_full, n_review, 100 * n_review / n_full)
    div(class = "status-note", txt)
  })

  output$plot_table_title <- renderText({
    title <- if (is.null(selected_status())) "All plots" else paste("Plots:", selected_status())
    if (isTRUE(input$ticked_only)) paste(title, "(ticked only)") else title
  })

  # Ticked plots: kept by plot ID until the app closes, so they survive
  # table redraws when the data refreshes
  ticked <- reactiveVal(character(0))
  observeEvent(input$plot_tick, {
    id <- as.character(input$plot_tick$id)
    ticked(if (isTRUE(input$plot_tick$checked)) union(ticked(), id) else setdiff(ticked(), id))
  })
  observeEvent(input$clear_ticks, ticked(character(0)))

  output$ticked_count <- renderText(paste(length(ticked()), "ticked"))

  plot_comments <- reactive({
    esc <- htmltools::htmlEscape
    reads() %>%
      filter(!is.na(comments), nzchar(str_trim(comments))) %>%
      arrange(observer) %>%
      group_by(id) %>%
      summarise(Comments = paste0("<div class='cmt'>",
                                  paste0("<b>", esc(observer), ":</b> ",
                                         esc(str_trim(comments)), collapse = "<br>"),
                                  "</div>"),
                .groups = "drop")
  })

  plot_table_data <- reactive({
    st <- status()
    showing <- selected_status()
    if (!is.null(showing)) st <- filter(st, status == showing)
    # Only redraw on a tick when filtering by ticks; otherwise the checkbox
    # already shows the change, and a redraw would reset the page
    tk <- if (isTRUE(input$ticked_only)) ticked() else isolate(ticked())
    if (isTRUE(input$ticked_only)) st <- filter(st, id %in% tk)
    d <- st %>%
      arrange(status, fire_name, id) %>%
      left_join(plot_comments(), by = "id") %>%
      left_join(plot_order(), by = "id") %>%
      transmute(` ` = sprintf("<input type='checkbox' class='plot-tick' data-id='%s'%s>",
                              htmltools::htmlEscape(id, attribute = TRUE),
                              if_else(id %in% tk, " checked", "")),
                `CE #` = plot_number, Plot = id, Fire = fire_name,
                Location = plot_location,
                Status = status,
                Counted = paste(n_obs, "of 2"),
                Issues = issues,
                `Low confidence` = low_conf_label,
                Comments = coalesce(Comments, ""))
    if (!is.null(showing) && showing == "Excluded") d <- select(d, -Issues)
    d
  })

  output$plot_table <- renderDT({
    d <- plot_table_data()
    comments_col <- match("Comments", names(d))
    expandable_datatable(
      d, details(), filter = "top", raw_html_cols = c(" ", "Comments"),
      dt_options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE,
                        order = list(list(1, "asc")),
                        columnDefs = list(list(width = "320px", targets = comments_col - 1),
                                          list(orderable = FALSE, searchable = FALSE,
                                               targets = 0)))
    ) %>%
      formatStyle("Status", backgroundColor = styleEqual(status_levels,
                                                         unname(status_colors)),
                  color = styleEqual(status_levels, unname(status_text)))
  })

  # Team review ----
  team_review_ids <- reactive({
    st <- filter(status(), status == "Team review")
    if (input$issue_filter != "All") {
      st <- filter(st, str_detect(issues, fixed(input$issue_filter)))
    }
    st$id
  })

  output$team_review_table <- renderDT({
    ids <- team_review_ids()
    validate(need(length(ids) > 0, "No plots currently need team review."))
    d <- status() %>%
      filter(id %in% ids) %>%
      arrange(fire_name, id) %>%
      transmute(Plot = id, Fire = fire_name, Location = plot_location, Issues = issues)
    expandable_datatable(d, details(), dt_options = list(pageLength = 15, scrollX = TRUE))
  })

  # Resolved ----
  resolved_plots <- reactive(filter(status(), status == "Resolved"))

  output$resolved_title <- renderText({
    n <- nrow(resolved_plots())
    n_missing <- sum(nzchar(resolved_plots()$res_missing))
    paste0("Resolved plots (", n, if (n_missing > 0) paste0(", ", n_missing, " with fields missing"), ")")
  })

  output$resolved_files <- renderUI({
    files <- unique(filter(raw_all(), is_resolved_file(source_file))$source_file)
    if (length(files) == 0) return(helpText("No resolved export found yet."))
    helpText(tags$em("Resolved export(s) in use:", paste(files, collapse = "; ")))
  })

  output$resolved_table <- renderDT({
    d <- resolved_plots()
    validate(need(nrow(d) > 0, "No Team review plots have been resolved yet."))
    esc <- htmltools::htmlEscape
    # Resolver's values in bold; a disagreeing field left blank highlighted
    show <- function(value, need, missing) {
      ifelse(missing, "<span class='res-missing'>missing</span>",
             ifelse(is.na(value), "–",
                    ifelse(need, paste0("<b>", esc(value), "</b>"), esc(value))))
    }
    d <- d %>%
      arrange(fire_name, id) %>%
      transmute(
        Plot = id, Fire = fire_name, Location = plot_location,
        `Was in review for` = issues, `Resolved by` = res_by,
        Suitable = ifelse(res_unsuit, "<b>no</b>", "yes"),
        `Pre-fire` = show(fmt_count(cons_pre), res_need_pre, res_need_pre & is.na(res_pre)),
        `Post-fire` = show(fmt_count(cons_post), res_need_post, res_need_post & is.na(res_post)),
        `Veg pre` = show(cons_pre_veg, res_need_vpre, res_need_vpre & is.na(res_vpre)),
        `Veg post` = show(cons_post_veg, res_need_vpost, res_need_vpost & is.na(res_vpost)),
        Mortality = ifelse(is.na(cons_mortality), "–", sprintf("%.0f%%", cons_mortality))
      )
    expandable_datatable(d, details(),
                         raw_html_cols = c("Suitable", "Pre-fire", "Post-fire",
                                           "Veg pre", "Veg post"),
                         dt_options = list(pageLength = 15, scrollX = TRUE))
  })

  # Low confidence ----
  flagged_low <- reactive({
    status() %>% filter(!is.na(low_conf_period))
  })

  output$low_confidence_summary <- renderUI({
    d <- flagged_low()
    if (nrow(d) == 0) return(NULL)
    n_pre <- sum(d$low_conf_period %in% c("Pre-fire", "Both"))
    n_post <- sum(d$low_conf_period %in% c("Post-fire", "Both"))
    div(class = "status-note",
        sprintf("%d plots flagged for the pre-fire count and %d for the post-fire count, across %d fires.",
                n_pre, n_post, n_distinct(d$fire_name)))
  })

  output$low_confidence_plot <- renderPlot({
    d <- flagged_low()
    validate(need(nrow(d) > 0, paste("No plots yet where", LOW_CONF_MIN,
                                     "or more observers marked low confidence.")))
    counts <- bind_rows(
      d %>% filter(low_conf_period %in% c("Pre-fire", "Both")) %>%
        transmute(fire_name, type = "Pre-fire count"),
      d %>% filter(low_conf_period %in% c("Post-fire", "Both")) %>%
        transmute(fire_name, type = "Post-fire count")
    ) %>%
      count(fire_name, type, name = "n") %>%
      mutate(fire_name = factor(fire_name, levels = rev(fire_order())),
             type = factor(type, levels = c("Pre-fire count", "Post-fire count"))) %>%
      complete(fire_name, type, fill = list(n = 0L)) %>%
      filter(fire_name %in% unique(d$fire_name))
    pal <- c("Pre-fire count" = "#D55E00", "Post-fire count" = "#0072B2")
    ggplot(counts, aes(fire_name, n, fill = type)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(aes(label = if_else(n > 0, as.character(n), "")),
                position = position_dodge(width = 0.8),
                vjust = -0.5, size = 3.6, color = "#0b0b0b") +
      scale_fill_manual(values = pal) +
      scale_x_discrete(labels = str_to_title) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = NULL, y = "Plots flagged") +
      viz_theme(14, vertical = TRUE)
  })

  low_conf_table <- function(which) {
    low_col <- if (which == "pre") "pre_low" else "post_low"
    d <- status() %>%
      filter(!is.na(.data[[low_col]]), .data[[low_col]] >= LOW_CONF_MIN) %>%
      arrange(desc(.data[[low_col]]), fire_name, id)
    tibble(Plot = d$id, Fire = d$fire_name, Location = d$plot_location,
           `Observers (low)` = paste(d[[low_col]], "of", d$n_rated),
           Status = d$status)
  }

  output$low_pre_title <- renderText({
    paste0("Low confidence: pre-fire count (", nrow(low_conf_table("pre")), " plots)")
  })
  output$low_post_title <- renderText({
    paste0("Low confidence: post-fire count (", nrow(low_conf_table("post")), " plots)")
  })

  render_low_table <- function(which) {
    renderDT({
      d <- low_conf_table(which)
      validate(need(nrow(d) > 0, paste("No plots yet where", LOW_CONF_MIN,
                                       "or more observers marked low confidence.")))
      expandable_datatable(d, details(),
                           dt_options = list(pageLength = 20, scrollX = TRUE)) %>%
        formatStyle("Status", backgroundColor = styleEqual(status_levels,
                                                           unname(status_colors)),
                    color = styleEqual(status_levels, unname(status_text)))
    })
  }
  output$low_confidence_pre_table <- render_low_table("pre")
  output$low_confidence_post_table <- render_low_table("post")

  # One row per plot: coordinates and all observers' comments, for downloads
  plot_coords <- reactive({
    reads() %>%
      group_by(id) %>%
      summarise(longitude = first(na.omit(location_x)),
                latitude = first(na.omit(location_y)),
                comments = paste(na.omit(comments), collapse = " | "),
                .groups = "drop")
  })

  output$dl_low_conf <- downloadHandler(
    filename = function() paste0("jt_low_confidence_", Sys.Date(), ".csv"),
    content = function(file) {
      flagged_low() %>%
        left_join(plot_coords(), by = "id") %>%
        arrange(fire_name, id) %>%
        transmute(plot_id = id, fire = fire_name, fire_year,
                  location = plot_location, low_confidence_count = low_conf_period,
                  pre_low_observers = pre_low, post_low_observers = post_low,
                  observers_rated = n_rated, status, longitude, latitude, comments) %>%
        write.csv(file, row.names = FALSE, na = "")
    }
  )

  # Summary plots ----
  output$status_by_fire_plot <- renderPlot({
    order <- fire_order()
    validate(need(length(order) > 0, "No plots yet."))
    totals <- status() %>%
      count(fire_name, name = "total") %>%
      left_join(status() %>% filter(status == "Team review") %>%
                  count(fire_name, name = "review"), by = "fire_name") %>%
      mutate(review = coalesce(review, 0L),
             label = if_else(review > 0,
                             paste0(total, "  (", review, " to review)"),
                             as.character(total)),
             fire_name = factor(fire_name, levels = order))
    d <- status() %>%
      count(fire_name, status, name = "n") %>%
      mutate(fire_name = factor(fire_name, levels = order),
             status = factor(status, levels = status_levels))
    ggplot(d, aes(fire_name, n, fill = status)) +
      geom_col(width = 0.65, color = "#fcfcfb", linewidth = 0.3) +
      geom_text(data = totals, aes(fire_name, total, label = label),
                inherit.aes = FALSE, hjust = -0.1, size = 3.6, color = "#0b0b0b") +
      scale_fill_manual(values = status_colors, breaks = rev(status_levels)) +
      scale_x_discrete(labels = str_to_title) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
      coord_flip() +
      labs(x = NULL, y = "Plots") +
      viz_theme(14)
  })

  output$conflict_rate_text <- renderUI({
    full <- status() %>% filter(fully_counted)
    n_full <- nrow(full)
    n_review <- sum(full$rule_status == "Team review")
    p(class = "summary-line", tags$strong("Plots needing team review: "),
      if (n_full == 0) "–" else
        sprintf("%.1f%% (%d of %d plots counted by both observers)",
                100 * n_review / n_full, n_review, n_full))
  })

  output$mortality_plot <- renderPlot({
    order <- fire_order()
    mort <- status() %>%
      filter(plot_location == "Inside", !is.na(cons_mortality)) %>%
      group_by(fire_name) %>%
      summarise(n = n(), pre = sum(cons_pre), dead = sum(cons_pre - cons_post),
                .groups = "drop") %>%
      mutate(pooled = 100 * dead / pre,
             label = sprintf("%.0f%%  (%d plots, %d trees)", pooled, n, as.integer(pre)),
             fire_name = factor(fire_name, levels = order))
    validate(need(nrow(mort) > 0, "No inside plots with resolved pre/post counts yet."))
    ggplot(mort, aes(fire_name, pooled)) +
      geom_col(width = 0.6, fill = "#4E6A3E") +
      geom_text(aes(label = label), hjust = -0.08, size = 3.6, color = "#0b0b0b") +
      scale_x_discrete(labels = str_to_title, drop = TRUE) +
      scale_y_continuous(limits = c(0, 135), breaks = seq(0, 100, 25),
                         expand = expansion(mult = c(0, 0))) +
      coord_flip() +
      labs(x = NULL, y = "Mortality (%)") +
      viz_theme(14)
  })

  # Observer A vs. observer B: a scatter of the two observers' raw counts for
  # every fully-counted, suitable plot -- a direct fit for a strict pairwise
  # comparison (unlike the old "each observer vs. combined median" view,
  # which only made sense with 3+ observers).
  output$pairwise_plot <- renderPlot({
    r <- reads() %>%
      semi_join(status() %>% filter(fully_counted), by = "id") %>%
      filter(!unsuitable) %>%
      arrange(id, observer) %>%
      group_by(id) %>%
      filter(n() == 2) %>%
      summarise(
        pre_a = jt_pre_fire[1], pre_b = jt_pre_fire[2],
        post_a = jt_post_fire[1], post_b = jt_post_fire[2],
        .groups = "drop"
      ) %>%
      left_join(select(status(), id, status), by = "id")
    long <- bind_rows(
      transmute(r, measure = "Pre-fire count", a = pre_a, b = pre_b, status),
      transmute(r, measure = "Post-fire count", a = post_a, b = post_b, status)
    ) %>%
      filter(!is.na(a), !is.na(b)) %>%
      mutate(measure = factor(measure, levels = c("Pre-fire count", "Post-fire count")))
    validate(need(nrow(long) > 0, "No plots counted by both observers yet."))
    ggplot(long, aes(a, b, color = status)) +
      geom_abline(slope = 1, intercept = 0, linetype = 2, color = "#898781") +
      geom_point(size = 2.4, alpha = 0.8) +
      facet_wrap(~measure, scales = "free") +
      scale_color_manual(values = status_colors, breaks = status_levels, drop = TRUE) +
      labs(x = "Observer A's count", y = "Observer B's count", color = NULL) +
      viz_theme(14) +
      theme(panel.grid.major = element_line(color = "#e1e0d9", linewidth = 0.4))
  })

  # Data ----
  output$observer_files <- renderDT({
    counts <- reads_all() %>%
      group_by(observer) %>%
      summarise(`Plots counted` = sum(completed),
                `Marked unsuitable` = sum(unsuitable),
                `Other operator names` = paste(unique(na.omit(other_operator)),
                                               collapse = ", "),
                .groups = "drop")
    file_info() %>%
      group_by(observer) %>%
      summarise(
        `File in use` = paste(source_file[used], collapse = "; "),
        `Newest save` = format(max(newest_save[used], na.rm = TRUE)),
        `Older files ignored` = sum(!used),
        .groups = "drop"
      ) %>%
      left_join(counts, by = "observer") %>%
      rename(Observer = observer) %>%
      datatable(fillContainer = FALSE, rownames = FALSE,
                options = list(dom = "t", scrollX = TRUE))
  })

  output$checks_table <- renderDT({
    unused <- resolved()
    if (!is.null(unused)) {
      unused <- unused %>%
        filter(!id %in% status()$id[status()$status %in% "Resolved"]) %>%
        mutate(check = check_labels[["res_unused"]]) %>%
        select(check, observer, id, fire_name, plot_location, jt_pre_fire,
               pre_confidence, jt_post_fire, post_confidence, pre_veg, post_veg,
               comments)
    }
    d <- bind_rows(data_checks(reads()), unused)
    if (input$check_filter != "All") {
      d <- filter(d, check == input$check_filter)
    }
    validate(need(nrow(d) > 0, "No entries match the current filter."))
    d %>%
      rename(Check = check, Observer = observer, Plot = id, Fire = fire_name,
             Location = plot_location, `Pre count` = jt_pre_fire,
             `Pre confidence` = pre_confidence, `Post count` = jt_post_fire,
             `Post confidence` = post_confidence,
             `Veg pre` = pre_veg, `Veg post` = post_veg, Comments = comments) %>%
      mutate(Check = factor(Check), Observer = factor(Observer)) %>%
      datatable(fillContainer = FALSE, rownames = FALSE, filter = "top",
                options = list(pageLength = 20, scrollX = TRUE,
                               columnDefs = list(list(width = "260px", targets = 0),
                                                 list(searchable = FALSE, targets = 0),
                                                 list(width = "110px",
                                                      targets = c(5, 6, 7, 8)))))
  })

  output$dl_ticked <- downloadHandler(
    filename = function() paste0("jt_ticked_plots_", Sys.Date(), ".csv"),
    content = function(file) {
      status() %>%
        filter(id %in% ticked()) %>%
        left_join(plot_order(), by = "id") %>%
        left_join(plot_coords(), by = "id") %>%
        arrange(plot_number, id) %>%
        transmute(ce_number = plot_number, plot_id = id, fire = fire_name, fire_year,
                  location = plot_location, status, n_obs, observers, issues,
                  pre_fire = cons_pre, post_fire = cons_post,
                  pre_veg = cons_pre_veg, post_veg = cons_post_veg,
                  low_confidence = low_conf_label, longitude, latitude, comments) %>%
        write.csv(file, row.names = FALSE, na = "")
    }
  )

  output$dl_consensus <- downloadHandler(
    filename = function() paste0("jt_consensus_v1_", Sys.Date(), ".csv"),
    content = function(file) {
      status() %>%
        arrange(fire_name, id) %>%
        transmute(plot_id = id, fire = fire_name, fire_year, location = plot_location,
                  status, n_obs, observers,
                  pre_fire = cons_pre, post_fire = cons_post,
                  pre_veg = cons_pre_veg, post_veg = cons_post_veg,
                  mortality_pct = round(cons_mortality, 1),
                  pre_low_confidence = coalesce(pre_low, 0L),
                  post_low_confidence = coalesce(post_low, 0L),
                  issues, resolved_by = res_by, resolution_missing = res_missing) %>%
        write.csv(file, row.names = FALSE, na = "")
    }
  )

  output$dl_reads <- downloadHandler(
    filename = function() paste0("jt_observer_counts_v1_", Sys.Date(), ".csv"),
    content = function(file) {
      reads() %>%
        left_join(select(status(), id, status), by = "id") %>%
        select(-row_in_file, -source_path) %>%
        write.csv(file, row.names = FALSE, na = "")
    }
  )

  output$dl_status <- downloadHandler(
    filename = function() paste0("jt_plot_status_v1_", Sys.Date(), ".csv"),
    content = function(file) {
      status() %>%
        select(-starts_with("f_")) %>%
        write.csv(file, row.names = FALSE, na = "")
    }
  )

  # Sidebar ----
  output$tolerance_example <- renderUI({
    s <- settings()
    tol <- function(m) format(round(allowed_difference(m, s$abs_tol, s$rel_tol, s$rule), 1))
    helpText(tags$em(sprintf(
      "Counts agree within %s. With these settings: a difference of up to %s tree(s) for plots averaging 5 trees, %s for 15, %s for 30.",
      rule_description(s$abs_tol, s$rel_tol, s$rule), tol(5), tol(15), tol(30))))
  })

  # How it works ----
  output$about_page <- renderUI({
    s <- settings()
    shiny::markdown(about_markdown(s$abs_tol, s$rel_tol, s$rule, input$newest_only, folder_source()))
  })
}

shinyApp(ui, server)
