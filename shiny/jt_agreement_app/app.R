# -----------------------------------------------------------------------
# Joshua tree counts: observer agreement app
# Reads each observer's Collect Earth exports, checks field-by-field
# agreement among the primary observers, uses a fourth reviewer only for
# disputed fields, flags remaining conflicts for joint review, and produces
# the consensus dataset (median agreeing counts, majority vegetation cover).
#
# Tabs: Overview | Summary plots | Low confidence | Joint review | Data checks | How it works
#
# Packages: shiny, bslib, DT, dplyr, tidyr, ggplot2, stringr, purrr, here,
#           googledrive (only if reading from Google Drive)
# Run:      shiny::runApp("jt_agreement_app")
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
# Option A (recommended): read straight from Google Drive, no Drive app needed.
#   Paste the folder's link from your browser, e.g.
#   "https://drive.google.com/drive/folders/1AbCdEfGhIjKlMnOp"
#   The first time, run googledrive::drive_auth() in the R console and
#   sign in (tick the box that allows access to your Drive files).
DRIVE_FOLDER <- "1xC1P6iVUxQd2cq3tSNHYuaojeWyqxvGL"
options(gargle_oauth_email = TRUE)  # reuse the saved Google sign-in, don't ask

# Option B: a local folder, e.g. one synced by Google Drive for desktop.
#   Only used when DRIVE_FOLDER is "".
DATA_FOLDER <- ""

# Every CSV with "collectedData" in its name is read, including subfolders.
# With neither set, the app works with the upload button only.

# Joint review decisions ----
# A cumulative CSV ledger committed to this git repo (not Drive, not edited
# in-app). Each downloaded template starts from the existing ledger, preserves
# every prior row/decision, and adds or refreshes plots that currently need
# joint review. Fill only the blank, still-disputed cells, save over this
# file, and commit the change -- git history is the record of every decision.
REVIEW_SHEET <- "joint_reviews"
LOCAL_REVIEW_PATH <- here("shiny", "jt_agreement_app", paste0(REVIEW_SHEET, ".csv"))

# Collect Earth plot number ----
# The CSV used to build whichever .cep observers currently have loaded is
# written in the same row order used for the KML -- Collect Earth's own
# ${placemark_index+1} (the number shown in Google Earth Pro's Places panel,
# and in Collect Earth Desktop's own plot list) is just that row's 1-based
# position. Reading it here lets a plot in this app be cross-referenced back
# to Collect Earth/Google Earth Pro by the same number.
# IMPORTANT: update ACTIVE_SURVEY whenever the survey observers are actively
# counting against changes (e.g. "test" while testing, then
# "joshuatree_survey_recent" once the real survey is in use) -- it must
# match output/collect_earth/placemark_<ACTIVE_SURVEY>.csv from whichever
# scripts/12*_collect_earth_export*.R was last run for the live survey.
ACTIVE_SURVEY <- "test"
PLOT_ORDER_PATH <- here("output", "collect_earth", paste0("placemark_", ACTIVE_SURVEY, ".csv"))

# Primary observers ----
# The first-stage agreement check must be based on the same three independent
# observers for every plot, so values that already agreed before a fourth
# review can be frozen. If you know their exact Collect Earth operator names,
# list them here, e.g. c("maya", "observer2", "observer3").
#
# Leave this empty to infer the primary observers as the n_required observers
# with the greatest number of completed plots. A fourth reviewer normally
# counts only flagged plots, so their coverage is much lower.
PRIMARY_OBSERVERS <- character(0)

# Low confidence ----
# A plot is flagged when at least this many observers marked low confidence
# for the same count (pre- or post-fire).
LOW_CONF_MIN <- 2

drive_poll_ms <- 60000   # how often to re-check Google Drive
folder_poll_ms <- 15000  # how often to re-check a local folder

export_pattern <- "collectedData.*\\.csv$"

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

# Google Drive: list the folder, download new or changed files to a cache
drive_cache <- file.path(tempdir(), "jt_drive_cache")

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

  # exports: one cached copy per file version, so unchanged files aren't
  # re-downloaded
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

# Status categories, in display order -- data-collection stages first
# (not started, in progress, incomplete), then the two tier-1 outcomes
# (decided straight from the three observers), then the escalation path
# (fourth review, then joint review) for whatever's left.
status_levels <- c("Not started", "In progress", "Incomplete", "Excluded",
                   "Accepted", "Needs 4th review", "Conflict")
status_help <- c(
  "Not started"      = "Opened in Collect Earth, but nobody has saved counts yet",
  "In progress"      = "Counted by some, but not yet all, of the three observers",
  "Incomplete"       = "A required field is blank, or one observer split from the other two on suitability",
  "Excluded"         = "A majority of observers marked the plot unsuitable",
  "Accepted"         = "Counts and vegetation cover agree (directly, or after a fourth review)",
  "Needs 4th review" = "The three observers didn't agree; a fourth reviewer's count is needed",
  "Conflict"         = "Still unresolved after a fourth review; needs joint review"
)
status_colors <- c(
  "Not started"      = "#B8B3A2",
  "In progress"      = "#C9A55A",
  "Incomplete"       = "#7C8DA6",
  "Excluded"         = "#6F7773",
  "Accepted"         = "#4E6A3E",
  "Needs 4th review" = "#C17A3D",
  "Conflict"         = "#A3452F"
)
status_text <- c("Not started" = "#2B2B2B", "In progress" = "#2B2B2B",
                 "Incomplete" = "#FFFFFF",
                 "Excluded" = "#FFFFFF", "Accepted" = "#FFFFFF",
                 "Needs 4th review" = "#FFFFFF", "Conflict" = "#FFFFFF")

# Suitability uses only the primary observers. A majority unsuitable
# excludes the plot; a minority split goes back for recheck and never uses a
# fourth reviewer. With three observers: 0 unsuitable continues, 1 is
# Incomplete, and 2 or 3 is Excluded.
issue_labels <- c(
  f_suit     = "Suitability minority split",
  f_missing  = "Missing count",
  f_pre      = "Pre-fire count",
  f_post     = "Post-fire count",
  f_vmissing = "Missing veg cover",
  f_vpre     = "Pre veg cover",
  f_vpost    = "Post veg cover"
)
# Which of those ever reach which tier -- a recheck issue always resolves
# into Incomplete (back to the original observer) before a disagreement
# issue is even evaluated, so it can never appear on a plot that reaches
# Conflict. Used to build the right Issue filter per tab (Data checks vs.
# Joint review) so a choice there can't be picked that will never match.
recheck_cols <- c("f_suit", "f_missing", "f_vmissing")
disagreement_cols <- c("f_pre", "f_post", "f_vpre", "f_vpost")
veg_levels <- c("low", "medium", "high")

check_labels <- c(
  post_gt_pre   = "Post-fire count higher than pre-fire (inside plot)",
  count_missing = "Pre- or post-fire count missing",
  veg_missing   = "Vegetation cover missing",
  veg_bad       = "Unrecognised vegetation cover class",
  conf_missing  = "Confidence missing",
  unsuit_counts = "Marked unsuitable but has counts",
  not_saved     = "Opened but nothing saved"
)

# Fire display order, matching the master sampling datasheet's row order
# (output/recent and output/historical *_sites_coordinates.csv) -- so charts
# read in the same order as the field spreadsheet, not an arbitrary sort.
# fire_label there (e.g. "elk_trail_2022") is transformed the same way
# clean_reads() derives fire_name from a site id, so the two line up.
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
# A quiet theme: hairline gridlines one step off the surface, muted axis
# text, no chart border -- the data carries the contrast, not the chrome.
# `vertical = TRUE` for vertical bars (gridlines across, angled fire names).
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
  # filename prefix). Assigned per file, not per row, because stray operator
  # names such as "shared" can appear inside one person's export.
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
# whether it's that observer's newest file. "Newest" = latest save date
# recorded inside the file; ties go to the file uploaded/modified later.
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
      # Mortality only for burned (inside) plots; on unburned outside plots
      # a one-tree difference in a sparse plot swings it wildly
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


# Identify the primary observers ----
# Explicit names are safest if they are known. Otherwise infer the primary
# observers from plot coverage: the three regular observers should have many
# more completed plots than a selective fourth reviewer.
choose_primary_observers <- function(reads, n_required,
                                     configured = PRIMARY_OBSERVERS) {
  configured <- unique(str_trim(configured[nzchar(str_trim(configured))]))
  if (length(configured) > 0) {
    if (length(configured) != n_required) {
      stop(sprintf("PRIMARY_OBSERVERS must contain exactly %d names (currently %d).",
                   n_required, length(configured)))
    }
    missing <- setdiff(configured, unique(reads$observer))
    if (length(missing) > 0) {
      stop(sprintf("PRIMARY_OBSERVERS not found in the loaded exports: %s",
                   paste(missing, collapse = ", ")))
    }
    return(configured)
  }

  reads %>%
    group_by(observer) %>%
    summarise(n_completed = sum(completed), .groups = "drop") %>%
    arrange(desc(n_completed), observer) %>%
    slice_head(n = n_required) %>%
    pull(observer)
}

# Agreement rules ----
# Suitability is evaluated from the primary observers only: a majority
# unsuitable excludes the plot; a minority split goes back for recheck.
# Counts: the primary observers must agree -- the spread of their
# counts (highest minus lowest) no more than the larger of the tree
# tolerance and the % tolerance of their median. The median (not mean) is
# used so the tolerance itself isn't pulled around by the very outlier it's
# supposed to be judging -- with three values the median is just the middle
# one, unaffected by how far the min or max (the two values that define the
# spread) happen to sit.
# If they don't, and a fourth reviewer's count is added, the plot resolves
# as soon as ANY three of the (now four) counts mutually agree -- the
# left-out value is the discarded outlier. Missing entries (fewer suitable
# counts than required) never resolve.
# resolved_trio() tries every n_required-sized combination of the available
# values and keeps the tightest-agreeing one. With exactly n_required
# values (the normal three-observer case) there is only one combination to
# try, so this is identical to the original "do all three agree" check.
resolved_trio <- function(x, abs_tol, rel_tol, n_required) {
  x <- x[!is.na(x)]
  if (n_required < 2 || length(x) < n_required) return(list(ok = NA, value = NA_real_))
  best <- NULL
  best_spread <- Inf
  for (idx in combn(length(x), n_required, simplify = FALSE)) {
    sub <- x[idx]
    tol <- max(abs_tol, rel_tol / 100 * median(sub))
    spread <- max(sub) - min(sub)
    if (spread <= tol && spread < best_spread) {
      best <- sub
      best_spread <- spread
    }
  }
  if (is.null(best)) list(ok = FALSE, value = NA_real_) else
    list(ok = TRUE, value = median_count(best))
}

resolved_ok <- function(x, abs_tol, rel_tol, n_required) {
  resolved_trio(x, abs_tol, rel_tol, n_required)$ok
}
resolved_value <- function(x, abs_tol, rel_tol, n_required) {
  resolved_trio(x, abs_tol, rel_tol, n_required)$value
}

# Vegetation cover: the class chosen by a majority (at least two observers,
# no tie)
majority_class <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  counts <- table(x)
  top <- max(counts)
  if (top >= 2 && sum(counts == top) == 1) names(counts)[counts == top] else
    NA_character_
}

# Median, rounded half up so consensus counts stay whole trees (only matters
# with an even number of observers, e.g. 12 and 13 -> 13)
median_count <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else floor(median(x) + 0.5)
}

# Diagnostic only (doesn't affect status/consensus): for a three-observer
# count disagreement, tells apart a single outlier (two observers already
# close enough to agree on their own, one far off -- a fourth reviewer who
# lands near that pair resolves it) from an evenly spread disagreement (no
# two of the three are close enough to agree -- a fourth reviewer has to
# land within tolerance of two *particular* observers to resolve it, so it's
# more likely to end up in joint review). Defined for the three primary
# values that triggered the original disagreement.
spread_pattern <- function(x, abs_tol, rel_tol, n_required) {
  x <- sort(x[!is.na(x)])
  if (n_required != 3 || length(x) != 3) return(NA_character_)
  pair_ok <- function(a, b) abs(a - b) <= max(abs_tol, rel_tol / 100 * median(c(a, b)))
  if (pair_ok(x[1], x[2]) || pair_ok(x[2], x[3])) "Outlier" else "Even spread"
}

# Joint review sheet ----
# joint_reviews.csv has exactly seven columns: plot_id, pre_fire, post_fire,
# pre_veg, post_veg, conflict_fields, note. A field's value is only ever used
# as a consensus override when that field is *currently* still unresolved
# (need_joint_pre etc., computed live below from the real data) -- so a
# pre-filled reference value sitting in an undisputed cell is automatically
# ignored, never treated as a human decision.
empty_reviews <- function() {
  tibble(id = character(0), rv_pre = numeric(0), rv_post = numeric(0),
         rv_pre_veg = character(0), rv_post_veg = character(0),
         rv_note = character(0), rv_started = logical(0))
}

read_reviews <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) return(empty_reviews())
  r <- read.csv(path, colClasses = "character", na.strings = c("", "NA"),
                check.names = FALSE)
  names(r) <- str_to_lower(str_trim(names(r)))
  if (!"plot_id" %in% names(r)) {
    stop("The review sheet needs a column called plot_id.")
  }
  # A freshly created/header-only review file is valid: there simply are no
  # joint-review decisions yet. Return the correctly typed empty table before
  # trying to add compatibility columns (assigning a length-1 NA column to a
  # zero-row data frame triggers "replacement has 1 row, data has 0").
  if (nrow(r) == 0) return(empty_reviews())

  for (col in c("pre_fire", "post_fire", "pre_veg", "post_veg", "note")) {
    if (!col %in% names(r)) r[[col]] <- NA_character_
  }

  r %>%
    transmute(
      id = str_trim(plot_id),
      rv_pre = suppressWarnings(as.numeric(pre_fire)),
      rv_post = suppressWarnings(as.numeric(post_fire)),
      rv_pre_veg = str_to_lower(str_trim(pre_veg)),
      rv_post_veg = str_to_lower(str_trim(post_veg)),
      rv_note = note,
      rv_started = !is.na(rv_pre) | !is.na(rv_post) | !is.na(rv_pre_veg) |
        !is.na(rv_post_veg) | (!is.na(note) & nzchar(str_trim(note)))
    ) %>%
    filter(!is.na(id), nzchar(id)) %>%
    group_by(id) %>% slice_tail(n = 1) %>% ungroup()
}

# Collect Earth plot number ----
# 1-based row position in the CSV used to build the active .cep, which is
# the same number Collect Earth/Google Earth Pro shows for that plot (see
# PLOT_ORDER_PATH above). Empty if the file isn't there.
read_plot_order <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) {
    return(tibble(id = character(0), plot_number = integer(0)))
  }
  read.csv(path, stringsAsFactors = FALSE) %>%
    transmute(id = as.character(id), plot_number = row_number())
}

# Low confidence per plot ----
# How many of the observers who judged the plot suitable marked low
# confidence, for each count. Used on the Overview, the Low confidence tab
# and in the consensus download.
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

# Plot status and consensus values ----
# A field is evaluated in two stages:
#   1. the primary observers only;
#   2. if (and only if) stage 1 disagreed, add the fourth-review value(s).
# Fields that resolved at stage 1 are frozen and cannot be changed by a later
# reviewer. Counts resolve when any n_required values agree within tolerance;
# vegetation resolves by majority.
count_stage <- function(primary_values, reviewer_values,
                        abs_tol, rel_tol, n_required) {
  p <- primary_values[!is.na(primary_values)]
  r <- reviewer_values[!is.na(reviewer_values)]

  initial <- resolved_trio(p, abs_tol, rel_tol, n_required)
  initial_disputed <- identical(initial$ok, FALSE)
  reviewer_attempted <- initial_disputed && length(r) > 0

  auto <- if (isTRUE(initial$ok)) {
    initial
  } else if (reviewer_attempted) {
    resolved_trio(c(p, r), abs_tol, rel_tol, n_required)
  } else {
    list(ok = initial$ok, value = NA_real_)
  }

  list(
    primary_n = length(p),
    initial_ok = initial$ok,
    initial_value = initial$value,
    initial_disputed = initial_disputed,
    reviewer_attempted = reviewer_attempted,
    auto_ok = auto$ok,
    auto_value = auto$value,
    spread = if (initial_disputed)
      spread_pattern(p, abs_tol, rel_tol, n_required) else NA_character_
  )
}

veg_stage <- function(primary_values, reviewer_values, n_required) {
  p <- primary_values[!is.na(primary_values)]
  r <- reviewer_values[!is.na(reviewer_values)]

  initial_value <- if (length(p) >= n_required) majority_class(p) else NA_character_
  initial_ok <- if (length(p) < n_required) NA else !is.na(initial_value)
  initial_disputed <- identical(initial_ok, FALSE)
  reviewer_attempted <- initial_disputed && length(r) > 0

  auto_value <- if (isTRUE(initial_ok)) {
    initial_value
  } else if (reviewer_attempted) {
    majority_class(c(p, r))
  } else {
    NA_character_
  }
  auto_ok <- if (isTRUE(initial_ok)) TRUE else if (reviewer_attempted) !is.na(auto_value) else initial_ok

  list(
    primary_n = length(p),
    initial_ok = initial_ok,
    initial_value = initial_value,
    initial_disputed = initial_disputed,
    reviewer_attempted = reviewer_attempted,
    auto_ok = auto_ok,
    auto_value = auto_value
  )
}

plot_status <- function(reads, abs_tol, rel_tol, n_required, reviews,
                        primary_observers) {
  all_plots <- reads %>% distinct(id, fire_name, fire_year, plot_location)

  judged <- reads %>%
    group_by(id) %>%
    group_modify(~ {
      d <- .x
      done <- d$completed
      is_primary <- d$observer %in% primary_observers
      primary_done <- done & is_primary
      reviewer_done <- done & !is_primary

      n_counts <- sum(done)
      n_primary <- sum(primary_done)
      n_reviewers <- sum(reviewer_done)
      n_unsuitable_primary <- sum(d$unsuitable[primary_done])
      n_unsuitable <- sum(d$unsuitable[done])
      n_suitable <- n_counts - n_unsuitable

      # Suitability is decided from the primary observers only. A majority
      # unsuitable excludes the plot immediately; a minority unsuitable split
      # goes back for recheck and is Incomplete. With three observers this is
      # exactly: 0 unsuitable -> continue, 1 -> Incomplete, 2/3 -> Excluded.
      excluded_rule <- n_primary >= n_required &&
        n_unsuitable_primary > n_primary / 2
      f_suit <- n_primary >= n_required && n_unsuitable_primary > 0 &&
        !excluded_rule

      primary_suitable <- primary_done & !d$unsuitable
      reviewer_suitable <- reviewer_done & !d$unsuitable

      pre_primary <- d$jt_pre_fire[primary_suitable]
      post_primary <- d$jt_post_fire[primary_suitable]
      vpre_primary <- d$pre_veg[primary_suitable]
      vpost_primary <- d$post_veg[primary_suitable]

      pre_review <- d$jt_pre_fire[reviewer_suitable]
      post_review <- d$jt_post_fire[reviewer_suitable]
      vpre_review <- d$pre_veg[reviewer_suitable]
      vpost_review <- d$post_veg[reviewer_suitable]

      pre <- count_stage(pre_primary, pre_review, abs_tol, rel_tol, n_required)
      post <- count_stage(post_primary, post_review, abs_tol, rel_tol, n_required)
      vpre <- veg_stage(vpre_primary, vpre_review, n_required)
      vpost <- veg_stage(vpost_primary, vpost_review, n_required)

      # Missing fields are only required from the primary observers at stage 1.
      # A fourth reviewer is required only for fields that actually disagreed.
      n_primary_suitable <- sum(primary_suitable)
      f_missing <- n_primary >= n_required && !excluded_rule && !f_suit &&
        n_primary_suitable > 0 &&
        (pre$primary_n < n_primary_suitable | post$primary_n < n_primary_suitable)
      f_vmissing <- n_primary >= n_required && !excluded_rule && !f_suit &&
        n_primary_suitable > 0 &&
        (vpre$primary_n < n_primary_suitable | vpost$primary_n < n_primary_suitable)

      initial_f_pre <- isTRUE(pre$initial_disputed)
      initial_f_post <- isTRUE(post$initial_disputed)
      initial_f_vpre <- isTRUE(vpre$initial_disputed)
      initial_f_vpost <- isTRUE(vpost$initial_disputed)
      any_initial_disagreement <- initial_f_pre | initial_f_post |
        initial_f_vpre | initial_f_vpost

      fourth_complete <- !any_initial_disagreement ||
        ((!initial_f_pre | pre$reviewer_attempted) &
         (!initial_f_post | post$reviewer_attempted) &
         (!initial_f_vpre | vpre$reviewer_attempted) &
         (!initial_f_vpost | vpost$reviewer_attempted))

      # f_* disagreement flags now mean "this field is still unresolved by
      # the automatic process". Before a fourth value arrives, an initially
      # disputed field therefore remains TRUE; after a successful 3-of-4 it
      # becomes FALSE and is frozen at that automatic consensus.
      f_pre <- initial_f_pre && !isTRUE(pre$auto_ok)
      f_post <- initial_f_post && !isTRUE(post$auto_ok)
      f_vpre <- initial_f_vpre && !isTRUE(vpre$auto_ok)
      f_vpost <- initial_f_vpost && !isTRUE(vpost$auto_ok)

      med_pre <- median_count(d$jt_pre_fire[done & !d$unsuitable])
      med_post <- median_count(d$jt_post_fire[done & !d$unsuitable])

      tibble(
        n_counts = n_counts,
        n_primary = n_primary,
        n_reviewers = n_reviewers,
        observers = paste(sort(d$observer[done]), collapse = ", "),
        n_unsuitable = n_unsuitable,
        n_unsuitable_primary = n_unsuitable_primary,
        n_suitable = n_suitable,
        pre_n = sum(!is.na(d$jt_pre_fire[done & !d$unsuitable])),
        post_n = sum(!is.na(d$jt_post_fire[done & !d$unsuitable])),
        vpre_n = sum(!is.na(d$pre_veg[done & !d$unsuitable])),
        vpost_n = sum(!is.na(d$post_veg[done & !d$unsuitable])),
        primary_pre_n = pre$primary_n,
        primary_post_n = post$primary_n,
        primary_vpre_n = vpre$primary_n,
        primary_vpost_n = vpost$primary_n,
        primary_pre_resolved = pre$initial_value,
        primary_post_resolved = post$initial_value,
        primary_pre_veg = vpre$initial_value,
        primary_post_veg = vpost$initial_value,
        pre_resolved = pre$auto_value,
        post_resolved = post$auto_value,
        maj_pre_veg = vpre$auto_value,
        maj_post_veg = vpost$auto_value,
        med_pre = med_pre,
        med_post = med_post,
        pre_spread = pre$spread,
        post_spread = post$spread,
        initial_f_pre = initial_f_pre,
        initial_f_post = initial_f_post,
        initial_f_vpre = initial_f_vpre,
        initial_f_vpost = initial_f_vpost,
        fourth_pre = pre$reviewer_attempted,
        fourth_post = post$reviewer_attempted,
        fourth_vpre = vpre$reviewer_attempted,
        fourth_vpost = vpost$reviewer_attempted,
        fourth_complete = fourth_complete,
        f_suit = f_suit,
        f_missing = f_missing,
        f_pre = f_pre,
        f_post = f_post,
        f_vmissing = f_vmissing,
        f_vpre = f_vpre,
        f_vpost = f_vpost,
        excluded_rule = excluded_rule,
        any_initial_disagreement = any_initial_disagreement
      )
    }) %>%
    ungroup()

  flag_cols <- names(issue_labels)
  judged$any_issue <- rowSums(as.matrix(judged[flag_cols])) > 0
  judged$recheck_issues <- apply(as.matrix(judged[recheck_cols]), 1, function(row) {
    paste(issue_labels[recheck_cols[row]], collapse = "; ")
  })
  judged$disagreement_issues <- apply(as.matrix(judged[disagreement_cols]), 1, function(row) {
    paste(issue_labels[disagreement_cols[row]], collapse = "; ")
  })
  judged$any_recheck <- judged$f_missing %in% TRUE | judged$f_vmissing %in% TRUE |
    judged$f_suit %in% TRUE
  judged$any_disagreement <- judged$f_pre %in% TRUE | judged$f_post %in% TRUE |
    judged$f_vpre %in% TRUE | judged$f_vpost %in% TRUE

  st <- all_plots %>%
    left_join(judged, by = "id") %>%
    mutate(
      n_counts = coalesce(n_counts, 0L),
      n_primary = coalesce(n_primary, 0L),
      n_reviewers = coalesce(n_reviewers, 0L),
      n_suitable = coalesce(n_suitable, 0L),
      rule_status = case_when(
        n_primary == 0 ~ "Not started",
        n_primary < n_required ~ "In progress",
        excluded_rule ~ "Excluded",
        any_recheck ~ "Incomplete",
        any_initial_disagreement & !fourth_complete ~ "Needs 4th review",
        any_disagreement ~ "Conflict",
        TRUE ~ "Accepted"
      ),
      issues = case_when(
        rule_status %in% c("Excluded", "Not started") ~ "",
        rule_status == "Incomplete" ~ coalesce(recheck_issues, ""),
        TRUE ~ coalesce(disagreement_issues, "")
      ),
      spread_label = case_when(
        f_pre %in% TRUE & f_post %in% TRUE & !is.na(pre_spread) & !is.na(post_spread) ~
          paste0("Pre: ", pre_spread, "; Post: ", post_spread),
        f_pre %in% TRUE & !is.na(pre_spread) ~ paste0("Pre: ", pre_spread),
        f_post %in% TRUE & !is.na(post_spread) ~ paste0("Post: ", post_spread),
        TRUE ~ ""
      )
    ) %>%
    left_join(reviews, by = "id") %>%
    mutate(
      reviewed = coalesce(rv_started, FALSE),
      need_joint_pre = rule_status == "Conflict" & f_pre %in% TRUE,
      need_joint_post = rule_status == "Conflict" & f_post %in% TRUE,
      need_joint_vpre = rule_status == "Conflict" & f_vpre %in% TRUE,
      need_joint_vpost = rule_status == "Conflict" & f_vpost %in% TRUE,
      valid_rv_pre_veg = !is.na(rv_pre_veg) & rv_pre_veg %in% veg_levels,
      valid_rv_post_veg = !is.na(rv_post_veg) & rv_post_veg %in% veg_levels,
      review_complete = rule_status == "Conflict" &
        (!need_joint_pre | !is.na(rv_pre)) &
        (!need_joint_post | !is.na(rv_post)) &
        (!need_joint_vpre | valid_rv_pre_veg) &
        (!need_joint_vpost | valid_rv_post_veg),
      status = case_when(
        rule_status == "Conflict" & review_complete ~ "Accepted",
        TRUE ~ rule_status
      ),
      status = factor(status, levels = status_levels),
      resolved = rule_status == "Conflict" & review_complete,
      accepted = status == "Accepted",
      # Backward-compatible aliases used by the review-template code.
      need_pre = need_joint_pre,
      need_post = need_joint_post,
      need_vpre = need_joint_vpre,
      need_vpost = need_joint_vpost,
      # Only a field that genuinely reached joint review can be overridden by
      # rv_*. Stale or accidental entries in the CSV cannot alter a field that
      # resolved automatically among the primary observers or after 3-of-4.
      field_available = n_primary >= n_required & !excluded_rule & status != "Excluded",
      cons_pre = case_when(
        !field_available ~ NA_real_,
        need_joint_pre & !is.na(rv_pre) ~ rv_pre,
        need_joint_pre ~ NA_real_,
        TRUE ~ pre_resolved
      ),
      cons_post = case_when(
        !field_available ~ NA_real_,
        need_joint_post & !is.na(rv_post) ~ rv_post,
        need_joint_post ~ NA_real_,
        TRUE ~ post_resolved
      ),
      cons_pre_veg = case_when(
        !field_available ~ NA_character_,
        need_joint_vpre & valid_rv_pre_veg ~ rv_pre_veg,
        need_joint_vpre ~ NA_character_,
        TRUE ~ maj_pre_veg
      ),
      cons_post_veg = case_when(
        !field_available ~ NA_character_,
        need_joint_vpost & valid_rv_post_veg ~ rv_post_veg,
        need_joint_vpost ~ NA_character_,
        TRUE ~ maj_post_veg
      ),
      cons_mortality = if_else(plot_location == "Inside" &
                                 !is.na(cons_pre) & cons_pre > 0 & !is.na(cons_post),
                               (cons_pre - cons_post) / cons_pre * 100, NA_real_),
      fully_counted = n_primary >= n_required,
      resolved_via_4th = status == "Accepted" & !resolved &
        any_initial_disagreement & fourth_complete
    ) %>%
    select(-valid_rv_pre_veg, -valid_rv_post_veg)
  st
}

# Each observer's record joined to its plot's flags (for shading)
observer_flags <- function(reads, st) {
  reads %>%
    filter(completed) %>%
    inner_join(select(st, id, status, rule_status, fully_counted, f_suit,
                      f_missing, f_pre, f_post, f_vmissing, f_vpre, f_vpost,
                      med_pre, med_post),
               by = "id")
}

# Per-observer values for the expandable detail rows ----
fmt_count <- function(x) ifelse(is.na(x), "\u2013", format(x, trim = TRUE))

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
      if_else(nzchar(issues), paste0(" \u00b7 ", esc(issues)), ""),
      if_else(resolved, paste0(" \u00b7 <b>resolved</b>",
                               if_else(is.na(rv_note), "", paste0(": ", esc(rv_note)))), "")
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
      Mortality = ifelse(is.na(mortality), "\u2013", sprintf("%.0f%%", mortality)),
      `Veg pre` = coalesce(pre_veg, "\u2013"),
      `Veg post` = coalesce(post_veg, "\u2013"),
      Suitable = ifelse(unsuitable, "no", "yes"),
      Comments = if_else(is.na(comments), "",
                         paste0("<div class='cmt'>", esc(comments), "</div>")),
      # hidden helper columns
      .id = id,
      .first = first_row,
      # An observer who marked the plot unsuitable never contributes to the
      # count/veg checks either way -- their values are excluded from the
      # calculation regardless of whether they happen to match. Shading
      # their cells anyway (just because the plot-level flag is TRUE) made
      # it look like their value was part of the disagreement, even when it
      # numerically agreed with one of the suitable observers. Gate on
      # !unsuitable so only the values that actually fed the check get
      # shaded.
      # The disagreement half (f_pre/f_post/f_vpre/f_vpost) is also gated on
      # the plot not being Incomplete: that comparison ran on whatever
      # subset happened to be suitable, which isn't a meaningful verdict yet
      # while something still needs to go back to the original observer --
      # showing it as a red disagreement here would read as a conclusion
      # the app hasn't actually reached. The blank-cell half still shows
      # regardless, since that's the actual, current reason to act.
      .pre = ((f_pre & rule_status != "Incomplete") |
                (f_missing & is.na(jt_pre_fire))) & !unsuitable,
      .post = ((f_post & rule_status != "Incomplete") |
                 (f_missing & is.na(jt_post_fire))) & !unsuitable,
      .vpre = ((f_vpre & rule_status != "Incomplete") |
                 (f_vmissing & is.na(pre_veg))) & !unsuitable,
      .vpost = ((f_vpost & rule_status != "Incomplete") |
                  (f_vmissing & is.na(post_veg))) & !unsuitable,
      .suit = f_suit %in% TRUE,
      # An observer who marked the plot unsuitable: their counts/veg classes
      # are greyed out everywhere except Comments, rather than shaded or
      # left plain, so it reads as "excluded from every check" rather than
      # either "part of the disagreement" or "an agreeing value".
      .excluded = unsuitable
    )
}

# Expandable rows ----
# The detail shown under a row when it's clicked: a one-line summary plus
# each observer's values. Built once for every plot whenever the data or
# settings change (not once per table render), and looked up by plot id.
plot_summary_line <- function(s) {
  fmt <- function(x) if (is.na(x)) "\u2013" else format(x)
  line <- if (s$status == "Accepted") {
    label <- if (isTRUE(s$resolved)) "Consensus after joint review" else
      if (isTRUE(s$resolved_via_4th)) "Consensus after fourth review" else "Consensus"
    sprintf("%s: pre-fire %s, post-fire %s, vegetation %s \u2192 %s%s",
            label, fmt(s$cons_pre), fmt(s$cons_post), fmt(s$cons_pre_veg),
            fmt(s$cons_post_veg),
            if (is.na(s$cons_mortality)) "" else
              sprintf(", mortality %.0f%%", s$cons_mortality))
  } else if (s$status == "Incomplete") {
    paste0("Incomplete: ", s$issues)
  } else if (s$status == "Needs 4th review") {
    paste0("Needs 4th review: ", s$issues,
           if (nzchar(s$spread_label)) paste0(" · ", s$spread_label) else "")
  } else if (s$status == "Conflict") {
    paste0("Conflict (unresolved after fourth review): ", s$issues,
           if (isTRUE(s$reviewed)) " \u00b7 joint review in progress" else "")
  } else if (s$status == "Excluded" && isTRUE(s$resolved)) {
    "Excluded after joint review"
  } else {
    as.character(s$status)
  }
  if (!is.na(s$rv_note) && nzchar(str_trim(s$rv_note))) {
    line <- paste0(line, " \u00b7 Note: ", s$rv_note)
  }
  paste0("<p class='detail-summary'>", htmltools::htmlEscape(line), "</p>")
}

plot_details_all <- function(flags, st) {
  d <- conflict_long(flags, st, st$id)
  # muted (greyed out) beats shaded: an excluded observer's values never
  # fed any check, so they read as "excluded", not "agreeing" or "disputed".
  cell_td <- function(value, flag, excluded = FALSE) {
    # ifelse()'s result takes the length of its *first* argument -- a
    # scalar `excluded` (the default, used wherever a column has no
    # "excluded observer" concept, e.g. Suitable) would silently collapse
    # the whole column to a single repeated class instead of one per row.
    # Recycle both test vectors up to length(value) first so that can't
    # happen regardless of what the caller passes.
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
# `d` must have a "Plot" column holding the plot id; `details` is the
# named vector from plot_details_all().
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

# joint_reviews.csv has exactly seven columns: plot_id, pre_fire, post_fire,
# pre_veg, post_veg, conflict_fields, note. A field pre-fills with its
# automatic consensus value when already resolved, or is blank when
# conflict_fields lists it as still disputed -- the reviewer only ever needs
# to fill the blanks.
review_template <- function(st) {
  conflicts <- st %>% filter(rule_status == "Conflict") %>% arrange(fire_name, id)
  flags <- cbind(conflicts$f_pre %in% TRUE, conflicts$f_post %in% TRUE,
                 conflicts$f_vpre %in% TRUE, conflicts$f_vpost %in% TRUE)
  field_labels <- c("pre_fire", "post_fire", "pre_veg", "post_veg")
  conflict_fields <- apply(flags, 1, function(row) {
    paste(field_labels[row], collapse = "; ")
  })

  conflicts %>%
    transmute(
      plot_id = id,
      pre_fire = if_else(f_pre %in% TRUE, NA_real_, pre_resolved),
      post_fire = if_else(f_post %in% TRUE, NA_real_, post_resolved),
      pre_veg = if_else(f_vpre %in% TRUE, NA_character_, maj_pre_veg),
      post_veg = if_else(f_vpost %in% TRUE, NA_character_, maj_post_veg),
      conflict_fields = conflict_fields,
      note = ""
    )
}

empty_review_file_data <- function() {
  tibble(plot_id = character(0), pre_fire = double(0), post_fire = double(0),
         pre_veg = character(0), post_veg = character(0),
         conflict_fields = character(0), note = character(0))
}

# Read the git-tracked ledger for merging into a fresh template download.
read_review_ledger <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) {
    return(empty_review_file_data())
  }

  r <- read.csv(path, colClasses = "character", na.strings = c("", "NA"),
                check.names = FALSE, stringsAsFactors = FALSE)
  names(r) <- str_to_lower(str_trim(names(r)))
  if (!"plot_id" %in% names(r)) {
    stop("The review sheet needs a column called plot_id.")
  }
  if (nrow(r) == 0) return(empty_review_file_data())

  for (col in c("pre_fire", "post_fire", "pre_veg", "post_veg",
               "conflict_fields", "note")) {
    if (!col %in% names(r)) r[[col]] <- NA_character_
  }

  r %>%
    transmute(
      plot_id = str_trim(plot_id),
      pre_fire = suppressWarnings(as.numeric(pre_fire)),
      post_fire = suppressWarnings(as.numeric(post_fire)),
      pre_veg = str_to_lower(str_trim(pre_veg)),
      post_veg = str_to_lower(str_trim(post_veg)),
      conflict_fields = coalesce(conflict_fields, ""),
      note = note
    ) %>%
    filter(!is.na(plot_id), nzchar(plot_id)) %>%
    group_by(plot_id) %>% slice_tail(n = 1) %>% ungroup()
}

# TRUE where `field` appears in that row's conflict_fields list.
has_field <- function(conflict_fields, field) {
  vapply(strsplit(coalesce(conflict_fields, ""), "; ", fixed = TRUE),
        function(x) field %in% x, logical(1))
}

# Cumulative ledger rule: never drop an existing row just because its plot
# isn't in the currently loaded batch. For a plot in both, a field keeps its
# existing value only while it's still listed in that *existing* row's
# conflict_fields (a preserved human decision, or one still awaited) AND the
# fresh template still shows it blank (still disputed right now) -- a field
# that has since resolved on its own always takes the fresh automatic value,
# regardless of whatever used to be in that cell.
merge_review_ledger <- function(existing, current) {
  if (nrow(current) == 0) return(existing)
  if (nrow(existing) == 0) return(current)

  fields <- c("pre_fire", "post_fire", "pre_veg", "post_veg")
  matched <- current %>%
    left_join(existing, by = "plot_id", suffix = c("", "_old"))

  for (field in fields) {
    old_value <- matched[[paste0(field, "_old")]]
    keep_old <- has_field(matched$conflict_fields_old, field) &
      !is.na(old_value) & is.na(matched[[field]])
    matched[[field]] <- if_else(keep_old, old_value, matched[[field]])
  }
  matched$note <- if_else(!is.na(matched$note_old) & nzchar(matched$note_old),
                          matched$note_old, matched$note)
  matched <- matched %>% select(all_of(names(current)))

  existing_only <- existing %>% filter(!plot_id %in% current$plot_id)
  bind_rows(matched, existing_only) %>% arrange(plot_id)
}

# Data checks: individual entries worth a second look ----
data_checks <- function(reads, st, primary_observers) {
  tagged <- reads %>%
    left_join(select(st, id, initial_f_pre, initial_f_post,
                     initial_f_vpre, initial_f_vpost), by = "id") %>%
    mutate(is_primary = observer %in% primary_observers)

  suitable <- tagged %>% filter(completed, !unsuitable)

  bind_rows(
    suitable %>% filter(plot_location == "Inside", !is.na(jt_post_fire),
                        !is.na(jt_pre_fire), jt_post_fire > jt_pre_fire) %>%
      mutate(check = check_labels[["post_gt_pre"]]),

    # Primary observers must complete both count fields. A fourth reviewer is
    # only expected to supply count fields that were disputed at stage 1.
    suitable %>% filter(
      (is_primary & (is.na(jt_pre_fire) | is.na(jt_post_fire))) |
        (!is_primary &
           ((initial_f_pre %in% TRUE & is.na(jt_pre_fire)) |
            (initial_f_post %in% TRUE & is.na(jt_post_fire))))
    ) %>% mutate(check = check_labels[["count_missing"]]),

    # Same rule for vegetation cover.
    suitable %>% filter(
      (is_primary & (is.na(pre_veg) | is.na(post_veg))) |
        (!is_primary &
           ((initial_f_vpre %in% TRUE & is.na(pre_veg)) |
            (initial_f_vpost %in% TRUE & is.na(post_veg))))
    ) %>% mutate(check = check_labels[["veg_missing"]]),

    suitable %>% filter(!pre_veg %in% c(veg_levels, NA) |
                          !post_veg %in% c(veg_levels, NA)) %>%
      mutate(check = check_labels[["veg_bad"]]),
    suitable %>% filter(
      (is_primary & (is.na(pre_confidence) | is.na(post_confidence))) |
        (!is_primary &
           ((initial_f_pre %in% TRUE & is.na(pre_confidence)) |
            (initial_f_post %in% TRUE & is.na(post_confidence))))
    ) %>% mutate(check = check_labels[["conf_missing"]]),
    tagged %>% filter(unsuitable, !is.na(jt_pre_fire) | !is.na(jt_post_fire)) %>%
      mutate(check = check_labels[["unsuit_counts"]]),

    # Do not flag untouched plots in a fourth reviewer's export as "not saved".
    tagged %>% filter(is_primary, !completed) %>%
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

Three observers independently count each plot in pre- and post-fire imagery,
assign a vegetation cover class, and flag unsuitable plots. The app checks
whether they agree, escalates disagreements to a fourth reviewer and then
joint review, and builds the consensus dataset used in the analysis.

### Plot status

| Status | Meaning |
|---|---|
| **Not started** | Opened in Collect Earth, but no counts saved yet |
| **In progress** | Counted by fewer than {N} observers so far |
| **Incomplete** | A required field is blank, or one observer split from the other two on suitability -- see Data checks |
| **Excluded** | Majority marked the plot unsuitable |
| **Accepted** | Counts and vegetation cover agree -- directly, after a fourth review, or after joint review |
| **Needs 4th review** | Observers didn\'t agree on at least one field; a fourth reviewer is needed |
| **Conflict** | Still disputed after a fourth review; needs joint review |

### How a plot gets there

Suitability, counts, and vegetation cover (pre and post) are each checked
independently -- a disagreement on one field never blocks another field that
already agreed, and **a field that already agrees is locked in for good**: a
fourth reviewer can never overturn it, only settle the field(s) that actually
disagreed.

- Suitability and vegetation cover are decided by **majority**; counts must
  **agree within tolerance** (see Agreement rule, below).
- A blank entry, or one observer splitting from the other two on
  suitability -> **Incomplete**: back to that observer, not a fourth
  reviewer.
- A disagreement on counts or vegetation cover -> **Needs 4th review**, then
  **Accepted** or **Conflict** once a fourth count is added (see Fourth
  review, below).
- Still disputed after that -> **Conflict**, resolved by joint review.

A plot always ends up Excluded or Accepted -- the review steps are skipped
entirely whenever the three observers already agree.

### Agreement rule

- **Counts** (pre- and post-fire): spread (highest minus lowest) no more
  than **{ABS} tree(s) or {REL}% of the median, whichever is larger**.
  {EXAMPLE}
- **Vegetation cover** (pre and post): majority of the suitable observers
  (at least 2 of 3).
- **Suitability**: majority of however many observers judged the plot --
  always resolves with three. A split sends the plot to Incomplete so the
  dissenting observer can double-check before being overruled, but it never
  needs a fourth reviewer or joint review.

### Fourth review

A plot in **Needs 4th review** needs one more independent count in Collect
Earth, but only for the field(s) listed in Issues -- fields that already
agreed stay locked at their original value. Export it like any other
observer\'s file and add it to the shared folder; the app treats anyone
beyond the original three as that plot\'s fourth reviewer automatically. It
resolves as soon as any three values agree on a disputed field (the odd one
out is discarded); otherwise that field moves to Conflict.

The **Spread pattern** column on Overview hints at how likely that is:
**Outlier** (two of the three already agree, one is off) usually resolves;
**Even spread** (no two agree) is more likely to end up in joint review
regardless of what the fourth reviewer counts.

### Joint review

Every plot still in Conflict is listed on the Joint review tab, with the
same observer-comparison view as Overview. Download the template and fill in
only the blank cells -- everything else is either already agreed (shown for
reference) or not something that needs a decision. This file is cumulative:
each new download already includes every plot anyone has ever reviewed, not
just the current conflicts, so a past decision is never lost just because
that plot isn\'t loaded today.

Save the filled-in file over **{SHEET}.csv** in `shiny/jt_agreement_app/`,
then commit it to git -- that commit history is the permanent record of
every decision. The app re-reads it automatically; observers\' own counts
are never changed.

### Consensus values

Counts are the **median** of whichever values were agreed to be used;
vegetation cover is the **majority** class (Swanson et al. 2016). Mortality
is calculated from consensus counts, inside plots only.

### Which data are used

{SOURCE} For each observer, {NEWEST} The most recent saved version is used
if a plot was saved more than once.

The three observers are identified automatically as whoever has counted the
most plots; anyone else is treated as a fourth reviewer. (A project
maintainer can lock their names instead -- see `PRIMARY_OBSERVERS` in
`app.R`.)

### Low confidence

Flagged when at least {LOWMIN} observers marked low confidence for the same
period. Can mean poor imagery or a hard-to-count plot -- check the comments.
'

about_markdown <- function(abs_tol, rel_tol, n_required, newest_only, source) {
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
  tol_at <- function(m) format(round(max(abs_tol, rel_tol / 100 * m), 1))
  example_text <- paste0("For example, the allowed spread is ", tol_at(5),
                         " for plots with a median of 5 trees and ", tol_at(30),
                         " for a median of 30.")
  md <- about_template
  fill <- c("{SOURCE}" = source_text, "{NEWEST}" = newest_text,
            "{EXAMPLE}" = example_text, "{ABS}" = format(abs_tol),
            "{REL}" = format(rel_tol), "{N}" = format(n_required),
            "{SHEET}" = REVIEW_SHEET, "{LOWMIN}" = format(LOW_CONF_MIN))
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
/* DataTables adds its own hover wash through box-shadow. Keep Overview quiet:
   rows remain clickable, but hovering does not recolour either the main row
   or the expanded observer detail. */
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

/* Joint review is read-only now, same quiet treatment as Overview. */
#review_table table.dataTable.display tbody tr:hover > *,
#review_table table.dataTable.hover tbody tr:hover > * {
  box-shadow: none !important;
}
#review_table tbody > tr:hover {
  outline: none !important;
  box-shadow: none !important;
}
.detail-summary { margin: 0 0 .6rem; }
.detail-table { margin: 0; }
.detail-table td, .detail-table th { padding: .3rem .7rem .3rem 0; vertical-align: top; }
.detail-table td.shade { background-color: #F4DED6; }
.detail-table td.muted { color: #AEA99C; }
"

ui <- page_navbar(
  title = "Joshua tree counts \u00b7 observer agreement",
  theme = theme,
  fillable = FALSE,
  header = tags$head(tags$style(HTML(app_css))),
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
    numericInput("n_required", "Observers per plot", value = 3,
                 min = 2, step = 1),
    numericInput("abs_tol", "Tolerance: trees", value = 2, min = 0, step = 1),
    numericInput("rel_tol", "Tolerance: % of median", value = 20, min = 0, step = 5),
    helpText("Counts must agree within tolerance: the spread between the",
             "highest and lowest may be no more than the tree tolerance, or",
             "the % of their median -- whichever allows more. Vegetation",
             "cover and suitability are decided by majority instead."),
    uiOutput("tolerance_example"),
    helpText("A count or vegetation disagreement goes to a fourth",
             "reviewer, then to joint review if still unresolved."),
    helpText("A blank entry, or one observer splitting from the other two",
             "on suitability, goes back to that observer instead, not to a",
             "fourth reviewer -- once it's resolved, the plot is checked",
             "against the agreement rule above as usual."),
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
      helpText("Click a row to show each observer's counts for that plot."),
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
    card(card_header("Each observer compared with the median"),
         plotOutput("consensus_plot", height = "420px"),
         helpText("Points on the dashed line match the median. Points are",
                  "slightly jittered so overlapping values stay visible."))
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
    "Data checks",
    card(card_header("Data checks: entries worth a second look"),
         helpText("Individual entries to fix in Collect Earth, e.g. missing",
                  "values or a post-fire count above the pre-fire count.",
                  "Click a column header to sort (e.g. by Observer) or use",
                  "the search boxes to filter. A missing count or vegetation",
                  "class puts a plot in the Incomplete status on Overview --",
                  "so does one observer splitting from the other two on",
                  "suitability (see the Issues column there)."),
         layout_columns(
           col_widths = c(4),
           selectInput("check_filter", "Issue", choices = c("All", unname(check_labels)))
         ),
         DTOutput("checks_table", fill = FALSE)),
    card(
      card_header("Downloads"),
      p(tags$strong("Consensus data:"), "a live snapshot, one row per plot,",
        "with the consensus counts, vegetation cover, mortality and",
        "low-confidence flags so far. Only fields that are still unresolved",
        "are blank; fields already settled remain available even if another",
        "field on that plot is in conflict. This is for checking progress,",
        "not the file to analyze: the final dataset is built separately by",
        tags$code("scripts/14_build_consensus_dataset_simple.R"),
        ", which only runs once every plot is fully resolved and records",
        "exactly which tier (primary, fourth review, or joint review)",
        "produced each value."),
      downloadButton("dl_consensus", "Consensus data"),
      p(class = "mt-3", tags$strong("Observer counts:"),
        "one row per observer per plot (their own, independent values)."),
      downloadButton("dl_reads", "Observer counts"),
      p(class = "mt-3", tags$strong("Plot status:"),
        "every plot with its status, issues and review details."),
      downloadButton("dl_status", "Plot status")
    )
  ),

  nav_panel(
    "Joint review",
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Plots needing joint review",
        downloadButton("dl_review_template", "Download review template", class = "btn-sm")
      ),
      uiOutput("ledger_warnings"),
      layout_columns(
        col_widths = c(4),
        # Only disagreement issues ever reach Conflict -- a recheck issue
        # (suitability split, missing value) always resolves into Incomplete
        # first, so it can never be the reason a plot is here.
        selectInput("issue_filter", "Issue",
                    choices = c("All", unname(issue_labels[disagreement_cols])))
      ),
      p("Click a row to see every observer's entries underneath",
        "(unresolved fields shaded red). Download the template: each row has",
        "the four fields (", tags$code("pre_fire"), ",", tags$code("post_fire"),
        ",", tags$code("pre_veg"), ",", tags$code("post_veg"), ") pre-filled",
        "wherever they already resolved automatically, and blank wherever",
        tags$code("conflict_fields"), "lists them as still disputed -- fill in",
        "only those blanks. An already-resolved field cannot be overridden",
        "by anything written in its cell. The template is cumulative: it",
        "preserves every existing row/decision in",
        tags$code(paste0(REVIEW_SHEET, ".csv")), "and adds or refreshes",
        "currently conflicted plots. Save the download over",
        tags$code(paste0(REVIEW_SHEET, ".csv")), "in",
        tags$code("shiny/jt_agreement_app/"), "and commit it to git. The app",
        "re-reads that ledger automatically."),
      DTOutput("review_table", fill = FALSE)
    )
  ),

  nav_panel(
    "How it works",
    div(class = "about", uiOutput("about_page"))
  )
)

# Server ----

server <- function(input, output, session) {

  # Shared folder: re-checked on a timer or on request. reactiveVal only
  # notifies when the files actually change.
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

  # Reviews come from a local, git-tracked file, not the shared folder --
  # re-read whenever its contents on disk change.
  reviews <- reactiveFileReader(2000, session, LOCAL_REVIEW_PATH, function(path) {
    tryCatch(read_reviews(path), error = function(e) {
      showNotification(paste("Review file:", conditionMessage(e)),
                       type = "error", duration = 15)
      empty_reviews()
    })
  })

  # Rarely changes (only when the site list is regenerated), so a slower
  # refresh than reviews is plenty.
  plot_order <- reactiveFileReader(60000, session, PLOT_ORDER_PATH, read_plot_order)

  output$folder_status <- renderUI({
    src <- folder_source()
    if (src == "none") {
      return(helpText("No shared folder set. To read a Google Drive folder",
                      "automatically, set DRIVE_FOLDER at the top of app.R."))
    }
    label <- if (src == "drive") "Google Drive folder" else
      paste("Folder:", basename(DATA_FOLDER))
    f <- folder_files()
    div(class = "small mb-3",
        tags$strong(label), tags$br(),
        if (!is.null(folder_error())) {
          div(class = "text-danger", "Couldn't read it: ", folder_error())
        } else if (is.null(folder_checked())) {
          span(class = "text-muted", "Checking\u2026")
        } else {
          tagList(
            paste0(nrow(f), " export file", if (nrow(f) == 1) "" else "s",
                   if (nrow(f) > 0) paste0(", newest ",
                                           format(max(f$mtime), "%d %b %H:%M",
                                                  tz = Sys.timezone()))),
            tags$br(),
            paste0("Review file: ", nrow(reviews()), " decisions recorded")
          )
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

  file_info <- reactive({
    summarise_files(raw_all()) %>%
      mutate(used = is_newest | !isTRUE(input$newest_only))
  })

  reads_all <- reactive({
    used_paths <- file_info()$source_path[file_info()$used]
    clean_reads(filter(raw_all(), source_path %in% used_paths))
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

  settings <- reactive({
    list(abs_tol = max(0, input$abs_tol %||% 2, na.rm = TRUE),
         rel_tol = max(0, input$rel_tol %||% 20, na.rm = TRUE),
         n_required = max(2, input$n_required %||% 3, na.rm = TRUE))
  })

  primary_observers <- reactive({
    s <- settings()
    tryCatch(
      choose_primary_observers(reads_all(), s$n_required),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 15)
        character(0)
      }
    )
  })

  low_conf <- reactive(low_confidence_by_plot(reads()))

  status <- reactive({
    s <- settings()
    validate(need(length(primary_observers()) == s$n_required,
                  "Could not identify the three observers."))
    plot_status(reads(), s$abs_tol, s$rel_tol, s$n_required, reviews(),
                primary_observers()) %>%
      left_join(select(low_conf(), id, pre_low, post_low, n_rated,
                       low_conf_label, low_conf_period), by = "id") %>%
      mutate(low_conf_label = coalesce(low_conf_label, ""))
  })

  flags <- reactive(observer_flags(reads(), status()))

  # Expandable-row details for every plot, built once per data/settings change
  details <- reactive(plot_details_all(flags(), status()))

  # Fire order for charts: the master datasheet's order (SURVEY_FIRE_ORDER),
  # then any other fire alphabetically. Reversed because coord_flip() puts
  # the first level at the bottom.
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
          "counted by all observers.")
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
    n_conf_before <- sum(full$rule_status == "Conflict")
    txt <- if (n_full == 0) "No plots have been counted by all observers yet." else
      sprintf(paste("%d plots counted by all observers: %d (%.0f%%) were in",
                    "conflict after a fourth review, %d of those resolved",
                    "by joint review."),
              n_full, n_conf_before, 100 * n_conf_before / n_full,
              sum(full$resolved))
    div(class = "status-note", txt)
  })

  output$plot_table_title <- renderText({
    if (is.null(selected_status())) "All plots" else paste("Plots:", selected_status())
  })

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
    d <- st %>%
      arrange(status, fire_name, id) %>%
      left_join(plot_comments(), by = "id") %>%
      left_join(plot_order(), by = "id") %>%
      transmute(`CE #` = plot_number, Plot = id, Fire = fire_name,
                Location = plot_location,
                Status = status,
                Counted = if_else(
                  n_reviewers > 0,
                  paste0(n_primary, " of ", settings()$n_required,
                         " + ", n_reviewers, " review"),
                  paste(n_primary, "of", settings()$n_required)),
                Issues = issues,
                `Spread pattern` = spread_label,
                `Low confidence` = low_conf_label,
                Comments = coalesce(Comments, ""))
    if (!is.null(showing) && showing == "Excluded") d <- select(d, -Issues)
    d
  })

  output$plot_table <- renderDT({
    d <- plot_table_data()
    comments_col <- match("Comments", names(d))
    expandable_datatable(
      d, details(), filter = "top", raw_html_cols = "Comments",
      dt_options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE,
                        columnDefs = list(list(width = "320px", targets = comments_col - 1)))
    ) %>%
      formatStyle("Status", backgroundColor = styleEqual(status_levels,
                                                         unname(status_colors)),
                  color = styleEqual(status_levels, unname(status_text)))
  })

  # Joint review ----
  # Read-only: decisions are made outside the app (see the Joint review tab's
  # instructions) and committed to LOCAL_REVIEW_PATH in git. This view just
  # lists what's still unresolved, with the same click-to-expand comparison
  # used on Overview.

  # A CSV has no cell-locking -- someone can type over an already-resolved
  # field by mistake and plot_status() will silently ignore it (the field
  # stays frozen at its automatic value either way). This surfaces that
  # mistake instead of letting it vanish unnoticed: any value sitting in a
  # field that row's own conflict_fields does NOT list as disputed, but that
  # disagrees with the current automatic value for that field, is flagged.
  ledger_warnings <- reactive({
    ledger <- tryCatch(read_review_ledger(LOCAL_REVIEW_PATH),
                       error = function(e) empty_review_file_data())
    if (nrow(ledger) == 0) return(tibble())

    live <- status() %>%
      transmute(plot_id = id, pre_resolved, post_resolved, maj_pre_veg,
               maj_post_veg)
    joined <- ledger %>% inner_join(live, by = "plot_id")
    if (nrow(joined) == 0) return(tibble())

    mismatch_rows <- function(field, ledger_val, auto_val) {
      undisputed <- !has_field(joined$conflict_fields, field)
      mismatch <- undisputed & !is.na(ledger_val) & !is.na(auto_val) &
        ledger_val != auto_val
      tibble(plot_id = joined$plot_id[mismatch], field = field,
            ledger_value = format(ledger_val)[mismatch],
            auto_value = format(auto_val)[mismatch])
    }

    bind_rows(
      mismatch_rows("pre_fire", joined$pre_fire, joined$pre_resolved),
      mismatch_rows("post_fire", joined$post_fire, joined$post_resolved),
      mismatch_rows("pre_veg", joined$pre_veg, joined$maj_pre_veg),
      mismatch_rows("post_veg", joined$post_veg, joined$maj_post_veg)
    )
  })

  output$ledger_warnings <- renderUI({
    w <- ledger_warnings()
    if (nrow(w) == 0) return(NULL)
    div(class = "alert alert-warning",
        tags$strong(sprintf(
          "%d value%s in joint_reviews.csv %s being ignored:",
          nrow(w), if (nrow(w) == 1) "" else "s",
          if (nrow(w) == 1) "is" else "are"
        )),
        tags$ul(
          pmap(w, function(plot_id, field, ledger_value, auto_value) {
            tags$li(sprintf(
              "%s: %s = %s in the file, but already resolved to %s -- ignored",
              plot_id, field, ledger_value, auto_value
            ))
          })
        ))
  })

  conflict_ids <- reactive({
    st <- filter(status(), status == "Conflict")
    if (input$issue_filter != "All") {
      st <- filter(st, str_detect(issues, fixed(input$issue_filter)))
    }
    st$id
  })

  output$review_table <- renderDT({
    ids <- conflict_ids()
    validate(need(length(ids) > 0, "No plots currently need joint review."))
    d <- status() %>%
      filter(id %in% ids) %>%
      arrange(fire_name, id) %>%
      transmute(Plot = id, Fire = fire_name, Location = plot_location, Issues = issues)
    expandable_datatable(d, details(), dt_options = list(pageLength = 15, scrollX = TRUE))
  })

  output$dl_review_template <- downloadHandler(
    filename = function() paste0(REVIEW_SHEET, "_template.csv"),
    content = function(file) {
      # Cumulative ledger: preserve every row/decision already committed in
      # joint_reviews.csv, then add or refresh any conflicts in the data that
      # are currently loaded. This makes weekly/batch downloads append-only
      # with respect to prior review decisions. With no old rows and no current
      # conflicts, the result is simply a valid header-only CSV.
      existing <- tryCatch(
        read_review_ledger(LOCAL_REVIEW_PATH),
        error = function(e) {
          showNotification(paste("Existing review ledger:", conditionMessage(e)),
                           type = "error", duration = 15)
          empty_review_file_data()
        }
      )

      current <- review_template(status())
      out <- merge_review_ledger(existing, current)
      write.csv(out, file, row.names = FALSE, na = "")
    }
  )

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
      # every fire gets both bars (0 if none), so bars and labels line up
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

  output$dl_low_conf <- downloadHandler(
    filename = function() paste0("jt_low_confidence_", Sys.Date(), ".csv"),
    content = function(file) {
      coords <- reads() %>%
        group_by(id) %>%
        summarise(longitude = first(na.omit(location_x)),
                  latitude = first(na.omit(location_y)),
                  comments = paste(na.omit(comments), collapse = " | "),
                  .groups = "drop")
      flagged_low() %>%
        left_join(coords, by = "id") %>%
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
      left_join(status() %>% filter(status == "Conflict") %>%
                  count(fire_name, name = "conflicts"), by = "fire_name") %>%
      mutate(conflicts = coalesce(conflicts, 0L),
             label = if_else(conflicts > 0,
                             paste0(total, "  (", conflicts, " to review)"),
                             as.character(total)),
             fire_name = factor(fire_name, levels = order))
    d <- status() %>%
      count(fire_name, status, name = "n") %>%
      mutate(fire_name = factor(fire_name, levels = order),
             # ggplot stacks the first level furthest from the axis, so this
             # puts conflict on the left and not started on the right
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
    n_conf <- sum(full$rule_status == "Conflict")
    p(class = "summary-line", tags$strong("Plots in conflict before review: "),
      if (n_full == 0) "\u2013" else
        sprintf("%.1f%% (%d of %d plots counted by all observers)",
                100 * n_conf / n_full, n_conf, n_full))
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

  output$consensus_plot <- renderPlot({
    d <- flags() %>%
      filter(fully_counted, !unsuitable)
    long <- bind_rows(
      transmute(d, observer, measure = "Pre-fire count", median = med_pre, value = jt_pre_fire),
      transmute(d, observer, measure = "Post-fire count", median = med_post, value = jt_post_fire)
    ) %>%
      filter(!is.na(value), !is.na(median)) %>%
      mutate(measure = factor(measure, levels = c("Pre-fire count", "Post-fire count")))
    validate(need(nrow(long) > 0, "No plots counted by all observers yet."))
    obs <- sort(unique(long$observer))
    base_hues <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4")
    hues <- if (length(obs) <= length(base_hues)) base_hues[seq_along(obs)] else
      c(base_hues, rep("#898781", length(obs) - length(base_hues)))
    ggplot(long, aes(median, value, color = observer)) +
      geom_abline(slope = 1, intercept = 0, linetype = 2, color = "#898781") +
      geom_point(position = position_jitter(width = 0.15, height = 0.15, seed = 1),
                 size = 2.2, alpha = 0.75) +
      facet_wrap(~measure, scales = "free") +
      scale_color_manual(values = setNames(hues, obs)) +
      labs(x = "Median of all observers", y = "Observer's count") +
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
      mutate(Role = if_else(observer %in% primary_observers(),
                            "Primary", "Fourth review")) %>%
      rename(Observer = observer) %>%
      select(Observer, Role, everything()) %>%
      datatable(fillContainer = FALSE, rownames = FALSE,
                options = list(dom = "t", scrollX = TRUE))
  })

  output$checks_table <- renderDT({
    d <- data_checks(reads(), status(), primary_observers())
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

  output$dl_consensus <- downloadHandler(
    filename = function() paste0("jt_consensus_", Sys.Date(), ".csv"),
    content = function(file) {
      status() %>%
        arrange(fire_name, id) %>%
        transmute(plot_id = id, fire = fire_name, fire_year, location = plot_location,
                  status, n_counts, resolved_by_review = resolved,
                  pre_fire = cons_pre, post_fire = cons_post,
                  pre_veg = cons_pre_veg, post_veg = cons_post_veg,
                  mortality_pct = round(cons_mortality, 1),
                  pre_low_confidence = coalesce(pre_low, 0L),
                  post_low_confidence = coalesce(post_low, 0L),
                  issues, review_note = rv_note) %>%
        write.csv(file, row.names = FALSE, na = "")
    }
  )

  output$dl_reads <- downloadHandler(
    filename = function() paste0("jt_observer_counts_", Sys.Date(), ".csv"),
    content = function(file) {
      reads() %>%
        left_join(select(status(), id, status), by = "id") %>%
        select(-row_in_file, -source_path) %>%
        write.csv(file, row.names = FALSE, na = "")
    }
  )

  output$dl_status <- downloadHandler(
    filename = function() paste0("jt_plot_status_", Sys.Date(), ".csv"),
    content = function(file) {
      status() %>%
        select(-starts_with("f_"), -any_issue, -any_recheck, -any_disagreement,
               -recheck_issues, -disagreement_issues, -accepted) %>%
        write.csv(file, row.names = FALSE, na = "")
    }
  )

  # Sidebar ----
  output$tolerance_example <- renderUI({
    s <- settings()
    tol <- function(m) format(round(max(s$abs_tol, s$rel_tol / 100 * m), 1))
    helpText(tags$em(sprintf(
      "With these settings: a spread of up to %s tree(s) for plots with a median of 5 trees, %s for 15, %s for 30.",
      tol(5), tol(15), tol(30))))
  })

  # How it works ----
  output$about_page <- renderUI({
    s <- settings()
    shiny::markdown(about_markdown(s$abs_tol, s$rel_tol, s$n_required,
                                   input$newest_only, folder_source()))
  })
}

shinyApp(ui, server)
