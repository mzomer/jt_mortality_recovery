# -----------------------------------------------------------------------
# Joshua tree counts: observer agreement app
# Reads each observer's Collect Earth exports, applies the two-of-three
# acceptance rule, flags conflicts for joint review, and produces the
# consensus dataset (median counts, majority vegetation cover).
#
# Packages: shiny, bslib, DT, dplyr, tidyr, ggplot2, stringr, purrr,
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
# A Google Sheet (or CSV) with this name, in the same folder, holding the
# values you agree on in joint reviews. The Download tab gives you a
# pre-filled template.
REVIEW_SHEET <- "joint_reviews"

drive_poll_ms <- 60000   # how often to re-check Google Drive
folder_poll_ms <- 15000  # how often to re-check a local folder

export_pattern <- "collectedData.*\\.csv$"
review_pattern <- paste0("^", REVIEW_SHEET, "(\\.csv)?$")

empty_files <- function() {
  data.frame(name = character(0), datapath = character(0),
             mtime = as.POSIXct(character(0)))
}

empty_folder <- function() list(exports = empty_files(), reviews = NA_character_)

list_folder_files <- function(folder) {
  folder <- path.expand(folder)
  if (!dir.exists(folder)) stop("Folder not found: ", folder)
  paths <- list.files(folder, pattern = export_pattern, ignore.case = TRUE,
                      full.names = TRUE, recursive = TRUE)
  exports <- if (length(paths) == 0) empty_files() else
    data.frame(name = basename(paths), datapath = paths,
               mtime = file.info(paths)$mtime)
  review_paths <- list.files(folder, pattern = paste0("^", REVIEW_SHEET, "\\.csv$"),
                             ignore.case = TRUE, full.names = TRUE,
                             recursive = TRUE)
  reviews <- if (length(review_paths) == 0) NA_character_ else
    review_paths[which.max(file.info(review_paths)$mtime)]
  list(exports = exports, reviews = reviews)
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

  # review sheet: the most recently edited file named joint_reviews
  rv <- found[grepl(review_pattern, found$name, ignore.case = TRUE), ]
  reviews <- NA_character_
  if (nrow(rv) > 0) {
    rv <- rv[order(rv$modified, decreasing = TRUE), ][1, ]
    reviews <- cache_path(rv$id, rv$modified, "_reviews")
    if (!file.exists(reviews) || is.na(rv$modified)) {
      is_sheet <- identical(rv$mime, "application/vnd.google-apps.spreadsheet")
      drive_fetch(rv$id, reviews, type = if (is_sheet) "csv" else NULL)
    }
  }
  list(exports = exports, reviews = reviews)
}

folder_source <- function() {
  if (nzchar(DRIVE_FOLDER)) "drive" else if (nzchar(DATA_FOLDER)) "local" else "none"
}

# Status categories, in display order ----
status_levels <- c("Not started", "In progress", "Conflict", "Excluded", "Accepted")
status_help <- c(
  "Not started" = "Opened in Collect Earth, but nobody has saved counts yet",
  "In progress" = "Counted by some, but not yet all, observers",
  "Conflict"    = "Not all observers agree; needs joint review",
  "Excluded"    = "Most observers marked the plot unsuitable",
  "Accepted"    = "All observers agree on every field"
)
status_colors <- c(
  "Not started" = "#B8B3A2",
  "In progress" = "#C9A55A",
  "Conflict"    = "#A3452F",
  "Excluded"    = "#6F7773",
  "Accepted"    = "#4E6A3E"
)
status_text <- c("Not started" = "#2B2B2B", "In progress" = "#2B2B2B",
                 "Conflict" = "#FFFFFF", "Excluded" = "#FFFFFF",
                 "Accepted" = "#FFFFFF")

issue_labels <- c(
  f_missing  = "Missing count",
  f_pre      = "Pre-fire count",
  f_post     = "Post-fire count",
  f_vmissing = "Missing veg cover",
  f_vpre     = "Pre veg cover",
  f_vpost    = "Post veg cover"
)
veg_levels <- c("low", "medium", "high")

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
# Validated categorical hues (distinct under simulated colorblindness,
# distinct for full-color readers, readable on a light surface even
# pairwise) -- used to tell two measures (e.g. pre-/post-fire) apart.
viz_observer_hues <- c("#2a78d6", "#eb6834", "#1baf7a")

# A quiet theme: hairline gridlines one step off the surface, muted axis
# text, no chart border -- the data carries the contrast, not the chrome.
# `angled_x`: rotate x-axis labels 45deg (for categorical labels like fire
# names); turned off for charts with a numeric x-axis (e.g. a scatter).
viz_theme <- function(base_size = 14, angled_x = TRUE) {
  theme_minimal(base_size = base_size) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "#e1e0d9", linewidth = 0.4),
      axis.ticks = element_blank(),
      axis.text = element_text(color = "#52514e"),
      axis.text.x = if (angled_x) element_text(angle = 45, hjust = 1) else element_text(),
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
      completed = unsuitable | !is.na(jt_pre_fire) | !is.na(jt_post_fire)
    ) %>%
    select(-fire_from_id)
}

# Acceptance rule ----
# Two counts agree if they differ by no more than the larger of an
# absolute tolerance (trees) and a relative one (% of their mean).
within_tol <- function(a, b, abs_tol, rel_tol) {
  abs(a - b) <= pmax(abs_tol, rel_tol / 100 * (a + b) / 2)
}

# TRUE if every count is within tolerance of the group's own mean (so every
# pair is within tolerance of each other too), FALSE if not, NA if fewer than
# n_required counts were entered (so one observer skipping this field, e.g.
# because they marked the plot unsuitable, counts as missing rather than
# being silently judged on whoever's left). One shared tolerance for the
# group, rather than a separate one per pair, so the threshold doesn't
# depend on which two values happen to be compared.
all_agree <- function(x, abs_tol, rel_tol, n_required) {
  x <- x[!is.na(x)]
  if (length(x) < n_required) return(NA)
  tol <- max(abs_tol, rel_tol / 100 * mean(x))
  max(x) - min(x) <= tol
}

# The class chosen by a majority (at least two observers, no tie)
majority_class <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  counts <- table(x)
  top <- max(counts)
  if (top >= 2 && sum(counts == top) == 1) names(counts)[counts == top] else
    NA_character_
}

# Median, rounded half up so consensus counts stay whole trees (only
# matters when two observers' counts are used, e.g. 12 and 13 -> 13)
median_count <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else floor(median(x) + 0.5)
}

# Joint review sheet ----
empty_reviews <- function() {
  tibble(id = character(0), rv_pre = numeric(0), rv_post = numeric(0),
         rv_pre_veg = character(0), rv_post_veg = character(0),
         rv_excluded = logical(0), rv_note = character(0))
}

read_reviews <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) return(empty_reviews())
  r <- read.csv(path, colClasses = "character", na.strings = c("", "NA"),
                check.names = FALSE)
  names(r) <- str_to_lower(str_trim(names(r)))
  if (!"plot_id" %in% names(r)) {
    stop("The review sheet needs a column called plot_id.")
  }
  for (col in c("pre_fire", "post_fire", "pre_veg", "post_veg", "excluded", "note")) {
    if (!col %in% names(r)) r[[col]] <- NA_character_
  }
  r %>%
    transmute(
      id = str_trim(plot_id),
      rv_pre = suppressWarnings(as.numeric(pre_fire)),
      rv_post = suppressWarnings(as.numeric(post_fire)),
      rv_pre_veg = str_to_lower(str_trim(pre_veg)),
      rv_post_veg = str_to_lower(str_trim(post_veg)),
      rv_excluded = str_to_lower(str_trim(coalesce(excluded, ""))) %in%
        c("yes", "y", "true", "1", "x"),
      rv_note = note
    ) %>%
    filter(!is.na(id), nzchar(id)) %>%
    # a row counts as a decision once anything has been filled in
    filter(rv_excluded | !is.na(rv_pre) | !is.na(rv_post) |
             !is.na(rv_pre_veg) | !is.na(rv_post_veg)) %>%
    group_by(id) %>% slice_tail(n = 1) %>% ungroup()
}

# Plot status and consensus values ----
plot_status <- function(reads, abs_tol, rel_tol, n_required, reviews) {
  all_plots <- reads %>% distinct(id, fire_name, fire_year, plot_location)

  judged <- reads %>%
    filter(completed) %>%
    group_by(id) %>%
    summarise(
      n_counts = n(),
      observers = paste(sort(observer), collapse = ", "),
      n_unsuitable = sum(unsuitable),
      pre_ok = all_agree(jt_pre_fire, abs_tol, rel_tol, n_required),
      post_ok = all_agree(jt_post_fire, abs_tol, rel_tol, n_required),
      vpre_n = sum(!is.na(pre_veg[!unsuitable])),
      vpost_n = sum(!is.na(post_veg[!unsuitable])),
      maj_pre_veg = majority_class(pre_veg[!unsuitable]),
      maj_post_veg = majority_class(post_veg[!unsuitable]),
      med_pre = median_count(jt_pre_fire),
      med_post = median_count(jt_post_fire),
      .groups = "drop"
    ) %>%
    mutate(
      f_missing = is.na(pre_ok) | is.na(post_ok),
      f_pre = pre_ok %in% FALSE,
      f_post = post_ok %in% FALSE,
      f_vmissing = vpre_n < 2 | vpost_n < 2,
      f_vpre = vpre_n >= 2 & is.na(maj_pre_veg),
      f_vpost = vpost_n >= 2 & is.na(maj_post_veg),
      excluded_rule = n_unsuitable > n_counts / 2
    )

  flag_cols <- names(issue_labels)
  judged$issues <- apply(as.matrix(judged[flag_cols]), 1, function(row) {
    paste(issue_labels[flag_cols[row]], collapse = "; ")
  })
  judged$any_issue <- rowSums(as.matrix(judged[flag_cols])) > 0

  st <- all_plots %>%
    left_join(judged, by = "id") %>%
    mutate(
      n_counts = coalesce(n_counts, 0L),
      rule_status = case_when(
        n_counts == 0 ~ "Not started",
        n_counts < n_required ~ "In progress",
        excluded_rule ~ "Excluded",
        any_issue ~ "Conflict",
        TRUE ~ "Accepted"
      ),
      issues = if_else(rule_status %in% c("Excluded", "Not started"), "",
                       coalesce(issues, ""))
    ) %>%
    left_join(reviews, by = "id") %>%
    mutate(
      reviewed = id %in% reviews$id,
      # A conflict is resolved only when every field that originally failed
      # the agreement rule has an explicit joint-review decision. Fields that
      # were already acceptable may be left blank and keep their automatic value.
      review_complete = case_when(
        !reviewed ~ FALSE,
        coalesce(rv_excluded, FALSE) ~ TRUE,
        rule_status != "Conflict" ~ TRUE,
        TRUE ~
          (!is.na(pre_ok)  | !is.na(rv_pre)) &
          (!is.na(post_ok) | !is.na(rv_post)) &
          (!(f_pre %in% TRUE)  | !is.na(rv_pre)) &
          (!(f_post %in% TRUE) | !is.na(rv_post)) &
          (!(vpre_n < 2)  | !is.na(rv_pre_veg)) &
          (!(vpost_n < 2) | !is.na(rv_post_veg)) &
          (!(f_vpre %in% TRUE)  | !is.na(rv_pre_veg)) &
          (!(f_vpost %in% TRUE) | !is.na(rv_post_veg))
      ),
      status = case_when(
        reviewed & coalesce(rv_excluded, FALSE) ~ "Excluded",
        rule_status == "Conflict" & review_complete ~ "Accepted",
        TRUE ~ rule_status
      ),
      status = factor(status, levels = status_levels),
      resolved = rule_status == "Conflict" & review_complete,
      # consensus values: review decisions where given, otherwise the
      # median / majority; only for accepted plots
      accepted = status == "Accepted",
      cons_pre = if_else(accepted, coalesce(rv_pre, med_pre), NA_real_),
      cons_post = if_else(accepted, coalesce(rv_post, med_post), NA_real_),
      cons_pre_veg = if_else(accepted, coalesce(rv_pre_veg, maj_pre_veg), NA_character_),
      cons_post_veg = if_else(accepted, coalesce(rv_post_veg, maj_post_veg), NA_character_),
      cons_mortality = if_else(accepted & plot_location == "Inside" &
                                 coalesce(cons_pre, 0) > 0 & !is.na(cons_post),
                               (cons_pre - cons_post) / cons_pre * 100, NA_real_),
      fully_counted = n_counts >= n_required
    )
  st
}

# Per observer, per plot: who differs from the consensus ----
# An observer is an outlier on a count when their own count is outside the
# tolerance of the plot's median. On an accepted plot this can never be
# TRUE (unanimous agreement already rules it out); it's meaningful for
# plots in conflict, to see who's furthest from the rest.
observer_flags <- function(reads, st, abs_tol, rel_tol) {
  reads %>%
    filter(completed) %>%
    inner_join(select(st, id, status, rule_status, fully_counted, pre_ok, post_ok,
                      med_pre, med_post, maj_pre_veg, maj_post_veg,
                      f_missing, f_pre, f_post, f_vmissing, f_vpre, f_vpost),
               by = "id") %>%
    mutate(
      pre_out = !unsuitable & !is.na(jt_pre_fire) &
        !within_tol(jt_pre_fire, med_pre, abs_tol, rel_tol),
      post_out = !unsuitable & !is.na(jt_post_fire) &
        !within_tol(jt_post_fire, med_post, abs_tol, rel_tol),
      outlier = pre_out | post_out,
      vpre_same = if_else(!unsuitable & !is.na(pre_veg) & !is.na(maj_pre_veg),
                          pre_veg == maj_pre_veg, NA),
      vpost_same = if_else(!unsuitable & !is.na(post_veg) & !is.na(maj_post_veg),
                           post_veg == maj_post_veg, NA)
    )
}

# Grouped table for the Conflicts tab: one row per observer, one block
# per plot, with the values in question shaded ----
fmt_count <- function(x) ifelse(is.na(x), "\u2013", format(x, trim = TRUE))

with_conf <- function(value, conf) {
  ifelse(is.na(conf), value,
         paste0(value, " <span class='conf'>", htmltools::htmlEscape(conf),
                "</span>"))
}

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
      # hidden helper columns for shading
      .first = first_row,
      .pre = f_pre | (f_missing & is.na(jt_pre_fire) & !unsuitable) | pre_out,
      .post = f_post | (f_missing & is.na(jt_post_fire) & !unsuitable) | post_out,
      .vpre = f_vpre | (f_vmissing & is.na(pre_veg) & !unsuitable),
      .vpost = f_vpost | (f_vmissing & is.na(post_veg) & !unsuitable)
    )
}

observer_table_dt <- function(d, page_length = 60) {
  helper_idx <- which(startsWith(names(d), ".")) - 1
  shade <- "#F4DED6"
  datatable(d, fillContainer = FALSE, rownames = FALSE, escape = FALSE,
            class = "compact conflict-table",
            options = list(ordering = FALSE, pageLength = page_length,
                           lengthMenu = c(30, 60, 150, 500), scrollX = TRUE,
                           dom = if (page_length < 30) "t" else "lfrtip",
                           columnDefs = list(
                             list(visible = FALSE, targets = helper_idx),
                             list(width = "220px", targets = 0),
                             list(width = "320px", targets = 8)))) %>%
    formatStyle(names(d)[1:9], valueColumns = ".first",
                borderTop = styleEqual(TRUE, "2px solid #9AA39C")) %>%
    formatStyle("Pre-fire", valueColumns = ".pre",
                backgroundColor = styleEqual(TRUE, shade)) %>%
    formatStyle("Post-fire", valueColumns = ".post",
                backgroundColor = styleEqual(TRUE, shade)) %>%
    formatStyle("Veg pre", valueColumns = ".vpre",
                backgroundColor = styleEqual(TRUE, shade)) %>%
    formatStyle("Veg post", valueColumns = ".vpost",
                backgroundColor = styleEqual(TRUE, shade))
}

# One-line status summary for a plot (consensus values, outlier, review note),
# used above its inline observer-detail table in the Overview tab.
plot_summary_html <- function(id, st) {
  esc <- htmltools::htmlEscape
  s <- st[st$id == id, ]
  fmt <- function(x) if (is.na(x)) "–" else format(x)
  summary_line <- if (s$status == "Accepted") {
    sprintf("Consensus: pre-fire %s, post-fire %s, vegetation %s → %s%s",
            fmt(s$cons_pre), fmt(s$cons_post), fmt(s$cons_pre_veg),
            fmt(s$cons_post_veg),
            if (is.na(s$cons_mortality)) "" else
              sprintf(", mortality %.0f%%", s$cons_mortality))
  } else if (s$status == "Conflict") {
    paste("Conflict:", s$issues)
  } else {
    as.character(s$status)
  }
  extra <- c(
    if (s$reviewed) paste0("Reviewed", if (is.na(s$rv_note)) "" else
      paste0(": ", s$rv_note))
  )
  paste0("<p class='detail-summary'>", esc(paste(c(summary_line, extra), collapse = " · ")),
         "</p>")
}

# Plain-HTML observer breakdown for one plot, used as a DataTables child row
# (shown directly under its row in the Overview table when clicked).
plot_detail_html <- function(id, flags, st) {
  d <- conflict_long(flags, st, id)
  if (nrow(d) == 0) {
    return(paste0(plot_summary_html(id, st),
                  "<p class='text-muted mb-0'>No observer has saved counts for this plot yet.</p>"))
  }
  shade_td <- function(value, flag) paste0("<td", if (isTRUE(flag)) " class='shade'", ">", value, "</td>")
  rows <- vapply(seq_len(nrow(d)), function(i) {
    r <- d[i, ]
    paste0(
      "<tr><td>", r$Observer, "</td>",
      shade_td(r$`Pre-fire`, r$.pre), shade_td(r$`Post-fire`, r$.post),
      "<td>", r$Mortality, "</td>",
      shade_td(r$`Veg pre`, r$.vpre), shade_td(r$`Veg post`, r$.vpost),
      "<td>", r$Suitable, "</td>",
      "<td>", r$Comments, "</td></tr>"
    )
  }, character(1))
  paste0(
    plot_summary_html(id, st),
    "<table class='table table-sm detail-table'><thead><tr>",
    "<th>Observer</th><th>Pre-fire</th><th>Post-fire</th><th>Mortality</th>",
    "<th>Veg pre</th><th>Veg post</th><th>Suitable</th><th>Comments</th>",
    "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table>"
  )
}

# A datatable where clicking any row expands a child row underneath it,
# showing each observer's counts and comments for that plot (via
# plot_detail_html), built into a hidden ".detail" column so no extra round
# trip to the server is needed on click. `d` must have a "Plot" column
# holding each row's plot id. `raw_html_cols` names any other columns that
# already contain HTML and shouldn't be escaped.
expandable_datatable <- function(d, flags, st, filter = "none",
                                  raw_html_cols = character(0), dt_options = list()) {
  d$.detail <- vapply(d$Plot, plot_detail_html, character(1), flags = flags, st = st)
  detail_col <- match(".detail", names(d))
  no_escape <- unique(c(match(raw_html_cols, names(d)), detail_col))
  dt_options$columnDefs <- c(dt_options$columnDefs,
                             list(list(visible = FALSE, targets = detail_col - 1)))
  datatable(
    d, fillContainer = FALSE, rownames = FALSE, filter = filter,
    escape = -no_escape, selection = "none",
    callback = JS(sprintf(
      "table.on('click', 'tbody tr', function() {
         var tr = $(this);
         var row = table.row(tr);
         if (row.child.isShown()) {
           row.child.hide();
           tr.removeClass('shown');
         } else {
           table.rows('.shown').every(function() {
             this.child.hide();
             $(this.node()).removeClass('shown');
           });
           row.child(row.data()[%d]).show();
           tr.addClass('shown');
         }
       });", detail_col - 1)),
    options = dt_options
  )
}

# Pre-filled review sheet for unresolved conflicts
review_template <- function(flags, st) {
  ids <- st$id[st$status == "Conflict"]
  if (length(ids) == 0) {
    return(tibble(plot_id = character(0), pre_fire = character(0),
                  post_fire = character(0), pre_veg = character(0),
                  post_veg = character(0), excluded = character(0),
                  note = character(0)))
  }
  ref <- flags %>%
    filter(id %in% ids) %>%
    arrange(observer) %>%
    group_by(id) %>%
    summarise(
      counts_pre = paste0(observer, " ", fmt_count(jt_pre_fire), collapse = " | "),
      counts_post = paste0(observer, " ", fmt_count(jt_post_fire), collapse = " | "),
      veg = paste0(observer, " ", coalesce(pre_veg, "-"), "/",
                   coalesce(post_veg, "-"), collapse = " | "),
      .groups = "drop"
    )
  st %>%
    filter(id %in% ids) %>%
    select(id, fire = fire_name, location = plot_location, issues) %>%
    left_join(ref, by = "id") %>%
    arrange(fire, id) %>%
    transmute(plot_id = id, fire, location, issues,
              `ref: pre counts` = counts_pre, `ref: post counts` = counts_post,
              `ref: veg pre/post` = veg,
              pre_fire = "", post_fire = "", pre_veg = "", post_veg = "",
              excluded = "", note = "")
}


# Per-observer data checks ----

data_checks <- function(reads) {
  suitable <- reads %>% filter(completed, !unsuitable)
  bind_rows(
    suitable %>% filter(plot_location == "Inside", !is.na(jt_post_fire),
                        !is.na(jt_pre_fire), jt_post_fire > jt_pre_fire) %>%
      mutate(check = "Post-fire count higher than pre-fire (inside plot)"),
    suitable %>% filter(is.na(jt_pre_fire) | is.na(jt_post_fire)) %>%
      mutate(check = "Pre- or post-fire count missing"),
    suitable %>% filter(is.na(pre_veg) | is.na(post_veg)) %>%
      mutate(check = "Vegetation cover missing"),
    suitable %>% filter(!pre_veg %in% c(veg_levels, NA) |
                          !post_veg %in% c(veg_levels, NA)) %>%
      mutate(check = "Unrecognised vegetation cover class"),
    suitable %>% filter(is.na(pre_confidence) | is.na(post_confidence)) %>%
      mutate(check = "Confidence missing"),
    reads %>% filter(unsuitable, !is.na(jt_pre_fire) | !is.na(jt_post_fire)) %>%
      mutate(check = "Marked unsuitable but has counts"),
    reads %>% filter(!completed) %>%
      mutate(check = "Opened but nothing saved")
  ) %>%
    select(check, observer, id, fire_name, plot_location, jt_pre_fire,
           jt_post_fire, pre_veg, post_veg, comments) %>%
    arrange(check, observer, id)
}

# Per plot, how many observers marked low confidence in each imagery period.
# One observer's low confidence is often just personal uncertainty; two or
# more is a stronger signal that the imagery itself (not the observer) is
# the problem, e.g. a candidate for re-acquiring.
low_confidence_by_plot <- function(reads) {
  reads %>%
    filter(completed, !unsuitable) %>%
    group_by(id, fire_name, plot_location) %>%
    summarise(
      pre_low = sum(pre_confidence %in% "low"),
      post_low = sum(post_confidence %in% "low"),
      pre_low_observers = paste(sort(observer[pre_confidence %in% "low"]), collapse = ", "),
      post_low_observers = paste(sort(observer[post_confidence %in% "low"]), collapse = ", "),
      .groups = "drop"
    )
}

# "How it works" page ----
about_template <- '
### What the app does

Each plot is counted independently by {N} observers. The app compares their
counts, accepts plots where observers agree, flags conflicts for joint
review, and builds the consensus dataset used in the analysis.

### Which data are used

{SOURCE} For each observer, {NEWEST} If a plot was saved more than once, the
most recent saved version is used.

### Plot status

| Status | Meaning |
|---|---|
| **Not started** | Opened in Collect Earth, but no counts saved yet |
| **In progress** | Counted by fewer than {N} observers so far |
| **Conflict** | Not all observers agree on at least one field (below); needs joint review |
| **Excluded** | Most observers marked the plot unsuitable |
| **Accepted** | All observers agree on every field |

### When observers agree

- **Counts** (pre- and post-fire): every observer\'s count must agree with
  every other observer\'s, differing by no more than **{ABS} tree(s) or
  {REL}% of their mean, whichever is larger**. A plot is accepted only when
  all observers agree. {EXAMPLE}
- **Vegetation cover** (pre and post): accepted when at least two observers
  chose the same class.
- **Missing values:** a count needs a value from every observer before it can
  be accepted; vegetation cover needs at least two observers to agree.
- **Unsuitable:** if one observer marked a plot unsuitable and the others
  counted it, their counts are used.

**Outlier** (Conflicts tab and plot details): among plots needing joint
review, an observer whose count is outside the tolerance of the median of
the other counts. It doesn\'t change whether the plot needs review —
that\'s already decided by the counts not all agreeing — it\'s there to
show which observers tend to differ most from the rest.

### Consensus values

For accepted plots, counts are the **median** of the observers\' counts
(rounded to whole trees when only two counts are available) and vegetation
cover is the **majority** class (Swanson et al. 2016). Mortality is calculated
from the consensus counts, for inside plots only.

### Joint review

Conflicts are re-examined together and the agreed values entered in the
**{SHEET}** sheet in the shared folder (the Download tab has a pre-filled
template). A conflicted plot becomes Accepted only after every field that
failed the agreement rule has an agreed review value, or Excluded if marked so.
Fields that were not in conflict may be left blank and keep their automatic
median/majority value. The observers\' own counts are never changed.
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
  example_text <- paste0("For example, the allowed difference is ", tol_at(5),
                         " for plots averaging 5 trees and ", tol_at(30),
                         " for plots averaging 30.")
  md <- about_template
  fill <- c("{SOURCE}" = source_text, "{NEWEST}" = newest_text,
            "{EXAMPLE}" = example_text, "{ABS}" = format(abs_tol),
            "{REL}" = format(rel_tol), "{N}" = format(n_required),
            "{SHEET}" = REVIEW_SHEET)
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
.about { max-width: 46rem; padding: .5rem 0 2rem; line-height: 1.55; }
.about h3 { margin-top: 1.8rem; font-size: 1.3rem; }
.about table { margin: .6rem 0 1rem; }
.about td, .about th { padding: .35rem .8rem .35rem 0; vertical-align: top;
                       border-bottom: 1px solid #e3e5e2; }
.plot-meta { color: #5f6660; font-size: .85em; margin-top: 2px; }
table.conflict-table td { white-space: normal; }
.summary-line { margin: .8rem 0 0; }
.cmt { min-width: 340px; white-space: normal; }
table.dataTable > tbody > tr.shown { box-shadow: inset 3px 0 0 #4E6A3E; }
table.dataTable > tbody > tr.child > td { background: #FAFAF8; padding: .8rem 1.2rem; }
.detail-summary { margin: 0 0 .6rem; }
.detail-table { margin: 0; }
.detail-table td, .detail-table th { padding: .3rem .7rem .3rem 0; vertical-align: top; }
.detail-table td.shade { background-color: #F4DED6; }
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
    numericInput("rel_tol", "Tolerance: % of mean", value = 20, min = 0, step = 5),
    helpText("Two counts agree if they differ by no more than the tree",
             "tolerance or the % of their average, whichever is larger.",
             "A plot is accepted only when every observer's count agrees",
             "with every other observer's."),
    uiOutput("tolerance_example"),
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
    card(card_header("Observers"), DTOutput("observer_files", fill = FALSE)),
    card(card_header("Plots counted per fire"), DTOutput("progress_table", fill = FALSE))
  ),

  nav_panel(
    "Conflicts",
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Plots to check",
        downloadButton("dl_conflicts", "Download CSV", class = "btn-sm")
      ),
      layout_columns(
        col_widths = c(4, 4),
        selectInput("conflict_view", "Show",
                    choices = c("Unresolved conflicts",
                                "All conflicts (incl. resolved)")),
        selectInput("issue_filter", "Issue", choices = c("All", unname(issue_labels)))
      ),
      helpText("One row per observer. Shaded cells are the values in question."),
      DTOutput("conflict_table", fill = FALSE)
    )
  ),

  nav_panel(
    "Plots",
    card(card_header("Plot status by fire"),
         plotOutput("status_by_fire_plot", height = "500px"),
         helpText("Every plot per fire, by status — same colors as the",
                  "Overview status bar. Shows review workload as a count,",
                  "not a percentage, so small fires don't look worse than",
                  "they are.")),
    card(card_header("Mortality by fire (inside plots, consensus only)"),
         plotOutput("mortality_plot", height = "500px"),
         helpText("Mean ± SD of each plot's consensus mortality, by fire",
                  "(accepted plots only).")),
    card(card_header("Plots likely needing new imagery, by fire"),
         helpText("Plots where at least two observers independently marked",
                  "low confidence in the same imagery period — a stronger",
                  "signal than one observer's personal uncertainty."),
         uiOutput("low_confidence_summary"),
         plotOutput("low_confidence_plot", height = "500px"))
  ),

  nav_panel(
    "Data checks",
    card(card_header("Entries worth a second look, per observer"),
         DTOutput("checks_table", fill = FALSE))
  ),

  nav_panel(
    "Low confidence imagery",
    helpText("Plot-level detail behind the \"Plots likely needing new",
             "imagery\" chart on the Plots tab — use these to find and",
             "review the specific plots."),
    accordion(
      open = FALSE,
      accordion_panel(
        "Low confidence: pre-fire count",
        helpText("Click a row to see each observer's counts and comments."),
        DTOutput("low_confidence_pre_table", fill = FALSE)
      ),
      accordion_panel(
        "Low confidence: post-fire count",
        helpText("Click a row to see each observer's counts and comments."),
        DTOutput("low_confidence_post_table", fill = FALSE)
      )
    )
  ),

  nav_panel(
    "How it works",
    div(class = "about", uiOutput("about_page"))
  ),

  nav_panel(
    "Download",
    card(
      card_header("Data"),
      p(tags$strong("Consensus data:"), "one row per plot, with the consensus",
        "counts, vegetation cover and mortality used in the analysis. Values",
        "are blank for conflicts until they are resolved."),
      downloadButton("dl_consensus", "Consensus data"),
      p(class = "mt-3", tags$strong("Observer counts:"),
        "one row per observer per plot (their own, independent values)."),
      downloadButton("dl_reads", "Observer counts"),
      p(class = "mt-3", tags$strong("Plot status:"),
        "every plot with its status, issues and review details."),
      downloadButton("dl_status", "Plot status")
    ),
    card(
      card_header("Joint review sheet"),
      p("A sheet listing the unresolved conflicts, with each observer's values",
        "for reference and empty columns for the values you agree on."),
      tags$ol(
        tags$li("Download it and upload it to the shared Drive folder."),
        tags$li("In Drive, right-click it \u2192 Open with \u2192 Google Sheets.",
                "Keep the name", tags$code(REVIEW_SHEET), "."),
        tags$li("During the review, fill in each field that is listed as a conflict,",
                "or mark excluded (yes). Fields that were not in conflict may be left",
                "blank and keep the automatic value; a note is optional."),
        tags$li("For later reviews, add new rows to the same sheet.")
      ),
      downloadButton("dl_template", "Review sheet template")
    )
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

  reviews <- reactive({
    tryCatch(read_reviews(folder_state()$reviews), error = function(e) {
      showNotification(paste("Review sheet:", conditionMessage(e)),
                       type = "error", duration = 15)
      empty_reviews()
    })
  })

  output$folder_status <- renderUI({
    src <- folder_source()
    if (src == "none") {
      return(helpText("No shared folder set. To read a Google Drive folder",
                      "automatically, set DRIVE_FOLDER at the top of app.R."))
    }
    label <- if (src == "drive") "Google Drive folder" else
      paste("Folder:", basename(DATA_FOLDER))
    f <- folder_files()
    has_sheet <- !is.na(folder_state()$reviews)
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
            if (has_sheet) paste0("Review sheet: ", nrow(reviews()), " decisions")
            else span(class = "text-muted", "No review sheet yet")
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

  status_base <- reactive({
    s <- settings()
    plot_status(reads(), s$abs_tol, s$rel_tol, s$n_required, reviews())
  })

  flags <- reactive({
    s <- settings()
    observer_flags(reads(), status_base(), s$abs_tol, s$rel_tol)
  })

  # Accepted plots now require every observer to agree, so an accepted plot
  # can never contain an outlier by construction — status is just the base
  # plot status, with no separate outlier join needed.
  status <- status_base

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
    st <- status()
    full <- st %>% filter(fully_counted)
    n_full <- nrow(full)
    n_conf_before <- sum(full$rule_status == "Conflict")
    n_resolved <- sum(full$resolved)
    txt <- if (n_full == 0) "No plots have been counted by all observers yet." else
      sprintf(paste("%d plots counted by all observers: %d (%.0f%%) were in",
                    "conflict before review, %d of those resolved."),
              n_full, n_conf_before, 100 * n_conf_before / n_full,
              n_resolved)
    div(class = "status-note", txt,
        " Click a category to filter the table; click again to clear.")
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
      transmute(Plot = id, Fire = fire_name, Location = plot_location,
                Status = status,
                Counted = paste(n_counts, "of", settings()$n_required),
                Observers = observers,
                Issues = issues,
                Reviewed = if_else(reviewed, "yes", ""),
                `Pre (consensus)` = cons_pre, `Post (consensus)` = cons_post,
                Comments = coalesce(Comments, ""))
    if (!is.null(showing) && showing %in% c("Not started", "In progress", "Excluded")) {
      d <- select(d, -`Pre (consensus)`, -`Post (consensus)`)
    }
    if (!is.null(showing) && showing == "Excluded") d <- select(d, -Issues)
    d
  })

  # Plot table: clicking a row expands a child row underneath it, showing
  # each observer's counts for that plot.
  output$plot_table <- renderDT({
    d <- plot_table_data()
    comments_col <- match("Comments", names(d))
    expandable_datatable(
      d, flags(), status(), filter = "top", raw_html_cols = "Comments",
      dt_options = list(pageLength = 15, scrollX = TRUE, scrollY = "500px",
                        scrollCollapse = TRUE, autoWidth = TRUE,
                        columnDefs = list(list(width = "320px", targets = comments_col - 1)))
    ) %>%
      formatStyle("Status", backgroundColor = styleEqual(status_levels,
                                                         unname(status_colors)),
                  color = styleEqual(status_levels, unname(status_text)))
  })


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

  output$progress_table <- renderDT({
    totals <- reads() %>% distinct(fire_name, id) %>% count(Fire = fire_name, name = "Plots")
    reads() %>%
      filter(completed) %>%
      count(Fire = fire_name, observer) %>%
      pivot_wider(names_from = observer, values_from = n, values_fill = 0) %>%
      right_join(totals, by = "Fire") %>%
      relocate(Plots, .after = Fire) %>%
      mutate(across(-c(Fire, Plots), ~ coalesce(.x, 0L))) %>%
      arrange(Fire) %>%
      datatable(fillContainer = FALSE, rownames = FALSE,
                options = list(pageLength = 20, dom = "tp", scrollX = TRUE))
  })

  # Conflicts ----
  conflict_ids <- reactive({
    st <- status()
    st <- switch(input$conflict_view,
      "Unresolved conflicts" = filter(st, status == "Conflict"),
      "All conflicts (incl. resolved)" = filter(st, rule_status == "Conflict"))
    if (input$issue_filter != "All") {
      st <- filter(st, str_detect(issues, fixed(input$issue_filter)))
    }
    st$id
  })

  output$conflict_table <- renderDT({
    ids <- conflict_ids()
    validate(need(length(ids) > 0, "No plots to show with the current settings."))
    observer_table_dt(conflict_long(flags(), status(), ids))
  })


  output$dl_conflicts <- downloadHandler(
    filename = function() paste0("jt_conflicts_", Sys.Date(), ".csv"),
    content = function(file) {
      flags() %>%
        filter(id %in% conflict_ids()) %>%
        left_join(select(status(), id, issues), by = "id") %>%
        select(id, fire_name, plot_location, issues, observer,
               jt_pre_fire, pre_confidence, jt_post_fire, post_confidence,
               pre_veg, post_veg, unsuitable, comments, location_x, location_y) %>%
        arrange(fire_name, id, observer) %>%
        write.csv(file, row.names = FALSE, na = "")
    }
  )

  # Plots ----
  # QA/progress chart uses blue (viz_observer_hues[1]); the results chart
  # (mortality) uses the app's green, to keep "process" and "result" visually
  # distinct at a glance.

  # Shared fire ordering, matching the master datasheet (SURVEY_FIRE_ORDER),
  # so every chart below lists fires the same way as the field spreadsheet.
  # Returned bottom-to-top, since coord_flip() puts the first level at the
  # bottom -- so the datasheet's first fire ends up at the top of the chart.
  # Any fire not found there (unexpected/renamed) is appended alphabetically.
  fire_order <- reactive({
    present <- unique(status()$fire_name)
    ordered <- SURVEY_FIRE_ORDER[SURVEY_FIRE_ORDER %in% present]
    extra <- sort(setdiff(present, ordered))
    rev(c(ordered, extra))
  })

  output$status_by_fire_plot <- renderPlot({
    order <- fire_order()
    validate(need(length(order) > 0, "No plots yet."))
    totals <- status() %>%
      count(fire_name, name = "total") %>%
      mutate(fire_name = factor(fire_name, levels = order))
    d <- status() %>%
      count(fire_name, status, name = "n") %>%
      mutate(fire_name = factor(fire_name, levels = order))
    ggplot(d, aes(fire_name, n, fill = status)) +
      geom_col(width = 0.65, color = "#fcfcfb", linewidth = 0.3) +
      geom_text(data = totals, aes(fire_name, total, label = total),
                inherit.aes = FALSE, hjust = -0.3, size = 3.6, color = "#0b0b0b") +
      scale_fill_manual(values = status_colors, breaks = status_levels, name = NULL) +
      scale_x_discrete(labels = str_to_title) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      coord_flip() +
      labs(x = NULL, y = "Plots") +
      viz_theme(14, angled_x = FALSE)
  })

  output$mortality_plot <- renderPlot({
    order <- fire_order()
    validate(need(length(order) > 0, "No plots yet."))
    mort <- status() %>%
      filter(plot_location == "Inside", !is.na(cons_mortality)) %>%
      group_by(fire_name) %>%
      summarise(n = n(), mean_mortality = mean(cons_mortality),
                sd_mortality = coalesce(sd(cons_mortality), 0), .groups = "drop")
    d <- tibble(fire_name = order) %>%
      left_join(mort, by = "fire_name") %>%
      mutate(fire_name = factor(fire_name, levels = order),
             ymin = pmax(0, mean_mortality - sd_mortality),
             ymax = pmin(100, mean_mortality + sd_mortality))
    validate(need(any(!is.na(d$mean_mortality)),
                  "No inside plots with a consensus mortality value yet."))
    # Points/error bars encode position, not length, so (unlike a bar chart)
    # the axis doesn't need to anchor at 0 -- zoom to the data so fires
    # clustered near 100% aren't squeezed into a sliver on one side.
    pad <- max(2, diff(range(c(d$ymin, d$ymax), na.rm = TRUE)) * 0.12)
    ggplot(d, aes(fire_name, mean_mortality)) +
      geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2,
                    color = "#52514e", linewidth = 0.5, na.rm = TRUE) +
      geom_point(size = 3, color = "#4E6A3E", na.rm = TRUE) +
      geom_text(aes(label = sprintf("n=%d", n), y = ymax), hjust = -0.3,
                size = 3.4, color = "#52514e", na.rm = TRUE) +
      scale_x_discrete(labels = str_to_title) +
      coord_flip(ylim = c(max(0, min(d$ymin, na.rm = TRUE) - pad),
                          min(100, max(d$ymax, na.rm = TRUE) + pad))) +
      labs(x = NULL, y = "Mortality (%), mean +/- SD") +
      viz_theme(14, angled_x = FALSE)
  })

  # Data checks ----
  output$checks_table <- renderDT({
    data_checks(reads()) %>%
      rename(Check = check, Observer = observer, Plot = id, Fire = fire_name,
             Location = plot_location, Pre = jt_pre_fire, Post = jt_post_fire,
             `Veg pre` = pre_veg, `Veg post` = post_veg, Comments = comments) %>%
      mutate(Check = factor(Check), Observer = factor(Observer)) %>%
      datatable(fillContainer = FALSE, rownames = FALSE, filter = "top",
                options = list(pageLength = 20, scrollX = TRUE,
                               columnDefs = list(list(width = "260px", targets = 0))))
  })

  low_conf <- reactive(low_confidence_by_plot(reads()))

  output$low_confidence_summary <- renderUI({
    d <- low_conf()
    n_pre <- sum(d$pre_low >= 2)
    n_post <- sum(d$post_low >= 2)
    if (n_pre == 0 && n_post == 0) return(NULL)
    n_fires <- n_distinct(d$fire_name[d$pre_low >= 2 | d$post_low >= 2])
    div(class = "status-note",
        sprintf(paste("%d plot%s flagged for the pre-fire count and %d plot%s",
                      "for the post-fire count, across %d fire%s."),
                n_pre, if (n_pre == 1) "" else "s",
                n_post, if (n_post == 1) "" else "s",
                n_fires, if (n_fires == 1) "" else "s"))
  })

  output$low_confidence_plot <- renderPlot({
    order <- fire_order()
    validate(need(length(order) > 0, "No plots yet."))
    d <- low_conf()
    counts <- bind_rows(
      d %>% filter(pre_low >= 2) %>% transmute(fire_name, type = "Pre-fire count"),
      d %>% filter(post_low >= 2) %>% transmute(fire_name, type = "Post-fire count")
    ) %>% count(fire_name, type, name = "n")
    validate(need(nrow(counts) > 0, "No plots yet where two or more observers marked low confidence."))
    counts$fire_name <- factor(counts$fire_name, levels = order)
    pal <- c("Pre-fire count" = "#D55E00", "Post-fire count" = "#0072B2")
    ggplot(counts, aes(fire_name, n, fill = type)) +
      geom_col(position = position_dodge(width = 0.75), width = 0.65) +
      geom_text(aes(label = n), position = position_dodge(width = 0.75),
                hjust = -0.3, size = 3.6, show.legend = FALSE) +
      scale_fill_manual(values = pal) +
      scale_x_discrete(labels = str_to_title, drop = FALSE) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      coord_flip() +
      labs(x = NULL, y = "Plots flagged", fill = NULL) +
      viz_theme(14, angled_x = FALSE)
  })

  output$low_confidence_pre_table <- renderDT({
    d <- low_conf() %>%
      filter(pre_low >= 2) %>%
      arrange(desc(pre_low), fire_name, id) %>%
      transmute(Plot = id, Fire = fire_name, Location = plot_location,
                `Observers (low)` = pre_low, Who = pre_low_observers)
    validate(need(nrow(d) > 0, "No plots yet where two or more observers marked low confidence in the pre-fire count."))
    expandable_datatable(d, flags(), status(),
                        dt_options = list(pageLength = 20, scrollX = TRUE))
  })

  output$low_confidence_post_table <- renderDT({
    d <- low_conf() %>%
      filter(post_low >= 2) %>%
      arrange(desc(post_low), fire_name, id) %>%
      transmute(Plot = id, Fire = fire_name, Location = plot_location,
                `Observers (low)` = post_low, Who = post_low_observers)
    validate(need(nrow(d) > 0, "No plots yet where two or more observers marked low confidence in the post-fire count."))
    expandable_datatable(d, flags(), status(),
                        dt_options = list(pageLength = 20, scrollX = TRUE))
  })

  output$tolerance_example <- renderUI({
    s <- settings()
    tol <- function(m) format(round(max(s$abs_tol, s$rel_tol / 100 * m), 1))
    helpText(tags$em(sprintf(
      "With these settings: up to %s tree(s) apart for plots averaging 5 trees, %s for 30.",
      tol(5), tol(30))))
  })

  # How it works ----
  output$about_page <- renderUI({
    s <- settings()
    shiny::markdown(about_markdown(s$abs_tol, s$rel_tol, s$n_required,
                                   input$newest_only, folder_source()))
  })

  # Downloads ----
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
        select(-starts_with("f_"), -any_issue, -accepted) %>%
        write.csv(file, row.names = FALSE, na = "")
    }
  )

  output$dl_template <- downloadHandler(
    filename = function() paste0(REVIEW_SHEET, ".csv"),
    content = function(file) {
      write.csv(review_template(flags(), status()), file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui, server)
