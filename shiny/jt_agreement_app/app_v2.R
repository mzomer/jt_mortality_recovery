# -----------------------------------------------------------------------
# Joshua tree counts: observer agreement app
# Reads each observer's Collect Earth exports, checks whether observers
# agree on each plot, flags conflicts for joint review, and produces the
# consensus dataset (median counts, majority vegetation cover).
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
# A Google Sheet (or CSV) with this name, in the same folder, holding the
# values you agree on in joint reviews. The Joint review tab gives you an
# editable, pre-filled sheet to download once it's filled in.
REVIEW_SHEET <- "joint_reviews"

# Low confidence ----
# A plot is flagged when at least this many observers marked low confidence
# for the same count (pre- or post-fire).
LOW_CONF_MIN <- 2

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
  "Conflict"    = "Observers disagree; needs joint review",
  "Excluded"    = "All observers marked the plot unsuitable",
  "Accepted"    = "All observers agree on suitability and counts; most agree on vegetation cover"
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
  f_suit     = "Suitability",
  f_missing  = "Missing count",
  f_pre      = "Pre-fire count",
  f_post     = "Post-fire count",
  f_vmissing = "Missing veg cover",
  f_vpre     = "Pre veg cover",
  f_vpost    = "Post veg cover"
)
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
      completed = unsuitable | !is.na(jt_pre_fire) | !is.na(jt_post_fire)
    ) %>%
    select(-fire_from_id)
}

# Agreement rules ----
# Suitability: all observers must agree; any split goes to review.
# Missing entries: any observer leaving a count or cover class blank sends the
# plot to review.
# Counts: every observer who judged the plot suitable must agree. They agree
# when the spread of their counts (highest minus lowest) is no more than the
# larger of the tree tolerance and the % tolerance of their mean. One shared
# tolerance per plot, so the threshold doesn't depend on which two values
# happen to be compared.
# Returns TRUE / FALSE, or NA when a count is missing (fewer counts than
# observers who judged the plot suitable, or fewer than two in total).
all_agree <- function(x, abs_tol, rel_tol, n_needed) {
  x <- x[!is.na(x)]
  if (n_needed < 2 || length(x) < n_needed) return(NA)
  tol <- max(abs_tol, rel_tol / 100 * mean(x))
  max(x) - min(x) <= tol
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
plot_status <- function(reads, abs_tol, rel_tol, n_required, reviews) {
  all_plots <- reads %>% distinct(id, fire_name, fire_year, plot_location)

  judged <- reads %>%
    filter(completed) %>%
    group_by(id) %>%
    summarise(
      n_counts = n(),
      observers = paste(sort(observer), collapse = ", "),
      n_unsuitable = sum(unsuitable),
      n_suitable = n_counts - n_unsuitable,
      pre_ok = all_agree(jt_pre_fire[!unsuitable], abs_tol, rel_tol, n_suitable),
      post_ok = all_agree(jt_post_fire[!unsuitable], abs_tol, rel_tol, n_suitable),
      pre_n = sum(!is.na(jt_pre_fire[!unsuitable])),
      post_n = sum(!is.na(jt_post_fire[!unsuitable])),
      vpre_n = sum(!is.na(pre_veg[!unsuitable])),
      vpost_n = sum(!is.na(post_veg[!unsuitable])),
      maj_pre_veg = majority_class(pre_veg[!unsuitable]),
      maj_post_veg = majority_class(post_veg[!unsuitable]),
      med_pre = median_count(jt_pre_fire[!unsuitable]),
      med_post = median_count(jt_post_fire[!unsuitable]),
      .groups = "drop"
    ) %>%
    mutate(
      f_suit = n_unsuitable > 0 & n_unsuitable < n_counts,
      # every observer who judged the plot suitable must enter both counts
      # and both cover classes
      f_missing = n_suitable > 0 & (pre_n < n_suitable | post_n < n_suitable),
      f_pre = pre_ok %in% FALSE,
      f_post = post_ok %in% FALSE,
      f_vmissing = n_suitable > 0 & (vpre_n < n_suitable | vpost_n < n_suitable),
      f_vpre = !f_vmissing & vpre_n >= 2 & is.na(maj_pre_veg),
      f_vpost = !f_vmissing & vpost_n >= 2 & is.na(maj_post_veg),
      excluded_rule = n_counts > 0 & n_unsuitable == n_counts
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
        # a suitability split needs agreed counts (or excluded = yes)
        TRUE ~
          (!(f_suit %in% TRUE) | (!is.na(rv_pre) & !is.na(rv_post))) &
          (!(f_pre %in% TRUE | pre_n < n_suitable)  | !is.na(rv_pre)) &
          (!(f_post %in% TRUE | post_n < n_suitable) | !is.na(rv_post)) &
          (!(f_vpre %in% TRUE | vpre_n < n_suitable)  | !is.na(rv_pre_veg)) &
          (!(f_vpost %in% TRUE | vpost_n < n_suitable) | !is.na(rv_post_veg))
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

# Each observer's record joined to its plot's flags (for shading)
observer_flags <- function(reads, st) {
  reads %>%
    filter(completed) %>%
    inner_join(select(st, id, status, rule_status, fully_counted, f_suit,
                      f_missing, f_pre, f_post, f_vmissing, f_vpre, f_vpost),
               by = "id")
}

# Reliability: ICC(2,1) ----
# Not shown in the app yet; kept here for when reliability is reported.
# reliability_summary(reads(), status()) returns the table for the paper.
# Two-way random effects, absolute agreement, single rater (Shrout & Fleiss
# 1979; McGraw & Wong 1996), as in Zett et al. (2022). Same formulas as
# irr::icc(model = "twoway", type = "agreement", unit = "single").
# `m` is a plots x observers matrix with no missing values.
icc_2_1 <- function(m, conf = 0.95) {
  m <- as.matrix(m)
  ns <- nrow(m); nr <- ncol(m)
  out <- list(n = ns, icc = NA_real_, lower = NA_real_, upper = NA_real_)
  if (ns < 3 || nr < 2) return(out)
  ss_total <- var(as.numeric(m)) * (ns * nr - 1)
  ms_r <- var(rowMeans(m)) * nr
  ms_c <- var(colMeans(m)) * ns
  ms_e <- (ss_total - ms_r * (ns - 1) - ms_c * (nr - 1)) / ((ns - 1) * (nr - 1))
  icc <- (ms_r - ms_e) / (ms_r + (nr - 1) * ms_e + (nr / ns) * (ms_c - ms_e))
  alpha <- 1 - conf
  a <- (nr * icc) / (ns * (1 - icc))
  b <- 1 + (nr * icc * (ns - 1)) / (ns * (1 - icc))
  v <- (a * ms_c + b * ms_e)^2 /
    ((a * ms_c)^2 / (nr - 1) + (b * ms_e)^2 / ((ns - 1) * (nr - 1)))
  fl <- qf(1 - alpha / 2, ns - 1, v)
  fu <- qf(1 - alpha / 2, v, ns - 1)
  lower <- (ns * (ms_r - fl * ms_e)) /
    (fl * (nr * ms_c + (nr * ns - nr - ns) * ms_e) + ns * ms_r)
  upper <- (ns * (fu * ms_r - ms_e)) /
    (nr * ms_c + (nr * ns - nr - ns) * ms_e + ns * fu * ms_r)
  list(n = ns, icc = icc, lower = lower, upper = upper)
}

koo_li_label <- function(x) {
  case_when(is.na(x) ~ NA_character_, x < 0.5 ~ "poor", x < 0.75 ~ "moderate",
            x < 0.9 ~ "good", TRUE ~ "excellent")
}

# Plots x observers matrix of the independent counts, for plots counted by
# every observer and judged suitable by all of them
count_matrix <- function(reads, st, var) {
  keep <- st$id[st$fully_counted & st$rule_status %in% c("Accepted", "Conflict")]
  wide <- reads %>%
    filter(completed, !unsuitable, id %in% keep) %>%
    select(id, observer, value = all_of(var)) %>%
    pivot_wider(names_from = observer, values_from = value)
  m <- as.matrix(wide[, -1, drop = FALSE])
  m[stats::complete.cases(m), , drop = FALSE]
}

mean_pairwise_diff <- function(m) {
  if (nrow(m) == 0 || ncol(m) < 2) return(NA_real_)
  pairs <- combn(ncol(m), 2)
  mean(apply(m, 1, function(r) mean(abs(r[pairs[1, ]] - r[pairs[2, ]]))))
}

reliability_summary <- function(reads, st) {
  map_dfr(c("Pre-fire count" = "jt_pre_fire", "Post-fire count" = "jt_post_fire"),
          function(var) {
            m <- count_matrix(reads, st, var)
            r <- icc_2_1(m)
            tibble(
              Plots = r$n,
              `ICC(2,1)` = round(r$icc, 3),
              `95% CI` = if (is.na(r$icc)) NA_character_ else
                sprintf("%.3f\u2013%.3f", r$lower, r$upper),
              `Koo & Li rating` = koo_li_label(r$icc),
              `Mean difference between observers (trees)` =
                round(mean_pairwise_diff(m), 2)
            )
          }, .id = "Count")
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
      .pre = f_pre | (f_missing & is.na(jt_pre_fire) & !unsuitable),
      .post = f_post | (f_missing & is.na(jt_post_fire) & !unsuitable),
      .vpre = f_vpre | (f_vmissing & is.na(pre_veg) & !unsuitable),
      .vpost = f_vpost | (f_vmissing & is.na(post_veg) & !unsuitable),
      .suit = f_suit %in% TRUE
    )
}

# Expandable rows ----
# The detail shown under a row when it's clicked: a one-line summary plus
# each observer's values. Built once for every plot whenever the data or
# settings change (not once per table render), and looked up by plot id.
plot_summary_line <- function(s) {
  fmt <- function(x) if (is.na(x)) "\u2013" else format(x)
  line <- if (s$status == "Accepted") {
    sprintf("Consensus: pre-fire %s, post-fire %s, vegetation %s \u2192 %s%s",
            fmt(s$cons_pre), fmt(s$cons_post), fmt(s$cons_pre_veg),
            fmt(s$cons_post_veg),
            if (is.na(s$cons_mortality)) "" else
              sprintf(", mortality %.0f%%", s$cons_mortality))
  } else if (s$status == "Conflict") {
    paste("Conflict:", s$issues)
  } else {
    as.character(s$status)
  }
  if (isTRUE(s$reviewed)) {
    line <- paste0(line, " \u00b7 Reviewed",
                   if (is.na(s$rv_note)) "" else paste0(": ", s$rv_note))
  }
  paste0("<p class='detail-summary'>", htmltools::htmlEscape(line), "</p>")
}

plot_details_all <- function(flags, st) {
  d <- conflict_long(flags, st, st$id)
  shade_td <- function(value, flag) {
    paste0("<td", ifelse(flag %in% TRUE, " class='shade'", ""), ">", value, "</td>")
  }
  d$row_html <- paste0(
    "<tr><td>", d$Observer, "</td>",
    shade_td(d$`Pre-fire`, d$.pre), shade_td(d$`Post-fire`, d$.post),
    "<td>", d$Mortality, "</td>",
    shade_td(d$`Veg pre`, d$.vpre), shade_td(d$`Veg post`, d$.vpost),
    shade_td(d$Suitable, d$.suit), "<td>", d$Comments, "</td></tr>")
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
                                 key_col = "Plot", editable = NULL,
                                 table_class = "display") {
  d$.detail <- unname(details[d[[key_col]]])
  detail_col <- match(".detail", names(d))
  no_escape <- unique(c(match(raw_html_cols, names(d)), detail_col))
  dt_options$columnDefs <- c(dt_options$columnDefs,
                             list(list(visible = FALSE, targets = detail_col - 1)))
  datatable(
    d, fillContainer = FALSE, rownames = FALSE, filter = filter,
    escape = -no_escape, selection = "none", editable = editable,
    class = table_class,
    callback = JS(sprintf(
      "table.on('click', 'tbody tr', function(e) {
         if ($(e.target).closest('td').hasClass('dt-editable-cell')) return;
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
           row.child(row.data()[%d]).show();
           tr.addClass('shown');
         }
       });", detail_col - 1)),
    options = dt_options
  )
}

# Pre-filled review sheet for unresolved conflicts. Reference values per
# observer aren't repeated here — they're in the expandable detail row
# (see plot_details_all()) so a disagreement is highlighted, not just listed.
review_template <- function(st) {
  ids <- st$id[st$status == "Conflict"]
  if (length(ids) == 0) {
    return(tibble(plot_id = character(0), fire = character(0),
                  location = character(0), issues = character(0),
                  pre_fire = character(0), post_fire = character(0),
                  pre_veg = character(0), post_veg = character(0),
                  excluded = character(0), note = character(0)))
  }
  st %>%
    filter(id %in% ids) %>%
    arrange(fire_name, id) %>%
    transmute(plot_id = id, fire = fire_name, location = plot_location, issues,
              pre_fire = "", post_fire = "", pre_veg = "", post_veg = "",
              excluded = "", note = "")
}

review_edit_cols <- c("pre_fire", "post_fire", "pre_veg", "post_veg", "excluded", "note")

# Rebuilds the review-sheet rows from the current conflict list (plots added
# or resolved elsewhere are added/dropped), while keeping whatever has
# already been typed into the editable columns for plots still in conflict.
refresh_review_edits <- function(old, st) {
  new_template <- review_template(st)
  if (is.null(old) || nrow(old) == 0 || nrow(new_template) == 0) return(new_template)
  old_edits <- old %>% select(plot_id, all_of(review_edit_cols))
  new_template %>%
    select(-all_of(review_edit_cols)) %>%
    left_join(old_edits, by = "plot_id") %>%
    mutate(across(all_of(review_edit_cols), ~ coalesce(.x, "")))
}

# Data checks: individual entries worth a second look ----
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
    suitable %>% filter(!pre_veg %in% c(veg_levels, NA) |
                          !post_veg %in% c(veg_levels, NA)) %>%
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
| **Conflict** | Observers disagree on at least one field (below); needs joint review |
| **Excluded** | All observers marked the plot unsuitable |
| **Accepted** | All observers agree on suitability and counts, and most agree on vegetation cover |

### When observers agree

- **Counts** (pre- and post-fire): **all** observers must agree. The spread of
  their counts (highest minus lowest) may be no more than **{ABS} tree(s) or
  {REL}% of their mean, whichever is larger**. {EXAMPLE}
- **Vegetation cover** (pre and post): at least two observers chose the same
  class (majority).
- **Suitability:** all observers must agree. A plot is excluded only if every
  observer marked it unsuitable; any split goes to joint review.
- **Missing values:** if any observer left a count or vegetation class blank,
  the plot goes to joint review.

### Consensus values

For accepted plots, counts are the **median** of the observers\' counts and
vegetation cover is the **majority** class (Swanson et al. 2016). Mortality is
calculated from the consensus counts, for inside plots only.

### Joint review

Conflicts are re-examined together, edited directly in the table on the
Joint review tab, then downloaded and uploaded as the **{SHEET}** sheet in the
shared folder. A conflict becomes Accepted once every field in question has
an agreed value (for a suitability split: both agreed counts), or Excluded if
marked so. Fields that were not in question may
be left blank and keep their automatic value. The observers\' own counts are
never changed.

### Low confidence

A count is flagged when at least {LOWMIN} observers marked low confidence for
the same period (pre- or post-fire). This can point to poor imagery, but also
to hard-to-count plots (clumps, standing dead trees), so check the comments.
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
                         " for plots averaging 5 trees and ", tol_at(30),
                         " for plots averaging 30.")
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
td.dt-editable-cell { cursor: text; }
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
table.dataTable > tbody > tr:not(.child):not(.shown):hover > td { background-color: #E6EAEE !important; }
table.dataTable > tbody > tr.shown { box-shadow: inset 3px 0 0 #4E6A3E; }
table.dataTable > tbody > tr.child > td { background: #FAFAF8; padding: .8rem 1.2rem; cursor: default; }
#review_table tbody > tr.shown {
  box-shadow: none;
}
#review_table tbody > tr.shown > td {
  background-color: #DCE6EE !important;
  border-top: 3px solid #4A6FA0;
}
#review_table tbody > tr.shown > td:first-child {
  border-left: 3px solid #4A6FA0;
}
#review_table tbody > tr.shown > td:last-child {
  border-right: 3px solid #4A6FA0;
}
#review_table tbody > tr.child > td {
  background: #E8F0F6 !important;
  border-left: 3px solid #4A6FA0;
  border-right: 3px solid #4A6FA0;
  border-bottom: 3px solid #4A6FA0;
  padding: .9rem 1.2rem 1.1rem;
  cursor: default;
}
.detail-summary { margin: 0 0 .6rem; }
.detail-table { margin: 0; }
.detail-table td, .detail-table th { padding: .3rem .7rem .3rem 0; vertical-align: top; }
.detail-table td.shade { background-color: #F4DED6; }
#review_table .detail-table tbody tr:hover > td,
#review_table .detail-table tbody tr:hover > th {
  background-color: #E8F0F6 !important;
}
#review_table .detail-table tbody tr:hover > td.shade {
  background-color: #F4DED6 !important;
}
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
    helpText("All observers' counts must agree: the spread (highest minus",
             "lowest) may be no more than the tree tolerance or the % of",
             "their mean, whichever is larger. Suitability must also be",
             "unanimous; vegetation cover goes by majority. Any blank",
             "entry sends the plot to review."),
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
    card(card_header("Observers"),
         helpText("Which export file is used for each observer, and how many",
                  "plots they have completed."),
         DTOutput("observer_files", fill = FALSE))
  ),

  nav_panel(
    "Joint review",
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Joint review",
        downloadButton("dl_review_edits", "Download completed sheet", class = "btn-sm")
      ),
      layout_columns(
        col_widths = c(4),
        selectInput("issue_filter", "Issue", choices = c("All", unname(issue_labels)))
      ),
      p("Each row is one plot in conflict. Click a row to see every",
        "observer's entries underneath (disagreements shaded red). Fill in",
        "the agreed values directly in the last six columns of the row",
        "itself (click a cell to edit it) — leave a field blank to keep",
        "its automatic value, and only fill in what's actually in conflict.",
        "For", tags$code("excluded"), "write yes if the plot should be",
        "excluded, otherwise leave it blank; for the vegetation columns use",
        paste(veg_levels, collapse = "/"), "."),
      DTOutput("review_table", fill = FALSE),
      p(class = "mt-3",
        "When you're done, download the completed sheet and upload it to the",
        "shared Drive folder (keep the name", tags$code(paste0(REVIEW_SHEET, ".csv")),
        "). The app re-reads it from there on the next refresh — this page",
        "doesn't save your edits anywhere by itself."),
      p(downloadButton("dl_conflicts", "Download raw per-observer CSV", class = "btn-sm"))
    )
  ),

  nav_panel(
    "Summary plots",
    card(card_header("Plot status by fire"),
         plotOutput("status_by_fire_plot", height = "500px"),
         uiOutput("conflict_rate_text")),
    card(card_header("Mortality so far, by fire (inside plots)"),
         plotOutput("mortality_plot", height = "500px"),
         helpText("Pooled mortality: total trees that died ÷ total",
                  "pre-fire trees, over the accepted inside plots of each",
                  "fire."))
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
                  "values or a post-fire count above the pre-fire count."),
         layout_columns(
           col_widths = c(4),
           selectInput("check_filter", "Issue", choices = c("All", unname(check_labels)))
         ),
         DTOutput("checks_table", fill = FALSE)),
    card(
      card_header("Downloads"),
      p(tags$strong("Consensus data:"), "one row per plot, with the consensus",
        "counts, vegetation cover, mortality and low-confidence flags used in",
        "the analysis. Values are blank for conflicts until they are resolved."),
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

  low_conf <- reactive(low_confidence_by_plot(reads()))

  status <- reactive({
    s <- settings()
    plot_status(reads(), s$abs_tol, s$rel_tol, s$n_required, reviews()) %>%
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
                    "conflict before review, %d of those resolved."),
              n_full, n_conf_before, 100 * n_conf_before / n_full,
              sum(full$resolved))
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
                Issues = issues,
                `Low confidence` = low_conf_label,
                Reviewed = if_else(reviewed, "yes", ""),
                `Pre (consensus)` = cons_pre, `Post (consensus)` = cons_post,
                Comments = coalesce(Comments, ""))
    if (!is.null(showing) && showing %in% c("Not started", "In progress", "Excluded")) {
      d <- select(d, -`Pre (consensus)`, -`Post (consensus)`)
    }
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
  conflict_ids <- reactive({
    st <- filter(status(), status == "Conflict")
    if (input$issue_filter != "All") {
      st <- filter(st, str_detect(issues, fixed(input$issue_filter)))
    }
    st$id
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

  # Edited in-app, one row per conflicted plot; click a row to expand the
  # per-observer detail (same shaded-disagreement view used elsewhere).
  review_edits <- reactiveVal(NULL)
  observe({
    review_edits(refresh_review_edits(isolate(review_edits()), status()))
  })

  # The subset actually rendered (after the Issue filter) — cell-edit row
  # indices from DT refer to this subset's own row order, not review_edits().
  review_view <- reactive({
    d <- review_edits()
    validate(need(!is.null(d), "No unresolved conflicts to review."))
    if (input$issue_filter != "All") {
      d <- filter(d, str_detect(issues, fixed(input$issue_filter)))
    }
    d
  })

  output$review_table <- renderDT({
    d <- review_view()
    validate(need(nrow(d) > 0, "No plots match the current filter."))
    editable_cols <- which(names(d) %in% review_edit_cols) - 1
    expandable_datatable(
      d, details(), key_col = "plot_id",
      editable = list(target = "cell",
                      disable = list(columns = setdiff(seq_along(d) - 1, editable_cols))),
      table_class = "display dt-joint-review",
      dt_options = list(pageLength = 10, scrollX = TRUE,
                        columnDefs = list(list(targets = editable_cols,
                                               className = "dt-editable-cell")))
    )
  })

  observeEvent(input$review_table_cell_edit, {
    edited <- DT::editData(review_view(), input$review_table_cell_edit, rownames = FALSE)
    full <- review_edits()
    full[match(edited$plot_id, full$plot_id), ] <- edited
    review_edits(full)
  })

  output$dl_review_edits <- downloadHandler(
    filename = function() paste0(REVIEW_SHEET, ".csv"),
    content = function(file) {
      write.csv(review_edits(), file, row.names = FALSE, na = "")
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
             # puts accepted on the left and not started on the right
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
    validate(need(nrow(mort) > 0, "No accepted inside plots yet."))
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
    d <- data_checks(reads())
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
        select(-starts_with("f_"), -any_issue, -accepted) %>%
        write.csv(file, row.names = FALSE, na = "")
    }
  )

  # Sidebar ----
  output$tolerance_example <- renderUI({
    s <- settings()
    tol <- function(m) format(round(max(s$abs_tol, s$rel_tol / 100 * m), 1))
    helpText(tags$em(sprintf(
      "With these settings: a spread of up to %s tree(s) for plots averaging 5 trees, %s for 15, %s for 30.",
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
