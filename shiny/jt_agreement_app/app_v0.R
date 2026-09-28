# ─────────────────────────────────────────────────────────────────────
# Joshua tree observer agreement — Shiny app
# Upload each observer's latest Collect Earth CSV export and get
# Colandr-style agreement / conflict status for every plot.
#
# Packages: shiny, bslib, DT, dplyr, tidyr, ggplot2, stringr, purrr
# Run locally:  shiny::runApp("jt_agreement_app")
# ─────────────────────────────────────────────────────────────────────

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)

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

drive_poll_ms <- 60000   # how often to re-check Google Drive
folder_poll_ms <- 15000  # how often to re-check a local folder

export_pattern <- "collectedData.*\\.csv$"

empty_files <- function() {
  data.frame(name = character(0), datapath = character(0),
             mtime = as.POSIXct(character(0)))
}

list_folder_files <- function(folder) {
  folder <- path.expand(folder)
  if (!dir.exists(folder)) stop("Folder not found: ", folder)
  paths <- list.files(folder, pattern = export_pattern, ignore.case = TRUE,
                      full.names = TRUE, recursive = TRUE)
  if (length(paths) == 0) return(empty_files())
  data.frame(name = basename(paths), datapath = paths,
             mtime = file.info(paths)$mtime)
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
                      character(1))
  )
}

drive_fetch <- function(id, path) {
  googledrive::drive_download(googledrive::as_id(id), path = path,
                              overwrite = TRUE)
}

list_drive_files <- function(folder) {
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("Install the googledrive package: install.packages(\"googledrive\")")
  }
  found <- drive_list_raw(folder)
  found <- found[grepl(export_pattern, found$name, ignore.case = TRUE), ]
  if (nrow(found) == 0) return(empty_files())
  dir.create(drive_cache, showWarnings = FALSE, recursive = TRUE)
  # one cached copy per file version, so unchanged files aren't re-downloaded
  paths <- file.path(drive_cache, paste0(found$id, "_",
                                         gsub("[^0-9]", "", found$modified),
                                         ".csv"))
  for (i in seq_len(nrow(found))) {
    if (!file.exists(paths[i]) || is.na(found$modified[i])) {
      drive_fetch(found$id[i], paths[i])
    }
  }
  data.frame(name = found$name, datapath = paths,
             mtime = as.POSIXct(found$modified, format = "%Y-%m-%dT%H:%M:%OS",
                                tz = "UTC"))
}

folder_source <- function() {
  if (nzchar(DRIVE_FOLDER)) "drive" else if (nzchar(DATA_FOLDER)) "local" else "none"
}

# Status categories, in display order (mirrors Colandr's bar) ----
status_levels <- c("Unscreened", "Single read", "Conflict", "Excluded", "Agreed")
status_help <- c(
  "Unscreened"  = "Plot opened in Collect Earth but no counts saved yet",
  "Single read" = "Only one observer has completed this plot",
  "Conflict"    = "Two or more reads that disagree beyond the tolerances",
  "Excluded"    = "All observers marked the plot unsuitable",
  "Agreed"      = "Two or more reads within tolerance"
)
status_colors <- c(
  "Unscreened"  = "#B8B3A2",
  "Single read" = "#C9A55A",
  "Conflict"    = "#A3452F",
  "Excluded"    = "#6F7773",
  "Agreed"      = "#4E6A3E"
)
status_text <- c("Unscreened" = "#2B2B2B", "Single read" = "#2B2B2B",
                 "Conflict" = "#FFFFFF", "Excluded" = "#FFFFFF",
                 "Agreed" = "#FFFFFF")

reason_labels <- c(
  suit_conflict     = "Suitability",
  missing_conflict  = "Missing count",
  pre_conflict      = "Pre-fire count",
  post_conflict     = "Post-fire count",
  mort_conflict     = "Mortality",
  veg_pre_conflict  = "Pre veg cover",
  veg_post_conflict = "Post veg cover"
)
veg_flags <- c("veg_pre_conflict", "veg_post_conflict")
veg_levels <- c("low", "medium", "high")

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

# Agreement logic ----

# TRUE if the spread of counts exceeds max(absolute, relative-to-mean) tolerance
beyond_tolerance <- function(x, abs_tol, rel_tol) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(FALSE)
  (max(x) - min(x)) > max(abs_tol, rel_tol / 100 * mean(x))
}

spread <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) NA_real_ else max(x) - min(x)
}

partly_missing <- function(x) any(is.na(x)) && any(!is.na(x))

plot_status <- function(reads, abs_tol, rel_tol, mort_tol, veg_blocks) {
  all_plots <- reads %>%
    distinct(id, fire_name, fire_year, plot_location)

  judged <- reads %>%
    filter(completed) %>%
    group_by(id) %>%
    summarise(
      n_reads = n(),
      observers = paste(sort(observer), collapse = ", "),
      n_unsuitable = sum(unsuitable),
      suit_conflict = n_unsuitable > 0 & n_unsuitable < n_reads,
      missing_conflict = sum(!unsuitable) >= 2 &
        (partly_missing(jt_pre_fire[!unsuitable]) |
           partly_missing(jt_post_fire[!unsuitable])),
      pre_conflict = beyond_tolerance(jt_pre_fire[!unsuitable], abs_tol, rel_tol),
      post_conflict = beyond_tolerance(jt_post_fire[!unsuitable], abs_tol, rel_tol),
      mort_conflict = coalesce(spread(mortality[!unsuitable]) > mort_tol, FALSE),
      veg_pre_conflict = n_distinct(na.omit(pre_veg[!unsuitable])) > 1,
      veg_post_conflict = n_distinct(na.omit(post_veg[!unsuitable])) > 1,
      pre_spread = spread(jt_pre_fire[!unsuitable]),
      post_spread = spread(jt_post_fire[!unsuitable]),
      mort_spread = spread(mortality[!unsuitable]),
      .groups = "drop"
    )

  if (nrow(judged) > 0) {
    flag_matrix <- as.matrix(judged[names(reason_labels)])
    blocking <- names(reason_labels)
    if (!veg_blocks) blocking <- setdiff(blocking, veg_flags)
    judged$disagreements <- apply(flag_matrix, 1, function(row) {
      paste(reason_labels[names(row)[row]], collapse = "; ")
    })
    judged$blocking <- rowSums(flag_matrix[, blocking, drop = FALSE]) > 0
  } else {
    judged$disagreements <- character(0)
    judged$blocking <- logical(0)
  }

  all_plots %>%
    left_join(judged, by = "id") %>%
    mutate(
      n_reads = coalesce(n_reads, 0L),
      status = case_when(
        n_reads == 0 ~ "Unscreened",
        n_reads == 1 ~ "Single read",
        n_unsuitable == n_reads ~ "Excluded",
        blocking ~ "Conflict",
        TRUE ~ "Agreed"
      ),
      status = factor(status, levels = status_levels)
    )
}

# Lin's concordance correlation coefficient
lins_ccc <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3) return(NA_real_)
  sxy <- mean((x - mean(x)) * (y - mean(y)))
  denom <- mean((x - mean(x))^2) + mean((y - mean(y))^2) + (mean(x) - mean(y))^2
  if (denom == 0) return(NA_real_)
  2 * sxy / denom
}

pct <- function(x) if (length(x) == 0) NA_real_ else round(mean(x) * 100, 1)

pairwise_agreement <- function(reads, abs_tol, rel_tol, mort_tol) {
  r <- reads %>% filter(completed, !unsuitable)
  observers <- sort(unique(r$observer))
  if (length(observers) < 2) return(NULL)

  within <- function(a, b) {
    ok <- !is.na(a) & !is.na(b)
    abs(a - b)[ok] <= pmax(abs_tol, rel_tol / 100 * (a + b) / 2)[ok]
  }
  absdiff <- function(a, b) abs(a - b)[!is.na(a) & !is.na(b)]
  same_class <- function(a, b) (a == b)[!is.na(a) & !is.na(b)]

  map_dfr(combn(observers, 2, simplify = FALSE), function(pair) {
    j <- inner_join(filter(r, observer == pair[1]),
                    filter(r, observer == pair[2]),
                    by = "id", suffix = c("_a", "_b"))
    tibble(
      `Observer A` = pair[1],
      `Observer B` = pair[2],
      `Shared plots` = nrow(j),
      `Pre: % within tol.` = pct(within(j$jt_pre_fire_a, j$jt_pre_fire_b)),
      `Pre: median |diff|` = median(absdiff(j$jt_pre_fire_a, j$jt_pre_fire_b)),
      `Pre: mean diff (A-B)` = round(mean(j$jt_pre_fire_a - j$jt_pre_fire_b,
                                          na.rm = TRUE), 2),
      `Pre: Lin's CCC` = round(lins_ccc(j$jt_pre_fire_a, j$jt_pre_fire_b), 3),
      `Post: % within tol.` = pct(within(j$jt_post_fire_a, j$jt_post_fire_b)),
      `Post: median |diff|` = median(absdiff(j$jt_post_fire_a, j$jt_post_fire_b)),
      `Post: Lin's CCC` = round(lins_ccc(j$jt_post_fire_a, j$jt_post_fire_b), 3),
      `Mortality: % within tol.` =
        pct(absdiff(j$mortality_a, j$mortality_b) <= mort_tol),
      `Mortality: median |diff| (pp)` =
        round(median(absdiff(j$mortality_a, j$mortality_b)), 1),
      `Veg pre: % same class` = pct(same_class(j$pre_veg_a, j$pre_veg_b)),
      `Veg post: % same class` = pct(same_class(j$post_veg_a, j$post_veg_b))
    )
  })
}

# Grouped conflict table: one row per observer, one block per plot ----

fmt_count <- function(x) ifelse(is.na(x), "\u2013", format(x, trim = TRUE))

with_conf <- function(value, conf) {
  ifelse(is.na(conf), value,
         paste0(value, " <span class='conf'>", htmltools::htmlEscape(conf),
                "</span>"))
}

conflict_long <- function(reads, st) {
  esc <- htmltools::htmlEscape
  reads %>%
    filter(id %in% st$id, completed) %>%
    inner_join(select(st, id, disagreements, suit_conflict, missing_conflict,
                      pre_conflict, post_conflict, mort_conflict,
                      veg_pre_conflict, veg_post_conflict),
               by = "id") %>%
    arrange(fire_name, id, observer) %>%
    group_by(id) %>%
    mutate(first_row = row_number() == 1) %>%
    ungroup() %>%
    transmute(
      Plot = ifelse(first_row,
                    paste0("<b>", esc(id), "</b><div class='plot-meta'>",
                           esc(plot_location), " \u00b7 ",
                           esc(disagreements), "</div>"),
                    ""),
      Observer = esc(observer),
      `Pre-fire` = with_conf(fmt_count(jt_pre_fire), pre_confidence),
      `Post-fire` = with_conf(fmt_count(jt_post_fire), post_confidence),
      Mortality = ifelse(is.na(mortality), "\u2013", sprintf("%.0f%%", mortality)),
      `Veg pre` = coalesce(pre_veg, "\u2013"),
      `Veg post` = coalesce(post_veg, "\u2013"),
      Suitable = ifelse(unsuitable, "no", "yes"),
      Comments = coalesce(esc(comments), ""),
      # hidden helper columns for styling
      .first = first_row,
      .pre = pre_conflict | missing_conflict,
      .post = post_conflict | missing_conflict,
      .mort = mort_conflict,
      .vpre = veg_pre_conflict,
      .vpost = veg_post_conflict,
      .suit = suit_conflict
    )
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

# "How it works" page ----
# Written as Markdown with placeholders filled from the current settings,
# so the explanation always matches what the app is doing.

about_template <- '
### What the app does

Each observer counts adult Joshua trees in the same 60 &times; 60 m plots in
Collect Earth. This app gathers everyone\'s exports, lines up the plots that
two or more people have completed, and flags where their readings disagree,
so disagreements can be reviewed and resolved before the counts are used in
analyses.

### Where the data comes from

{SOURCE} Only CSV files with **collectedData** in the name are read (the
default Collect Earth export name); anything else in the folder is ignored.
Files added through the upload button are included for your session only.

**Which file is used for each observer.** {NEWEST}

**Who the file belongs to.** The observer is the most common Collect Earth
operator name in the file. Occasional rows saved under another name (such as
"shared") are still counted as that observer\'s, and the name is listed in
the observer table so you can see it happened.

**One record per plot.** A file can contain several versions of the same
plot (for example an early placeholder and the real count). The app keeps
the version that was actively saved (Save clicked in Collect Earth), and
among those the most recent save date. Collect Earth records the save date
but not the time, so two saves on the same day are separated by file order.

### What is derived from each record

- **Fire, year and plot location** come from the plot ID, e.g.
  `york_2023_inside_02` &rarr; York fire, 2023, inside (burned) plot.
- **Unsuitable** means the observer marked the plot unsuitable.
- **Completed** means the observer either entered a count or marked the plot
  unsuitable.
- **Mortality** = (pre-fire count &minus; post-fire count) &divide; pre-fire
  count &times; 100. It is calculated for suitable **inside** plots only, when
  the pre-fire count is above zero. It is not calculated for outside plots,
  where a one-tree difference in a sparse unburned plot would swing it wildly.

### Plot status

Every plot that appears in any file gets one status, shown in the bar on the
Overview tab:

| Status | Meaning |
|---|---|
| **Unscreened** | Opened in Collect Earth, but nobody has entered counts yet |
| **Single read** | Only one observer has completed it; waiting for a second read |
| **Excluded** | Two or more observers completed it and **all** marked it unsuitable |
| **Conflict** | Two or more reads, and they disagree on at least one rule below |
| **Agreed** | Two or more reads, and none of the rules below are broken |

The headline agreement rate is Agreed &divide; (Agreed + Conflict): the share
of plots with at least two suitable reads that agree.

### When reads count as a conflict

A plot read by two or more observers is a conflict if **any** of these apply.
With three or more observers, the spread is the highest value minus the
lowest.

1. **Suitability:** some observers marked it unsuitable and others did not.
2. **Missing count:** one observer entered a pre- or post-fire count that
   another observer left blank.
3. **Pre-fire count:** the counts differ by more than the tolerance (below).
4. **Post-fire count:** same rule, applied to the post-fire counts.
5. **Mortality** (inside plots only): mortality differs by more than
   **{MORT} percentage points**.
6. **Vegetation cover:** {VEG}

Observers who marked a plot unsuitable are left out of rules 2&ndash;6; their
disagreement is already captured by rule 1.

**The count tolerance** is the larger of an absolute and a relative
allowance. Currently counts may differ by up to **{ABS} tree(s)** or
**{REL}% of the average count**, whichever is larger. The relative part lets
dense plots tolerate a slightly bigger absolute difference than sparse ones.
{EXAMPLE}

All tolerances can be changed in the sidebar; statuses, tables and this page
update immediately.

### The Agreement tab

**Pairwise observer agreement** compares each pair of observers on the
suitable plots they have both completed.

- **% within tol.:** share of shared plots where the two counts are within
  the count tolerance above (for mortality, within {MORT} percentage points).
- **Median |diff|:** the typical size of the difference, in trees (or
  percentage points for mortality), ignoring direction.
- **Mean diff (A&minus;B):** the average signed difference. Near zero means
  no systematic bias; a positive value means observer A tends to count more
  trees than observer B.
- **Lin\'s CCC** (concordance correlation coefficient): how closely the pairs
  of counts fall on the 1:1 line, from &minus;1 to 1, where 1 is perfect
  agreement. Unlike an ordinary correlation, it is lowered by systematic bias
  as well as by scatter. It needs at least three shared plots, and it can look
  low when counts are all similar (e.g. mostly zeros post-fire) even if
  differences are small, so read it alongside the median difference.
- **Veg pre / Veg post: % same class:** share of shared plots given the same
  vegetation cover class (low, medium, high).

**By fire and plot location** summarises plots with two or more suitable
reads: how many there are, the % agreed, the number of conflicts, and the
median spread (highest minus lowest reading) for pre-fire counts, post-fire
counts and mortality. It is sorted with the lowest agreement first, to show
which fires are hardest to count.

### Other tabs

- **Conflicts:** every conflicting plot, one row per observer, with the
  values in disagreement shaded and each observer\'s confidence and comments
  alongside. Download it as a CSV to work through reconciliation.
- **Plots:** each pair of observers\' counts plotted against the 1:1 line
  (points on the dashed line agree exactly), and mortality by fire and
  observer.
- **Data checks:** individual entries worth a second look regardless of
  agreement: post-fire counts higher than pre-fire on inside plots, missing
  counts, vegetation cover or confidence, plots marked unsuitable that still
  have counts, and plots opened but never saved.
- **Download:** the cleaned data (one row per observer &times; plot, with the
  derived fields and status) and the plot status summary.

### Keep in mind

- **Agreement is not accuracy.** Two observers can agree and both be wrong,
  e.g. if the same shrub is consistently mistaken for a Joshua tree.
- **Tolerances are a judgement call.** Choose values that reflect what you
  consider an acceptable counting difference, and report them with the results.
- **Statuses are recalculated from the files every time.** The app does not
  store any decisions; resolving a conflict means an observer corrects their
  entry in Collect Earth and exports again.
'

about_markdown <- function(abs_tol, rel_tol, mort_tol, veg_blocks,
                           newest_only, source) {
  source_text <- switch(
    source,
    drive = paste("The app reads the shared Google Drive folder set in",
                  "`DRIVE_FOLDER` (including subfolders) when it opens, and",
                  "re-checks it every minute. Only new or changed files are",
                  "downloaded."),
    local = paste("The app reads the local folder set in `DATA_FOLDER`",
                  "(including subfolders) and re-checks it every 15 seconds."),
    paste("No shared folder is set, so the app uses uploaded files only.",
          "Set `DRIVE_FOLDER` at the top of app.R to read a Google Drive",
          "folder automatically.")
  )
  newest_text <- if (isTRUE(newest_only)) {
    paste("Only each observer\'s newest export is used (the sidebar",
          "option is on). Collect Earth exports are cumulative, so the newest",
          "file should contain all of that observer\'s plots. The newest file",
          "is the one with the latest save date inside it, not the latest",
          "upload, so re-uploading an old export does no harm. If two files",
          "tie, the one uploaded later wins. Older files are ignored and",
          "counted in the observer table on the Overview tab.")
  } else {
    paste("All of each observer\'s files are merged (the \"newest file\"",
          "option is off). If the same plot appears in several files, the",
          "most recently saved version is kept, as described below.")
  }
  veg_text <- if (isTRUE(veg_blocks)) {
    paste("observers chose different cover classes before or after the fire.",
          "This currently **does** count as a conflict (sidebar option on).")
  } else {
    paste("observers chose different cover classes. This is listed under",
          "Disagreements but currently does **not** make a plot a conflict",
          "(sidebar option off).")
  }
  tol_at <- function(m) format(round(max(abs_tol, rel_tol / 100 * m), 1))
  example_text <- paste0(
    "For example, with the current settings the allowed difference is ",
    tol_at(3), " tree(s) for plots averaging 3 trees, ", tol_at(20),
    " for plots averaging 20, and ", tol_at(40), " for plots averaging 40. ",
    "A difference exactly equal to the tolerance still counts as agreement.")

  md <- about_template
  fill <- c("{SOURCE}" = source_text, "{NEWEST}" = newest_text,
            "{VEG}" = veg_text, "{EXAMPLE}" = example_text,
            "{ABS}" = format(abs_tol), "{REL}" = format(rel_tol),
            "{MORT}" = format(mort_tol))
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
.about { max-width: 46rem; padding: .5rem 0 2rem; line-height: 1.55; }
.about h3 { margin-top: 1.8rem; font-size: 1.3rem; }
.about table { margin: .6rem 0 1rem; }
.about td, .about th { padding: .35rem .8rem .35rem 0; vertical-align: top;
                       border-bottom: 1px solid #e3e5e2; }
.plot-meta { color: #5f6660; font-size: .85em; margin-top: 2px; }
table.conflict-table td { white-space: normal; }
table.dataTable th { word-break: normal; overflow-wrap: normal; }
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
    helpText("Uploads are added to the shared folder's files for this",
             "session only; other people won't see them."),
    tags$hr(),
    tags$strong("Conflict tolerances"),
    numericInput("abs_tol", "Counts: absolute (trees)", value = 1,
                 min = 0, step = 1),
    numericInput("rel_tol", "Counts: relative (% of mean)", value = 10,
                 min = 0, step = 5),
    helpText("A count conflicts if observers differ by more than the",
             "larger of the two."),
    numericInput("mort_tol", "Mortality (percentage points)", value = 10,
                 min = 0, step = 5),
    checkboxInput("newest_only", "Use only each observer's newest file",
                  value = TRUE),
    helpText("Collect Earth exports are cumulative, so the newest file",
             "should hold everything. Untick to merge all files instead."),
    checkboxInput("veg_blocks", "Vegetation cover disagreement is a conflict",
                  value = FALSE),
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
      DTOutput("plot_table", fill = FALSE)
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Uploaded observers"), DTOutput("observer_table", fill = FALSE)),
      card(card_header("Plots completed per fire"), DTOutput("progress_table", fill = FALSE))
    )
  ),

  nav_panel(
    "Conflicts",
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Plots to reconcile",
        downloadButton("dl_conflicts", "Download CSV", class = "btn-sm")
      ),
      selectInput("reason_filter", "Disagreement type",
                  choices = c("All", unname(reason_labels)), width = "260px"),
      helpText("One row per observer. Shaded cells are the values in disagreement."),
      DTOutput("conflict_table", fill = FALSE)
    )
  ),

  nav_panel(
    "Agreement",
    card(card_header("Pairwise observer agreement (suitable plots only)"),
         DTOutput("pairwise_table", fill = FALSE),
         helpText("Lin's CCC measures agreement with the 1:1 line (1 = perfect).",
                  "Mean diff shows whether one observer systematically counts",
                  "more trees than the other.")),
    card(card_header("By fire and plot location"), DTOutput("fire_table", fill = FALSE))
  ),

  nav_panel(
    "Plots",
    layout_columns(
      col_widths = c(12),
      card(
        card_header(
          class = "d-flex gap-3 align-items-center",
          "Observer vs observer",
          selectInput("pair_a", NULL, choices = NULL, width = "180px"),
          selectInput("pair_b", NULL, choices = NULL, width = "180px")
        ),
        plotOutput("pair_plot", height = "380px")
      ),
      card(card_header("Mortality by fire and observer (inside plots)"),
           plotOutput("mortality_plot", height = "420px"))
    )
  ),

  nav_panel(
    "Data checks",
    card(card_header("Entries worth a second look, per observer"),
         DTOutput("checks_table", fill = FALSE))
  ),

  nav_panel(
    "How it works",
    div(class = "about", uiOutput("about_page"))
  ),

  nav_panel(
    "Download",
    card(
      card_header("Cleaned data"),
      p("One row per observer \u00d7 plot (latest saved version), with derived",
        "fire, location, mortality, and plot status."),
      downloadButton("dl_reads", "Cleaned reads (long format)"),
      downloadButton("dl_status", "Plot status summary")
    )
  )
)

# Server ----

server <- function(input, output, session) {

  # Files in the shared folder, re-checked on a timer or on request.
  # reactiveVal only notifies when the file list actually changes.
  folder_files <- reactiveVal(empty_files())
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
      msg <- gsub("\033\\[[0-9;]*m", "", conditionMessage(result))  # drop colour codes
      msg <- gsub("\\s+", " ", msg)
      if (grepl("insufficient authentication scopes|insufficientPermissions", msg)) {
        msg <- paste("R is signed in to Google without Drive access. Stop the app,",
                     "run unlink(\"~/Library/Caches/gargle\", recursive = TRUE)",
                     "then googledrive::drive_auth() and tick the Drive checkbox",
                     "on the permissions page.")
      } else if (grepl("File not found|404", msg)) {
        msg <- paste("Drive folder not found. Check the DRIVE_FOLDER link and that",
                     "this Google account can open the folder.")
      }
      folder_error(msg)
    } else {
      folder_error(NULL)
      folder_files(result)
    }
    folder_checked(Sys.time())
  })

  all_files <- reactive({
    uploaded <- if (is.null(input$files)) NULL else
      data.frame(name = input$files$name, datapath = input$files$datapath,
                 mtime = Sys.time())
    rbind(folder_files(), uploaded)
  })

  has_data <- reactive(nrow(all_files()) > 0)

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
    obs <- sort(unique(reads_all()$observer))
    updateSelectInput(session, "pair_a", choices = obs, selected = obs[1])
    updateSelectInput(session, "pair_b", choices = obs,
                      selected = obs[min(2, length(obs))])
  })

  reads <- reactive({
    r <- reads_all()
    if (length(input$fire_filter) > 0) r <- filter(r, fire_name %in% input$fire_filter)
    r
  })

  status <- reactive({
    plot_status(reads(), input$abs_tol, input$rel_tol, input$mort_tol,
                input$veg_blocks)
  })

  # Status bar with click-to-filter
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
      return(card(
        card_body(
          h4("No exports yet"),
          p("Observers save their Collect Earth CSV exports into the shared",
            "folder (or you can upload them in the sidebar). The app keeps",
            "the latest saved version of each plot per observer and compares",
            "every plot that two or more people have completed.")
        )
      ))
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
    two_plus <- st %>% filter(status %in% c("Conflict", "Agreed"))
    agree_rate <- if (nrow(two_plus) > 0) {
      sprintf("%.0f%% of the %d suitable plots with two or more reads are in agreement.",
              mean(two_plus$status == "Agreed") * 100, nrow(two_plus))
    } else {
      "No plots have two or more suitable reads yet."
    }
    div(class = "status-note",
        agree_rate, " Click a category to filter the table below; click again to clear.")
  })

  output$plot_table_title <- renderText({
    if (is.null(selected_status())) "All plots" else paste("Plots:", selected_status())
  })

  # Each observer's comment on a plot, labelled with their name
  plot_comments <- reactive({
    esc <- htmltools::htmlEscape
    reads() %>%
      filter(!is.na(comments), nzchar(str_trim(comments))) %>%
      arrange(observer) %>%
      group_by(id) %>%
      summarise(Comments = paste0("<b>", esc(observer), ":</b> ",
                                  esc(str_trim(comments)), collapse = "<br>"),
                .groups = "drop")
  })

  output$plot_table <- renderDT({
    st <- status()
    showing <- selected_status()
    if (!is.null(showing)) st <- filter(st, status == showing)
    d <- st %>%
      arrange(status, fire_name, id) %>%
      left_join(plot_comments(), by = "id") %>%
      transmute(Plot = id, Fire = fire_name, Location = plot_location,
                Status = status, Reads = n_reads, Observers = observers,
                Disagreements = disagreements,
                `Pre spread` = pre_spread, `Post spread` = post_spread,
                `Mortality spread (pp)` = round(mort_spread, 1),
                Comments = coalesce(Comments, ""))
    # Counts are never compared for these statuses, so drop the empty columns
    if (!is.null(showing) && showing %in% c("Excluded", "Unscreened", "Single read")) {
      d <- select(d, -Disagreements, -`Pre spread`, -`Post spread`,
                  -`Mortality spread (pp)`)
    }
    comments_col <- match("Comments", names(d))
    datatable(d, fillContainer = FALSE, rownames = FALSE, filter = "top",
              escape = -comments_col,
              options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE,
                             columnDefs = list(list(width = "340px",
                                                    targets = comments_col - 1)))) %>%
      formatStyle("Status", backgroundColor = styleEqual(status_levels,
                                                         unname(status_colors)),
                  color = styleEqual(status_levels, unname(status_text)))
  })


  output$about_page <- renderUI({
    shiny::markdown(about_markdown(input$abs_tol, input$rel_tol,
                                   input$mort_tol, input$veg_blocks,
                                   input$newest_only, folder_source()))
  })

  output$observer_table <- renderDT({
    counts <- reads_all() %>%
      group_by(observer) %>%
      summarise(
        `Plots completed` = sum(completed),
        `Marked unsuitable` = sum(unsuitable),
        `Other operator names` = paste(unique(na.omit(other_operator)),
                                       collapse = ", "),
        .groups = "drop"
      )
    file_info() %>%
      group_by(observer) %>%
      summarise(
        `File in use` = paste(source_file[used], collapse = "; "),
        `Newest save in file` = format(max(newest_save[used], na.rm = TRUE)),
        `Older files ignored` = sum(!used),
        .groups = "drop"
      ) %>%
      left_join(counts, by = "observer") %>%
      rename(Observer = observer) %>%
      datatable(fillContainer = FALSE, rownames = FALSE,
                options = list(dom = "t", scrollX = TRUE))
  })


  output$progress_table <- renderDT({
    totals <- reads() %>% distinct(fire_name, id) %>% count(Fire = fire_name,
                                                            name = "Plots")
    reads() %>%
      filter(completed) %>%
      count(Fire = fire_name, observer) %>%
      pivot_wider(names_from = observer, values_from = n, values_fill = 0) %>%
      right_join(totals, by = "Fire") %>%
      relocate(Plots, .after = Fire) %>%
      mutate(across(-c(Fire, Plots), ~ coalesce(.x, 0L))) %>%
      arrange(Fire) %>%
      datatable(fillContainer = FALSE, rownames = FALSE, options = list(pageLength = 20, dom = "tp",
                                                 scrollX = TRUE))
  })

  # Conflicts
  conflict_rows <- reactive({
    st <- status() %>% filter(status == "Conflict")
    if (input$reason_filter != "All") {
      st <- filter(st, str_detect(disagreements, fixed(input$reason_filter)))
    }
    st
  })

  output$conflict_table <- renderDT({
    st <- conflict_rows()
    validate(need(nrow(st) > 0, "No conflicts with the current settings."))
    d <- conflict_long(reads(), st)
    helper_idx <- which(startsWith(names(d), ".")) - 1
    highlight <- "#F4DED6"
    datatable(d, fillContainer = FALSE, rownames = FALSE, escape = FALSE,
              class = "compact conflict-table",
              options = list(ordering = FALSE, pageLength = 60,
                             lengthMenu = c(30, 60, 150, 500), scrollX = TRUE,
                             columnDefs = list(
                               list(visible = FALSE, targets = helper_idx),
                               list(width = "210px", targets = 0),
                               list(width = "320px", targets = 9)))) %>%
      formatStyle(names(d)[1:9], valueColumns = ".first",
                  borderTop = styleEqual(TRUE, "2px solid #9AA39C")) %>%
      formatStyle("Pre-fire", valueColumns = ".pre",
                  backgroundColor = styleEqual(TRUE, highlight)) %>%
      formatStyle("Post-fire", valueColumns = ".post",
                  backgroundColor = styleEqual(TRUE, highlight)) %>%
      formatStyle("Mortality", valueColumns = ".mort",
                  backgroundColor = styleEqual(TRUE, highlight)) %>%
      formatStyle("Veg pre", valueColumns = ".vpre",
                  backgroundColor = styleEqual(TRUE, highlight)) %>%
      formatStyle("Veg post", valueColumns = ".vpost",
                  backgroundColor = styleEqual(TRUE, highlight)) %>%
      formatStyle("Suitable", valueColumns = ".suit",
                  backgroundColor = styleEqual(TRUE, highlight))
  })

  output$dl_conflicts <- downloadHandler(
    filename = function() paste0("jt_conflicts_", Sys.Date(), ".csv"),
    content = function(file) {
      st <- conflict_rows()
      reads() %>%
        filter(id %in% st$id, completed) %>%
        left_join(select(st, id, disagreements), by = "id") %>%
        select(id, fire_name, plot_location, disagreements, observer,
               jt_pre_fire, pre_confidence, jt_post_fire, post_confidence,
               mortality, pre_veg, post_veg, unsuitable, comments,
               location_x, location_y) %>%
        arrange(fire_name, id, observer) %>%
        write.csv(file, row.names = FALSE, na = "")
    }
  )

  # Agreement tables
  output$pairwise_table <- renderDT({
    pw <- pairwise_agreement(reads(), input$abs_tol, input$rel_tol, input$mort_tol)
    validate(need(!is.null(pw), "Upload files from at least two observers."))
    datatable(fillContainer = FALSE, pw, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$fire_table <- renderDT({
    status() %>%
      filter(status %in% c("Conflict", "Agreed")) %>%
      group_by(Fire = fire_name, Location = plot_location) %>%
      summarise(
        `Plots with 2+ reads` = n(),
        `% agreed` = round(mean(status == "Agreed") * 100, 1),
        Conflicts = sum(status == "Conflict"),
        `Median pre spread` = median(pre_spread, na.rm = TRUE),
        `Median post spread` = median(post_spread, na.rm = TRUE),
        `Median mortality spread (pp)` = round(median(mort_spread, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      arrange(`% agreed`) %>%
      datatable(fillContainer = FALSE, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })

  # Plots
  output$pair_plot <- renderPlot({
    req(input$pair_a, input$pair_b)
    validate(need(input$pair_a != input$pair_b, "Pick two different observers."))
    r <- reads() %>% filter(completed, !unsuitable)
    j <- inner_join(filter(r, observer == input$pair_a),
                    filter(r, observer == input$pair_b),
                    by = "id", suffix = c("_a", "_b")) %>%
      left_join(select(status(), id, status), by = "id")
    validate(need(nrow(j) > 0, "These two observers have no suitable plots in common."))
    long <- bind_rows(
      transmute(j, id, status, measure = "Pre-fire count", a = jt_pre_fire_a, b = jt_pre_fire_b),
      transmute(j, id, status, measure = "Post-fire count", a = jt_post_fire_a, b = jt_post_fire_b),
      transmute(j, id, status, measure = "Mortality (%)", a = mortality_a, b = mortality_b)
    ) %>%
      filter(!is.na(a), !is.na(b)) %>%
      mutate(measure = factor(measure, levels = c("Pre-fire count",
                                                  "Post-fire count",
                                                  "Mortality (%)")))
    ggplot(long, aes(a, b, color = status)) +
      geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey55") +
      geom_point(size = 2.4, alpha = 0.75) +
      facet_wrap(~measure, scales = "free") +
      scale_color_manual(values = status_colors, drop = TRUE) +
      labs(x = input$pair_a, y = input$pair_b, color = "Plot status") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top", panel.grid.minor = element_blank(),
            strip.text = element_text(face = "bold"))
  })

  output$mortality_plot <- renderPlot({
    d <- reads() %>% filter(completed, !unsuitable, plot_location == "Inside",
                            !is.na(mortality))
    validate(need(nrow(d) > 0, "No inside plots with mortality yet."))
    ggplot(d, aes(str_to_title(fire_name), mortality, fill = observer,
                  color = observer)) +
      geom_boxplot(position = position_dodge(width = 0.8), width = 0.7,
                   outlier.shape = NA, alpha = 0.25) +
      geom_point(position = position_jitterdodge(jitter.width = 0.15,
                                                 dodge.width = 0.8),
                 size = 1.8, alpha = 0.8) +
      coord_cartesian(ylim = c(0, 100)) +
      labs(x = NULL, y = "Mortality (%)", fill = "Observer", color = "Observer") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top", panel.grid.major.x = element_blank(),
            axis.text.x = element_text(angle = 35, hjust = 1))
  })

  output$checks_table <- renderDT({
    data_checks(reads()) %>%
      rename(Check = check, Observer = observer, Plot = id, Fire = fire_name,
             Location = plot_location, Pre = jt_pre_fire, Post = jt_post_fire,
             `Veg pre` = pre_veg, `Veg post` = post_veg, Comments = comments) %>%
      mutate(Check = factor(Check), Observer = factor(Observer)) %>%
      datatable(fillContainer = FALSE, rownames = FALSE, filter = "top",
                options = list(pageLength = 20, scrollX = TRUE))
  })

  # Downloads
  output$dl_reads <- downloadHandler(
    filename = function() paste0("jt_reads_clean_", Sys.Date(), ".csv"),
    content = function(file) {
      reads() %>%
        left_join(select(status(), id, status, disagreements), by = "id") %>%
        select(-row_in_file) %>%
        write.csv(file, row.names = FALSE, na = "")
    }
  )

  output$dl_status <- downloadHandler(
    filename = function() paste0("jt_plot_status_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(status(), file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui, server)
