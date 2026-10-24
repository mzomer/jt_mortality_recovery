# -----------------------------------------------------------------------
# Joshua tree counts: observer agreement app (final)
# Three observers each complete the Collect Earth survey for every plot.
# Once all three have saved a plot, it is judged by majority:
#   - suitability: the majority decides (2 of 3 unsuitable -> Excluded)
#   - counts: at least two counts must agree within tolerance. All three
#     agreeing -> final = median; otherwise the closest agreeing pair decides
#     (final = their rounded mean) and the third count is noted as an outlier
#   - vegetation cover (optional, off by default): at least two must give the
#     same class
# Anything without a majority goes to Team review; Team review plots with a
# saved entry in a "resolved..." export become Resolved.
#
# Tabs: Overview (status bar, per-observer progress, plot table)
#       | Counts by fire (every observer's counts per plot, interactive)
#
# Packages: shiny, bslib, DT, dplyr, stringr, purrr, here, ggplot2, ggiraph,
#           scico, googledrive (only if reading from Google Drive)
# Run:      open app_final.R and run it, or
#           shiny::runApp("shiny/jt_agreement_app/app_final.R")
#           -- this file is independent of app.R and app_v1.R.
# -----------------------------------------------------------------------

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(stringr)
library(purrr)
library(here)
library(ggplot2)
library(ggiraph)

options(shiny.maxRequestSize = 30 * 1024^2)

`%||%` <- function(x, y) if (is.null(x)) y else x

# Where the exports live ----
# Google Drive folder ID (takes priority). Set to "" to read DATA_FOLDER
# instead, e.g. DATA_FOLDER <- here("data", "from_drive").
DRIVE_FOLDER <- "1xC1P6iVUxQd2cq3tSNHYuaojeWyqxvGL"
options(gargle_oauth_email = TRUE)  # reuse the saved Google sign-in, don't ask

DATA_FOLDER <- ""

# Observers ----
# Every plot is counted by this many observers
N_OBSERVERS <- 3

# Collect Earth plot number ----
# CE # is the plot's row in output/collect_earth/placemark_<ACTIVE_SURVEY>.csv.
# It is only shown when that file is identical to the plot list inside
# output/collect_earth/<ACTIVE_SURVEY>.cep, the bundle observers work from.
ACTIVE_SURVEY <- "joshuatree_survey_recent"
PLOT_ORDER_PATH <- here("output", "collect_earth", paste0("placemark_", ACTIVE_SURVEY, ".csv"))
SURVEY_CEP_PATH <- here("output", "collect_earth", paste0(ACTIVE_SURVEY, ".cep"))

# Low confidence ----
# A plot is flagged when at least this many observers marked low confidence
# for the same count (pre- or post-fire).
LOW_CONF_MIN <- 2

drive_poll_ms <- 60000   # how often to re-check Google Drive
folder_poll_ms <- 15000  # how often to re-check a local folder

export_pattern <- "collectedData.*\\.csv$"
# Exports whose file name starts with "resolved" hold the team's resolutions
# of Team review plots, not another observer's counts
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
# Its own cache dir, so this app never contends with app.R / app_v1.R.
drive_cache <- file.path(tempdir(), "jt_agreement_final_drive_cache")

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
  "Not started" = "Opened in Collect Earth, but no observer has saved it yet",
  "In progress" = "Saved by some, but not yet all, observers",
  "Incomplete"  = "All observers have saved, but a required field is blank in at least one entry",
  "Excluded"    = "The majority of observers marked the plot unsuitable",
  "Accepted"    = "At least two observers agree on the counts (and vegetation cover, if compared)",
  "Team review" = "No majority on a count (or vegetation cover, if compared)",
  "Resolved"    = "Was in Team review; settled in a resolved export"
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
  f_missing  = "Missing count",
  f_vmissing = "Missing vegetation cover",
  f_pre      = "No agreement on pre-fire count",
  f_post     = "No agreement on post-fire count",
  f_vpre     = "No agreement on pre vegetation cover",
  f_vpost    = "No agreement on post vegetation cover",
  f_extra    = paste("More than", N_OBSERVERS, "observers")
)

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

# CE numbers: the plot's row in the survey's plot list, but only if that
# list is identical to the one bundled in the survey observers work from.
# Returns the order (possibly empty) and, if it can't be trusted, why.
read_plot_order <- function(path, cep_path) {
  none <- tibble(id = character(0), plot_number = integer(0))
  if (!file.exists(path)) {
    return(list(order = none, problem = paste("Plot list not found:", basename(path))))
  }
  if (!file.exists(cep_path)) {
    return(list(order = none, problem = paste(
      "Can't check the plot list: survey bundle", basename(cep_path), "not found")))
  }
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  bundled <- tryCatch({
    utils::unzip(cep_path, files = "grid/placemark.csv", exdir = tmp)
    unname(tools::md5sum(file.path(tmp, "grid", "placemark.csv")))
  }, error = function(e) NA_character_)
  if (!identical(bundled, unname(tools::md5sum(path)))) {
    return(list(order = none, problem = paste(
      basename(path), "does not match the plot list inside", basename(cep_path),
      "-- re-run the 12* export script for this survey")))
  }
  list(order = read.csv(path, stringsAsFactors = FALSE) %>%
         transmute(id = as.character(id), plot_number = row_number()),
       problem = NULL)
}

# 1, 2, 3, 5, 7, 8 -> "1–3, 5, 7–8"
compress_ranges <- function(x, empty = "none") {
  x <- sort(unique(x[!is.na(x)]))
  if (length(x) == 0) return(empty)
  runs <- split(x, cumsum(c(1, diff(x) != 1)))
  paste(vapply(runs, function(r) {
    if (length(r) == 1) as.character(r) else paste0(r[1], "–", r[length(r)])
  }, character(1)), collapse = ", ")
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
      )
    )
}

# Agreement rule ----

# Median, rounded half up (with 2 values: the mean, rounded half up rather
# than R's round-to-even -- standard for count data).
median_count <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else floor(median(x) + 0.5)
}

# Two counts agree if their difference is no more than the allowed
# difference for their mean: abs_tol trees and/or rel_tol% of the mean.
# rule = "either" allows the larger of the two, "both" the smaller. A
# tolerance set to NA is not used; with neither, counts must match exactly.
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

# Plain-language version of the count rule, for the sidebar
rule_description <- function(abs_tol, rel_tol, rule) {
  abs_txt <- paste0(format(abs_tol), " tree(s)")
  rel_txt <- paste0(format(rel_tol), "% of their average")
  if (is.na(abs_tol) && is.na(rel_tol)) return("0 trees: counts must match exactly")
  if (is.na(rel_tol)) return(abs_txt)
  if (is.na(abs_tol)) return(rel_txt)
  if (rule == "both") paste(abs_txt, "and", rel_txt, "(both must hold)")
  else paste(abs_txt, "or", rel_txt, "(whichever is larger)")
}

# Majority on one count. Every pair within tolerance -> all agree, final =
# median. Otherwise the closest agreeing pair decides (final = its rounded
# mean) and the remaining count is an outlier; if two pairs are equally
# close (e.g. 10, 12, 14) the median is used and no count is singled out.
# No agreeing pair -> no majority.
count_majority <- function(x, obs, abs_tol, rel_tol, rule) {
  out <- list(ok = FALSE, value = NA_real_, outliers = character(0))
  keep <- !is.na(x)
  x <- x[keep]
  obs <- obs[keep]
  if (length(x) < 2) return(out)
  pairs <- combn(length(x), 2, simplify = FALSE)
  agree <- vapply(pairs, function(p) counts_agree(x[p[1]], x[p[2]], abs_tol, rel_tol, rule),
                  logical(1))
  if (!any(agree)) return(out)
  out$ok <- TRUE
  if (all(agree)) {
    out$value <- median_count(x)
    return(out)
  }
  gaps <- vapply(pairs[agree], function(p) abs(x[p[1]] - x[p[2]]), numeric(1))
  best <- pairs[agree][gaps == min(gaps)]
  if (length(best) > 1) {
    out$value <- median_count(x)
    return(out)
  }
  out$value <- median_count(x[best[[1]]])
  out$outliers <- obs[-best[[1]]]
  out
}

# Majority on a vegetation cover class: at least two entries with the same
# class (with only two entries, both must match). Blank entries don't count.
class_majority <- function(x, obs) {
  out <- list(ok = FALSE, value = NA_character_, outliers = character(0))
  tab <- table(x[!is.na(x)])
  if (length(tab) == 0 || max(tab) < 2) return(out)
  top <- names(tab)[tab == max(tab)]
  if (length(top) > 1) return(out)
  out$ok <- TRUE
  out$value <- top
  out$outliers <- obs[!is.na(x) & x != top]
  out
}

# Plot status ----
# One plot's completed entries (one per observer) -> its status, why, and
# the agreed value of every field (even if the plot as a whole is in Team
# review, so a resolution only has to fill in the fields without a majority).
# check_veg = FALSE: vegetation cover is neither required nor compared.
judge_plot <- function(d, abs_tol, rel_tol, rule, check_veg) {
  n_obs <- nrow(d)
  result <- function(rule_status, issues = "", ...) {
    row <- tibble(
      n_obs = n_obs, observers = paste(sort(d$observer), collapse = ", "),
      rule_status = rule_status, issues = issues, notes = "",
      f_missing = FALSE, f_vmissing = FALSE,
      f_pre = FALSE, f_post = FALSE, f_vpre = FALSE, f_vpost = FALSE,
      field_pre = NA_real_, field_post = NA_real_,
      field_vpre = NA_character_, field_vpost = NA_character_,
      out_pre = "", out_post = "", out_vpre = "", out_vpost = "", out_suit = ""
    )
    changes <- list(...)
    for (k in names(changes)) row[[k]] <- changes[[k]]
    row
  }
  collapse <- function(x, sep = ", ") paste(x, collapse = sep)

  if (n_obs < N_OBSERVERS) return(result("In progress"))
  if (n_obs > N_OBSERVERS) {
    return(result("Team review", issue_labels[["f_extra"]],
                  f_pre = TRUE, f_post = TRUE, f_vpre = check_veg, f_vpost = check_veg))
  }

  complete <- d$unsuitable |
    (!is.na(d$jt_pre_fire) & !is.na(d$jt_post_fire) &
       (!check_veg | (!is.na(d$pre_veg) & !is.na(d$post_veg))))
  if (!all(complete)) {
    missing_count <- any(!d$unsuitable & (is.na(d$jt_pre_fire) | is.na(d$jt_post_fire)))
    missing_veg <- check_veg && any(!d$unsuitable & (is.na(d$pre_veg) | is.na(d$post_veg)))
    return(result("Incomplete",
                  collapse(c(if (missing_count) issue_labels[["f_missing"]],
                             if (missing_veg) issue_labels[["f_vmissing"]]), "; "),
                  f_missing = missing_count, f_vmissing = missing_veg))
  }

  # Suitability: the majority decides
  if (sum(d$unsuitable) * 2 > n_obs) {
    counted_by <- d$observer[!d$unsuitable]
    return(result("Excluded",
                  notes = if (length(counted_by)) paste("Counted anyway by", collapse(counted_by)) else "",
                  out_suit = collapse(counted_by, ";")))
  }
  unsuit_by <- d$observer[d$unsuitable]
  s <- d[!d$unsuitable, ]

  pre <- count_majority(s$jt_pre_fire, s$observer, abs_tol, rel_tol, rule)
  post <- count_majority(s$jt_post_fire, s$observer, abs_tol, rel_tol, rule)
  vpre <- class_majority(s$pre_veg, s$observer)
  vpost <- class_majority(s$post_veg, s$observer)
  vpre_ok <- vpre$ok || !check_veg
  vpost_ok <- vpost$ok || !check_veg
  all_ok <- pre$ok && post$ok && vpre_ok && vpost_ok

  issues <- collapse(c(if (!pre$ok) issue_labels[["f_pre"]],
                       if (!post$ok) issue_labels[["f_post"]],
                       if (!vpre_ok) issue_labels[["f_vpre"]],
                       if (!vpost_ok) issue_labels[["f_vpost"]]), "; ")
  notes <- collapse(c(
    if (length(unsuit_by)) paste("Marked unsuitable by", collapse(unsuit_by)),
    if (length(pre$outliers)) paste("Pre-fire outlier:", collapse(pre$outliers)),
    if (length(post$outliers)) paste("Post-fire outlier:", collapse(post$outliers)),
    if (check_veg && length(vpre$outliers)) paste("Pre veg outlier:", collapse(vpre$outliers)),
    if (check_veg && length(vpost$outliers)) paste("Post veg outlier:", collapse(vpost$outliers))
  ), "; ")

  result(if (all_ok) "Accepted" else "Team review", issues, notes = notes,
         f_pre = !pre$ok, f_post = !post$ok, f_vpre = !vpre_ok, f_vpost = !vpost_ok,
         field_pre = pre$value, field_post = post$value,
         field_vpre = vpre$value, field_vpost = vpost$value,
         out_pre = collapse(pre$outliers, ";"), out_post = collapse(post$outliers, ";"),
         out_vpre = if (check_veg) collapse(vpre$outliers, ";") else "",
         out_vpost = if (check_veg) collapse(vpost$outliers, ";") else "",
         out_suit = collapse(unsuit_by, ";"))
}

plot_status <- function(reads, abs_tol, rel_tol, rule, check_veg) {
  all_plots <- reads %>% distinct(id, fire_name, fire_year, plot_location)
  judged <- reads %>%
    filter(completed) %>%
    arrange(id, observer) %>%
    group_by(id) %>%
    group_modify(~ judge_plot(.x, abs_tol, rel_tol, rule, check_veg)) %>%
    ungroup()

  all_plots %>%
    left_join(judged, by = "id") %>%
    mutate(
      n_obs = coalesce(n_obs, 0L),
      rule_status = coalesce(rule_status, "Not started"),
      across(c(observers, issues, notes, out_pre, out_post, out_vpre, out_vpost, out_suit),
             ~ coalesce(.x, "")),
      across(c(f_missing, f_vmissing, f_pre, f_post, f_vpre, f_vpost), ~ coalesce(.x, FALSE)),
      accepted = rule_status == "Accepted",
      cons_pre = if_else(accepted, field_pre, NA_real_),
      cons_post = if_else(accepted, field_post, NA_real_),
      cons_pre_veg = if_else(accepted, field_vpre, NA_character_),
      cons_post_veg = if_else(accepted, field_vpost, NA_character_)
    ) %>%
    select(-accepted)
}

# Resolutions ----
# A Team review plot with a saved entry in a resolved export becomes
# Resolved. The resolver only fills in the fields without a majority; the
# rest keep the observers' majority value (a value the resolver did give
# still wins). A field without a majority that the resolver left blank stays
# empty and is listed in res_missing. Marking the plot unsuitable settles it
# with no values.
res_field_labels <- c(pre = "pre-fire count", post = "post-fire count",
                      vpre = "pre vegetation cover", vpost = "post vegetation cover")

apply_resolutions <- function(st, res) {
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
  missing_text <- function(m) {
    vapply(seq_len(nrow(m)), function(i) paste(res_field_labels[m[i, ]], collapse = ", "),
           character(1))
  }

  st %>%
    left_join(r, by = "id") %>%
    mutate(
      resolved = rule_status == "Team review" & !is.na(res_by),
      res_unsuit = resolved & coalesce(res_unsuit, FALSE),
      keep = resolved & !res_unsuit,
      res_need_pre = keep & f_pre,
      res_need_post = keep & f_post,
      res_need_vpre = keep & f_vpre,
      res_need_vpost = keep & f_vpost,
      res_missing = missing_text(cbind(
        res_need_pre & is.na(res_pre), res_need_post & is.na(res_post),
        res_need_vpre & is.na(res_vpre), res_need_vpost & is.na(res_vpost))),
      cons_pre = if_else(keep, if_else(res_need_pre, res_pre, coalesce(res_pre, field_pre)),
                         cons_pre),
      cons_post = if_else(keep, if_else(res_need_post, res_post, coalesce(res_post, field_post)),
                          cons_post),
      cons_pre_veg = if_else(keep, if_else(res_need_vpre, res_vpre,
                                           coalesce(res_vpre, field_vpre)), cons_pre_veg),
      cons_post_veg = if_else(keep, if_else(res_need_vpost, res_vpost,
                                            coalesce(res_vpost, field_vpost)), cons_post_veg),
      rule_status = if_else(resolved, "Resolved", rule_status),
      # A resolution only applies to a Team review plot; elsewhere drop it
      across(c(res_by, res_pre, res_post, res_vpre, res_vpost, res_comments),
             ~ replace(.x, !resolved, NA))
    ) %>%
    select(-keep, -resolved)
}

# Final status factor and mortality, once resolutions are applied
finish_status <- function(st) {
  st %>%
    mutate(
      status = factor(rule_status, levels = status_levels),
      cons_mortality = if_else(plot_location == "Inside" & !is.na(cons_pre) & cons_pre > 0 &
                                 !is.na(cons_post),
                               (cons_pre - cons_post) / cons_pre * 100, NA_real_),
      fully_counted = n_obs >= N_OBSERVERS
    )
}

# Expandable details: every observer's entry, plus the resolution ----
fmt_count <- function(x) ifelse(is.na(x), "–", format(x, trim = TRUE))

with_conf <- function(value, conf) {
  ifelse(is.na(conf), value,
         paste0(value, " <span class='conf'>conf. ",
                htmltools::htmlEscape(conf), "</span>"))
}

plot_summary_line <- function(s) {
  fmt <- function(x) if (is.na(x)) "–" else format(x)
  values <- sprintf("pre-fire %s, post-fire %s, vegetation %s → %s%s",
                    fmt(s$cons_pre), fmt(s$cons_post), fmt(s$cons_pre_veg),
                    fmt(s$cons_post_veg),
                    if (is.na(s$cons_mortality)) "" else
                      sprintf(", mortality %.0f%%", s$cons_mortality))
  with_notes <- function(x) if (nzchar(s$notes)) paste0(x, ". ", s$notes) else x
  line <- switch(
    as.character(s$status),
    "Accepted" = with_notes(paste("Accepted:", values)),
    "Incomplete" = paste("Incomplete:", s$issues),
    "Team review" = with_notes(paste("Team review:", s$issues)),
    "Excluded" = with_notes("Excluded: the majority marked the plot unsuitable"),
    "Resolved" = if (s$res_unsuit) {
      sprintf("Resolved by %s: plot marked unsuitable (was in Team review: %s)",
              s$res_by, s$issues)
    } else {
      sprintf("Resolved by %s: %s%s (was in Team review: %s)", s$res_by, values,
              if (nzchar(s$res_missing)) paste0(". Still missing: ", s$res_missing) else "",
              s$issues)
    },
    "In progress" = sprintf("In progress: %d of %d observers", s$n_obs, N_OBSERVERS),
    as.character(s$status)
  )
  paste0("<p class='detail-summary'>", htmltools::htmlEscape(line), "</p>")
}

plot_details_all <- function(reads, st) {
  esc <- htmltools::htmlEscape
  in_list <- function(obs, lists) {
    vapply(seq_along(obs), function(i) obs[i] %in% strsplit(lists[i], ";", fixed = TRUE)[[1]],
           logical(1))
  }
  d <- reads %>%
    filter(completed) %>%
    inner_join(select(st, id, rule_status, f_missing, f_vmissing, f_pre, f_post,
                      f_vpre, f_vpost, out_pre, out_post, out_vpre, out_vpost, out_suit),
               by = "id") %>%
    arrange(id, observer)
  # Cell shading: red = no majority on this field (or missing), blue = the
  # outlier against the majority, grey = observer marked the plot unsuitable
  cls <- function(disputed, outlier) {
    ifelse(d$unsuitable, "muted", ifelse(disputed, "shade", ifelse(outlier, "outlier", NA)))
  }
  td <- function(value, class) {
    paste0("<td", ifelse(is.na(class), "", paste0(" class='", class, "'")), ">", value, "</td>")
  }
  review <- d$rule_status != "Incomplete"
  d$row_html <- paste0(
    "<tr><td>", esc(d$observer), "</td>",
    td(with_conf(fmt_count(d$jt_pre_fire), d$pre_confidence),
       cls(d$f_pre & review | d$f_missing & is.na(d$jt_pre_fire), in_list(d$observer, d$out_pre))),
    td(with_conf(fmt_count(d$jt_post_fire), d$post_confidence),
       cls(d$f_post & review | d$f_missing & is.na(d$jt_post_fire), in_list(d$observer, d$out_post))),
    td(ifelse(is.na(d$mortality), "–", sprintf("%.0f%%", d$mortality)), cls(FALSE, FALSE)),
    td(coalesce(d$pre_veg, "–"),
       cls(d$f_vpre & review | d$f_vmissing & is.na(d$pre_veg), in_list(d$observer, d$out_vpre))),
    td(coalesce(d$post_veg, "–"),
       cls(d$f_vpost & review | d$f_vmissing & is.na(d$post_veg), in_list(d$observer, d$out_vpost))),
    td(ifelse(d$unsuitable, "no", "yes"), ifelse(in_list(d$observer, d$out_suit), "outlier", NA)),
    "<td>", if_else(is.na(d$comments), "",
                    paste0("<div class='cmt'>", esc(d$comments), "</div>")), "</td></tr>")
  rows_by_plot <- tapply(d$row_html, d$id, paste, collapse = "")

  # The resolution as an extra row under the observers' entries
  res_st <- st[st$status %in% "Resolved", ]
  for (i in seq_len(nrow(res_st))) {
    s <- res_st[i, ]
    res_td <- function(value, need) {
      if (need && is.na(value)) return("<td class='res-missing'>missing</td>")
      paste0("<td>", if (is.na(value)) "–" else esc(format(value)), "</td>")
    }
    rows_by_plot[s$id] <- paste0(
      coalesce(rows_by_plot[s$id], ""),
      "<tr class='res-row'><td><b>Resolved</b> (", esc(s$res_by), ")</td>",
      res_td(s$res_pre, s$res_need_pre), res_td(s$res_post, s$res_need_post),
      "<td>", if (is.na(s$cons_mortality)) "–" else sprintf("%.0f%%", s$cons_mortality), "</td>",
      res_td(s$res_vpre, s$res_need_vpre), res_td(s$res_vpost, s$res_need_vpost),
      "<td>", if (s$res_unsuit) "no" else "–", "</td>",
      "<td>", if (is.na(s$res_comments)) "" else
        paste0("<div class='cmt'>", esc(s$res_comments), "</div>"),
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
           if (is.na(rows)) "<p class='text-muted mb-0'>No observer has saved this plot yet.</p>"
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

# Counts by fire figure ----
# One row of panels per fire (in CE # order, so fires being counted first
# are on top), pre- and post-fire side by side; x = CE #. Every plot x
# observer gets a point: filled = that observer's count, unfilled at 0 = not
# counted yet (or marked unsuitable). Hover for plot, observer and value.
# markers: one row per plot x period where the counts are within the tree
# tolerance ("+") or the % tolerance ("x"), drawn at the top of the panel;
# both together overlap into a star.
counts_figure <- function(d, markers = NULL) {
  fires <- d %>% group_by(fire_name) %>% summarise(first = min(x_order), .groups = "drop") %>%
    arrange(first)
  d <- d %>%
    mutate(fire_name = factor(fire_name, levels = fires$fire_name),
           x = factor(x_label, levels = unique(x_label[order(x_order)])))
  observers <- sort(unique(d$observer))
  shapes <- setNames(rep(c(21, 22, 23, 24, 25), length.out = length(observers)), observers)
  counted <- filter(d, counted)
  if (is.null(markers) || nrow(markers) == 0) {
    markers <- tibble(x = factor(character(0), levels = levels(d$x)), y = numeric(0),
                      fire_name = d$fire_name[0], period = d$period[0], marker = numeric(0))
  } else {
    markers <- markers %>%
      mutate(fire_name = factor(fire_name, levels = levels(d$fire_name)),
             x = factor(x_label, levels = levels(d$x)))
  }
  ggplot(d, aes(x = x, y = value)) +
    facet_wrap(vars(fire_name, period), ncol = 2, scales = "free",
               labeller = labeller(.multi_line = FALSE)) +
    geom_line(data = counted, aes(group = id), color = "#8f8d86", linewidth = 0.4) +
    geom_point_interactive(data = filter(d, !counted),
                           aes(shape = observer, tooltip = tooltip, data_id = id),
                           fill = NA, color = "#8f8d86", size = 2.2) +
    geom_point_interactive(data = counted,
                           aes(shape = observer, fill = observer, tooltip = tooltip, data_id = id),
                           color = "black", size = 2.4, alpha = 0.8) +
    geom_point(data = filter(markers, marker == 3), aes(x = x, y = y),
               shape = 3, size = 2, stroke = 0.9, color = "black", inherit.aes = FALSE) +
    geom_point(data = filter(markers, marker == 4), aes(x = x, y = y),
               shape = 4, size = 2, stroke = 0.9, color = "black", inherit.aes = FALSE) +
    scale_shape_manual(values = shapes) +
    scico::scale_fill_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
    expand_limits(y = c(0, 1)) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(x = "CE #", y = "Joshua trees", shape = NULL, fill = NULL) +
    theme_bw(base_size = 11) +
    theme(legend.position = "top",
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "#F1F0EC", color = NA),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7))
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
.status-warn { color: #8a4b08; font-size: .92rem; margin-top: .3rem; }
.conf { color: #7b817c; font-size: .85em; }
table.dataTable td { vertical-align: top; }
table.dataTable th { word-break: normal; overflow-wrap: normal; }
#plot_table table.dataTable > tbody > tr { cursor: pointer; }
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
.detail-summary { margin: 0 0 .6rem; }
.detail-table { margin: 0; }
.detail-table td, .detail-table th { padding: .3rem .7rem .3rem 0; vertical-align: top; }
.detail-table td.shade { background-color: #F4DED6; }
.detail-table td.outlier { background-color: #DCE4EE; }
.detail-table td.muted { color: #AEA99C; }
.detail-table tr.res-row td { border-top: 2px solid #5E5A8C; }
.detail-table td.res-missing { background-color: #F3DFA2; }
input.plot-tick { width: 1.05rem; height: 1.05rem; cursor: pointer; }
"

# Plot table ticks: report each checkbox change to the server
tick_js <- "
$(document).on('change', 'input.plot-tick', function() {
  Shiny.setInputValue('plot_tick', {id: $(this).data('id'), checked: this.checked},
                      {priority: 'event'});
});
"

ui <- page_navbar(
  title = sprintf("Joshua tree counts · observer agreement (%d observers)", N_OBSERVERS),
  theme = theme,
  fillable = FALSE,
  header = tags$head(tags$style(HTML(app_css)), tags$script(HTML(tick_js))),
  sidebar = sidebar(
    width = 320,
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
      radioButtons("tol_rule", "Two counts agree if within",
                   choices = c("either tolerance (looser)" = "either",
                               "both tolerances (stricter)" = "both"),
                   selected = "either")),
    uiOutput("tolerance_example"),
    checkboxInput("check_veg", "Compare vegetation cover", value = FALSE),
    helpText(sprintf(paste(
      "Once all %d observers have saved a plot: the majority decides",
      "suitability; at least two counts must agree within tolerance (final",
      "count = median if all agree, else the closest agreeing pair, the",
      "third noted as an outlier). With vegetation cover compared, at least",
      "two must give the same class; when off, it is not required and kept",
      "only where two agree."), N_OBSERVERS)),
    tags$hr(),
    selectInput("fire_filter", "Fires", choices = NULL, multiple = TRUE),
    helpText("Leave empty to include all fires.")
  ),
  nav_panel(
    "Overview",
    uiOutput("status_bar"),
    uiOutput("status_note"),
    card(
      card_header("Observer progress"),
      helpText("Plots each observer has saved, by CE #. Missing: plots up to",
               "their furthest plot that they haven't saved (skipped)."),
      DTOutput("progress_table", fill = FALSE)
    ),
    card(
      card_header(textOutput("plot_table_title", inline = TRUE)),
      helpText("Click a row to show each observer's entry. Shading: red = no",
               "majority, blue = outlier against the majority, grey = observer",
               "marked the plot unsuitable. Tick the box to add a plot to your",
               "selection."),
      div(class = "d-flex flex-wrap align-items-center gap-3 mb-2",
          checkboxInput("ticked_only", "Show ticked plots only", width = "auto"),
          textOutput("ticked_count", inline = TRUE),
          actionLink("clear_ticks", "Clear ticks"),
          downloadButton("dl_ticked", "Download ticked (CSV)", class = "btn-sm"),
          downloadButton("dl_all", "Download all plots (CSV)", class = "btn-sm")),
      DTOutput("plot_table", fill = FALSE)
    )
  ),
  nav_panel(
    "Counts by fire",
    card(
      card_header("Counts by fire"),
      helpText("Each observer's pre- and post-fire count per plot, by CE #.",
               "Fires in CE # order. Unfilled symbols at 0: not counted yet",
               "(or marked unsuitable). Markers at the top: + = counts within",
               "the tree tolerance, × = within the % tolerance, both = a star",
               "(as set in the sidebar; the difference between the highest and",
               "lowest count).",
               "Hover over a point for the plot, observer and value. Use the",
               "Fires filter in the sidebar to show fewer fires."),
      girafeOutput("counts_plot", width = "100%", height = NULL)
    )
  )
)

# Server ----

server <- function(input, output, session) {

  # Data source ----
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

  plot_order_info <- reactiveFileReader(60000, session, PLOT_ORDER_PATH,
                                        function(p) read_plot_order(p, SURVEY_CEP_PATH))
  plot_order <- reactive(plot_order_info()$order)

  output$folder_status <- renderUI({
    src <- folder_source()
    if (src == "none") {
      return(helpText("No shared folder set. To read a Google Drive folder",
                      "automatically, set DRIVE_FOLDER at the top of app_final.R."))
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

  observers_known <- reactive(sort(unique(file_info()$observer)))

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

  observeEvent(reads_all(), {
    fires <- sort(unique(reads_all()$fire_name))
    updateSelectInput(session, "fire_filter", choices = fires,
                      selected = intersect(input$fire_filter, fires))
  })

  by_fire <- function(r) {
    if (!is.null(r) && length(input$fire_filter) > 0) r <- filter(r, fire_name %in% input$fire_filter)
    r
  }
  reads <- reactive(by_fire(reads_all()))
  resolved <- reactive(by_fire(resolved_all()))

  # A tolerance that is ticked off is NA, i.e. not used
  settings <- reactive({
    list(abs_tol = if (isFALSE(input$use_abs)) NA_real_ else
           max(0, input$abs_tol %||% 2, na.rm = TRUE),
         rel_tol = if (isFALSE(input$use_rel)) NA_real_ else
           max(0, input$rel_tol %||% 20, na.rm = TRUE),
         rule = input$tol_rule %||% "either",
         check_veg = isTRUE(input$check_veg))
  })

  low_conf <- reactive(low_confidence_by_plot(reads()))

  status <- reactive({
    s <- settings()
    plot_status(reads(), s$abs_tol, s$rel_tol, s$rule, s$check_veg) %>%
      apply_resolutions(resolved()) %>%
      finish_status() %>%
      left_join(select(low_conf(), id, pre_low, post_low, low_conf_label), by = "id") %>%
      mutate(low_conf_label = coalesce(low_conf_label, ""))
  })

  details <- reactive(plot_details_all(reads(), status()))

  # Resolutions for plots that aren't in Team review (not applied)
  unused_resolutions <- reactive({
    r <- resolved()
    if (is.null(r)) return(character(0))
    setdiff(r$id, status()$id[status()$status %in% "Resolved"])
  })

  # Status bar ----
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
          "(or upload them in the sidebar). Once all observers have saved a",
          "plot, the app checks it for a majority.")
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
    n_review <- sum(full$rule_status == "Team review")
    txt <- if (nrow(full) == 0) {
      sprintf("No plots have been saved by all %d observers yet.", N_OBSERVERS)
    } else {
      sprintf("%d plots saved by all %d observers: %d (%.0f%%) need team review.",
              nrow(full), N_OBSERVERS, n_review, 100 * n_review / nrow(full))
    }
    unused <- unused_resolutions()
    problem <- plot_order_info()$problem
    tagList(
      div(class = "status-note", txt),
      if (length(unused) > 0) div(class = "status-warn", sprintf(
        "Resolution ignored for %d plot(s) not in Team review: %s",
        length(unused), paste(unused, collapse = ", "))),
      if (!is.null(problem)) div(class = "status-warn", "CE # not shown: ", problem)
    )
  })

  # Observer progress ----
  progress_data <- reactive({
    po <- plot_order()
    done <- reads_all() %>%
      filter(completed) %>%
      left_join(po, by = "id")
    have_numbers <- nrow(po) > 0
    progress <- done %>%
      group_by(observer) %>%
      summarise(
        n_done = n(),
        furthest = if (any(!is.na(plot_number))) max(plot_number, na.rm = TRUE) else NA_integer_,
        missing = if (is.na(furthest)) "–" else
          compress_ranges(setdiff(seq_len(furthest), plot_number)),
        not_listed = sum(is.na(plot_number)),
        .groups = "drop"
      )
    newest <- file_info() %>%
      filter(used) %>%
      group_by(observer) %>%
      summarise(newest = if (all(is.na(newest_save))) "–" else
                  format(max(newest_save, na.rm = TRUE), "%d %b"),
                .groups = "drop")
    d <- tibble(observer = observers_known()) %>%
      left_join(progress, by = "observer") %>%
      left_join(newest, by = "observer") %>%
      transmute(Observer = observer,
                `Plots saved` = coalesce(n_done, 0L),
                Finished = if (have_numbers) ifelse(is.na(furthest), "–", paste0("1–", furthest)) else "–",
                Missing = if (have_numbers) coalesce(missing, "–") else "–",
                `Last saved` = coalesce(newest, "–"))
    if (any(coalesce(progress$not_listed, 0L) > 0)) {
      d$`Not in plot list` <- coalesce(progress$not_listed[match(d$Observer, progress$observer)], 0L)
    }
    d
  })

  output$progress_table <- renderDT({
    datatable(progress_data(), fillContainer = FALSE, rownames = FALSE, selection = "none",
              options = list(dom = "t", ordering = FALSE, scrollX = TRUE,
                             pageLength = 50))
  })

  # Plot table ----
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

  # Who still has to save each plot
  waiting_for <- function(observers) {
    known <- observers_known()
    vapply(observers, function(o) {
      left <- setdiff(known, strsplit(o, ", ", fixed = TRUE)[[1]])
      if (length(left) == 0) "" else paste(left, collapse = ", ")
    }, character(1), USE.NAMES = FALSE)
  }

  plot_table_data <- reactive({
    st <- status()
    showing <- selected_status()
    if (!is.null(showing)) st <- filter(st, status == showing)
    # Only redraw on a tick when filtering by ticks; otherwise the checkbox
    # already shows the change, and a redraw would reset the page
    tk <- if (isTRUE(input$ticked_only)) ticked() else isolate(ticked())
    if (isTRUE(input$ticked_only)) st <- filter(st, id %in% tk)
    st %>%
      arrange(status, fire_name, id) %>%
      left_join(plot_comments(), by = "id") %>%
      left_join(plot_order(), by = "id") %>%
      transmute(` ` = sprintf("<input type='checkbox' class='plot-tick' data-id='%s'%s>",
                              htmltools::htmlEscape(id, attribute = TRUE),
                              if_else(id %in% tk, " checked", "")),
                `CE #` = plot_number, Plot = id, Fire = fire_name,
                Location = plot_location,
                Status = status,
                Saved = paste(n_obs, "of", N_OBSERVERS),
                `Waiting for` = waiting_for(observers),
                Issues = issues,
                Notes = notes,
                `Low confidence` = low_conf_label,
                Comments = coalesce(Comments, ""))
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

  # Downloads ----
  # One row per plot: coordinates and all observers' comments
  plot_coords <- reactive({
    reads() %>%
      group_by(id) %>%
      summarise(longitude = first(na.omit(location_x)),
                latitude = first(na.omit(location_y)),
                comments = paste(na.omit(comments), collapse = " | "),
                .groups = "drop")
  })

  plots_csv <- function(st, file) {
    st %>%
      left_join(plot_order(), by = "id") %>%
      left_join(plot_coords(), by = "id") %>%
      arrange(plot_number, id) %>%
      transmute(ce_number = plot_number, plot_id = id, fire = fire_name, fire_year,
                location = plot_location, status, n_saved = n_obs, observers,
                waiting_for = waiting_for(observers), issues, notes,
                pre_fire = cons_pre, post_fire = cons_post,
                pre_veg = cons_pre_veg, post_veg = cons_post_veg,
                mortality_pct = round(cons_mortality, 1),
                low_confidence = low_conf_label,
                resolved_by = res_by, resolution_missing = res_missing,
                longitude, latitude, comments) %>%
      write.csv(file, row.names = FALSE, na = "")
  }

  output$dl_ticked <- downloadHandler(
    filename = function() paste0("jt_ticked_plots_", Sys.Date(), ".csv"),
    content = function(file) plots_csv(filter(status(), id %in% ticked()), file)
  )

  output$dl_all <- downloadHandler(
    filename = function() paste0("jt_plots_final_", Sys.Date(), ".csv"),
    content = function(file) plots_csv(status(), file)
  )

  # Counts by fire ----
  # Every plot in the survey's plot list (or, without one, every plot in
  # the exports) x every observer x pre/post, with the count if saved
  counts_plot_data <- reactive({
    po <- plot_order()
    known <- observers_known()
    r <- reads() %>% filter(completed)
    plots <- if (nrow(po) > 0) {
      po %>% mutate(fire_name = str_match(id, "^(.+?)_(\\d{4})_")[, 2] %>%
                      str_replace_all("_", " ") %>% str_to_upper())
    } else {
      r %>% distinct(id, fire_name) %>% arrange(id) %>% mutate(plot_number = NA_integer_)
    }
    if (length(input$fire_filter) > 0) plots <- filter(plots, fire_name %in% input$fire_filter)
    validate(need(nrow(plots) > 0 && length(known) > 0, "No plots to show yet."))
    grid <- tidyr::expand_grid(plots, observer = known, period = c("Pre-fire", "Post-fire"))
    entries <- bind_rows(
      r %>% transmute(id, observer, period = "Pre-fire", count = jt_pre_fire, unsuitable),
      r %>% transmute(id, observer, period = "Post-fire", count = jt_post_fire, unsuitable))
    grid %>%
      left_join(entries, by = c("id", "observer", "period")) %>%
      mutate(
        counted = !is.na(count),
        value = coalesce(count, 0),
        x_order = coalesce(plot_number, match(id, unique(id))),
        x_label = if_else(is.na(plot_number), id, as.character(plot_number)),
        period = factor(period, levels = c("Pre-fire", "Post-fire")),
        tooltip = paste0(
          htmltools::htmlEscape(id),
          if_else(is.na(plot_number), "", paste0(" (CE #", plot_number, ")")),
          "<br>", htmltools::htmlEscape(observer), ", ", tolower(period), ": ",
          case_when(counted ~ format(count, trim = TRUE),
                    unsuitable %in% TRUE ~ "marked unsuitable",
                    TRUE ~ "not counted yet"))
      )
  })

  # As in scripts/16_plot_counts.R: per plot x period, the spread (highest -
  # lowest) of the observers' counts, if at least two counted; "+" when within
  # the tree tolerance, "x" when within the % tolerance of the mean of
  # highest and lowest. Drawn at the top of each panel.
  counts_markers <- reactive({
    d <- counts_plot_data()
    s <- settings()
    spread <- d %>%
      filter(counted) %>%
      group_by(fire_name, period, id, x_label) %>%
      summarise(n = n(), hi = max(value), lo = min(value), .groups = "drop") %>%
      filter(n >= 2) %>%
      mutate(delta = hi - lo, avg = (hi + lo) / 2)
    tops <- d %>%
      group_by(fire_name, period) %>%
      summarise(top = max(value, 1), .groups = "drop")
    within <- function(ok, marker) {
      spread %>%
        filter(ok) %>%
        left_join(tops, by = c("fire_name", "period")) %>%
        transmute(fire_name, period, id, x_label, marker, y = top * 1.15)
    }
    bind_rows(
      if (!is.na(s$abs_tol)) within(spread$delta <= s$abs_tol, 3),
      if (!is.na(s$rel_tol)) within(spread$delta <= s$rel_tol / 100 * spread$avg, 4)
    )
  })

  output$counts_plot <- renderGirafe({
    d <- counts_plot_data()
    n_fires <- n_distinct(d$fire_name)
    girafe(ggobj = counts_figure(d, counts_markers()), width_svg = 11, height_svg = 1.2 + 1.9 * n_fires,
           options = list(opts_hover(css = "stroke-width:2px;"),
                          opts_hover_inv(css = "opacity:0.35;"),
                          opts_tooltip(css = paste("background:#1F2622;color:#fff;",
                                                   "padding:4px 8px;border-radius:4px;",
                                                   "font-size:12px;")),
                          opts_toolbar(saveaspng = TRUE)))
  })

  # Sidebar ----
  output$tolerance_example <- renderUI({
    s <- settings()
    tol <- function(m) format(round(allowed_difference(m, s$abs_tol, s$rel_tol, s$rule), 1))
    helpText(tags$em(sprintf(
      "Two counts agree within %s: a difference of up to %s tree(s) for plots averaging 5 trees, %s for 15, %s for 30.",
      rule_description(s$abs_tol, s$rel_tol, s$rule), tol(5), tol(15), tol(30))))
  })
}

shinyApp(ui, server)
