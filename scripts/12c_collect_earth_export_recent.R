# ─────────────────────────────────────────────────────────────────────
# 12c_collect_earth_export_recent.R — Build a Collect Earth bundle (.cep)
#                                      for all sites of recent fires only
#                                      (2011–present), titled jt_fire_plots.
# ─────────────────────────────────────────────────────────────────────
# Same patching logic as 12_collect_earth_export.R, but:
#   - only recent-fire sites (no historical sites)
#   - repacked under a new survey name/title: jt_fire_plots
# Reuses the jt_fire_recovery Designer export as the base form (same
# balloon card, plot boundary, IDM) — only the site list and the
# survey title differ.
#
# Input:  output/recent/recent_sites_coordinates.csv
#         ~/Downloads/jt_fire_recovery_*.cep  (latest Designer export)
# Output: output/collect_earth/joshuatree_survey_recent.cep
#         output/collect_earth/placemark_joshuatree_survey_recent.csv
# ─────────────────────────────────────────────────────────────────────

source(here::here("scripts", "00_utils.R"))

DESIGNER_SURVEY_NAME <- "jt_fire_recovery"
SURVEY_TITLE         <- "jt_fire_plots"           # shown as the project/survey title in Collect Earth / Google Earth
FILE_NAME            <- "joshuatree_survey_recent" # the .cep file name
MIN_FIRE_YEAR        <- 2011

SUPPLEMENTARY_COLS <- c(
  "fire_year", "site_other_burn_years",
  "imagery_pre_fire_date", "imagery_post_fire_date"
)

CORE_COLS <- c(
  "site_id", "pair_id", "fire_label",
  "fire_year", "fire_name", "fire_date",
  "type", "region", "lat", "lon", "site_other_burn_years",
  "imagery_pre_fire_date", "imagery_post_fire_date"
)

read_sites <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE) %>%
    select(any_of(CORE_COLS))
  for (col in setdiff(CORE_COLS, names(df))) df[[col]] <- NA_character_
  df[CORE_COLS]
}

recent_sites <- read_sites(file.path(OUT_RECENT, "recent_sites_coordinates.csv")) %>%
  filter(fire_year >= MIN_FIRE_YEAR)
message(sprintf("Recent fires (>= %d): %d fires -> %d sites",
                MIN_FIRE_YEAR, n_distinct(recent_sites$fire_label), nrow(recent_sites)))

sites <- recent_sites %>%
  mutate(pair_id = if_else(is.na(pair_id), "", pair_id)) %>%
  rename(id = site_id, YCoordinate = lat, XCoordinate = lon) %>%
  select(id, fire_name, fire_date,
         YCoordinate, XCoordinate,
         all_of(SUPPLEMENTARY_COLS)) %>%
  rename(
    date_pre_fire_imagery  = imagery_pre_fire_date,
    date_post_fire_imagery = imagery_post_fire_date
  ) %>%
  mutate(across(
    c(fire_date, date_pre_fire_imagery, date_post_fire_imagery),
    ~ {
      sapply(.x, function(val) {
        if (is.na(val) || val == "") return(val)
        parts <- trimws(strsplit(val, ";")[[1]])
        fmt   <- sapply(parts, function(p) {
          d <- tryCatch(as.Date(p), error = function(e) NA)
          if (!is.na(d)) format(d, "%d-%b-%Y") else p
        })
        paste(fmt, collapse = "; ")
      }, USE.NAMES = FALSE)
    }
  ))

# ── Find latest Designer export (same source as the full export) ─────
designer_ceps <- list.files(
  here("output", "collect_earth"),
  pattern = paste0("^", DESIGNER_SURVEY_NAME, "_.+\\.cep$"),
  full.names = TRUE
)
if (length(designer_ceps) == 0) {
  stop(
    "No Designer export found in output/collect_earth/\n",
    "Export from http://localhost:8888/collect and move the .cep there."
  )
}
DESIGNER_CEP <- designer_ceps[which.max(file.mtime(designer_ceps))]
message("Using Designer export: ", basename(DESIGNER_CEP))

# ── Patch and repack under the new name/title ─────────────────────────
out_dir <- here("output", "collect_earth")
dir.create(out_dir, showWarnings = FALSE)

write.csv(sites, file.path(out_dir, paste0("placemark_", FILE_NAME, ".csv")),
          row.names = FALSE)

tmp_dir <- tempfile()
dir.create(tmp_dir)
unzip(DESIGNER_CEP, exdir = tmp_dir)

csv_dest <- file.path(tmp_dir, "grid", "placemark.csv")
if (!dir.exists(dirname(csv_dest))) dir.create(dirname(csv_dest), recursive = TRUE)
write.csv(sites, csv_dest, row.names = FALSE)

props_file  <- file.path(tmp_dir, "project_definition.properties")
props_lines <- readLines(props_file)
props_lines <- gsub("distance_to_plot_boundaries=.*",
                    "distance_to_plot_boundaries=30", props_lines)
# Retitle the project (this is what shows as the survey/project name in
# Collect Earth / the Google Earth balloon), independent of the Designer
# form's own internal survey name.
props_lines <- gsub("^survey_name=.*",
                    paste0("survey_name=", SURVEY_TITLE), props_lines)
writeLines(props_lines, props_file)

# Collect Earth Desktop keys each project's locally-stored collected data by
# the survey's <uri> in placemark.idm.xml (project_definition.properties'
# survey_name is just the display title). Leaving the uri as
# .../jt_fire_recovery would make Desktop treat this as the SAME survey as
# the one you've already been filling out, and show its saved answers.
# Giving it its own uri (and default <project> title) makes this a distinct,
# empty survey on import.
idm_file  <- file.path(tmp_dir, "placemark.idm.xml")
idm_lines <- readLines(idm_file, warn = FALSE)
idm_lines <- gsub(
  paste0("<uri>http://www.openforis.org/idm/", DESIGNER_SURVEY_NAME, "</uri>"),
  paste0("<uri>http://www.openforis.org/idm/", SURVEY_TITLE, "</uri>"),
  idm_lines, fixed = TRUE
)
idm_lines <- gsub(
  "<project>JT Fire Recovery</project>",
  paste0("<project>", SURVEY_TITLE, "</project>"),
  idm_lines, fixed = TRUE
)
writeLines(idm_lines, idm_file)

cep_path <- file.path(out_dir, paste0(FILE_NAME, ".cep"))
if (file.exists(cep_path)) file.remove(cep_path)
old_wd <- setwd(tmp_dir)
on.exit(setwd(old_wd), add = TRUE)
zip(cep_path,
    files = list.files(tmp_dir, recursive = TRUE, full.names = FALSE),
    flags = "-r9X")
setwd(old_wd)
unlink(tmp_dir, recursive = TRUE)

message(sprintf("Sites: %d  ->  %s", nrow(sites), cep_path))
