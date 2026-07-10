# ─────────────────────────────────────────────────────────────────────
# 12_collect_earth_export.R — Build Collect Earth project bundle (.cep)
# ─────────────────────────────────────────────────────────────────────
# Workflow:
#   1. Design survey form in Collect Survey Designer (localhost:8888/collect)
#   2. Export .cep → it lands in ~/Downloads/
#   3. Run this script — it auto-finds the latest jt_fire_recovery_*.cep
#      in Downloads, patches it with your real CSV + 60x60m plot boundary
#      + balloon card + fire boundary layers, and writes the final bundle.
#   4. Import output/collect_earth/jt_fire_recovery.cep into Collect Earth.
#
# Input:  output/recent/recent_sites_coordinates.csv
#         output/historical/historical_sites_coordinates.csv
#         ~/Downloads/jt_fire_recovery_*.cep  (latest Designer export)
# Output: output/collect_earth/jt_fire_recovery.cep  (import this into CE)
#         output/collect_earth/placemark.csv          (standalone copy)
# ─────────────────────────────────────────────────────────────────────

source(here::here("scripts", "00_utils.R"))

SURVEY_NAME <- "jt_fire_recovery"

# CSV columns to include (beyond the required CE columns id/YCoordinate/XCoordinate)
# Names here must exactly match the CSV file columns; imagery dates are renamed
# below to match the IDM attribute names used in the Designer form.
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

sites <- bind_rows(
  read_sites(file.path(OUT_RECENT, "recent_sites_coordinates.csv")),
  read_sites(file.path(OUT_HIST,   "historical_sites_coordinates.csv"))
) %>%
  mutate(pair_id = if_else(is.na(pair_id), "", pair_id)) %>%
  rename(id = site_id, YCoordinate = lat, XCoordinate = lon) %>%
  # Key columns (id, fire_name, fire_date) must come before YCoordinate/XCoordinate
  # because fire_name and fire_date are marked as keys in the Designer IDM.
  select(id, fire_name, fire_date,
         YCoordinate, XCoordinate,
         all_of(SUPPLEMENTARY_COLS)) %>%
  # Rename to match Designer IDM attribute names (required for "From CSV" autofill)
  rename(
    date_pre_fire_imagery  = imagery_pre_fire_date,
    date_post_fire_imagery = imagery_post_fire_date
  ) %>%
  # Format dates as 23-May-2024 (unambiguous across locales).
  # Handles semicolon-separated multi-date values; leaves year-only strings as-is.
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

# ── Find latest Designer export ──────────────────────────────────────
# Move your Designer .cep export into output/collect_earth/ then run this
# script.  Exports have a timestamp suffix (e.g. jt_fire_recovery_en_2026-..cep)
# so they're distinguishable from the patched output (jt_fire_recovery.cep).
designer_ceps <- list.files(
  here("output", "collect_earth"),
  pattern = paste0("^", SURVEY_NAME, "_.+\\.cep$"),
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

# ── Patch and repack ─────────────────────────────────────────────────
out_dir <- here("output", "collect_earth")
dir.create(out_dir, showWarnings = FALSE)

# Write standalone CSV copy (useful for manual CE upload)
write.csv(sites, file.path(out_dir, "placemark.csv"), row.names = FALSE)

tmp_dir <- tempfile()
dir.create(tmp_dir)
unzip(DESIGNER_CEP, exdir = tmp_dir)

# 1. Replace sampling CSV (Designer stores it under grid/)
csv_dest <- file.path(tmp_dir, "grid", "placemark.csv")
if (!dir.exists(dirname(csv_dest))) dir.create(dirname(csv_dest), recursive = TRUE)
write.csv(sites, csv_dest, row.names = FALSE)

# 2. Patch project_definition.properties
props_file  <- file.path(tmp_dir, "project_definition.properties")
props_lines <- readLines(props_file)

#    60x60m plot boundary (Designer default is 25 → 50x50m)
props_lines <- gsub("distance_to_plot_boundaries=.*",
                    "distance_to_plot_boundaries=30", props_lines)

writeLines(props_lines, props_file)

# 4. Repack
cep_path <- file.path(out_dir, paste0(SURVEY_NAME, ".cep"))
if (file.exists(cep_path)) file.remove(cep_path)
old_wd <- setwd(tmp_dir)
on.exit(setwd(old_wd), add = TRUE)
zip(cep_path,
    files = list.files(tmp_dir, recursive = TRUE, full.names = FALSE),
    flags = "-r9X")
setwd(old_wd)
unlink(tmp_dir, recursive = TRUE)

message(sprintf("Sites: %d  →  %s", nrow(sites), cep_path))

# Per-fire imagery date reference table
fire_ref <- sites %>%
  distinct(fire_name, fire_year, fire_date,
           date_pre_fire_imagery, date_post_fire_imagery) %>%
  arrange(fire_year, fire_name)

fire_ref_path <- file.path(out_dir, "fire_imagery_dates.csv")
write.csv(fire_ref, fire_ref_path, row.names = FALSE)
message(sprintf("Fire reference  →  %s", fire_ref_path))
