# ─────────────────────────────────────────────────────────────────────
# 12b_collect_earth_export_test.R — Build a small test Collect Earth
#                                    bundle (.cep): one border pair +
#                                    one interior plot per recent fire.
# ─────────────────────────────────────────────────────────────────────
# Same patching logic as 12_collect_earth_export.R, but subsets the
# sites to a handful per fire for quick testing. Does not touch the
# full jt_fire_recovery.cep / placemark.csv outputs.
#
# Input:  output/recent/recent_sites_coordinates.csv
#         ~/Downloads/jt_fire_recovery_*.cep  (latest Designer export)
# Output: output/collect_earth/jt_fire_recovery_test.cep
#         output/collect_earth/placemark_test.csv
# ─────────────────────────────────────────────────────────────────────

source(here::here("scripts", "00_utils.R"))

SURVEY_NAME <- "jt_fire_recovery"

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

recent_sites <- read_sites(file.path(OUT_RECENT, "recent_sites_coordinates.csv"))

# ── Subset: one border pair + one interior plot per fire ──────────────
# Picks the lowest pair_id and the lowest-numbered interior site_id per
# fire, so the subset is deterministic and reproducible.
subset_one_pair_one_interior <- function(df) {
  df %>%
    group_by(fire_label) %>%
    group_modify(~ {
      fire_df <- .x
      pair_rows <- fire_df %>%
        filter(!is.na(pair_id) & pair_id != "" & pair_id != "NA")
      if (nrow(pair_rows) > 0) {
        keep_pair_id <- sort(unique(pair_rows$pair_id))[1]
        pair_rows <- pair_rows %>% filter(pair_id == keep_pair_id)
      }
      interior_rows <- fire_df %>%
        filter(type == "interior") %>%
        arrange(site_id) %>%
        slice_head(n = 1)
      bind_rows(pair_rows, interior_rows)
    }) %>%
    ungroup()
}

test_sites_raw <- subset_one_pair_one_interior(recent_sites)
message(sprintf("Subset: %d fires -> %d sites", n_distinct(test_sites_raw$fire_label), nrow(test_sites_raw)))

sites <- test_sites_raw %>%
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

# ── Patch and repack into a separate test bundle ──────────────────────
out_dir <- here("output", "collect_earth")
dir.create(out_dir, showWarnings = FALSE)

write.csv(sites, file.path(out_dir, "placemark_test.csv"), row.names = FALSE)

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
writeLines(props_lines, props_file)

cep_path <- file.path(out_dir, paste0(SURVEY_NAME, "_test.cep"))
if (file.exists(cep_path)) file.remove(cep_path)
old_wd <- setwd(tmp_dir)
on.exit(setwd(old_wd), add = TRUE)
zip(cep_path,
    files = list.files(tmp_dir, recursive = TRUE, full.names = FALSE),
    flags = "-r9X")
setwd(old_wd)
unlink(tmp_dir, recursive = TRUE)

message(sprintf("Sites: %d  ->  %s", nrow(sites), cep_path))
