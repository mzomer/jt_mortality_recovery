# ─────────────────────────────────────────────────────────────────────
# 09_historical_annotate.R — Add imagery dates and site notes to
#                            historical sites CSV
# ─────────────────────────────────────────────────────────────────────
# Reads historical_sites_coordinates.csv produced by 07_historical_sites.R
# and joins fire-level imagery dates and plot-level site notes.  Run this
# instead of re-running the sampling script when you only need to update notes.
# Input/output: output/historical/historical_sites_coordinates.csv
# ─────────────────────────────────────────────────────────────────────

source(here::here("scripts", "00_utils.R"))

sites_df <- read.csv(file.path(OUT_HIST, "historical_sites_coordinates.csv"),
                     stringsAsFactors = FALSE)

# ── Fire-level imagery dates ──────────────────────────────────────────
# imagery_post_fire_date: dates with good recovery imagery (semicolon-separated)
# imagery_pre_fire_date:  Landsat archive dates (to be added)
imagery_dates <- data.frame(
  fire_label = c(
    "joshua_1989",
    "blackbrush_1987",
    "mary_1984",
    "beacon_1993",
    "bunker_1993"
  ),
  imagery_pre_fire_date = NA_character_,
  imagery_post_fire_date = c(
    "2021",
    "2021;2025",
    "2025",
    "2015;2025",
    "2015;2025"
  ),
  imagery_notes = c(
    "2021 imagery good",
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_
  ),
  stringsAsFactors = FALSE
)

# ── Plot-level site notes ─────────────────────────────────────────────
site_notes_tbl <- data.frame(
  site_id    = character(0),
  site_notes = character(0),
  stringsAsFactors = FALSE
)

# ── Drop existing annotation columns before re-joining ────────────────
drop_cols <- c("imagery_pre_fire_date", "imagery_post_fire_date",
               "imagery_notes", "site_notes")
sites_df  <- sites_df[, setdiff(names(sites_df), drop_cols)]

# ── Join and normalise ────────────────────────────────────────────────
sites_df <- dplyr::left_join(sites_df, imagery_dates,  by = "fire_label")
sites_df <- dplyr::left_join(sites_df, site_notes_tbl, by = "site_id")

sites_df$imagery_pre_fire_date  <- ifelse(is.na(sites_df$imagery_pre_fire_date),
                                          NA_character_, sites_df$imagery_pre_fire_date)
sites_df$imagery_post_fire_date <- ifelse(is.na(sites_df$imagery_post_fire_date),
                                          NA_character_, sites_df$imagery_post_fire_date)
sites_df$imagery_notes          <- ifelse(is.na(sites_df$imagery_notes),
                                          NA_character_, sites_df$imagery_notes)
sites_df$site_notes             <- ifelse(is.na(sites_df$site_notes),
                                          NA_character_, sites_df$site_notes)

# ── Write back ────────────────────────────────────────────────────────
write.csv(sites_df, file.path(OUT_HIST, "historical_sites_coordinates.csv"), row.names = FALSE)
message(sprintf("Annotated %d sites across %d fires → %s",
                nrow(sites_df),
                length(unique(sites_df$fire_label)),
                file.path(OUT_HIST, "historical_sites_coordinates.csv")))
