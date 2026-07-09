# ─────────────────────────────────────────────────────────────────────
# 05_recent_annotate.R — Add imagery dates and site notes to output CSV
# ─────────────────────────────────────────────────────────────────────
# Reads recent_sites_coordinates.csv produced by 02_recent_sites.R and joins
# fire-level imagery dates and plot-level site notes.  Run this instead
# of re-running the sampling script when you only need to update notes.
# Input/output: output/recent/recent_sites_coordinates.csv
# ─────────────────────────────────────────────────────────────────────

source(here::here("scripts", "00_utils.R"))

sites_df <- read.csv(file.path(OUT_RECENT, "recent_sites_coordinates.csv"),
                     stringsAsFactors = FALSE)

# ── Fire-level imagery dates ──────────────────────────────────────────
imagery_dates <- data.frame(
  fire_label = c(
    "ivanpah_2020",
    "star_2019",
    "cahuila_2017",
    "pond_2021",
    "dome_2020",
    "nadeau_2020",
    "boulevard_2023",
    "sheep_2022",
    "mojave_2021",
    "johnson_2024",
    "cal_2024",
    "york_2023",
    "geology_2023",
    "quail_2012",
    "elk_trail_2022",
    "coles_flat_2020"
  ),
  imagery_pre_fire_date = c(
    "2017-11-18;2018-05-24",
    "2016-07-02",
    "2015-01-01;2016-09-04",
    "2021-03-28",
    "2016-07-02",
    "2017-07-01",
    "2017-04-29",
    "2022-05-30",
    "2021-03-27;2016-07-02",
    "2022",
    "2017-04-29;2023-01-25",
    "2017-11-18;2021-03-28",
    "2021-06-11;2019-12-11;2018-02-17",
    "2011-09-16;2011-08-23",
    "2019-12-12;2021-06-11",
    "2017-07-01"
  ),
  imagery_post_fire_date = c(
    "2021-03-28;2023-07-17",
    "2021-03-27",
    "2017-06-13;2017-12-27;2018-08-25;2022-05-30;2022-10-02;2025-03-14",
    "2023-07-17",
    "2021-03-27;2023-07-17;2024-06-17",
    "2023-11-22",
    "2023-09-16",
    "2023-05-19;2025-06-16",
    "2023-07-17",
    "2025-05-23",
    "2025-04-23",
    "2023-10-27",
    "2025-05-24",
    "2025-05-24;2021-06-11;2018-08-24",
    "2025-05-24;2026-03-14",
    "2024-05-04"
  ),
  imagery_notes = c(
    NA_character_,
    NA_character_,
    NA_character_,
    "only one post-fire date available (~2 years post-fire)",
    NA_character_,
    NA_character_,
    "not sure; only post-fire date is 1 month post-fire",
    NA_character_,
    "post-fire imagery difficult",
    "post-fire imagery difficult to see; pre-fire: year only (no specific date found)",
    "too many markers dropped?",
    "post-fire imagery difficult; not sure review is possible",
    "post-fire imagery clear",
    NA_character_,
    NA_character_,
    NA_character_
  ),
  stringsAsFactors = FALSE
)

# ── Plot-level site notes ─────────────────────────────────────────────
site_notes_tbl <- data.frame(
  site_id    = c(
    "ivanpah_2020_inside_05",
    "nadeau_2020_inside_03",
    "mojave_2021_inside_04",
    "geology_2023_inside_04",
    "geology_2023_outside_04",
    "geology_2023_inside_07"
  ),
  site_notes = c(
    "unburned",
    "burned?",
    "didn't burn?",
    "no JT",
    "no JT",
    "no JT"
  ),
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
write.csv(sites_df, file.path(OUT_RECENT, "recent_sites_coordinates.csv"), row.names = FALSE)
message(sprintf("Annotated %d sites across %d fires → %s",
                nrow(sites_df),
                length(unique(sites_df$fire_label)),
                file.path(OUT_RECENT, "recent_sites_coordinates.csv")))
