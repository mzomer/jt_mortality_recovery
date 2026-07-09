# ─────────────────────────────────────────────────────────────────────
# 11_export_boundaries.R — Export confirmed fire boundaries for RBR
# ─────────────────────────────────────────────────────────────────────
# Combines confirmed recent and historical fire perimeters into a single
# GeoJSON for upload to GEE as a FeatureCollection asset.
# Derives confirmed fire sets from the sampled-sites CSVs so this stays
# in sync automatically when fires are added or removed.
#
# Input:  output/recent/recent_sites_coordinates.csv
#         output/recent/fire_perimeters_candidates.gpkg
#         output/historical/historical_sites_coordinates.csv
#         output/historical/historical_fire_perimeters_candidates.gpkg
# Output: output/fire_boundaries_confirmed.geojson
# ─────────────────────────────────────────────────────────────────────

source(here::here("scripts", "00_utils.R"))

# Landsat sensor flag — used by GEE workflow to select the right collection
# recent  (2012+)   → Landsat 8/9
# historical (1984–1994) → Landsat 5 TM

load_perims <- function(gpkg_path, sites_csv_path, type_label, sensor) {
  confirmed <- unique(read.csv(sites_csv_path, stringsAsFactors = FALSE)$fire_label)
  st_read(gpkg_path, quiet = TRUE) %>%
    mutate(label = norm_label(label)) %>%
    filter(label %in% confirmed) %>%
    filter(!duplicated(label)) %>%
    st_transform(CRS_WGS) %>%
    mutate(
      fire_type = type_label,
      sensor    = sensor,
      label     = label
    ) %>%
    select(label, fire_name, fire_date, year, acres, fire_type, sensor)
}

recent_perims <- load_perims(
  gpkg_path   = file.path(OUT_RECENT, "fire_perimeters_candidates.gpkg"),
  sites_csv_path = file.path(OUT_RECENT, "recent_sites_coordinates.csv"),
  type_label  = "recent",
  sensor      = "L8_L9"
)

hist_perims <- load_perims(
  gpkg_path   = file.path(OUT_HIST, "historical_fire_perimeters_candidates.gpkg"),
  sites_csv_path = file.path(OUT_HIST, "historical_sites_coordinates.csv"),
  type_label  = "historical",
  sensor      = "L5_TM"
)

all_perims <- rbind(recent_perims, hist_perims) %>%
  arrange(year)

out_geojson <- here("output", "fire_boundaries_confirmed.geojson")
st_write(all_perims, out_geojson, delete_dsn = TRUE, quiet = TRUE)

# Shapefile — field names truncated to 10 chars by driver
shp_dir <- here("output", "fire_boundaries_confirmed_shp")
dir.create(shp_dir, showWarnings = FALSE)
st_write(all_perims, file.path(shp_dir, "fire_boundaries_confirmed.shp"),
         delete_dsn = TRUE, quiet = TRUE)
shp_zip <- here("output", "fire_boundaries_confirmed.zip")
old_wd <- setwd(here("output"))
zip(shp_zip, files = basename(shp_dir), flags = "-r9X")
setwd(old_wd)

message(sprintf("Saved %d fire boundaries", nrow(all_perims)))
message(sprintf("  GeoJSON  → %s", out_geojson))
message(sprintf("  Shapefile → %s  (zipped)", shp_zip))
message(sprintf("  recent (%s): %d fires",
                paste(sort(unique(recent_perims$year)), collapse = ", "),
                nrow(recent_perims)))
message(sprintf("  historical (%s): %d fires",
                paste(sort(unique(hist_perims$year)), collapse = ", "),
                nrow(hist_perims)))
