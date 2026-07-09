# ─────────────────────────────────────────────────────────────────────
# 06_historical_perimeters.R — Historical fire candidate selection
#                              for the long-term recovery analysis
# ─────────────────────────────────────────────────────────────────────
# Fires 1984–1994 (Landsat 5 TM era; ≥ 30 years before 2024).
# Filters to fires ≥ MIN_ACRES with ≥ 30% JT range overlap.
# Run this script, review candidates in GEP, then fill in
# CONFIRMED_HIST_FIRES in 07_historical_sites.R.
#
# Output: output/historical/historical_fire_perimeters_candidates.gpkg
#         output/historical/historical_fire_perimeters_candidates.kmz
#         output/historical/all_fires_utm.rds
# ─────────────────────────────────────────────────────────────────────

source(here::here("scripts", "00_utils.R"))

# Parameters ----
HIST_YEAR_MIN <- 1984   # Landsat 5 TM launch — earliest reliable dNBR
HIST_YEAR_MAX <- 1994   # ≥ 30 years before 2024
MIN_JT_PCT    <- 0.30   # override 00_utils.R default of 0.60 for historical fires

# Load California fire data — CalFire GDB
calfire_raw <- st_read(here("data", "fire_boundaries", "fire25_1.gdb"),
                       layer = "firep25_1", quiet = TRUE)
calfire_raw <- calfire_raw[
  as.character(st_geometry_type(calfire_raw)) %in% c("POLYGON", "MULTIPOLYGON"), ]

calfire <- calfire_raw %>%
  st_transform(CRS_WGS) %>%
  st_set_geometry("geometry") %>%
  mutate(
    year      = as.integer(YEAR_),
    fire_date = as.Date(as.character(ALARM_DATE)),
    acres     = as.numeric(GIS_ACRES),
    fire_name = trimws(if_else(is.na(FIRE_NAME) | FIRE_NAME == "", "Unnamed", FIRE_NAME)),
    source    = "calfire"
  ) %>%
  filter(!is.na(year), year >= HIST_YEAR_MIN, year <= HIST_YEAR_MAX, acres >= MIN_ACRES) %>%
  select(fire_name, year, fire_date, acres, source)
message(sprintf("CalFire: %d fires (%d–%d, ≥%d ac)", nrow(calfire), HIST_YEAR_MIN, HIST_YEAR_MAX, MIN_ACRES))

# Load Nevada fire data
nv_raw <- st_read(here("data", "fire_boundaries",
                        "Nevada_Wildland_Fire_History_2313768665656203223",
                        "Wildland_Fire_History.shp"), quiet = TRUE) %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>%
  st_transform(CRS_WGS)

nv_fires <- nv_raw %>%
  mutate(
    year      = suppressWarnings(as.integer(as.character(Fire_Year))),
    fire_date = as.Date(Fire_Disco),
    acres     = as.numeric(Acres),
    fire_name = if_else(is.na(Incident_N) | Incident_N == "", "Unnamed", Incident_N),
    source    = "nv_wildfire"
  ) %>%
  filter(!is.na(year), year >= HIST_YEAR_MIN, year <= HIST_YEAR_MAX, acres >= MIN_ACRES) %>%
  select(fire_name, year, fire_date, acres, source)
message(sprintf("Nevada: %d fires (%d–%d, ≥%d ac)", nrow(nv_fires), HIST_YEAR_MIN, HIST_YEAR_MAX, MIN_ACRES))

fires_combined <- bind_rows(calfire, nv_fires) %>% st_make_valid()
message(sprintf("Combined: %d fires before spatial filter", nrow(fires_combined)))

# Filter to ≥30% JT presence overlap
jt_presence <- load_jt_presence()
fires_utm   <- st_transform(fires_combined, CRS_UTM)
jt_sf_utm   <- st_sf(geometry = jt_presence) %>% st_transform(CRS_UTM)

intersects_jt  <- lengths(st_intersects(fires_utm, jt_sf_utm)) > 0
message(sprintf("%d / %d fires intersect JT range", sum(intersects_jt), nrow(fires_utm)))

fires_cand_utm <- fires_utm[intersects_jt, ]
pct_jt_cand <- sapply(seq_len(nrow(fires_cand_utm)), function(i) {
  int <- suppressWarnings(st_intersection(fires_cand_utm[i, ], jt_sf_utm))
  if (nrow(int) == 0) return(0)
  as.numeric(sum(st_area(int))) / as.numeric(st_area(fires_cand_utm[i, ]))
})

fires_jt <- fires_combined[intersects_jt, ][pct_jt_cand >= MIN_JT_PCT, ] %>%
  mutate(pct_jt = pct_jt_cand[pct_jt_cand >= MIN_JT_PCT])
message(sprintf("After ≥30%% JT filter: %d candidate fires", nrow(fires_jt)))

# All fires (full record, no year filter) for reburn history
all_fires_utm <- bind_rows(
  calfire_raw %>%
    st_transform(CRS_WGS) %>% st_set_geometry("geometry") %>%
    mutate(year = as.integer(YEAR_)) %>% filter(!is.na(year)) %>% select(year),
  nv_raw %>%
    mutate(year = as.integer(Fire_Year)) %>% filter(!is.na(year)) %>% select(year)
) %>% st_transform(CRS_UTM)

# Burn history (informational — for GEP review table)
fires_jt_utm <- st_transform(fires_jt, CRS_UTM) %>% mutate(row_id = seq_len(n()))
burn_join <- st_join(fires_jt_utm["row_id"], all_fires_utm, join = st_intersects) %>%
  st_drop_geometry() %>%
  rename(hist_year = year) %>%
  left_join(st_drop_geometry(fires_jt_utm[, c("row_id", "year")]), by = "row_id") %>%
  filter(hist_year != year)

burn_history <- sapply(seq_len(nrow(fires_jt_utm)), function(i) {
  yrs <- burn_join$hist_year[burn_join$row_id == i]
  if (length(yrs) == 0) return("none")
  paste(sort(unique(yrs)), collapse = ";")
})

fires_jt <- fires_jt %>%
  mutate(other_burn_years = burn_history,
         label = paste0(fire_name, "_", year)) %>%
  group_by(label) %>%
  mutate(label = if (n() > 1) paste0(label, "_", row_number()) else label) %>%
  ungroup()

# Add MTBS CA fires not covered by CalFire (federal lands)
mtbs_raw <- st_read(here("data", "fire_boundaries", "mtbs_perimeter_data", "mtbs_perims_DD.shp"),
                    quiet = TRUE) %>%
  filter(startsWith(event_id, "CA"), incid_type == "Wildfire") %>%
  st_set_geometry("geometry") %>%
  st_transform(CRS_UTM) %>%
  mutate(year      = as.integer(format(as.Date(ig_date), "%Y")),
         fire_date = as.Date(ig_date),
         acres     = round(as.numeric(burnbndac)),
         fire_name = if_else(is.na(incid_name) | incid_name == "", "Unnamed", incid_name),
         source    = "mtbs") %>%
  filter(!is.na(year), year >= HIST_YEAR_MIN, year <= HIST_YEAR_MAX, acres >= MIN_ACRES)

mtbs_jt_pct <- sapply(seq_len(nrow(mtbs_raw)), function(i) {
  int <- suppressWarnings(st_intersection(mtbs_raw[i, ], jt_sf_utm))
  if (nrow(int) == 0) return(0)
  as.numeric(sum(st_area(int))) / as.numeric(st_area(mtbs_raw[i, ]))
})
mtbs_pass <- mtbs_raw[mtbs_jt_pct >= MIN_JT_PCT, ] %>%
  mutate(pct_jt = mtbs_jt_pct[mtbs_jt_pct >= MIN_JT_PCT])

fires_jt_utm_dedup <- st_transform(fires_jt, CRS_UTM)
keep_mtbs <- vapply(seq_len(nrow(mtbs_pass)), function(i) {
  overlap <- suppressWarnings(
    as.numeric(st_area(st_intersection(mtbs_pass[i, ], st_union(fires_jt_utm_dedup)))) /
    as.numeric(st_area(mtbs_pass[i, ]))
  )
  if (length(overlap) == 0 || is.na(overlap)) return(TRUE)
  overlap <= 0.70
}, logical(1))

mtbs_new <- mtbs_pass[keep_mtbs, ] %>%
  st_transform(CRS_WGS) %>%
  select(fire_name, year, fire_date, acres, source, pct_jt)
message(sprintf("MTBS CA adds %d new fires not in CalFire", nrow(mtbs_new)))

if (nrow(mtbs_new) > 0) {
  mtbs_new_utm <- st_transform(mtbs_new, CRS_UTM) %>%
    mutate(row_id = seq_len(n()) + nrow(fires_jt))
  bj2 <- st_join(mtbs_new_utm["row_id"], all_fires_utm, join = st_intersects) %>%
    st_drop_geometry() %>% rename(hist_year = year) %>%
    left_join(st_drop_geometry(mtbs_new_utm[, c("row_id", "year")]), by = "row_id") %>%
    filter(hist_year != year)
  bh2 <- sapply(seq_len(nrow(mtbs_new_utm)), function(i) {
    rid <- mtbs_new_utm$row_id[i]
    yrs <- bj2$hist_year[bj2$row_id == rid]
    if (length(yrs) == 0) return("none")
    paste(sort(unique(yrs)), collapse = ";")
  })
  mtbs_new <- mtbs_new %>%
    mutate(other_burn_years = bh2,
           label = paste0(fire_name, "_", year))
  fires_jt <- bind_rows(fires_jt, mtbs_new)
  message(sprintf("After MTBS CA additions: %d candidate fires", nrow(fires_jt)))
}

# Print candidate table ----
fires_jt %>%
  st_drop_geometry() %>%
  mutate(acres  = round(acres),
         pct_jt = paste0(round(pct_jt * 100), "%")) %>%
  select(label, fire_name, year, acres, pct_jt, other_burn_years, source) %>%
  arrange(year) %>%
  print(n = Inf)

# Save candidates ----
st_write(fires_jt,
         file.path(OUT_HIST, "historical_fire_perimeters_candidates.gpkg"),
         layer = "fire_perimeters", delete_layer = TRUE, quiet = TRUE)

saveRDS(all_fires_utm, file.path(OUT_HIST, "all_fires_utm.rds"))

kml_tmp  <- tempfile(fileext = ".kml")
kmz_path <- file.path(OUT_HIST, "historical_fire_perimeters_candidates.kmz")
fires_jt %>%
  st_transform(CRS_WGS) %>%
  mutate(Name        = label,
         Description = sprintf("%s (%d) — %d ac | JT: %s | other burns: %s",
                               fire_name, year, round(acres),
                               paste0(round(pct_jt * 100), "%"),
                               other_burn_years)) %>%
  select(Name, Description) %>%
  st_write(kml_tmp, driver = "KML", delete_dsn = FALSE, quiet = TRUE)
zip(zipfile = kmz_path, files = kml_tmp, flags = "-j")

message(sprintf("Saved %d candidate fires → .gpkg + .kmz", nrow(fires_jt)))

# Interactive candidate map ----
years_hist <- sort(unique(fires_jt$year))
pal_hist   <- colorNumeric("viridis", domain = range(years_hist), reverse = TRUE)
fires_map  <- fires_jt %>% st_make_valid() %>% st_cast("MULTIPOLYGON")

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = fires_map,
              fillColor   = ~pal_hist(year),
              fillOpacity = 0.8,
              color       = ~pal_hist(year),
              weight      = 2,
              label       = ~sprintf("%s (%d) — %d ac | JT: %s | burns: %s",
                                     fire_name, year, round(acres),
                                     paste0(round(pct_jt * 100), "%"),
                                     other_burn_years) %>%
                              lapply(htmltools::HTML)) %>%
  addLegend(pal = pal_hist, values = fires_map$year,
            labFormat = labelFormat(big.mark = ""),
            title     = sprintf("Historical candidates<br><small>%d–%d · n=%d</small>",
                                HIST_YEAR_MIN, HIST_YEAR_MAX, nrow(fires_map)),
            position  = "bottomright")
