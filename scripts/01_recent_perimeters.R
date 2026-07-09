# ────────────────────────────────────────────────────────────────
# 01_recent_perimeters.R — Load and filter wildfire perimeters
# ────────────────────────────────────────────────────────────────
#
# Sources: CA fire data (CalFire GDB) + Nevada Wildland Fire History
# Filters: year, size, ≥ 60% overlap with JT presence cells
# Output:  fire_perimeters_candidates.gpkg + candidate table
#
# NOTE: After running, inspect candidates in Google Earth Pro to confirm
# visible Joshua trees, then add confirmed labels to CONFIRMED_FIRES
# in 02_recent_sites.R before running that script.

source(here::here("scripts", "00_utils.R"))

# Load California fire data — CalFire GDB -----
calfire_raw <- st_read(here("data", "fire_boundaries", "fire25_1.gdb"),
                       layer = "firep25_1", quiet = TRUE)
calfire_raw <- calfire_raw[
  as.character(st_geometry_type(calfire_raw)) %in% c("POLYGON", "MULTIPOLYGON"), ]

message(sprintf("CalFire native CRS: EPSG %s", st_crs(calfire_raw)$epsg))
message(sprintf("CalFire raw bbox: %.2f – %.2f, %.2f – %.2f",
                st_bbox(calfire_raw)[1], st_bbox(calfire_raw)[3],
                st_bbox(calfire_raw)[2], st_bbox(calfire_raw)[4]))

calfire <- calfire_raw %>%
  st_transform(CRS_WGS) %>%
  st_set_geometry("geometry") %>%   # standardise geometry column name (GDB uses "Shape")
  mutate(
    year      = as.integer(YEAR_),
    fire_date = as.Date(as.character(ALARM_DATE)),
    acres     = as.numeric(GIS_ACRES),
    fire_name = trimws(if_else(is.na(FIRE_NAME) | FIRE_NAME == "", "Unnamed", FIRE_NAME)),
    source    = "calfire"
  ) %>%
  filter(!is.na(year),
         year >= YEAR_MIN, year <= YEAR_MAX,
         acres >= MIN_ACRES) %>%
  select(fire_name, year, fire_date, acres, source)
message(sprintf("CalFire: %d fires loaded (%d–%d, ≥%d ac)",
                nrow(calfire), YEAR_MIN, YEAR_MAX, MIN_ACRES))
message(sprintf("CalFire WGS84 bbox: lon %.2f–%.2f, lat %.2f–%.2f",
                st_bbox(calfire)[1], st_bbox(calfire)[3],
                st_bbox(calfire)[2], st_bbox(calfire)[4]))

# Load Nevada fires — Nevada Wildland Fire History -----
nv_raw <- st_read(here("data", "fire_boundaries",
                        "Nevada_Wildland_Fire_History_2313768665656203223",
                        "Wildland_Fire_History.shp"), quiet = TRUE) %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>%           # coerce to MULTIPOLYGON without splitting rows
  st_transform(CRS_WGS)

nv_fires <- nv_raw %>%
  mutate(
    year      = suppressWarnings(as.integer(as.character(Fire_Year))),
    fire_date = as.Date(Fire_Disco),
    acres     = as.numeric(Acres),
    fire_name = if_else(is.na(Incident_N) | Incident_N == "", "Unnamed", Incident_N),
    source    = "nv_wildfire"
  ) %>%
  filter(!is.na(year),
         year >= YEAR_MIN, year <= YEAR_MAX,
         acres >= MIN_ACRES) %>%
  select(fire_name, year, fire_date, acres, source)

message(sprintf("Nevada fire data: %d fires loaded (%d–%d, ≥%d ac)",
                nrow(nv_fires), YEAR_MIN, YEAR_MAX, MIN_ACRES))

fires_combined <- bind_rows(calfire, nv_fires) %>%
  st_make_valid()
message(sprintf("Combined: %d fires before spatial filter", nrow(fires_combined)))

# Filter to JT presence range (≥ 60% overlap) -----
jt_presence <- load_jt_presence()
fires_utm   <- st_transform(fires_combined, CRS_UTM)
# jt_presence is already an sfc with WGS84 CRS — wrap directly in st_sf then transform
jt_sf_utm   <- st_sf(geometry = jt_presence) %>% st_transform(CRS_UTM)

# Coordinate sanity check
message(sprintf("Fires bbox  (UTM): %.0f – %.0f E, %.0f – %.0f N",
                st_bbox(fires_utm)[1], st_bbox(fires_utm)[3],
                st_bbox(fires_utm)[2], st_bbox(fires_utm)[4]))
message(sprintf("JT range bbox (UTM): %.0f – %.0f E, %.0f – %.0f N",
                st_bbox(jt_sf_utm)[1], st_bbox(jt_sf_utm)[3],
                st_bbox(jt_sf_utm)[2], st_bbox(jt_sf_utm)[4]))

# Step 1: fast prefilter — keep fires that intersect JT range at all
intersects_jt  <- lengths(st_intersects(fires_utm, jt_sf_utm)) > 0
message(sprintf("%d / %d fires intersect JT range", sum(intersects_jt), nrow(fires_utm)))

# Step 2: compute exact overlap fraction for intersecting fires only
fires_cand_utm <- fires_utm[intersects_jt, ]
pct_jt_cand <- sapply(seq_len(nrow(fires_cand_utm)), function(i) {
  int <- suppressWarnings(st_intersection(fires_cand_utm[i, ], jt_sf_utm))
  if (nrow(int) == 0) return(0)
  as.numeric(sum(st_area(int))) / as.numeric(st_area(fires_cand_utm[i, ]))
})

fires_jt <- fires_combined[intersects_jt, ][pct_jt_cand >= MIN_JT_PCT, ] %>%
  mutate(pct_jt = pct_jt_cand[pct_jt_cand >= MIN_JT_PCT])
message(sprintf("After ≥60%% JT filter: %d candidate fires", nrow(fires_jt)))

# Burn history — flag overlapping fires from CalFire and Nevada Wildland Fire History -----
# Records all years in which any other fire burned within each candidate's perimeter,
# both before and after the study fire. This column is informational here — it appears
# in the candidate table to guide manual GEP review. The actual plot-level exclusion
# of multiply-burned areas is applied in 02_recent_sites.R using build_reburn_mask().
all_fires_ca <- calfire_raw %>%
  st_transform(CRS_WGS) %>%
  st_set_geometry("geometry") %>%
  mutate(year = as.integer(YEAR_)) %>%
  filter(!is.na(year)) %>%
  select(year) %>%
  st_transform(CRS_UTM)

all_fires_nv <- nv_raw %>%
  mutate(year = as.integer(Fire_Year)) %>%
  filter(!is.na(year)) %>%
  select(year) %>%
  st_transform(CRS_UTM)

all_fires_utm <- bind_rows(all_fires_ca, all_fires_nv)

message(sprintf("all_fires_utm: %d records, years %d–%d, bbox %.0f–%.0f E, %.0f–%.0f N",
                nrow(all_fires_utm),
                min(all_fires_utm$year, na.rm = TRUE),
                max(all_fires_utm$year, na.rm = TRUE),
                st_bbox(all_fires_utm)[1], st_bbox(all_fires_utm)[3],
                st_bbox(all_fires_utm)[2], st_bbox(all_fires_utm)[4]))

fires_jt_utm <- st_transform(fires_jt, CRS_UTM) %>%
  mutate(row_id = seq_len(n()))

# Use st_join for vectorised intersection — more reliable than per-row st_intersects
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

n_with_burns <- sum(burn_history != "none")
message(sprintf("Burn history: %d / %d fires have overlapping burns in record",
                n_with_burns, length(burn_history)))

fires_jt <- fires_jt %>%
  mutate(other_burn_years = burn_history,
         label = paste0(fire_name, "_", year))

# Make duplicate labels unique (cross-border fires in both datasets)
fires_jt <- fires_jt %>%
  group_by(label) %>%
  mutate(label = if (n() > 1) paste0(label, "_", row_number()) else label) %>%
  ungroup()

# Add MTBS California fires not already covered by CalFire -----
# CalFire GDB covers State Responsibility Area only; federal land fires (BLM/NPS/USFS)
# in California are only in MTBS. Dedup: drop any MTBS fire with >70% spatial overlap
# with an already-selected CalFire candidate.
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
  filter(!is.na(year), year >= YEAR_MIN, year <= YEAR_MAX, acres >= MIN_ACRES)

# JT spatial filter on MTBS CA
mtbs_jt_pct <- sapply(seq_len(nrow(mtbs_raw)), function(i) {
  int <- suppressWarnings(st_intersection(mtbs_raw[i, ], jt_sf_utm))
  if (nrow(int) == 0) return(0)
  as.numeric(sum(st_area(int))) / as.numeric(st_area(mtbs_raw[i, ]))
})
mtbs_pass <- mtbs_raw[mtbs_jt_pct >= MIN_JT_PCT, ] %>%
  mutate(pct_jt = mtbs_jt_pct[mtbs_jt_pct >= MIN_JT_PCT])

# Dedup: remove MTBS fires already covered by CalFire candidates (>70% overlap)
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

# Build burn history for new MTBS fires
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

# Final candidate table -----
fires_jt %>%
  st_drop_geometry() %>%
  mutate(acres  = round(acres),
         pct_jt = if_else(is.na(pct_jt), "manual", paste0(round(pct_jt * 100), "%"))) %>%
  select(label, fire_name, year, acres, pct_jt, other_burn_years, source) %>%
  arrange(year) %>%
  print(n = Inf)

# Save candidates -----
dir.create(file.path(OUT_RECENT, "shapefiles"), recursive = TRUE, showWarnings = FALSE)

st_write(fires_jt, file.path(OUT_RECENT, "fire_perimeters_candidates.gpkg"),
         layer = "fire_perimeters", delete_layer = TRUE, quiet = TRUE)

st_write(fires_jt, file.path(OUT_RECENT, "shapefiles", "fire_perimeters_candidates.shp"),
         delete_layer = TRUE, quiet = TRUE)

message(sprintf("Saved %d candidate fires → .gpkg and shapefiles/", nrow(fires_jt)))



# Interactive map of candidate fires -----
library(leaflet)

# Load layers for map
yubr_map <- st_read(here("data", "Esque_distribution", "YUBR_Presence_Cells",
                          "YUBR_Presence_Cells.shp"), quiet = TRUE) %>%
  st_transform(CRS_UTM) %>% st_union() %>% st_make_valid() %>%
  st_simplify(dTolerance = 1000) %>% st_transform(CRS_WGS)

yuja_map <- st_read(here("data", "Esque_distribution", "YUJA_Presence_Cells",
                          "YUJA_Presence_Cells.shp"), quiet = TRUE) %>%
  st_transform(CRS_UTM) %>% st_union() %>% st_make_valid() %>%
  st_simplify(dTolerance = 1000) %>% st_transform(CRS_WGS)

jotr_bound <- st_read(here("data", "fire_boundaries", "JoshuaTree",
                           "Joshua_Tree_National_Park.shp"), quiet = TRUE) %>%
  st_transform(CRS_WGS)

moja_bound <- st_read(here("data", "fire_boundaries", "Mojave", "Mojave_National_Preserve.shp"),
                      quiet = TRUE) %>%
  st_transform(CRS_WGS)

# Colour palette by year (magma, matching Python maps)
years <- sort(unique(fires_jt$year))
pal   <- colorNumeric("magma", domain = range(years), reverse = TRUE)

# Ensure fires are valid multipolygons for leaflet
fires_map <- fires_jt %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON")

map_candidates <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  # YUBR presence (western JT) — green
  addPolygons(data = st_sf(geometry = yubr_map),
              fillColor = "#2E7D32", fillOpacity = 0.22,
              color = "#2E7D32", weight = 0.5,
              group = "YUBR presence (western JT)") %>%
  # YUJA presence (eastern JT) — blue
  addPolygons(data = st_sf(geometry = yuja_map),
              fillColor = "#1565C0", fillOpacity = 0.22,
              color = "#1565C0", weight = 0.5,
              group = "YUJA presence (eastern JT)") %>%
  # Park boundaries
  addPolygons(data = jotr_bound,
              fillOpacity = 0, color = "#FF6F00", weight = 2,
              dashArray = "6,4", group = "JOTR boundary") %>%
  addPolygons(data = moja_bound,
              fillOpacity = 0, color = "#6A1B9A", weight = 2,
              dashArray = "6,4", group = "Mojave NP boundary") %>%
  # Candidate fires coloured by year
  addPolygons(data = fires_map,
              fillColor   = ~pal(year),
              fillOpacity = 1,
              color       = ~pal(year),
              weight      = 2.5,
              label       = ~sprintf("%s (%d) — %d ac | JT: %s | burns: %s",
                                     fire_name, year, round(acres),
                                     paste0(round(pct_jt * 100), "%"),
                                     other_burn_years) %>%
                              lapply(htmltools::HTML),
              group = "Candidate fires") %>%
  addLayersControl(
    overlayGroups = c("YUBR presence (western JT)", "YUJA presence (eastern JT)",
                      "JOTR boundary", "Mojave NP boundary", "Candidate fires"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(
    position = "bottomleft",
    colors   = c("#2E7D32", "#1565C0", "#FF6F00", "#6A1B9A"),
    labels   = c("YUBR presence (western JT)", "YUJA presence (eastern JT)",
                 "JOTR boundary", "Mojave NP boundary"),
    title    = "Layers", opacity = 0.7
  ) %>%
  addLegend(
    pal       = pal,
    values    = fires_map$year,
    labFormat = labelFormat(big.mark = ""),   # removes comma in year (2,012 → 2012)
    title     = sprintf("Candidate fires<br><small>%d–%d · ≥60%% JT · ≥%d ac · n=%d</small>",
                        YEAR_MIN, YEAR_MAX, MIN_ACRES, nrow(fires_map)),
    position  = "bottomright"
  )

map_candidates
