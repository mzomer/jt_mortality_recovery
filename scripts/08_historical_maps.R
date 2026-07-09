# ─────────────────────────────────────────────────────────────────────
# 08_historical_maps.R — Interactive review maps for confirmed
#                        historical fires + sampled sites + reburns
# ─────────────────────────────────────────────────────────────────────
# Requires outputs from 06_historical_perimeters.R and 07_historical_sites.R:
#   output/historical/historical_fire_perimeters_candidates.gpkg
#   output/historical/all_fires_utm.rds
#   output/historical/historical_sampled_sites.gpkg
# ─────────────────────────────────────────────────────────────────────

source(here::here("scripts", "00_utils.R"))

CONFIRMED_HIST_FIRES <- c(
  "joshua_1989",
  "blackbrush_1987",
  "mary_1984",
  "beacon_1993",
  "bunker_1993",
  "joshua_1994"
)

# Load data ----
perims <- st_read(file.path(OUT_HIST, "historical_fire_perimeters_candidates.gpkg"), quiet = TRUE) %>%
  mutate(label = norm_label(label)) %>%
  filter(label %in% CONFIRMED_HIST_FIRES) %>%
  st_transform(CRS_UTM)

missing <- setdiff(CONFIRMED_HIST_FIRES, perims$label)
if (length(missing) > 0)
  warning("Not found in candidates GPKG: ", paste(missing, collapse = ", "))

all_fires_utm <- readRDS(file.path(OUT_HIST, "all_fires_utm.rds"))

sites_sf <- st_read(file.path(OUT_HIST, "historical_sampled_sites.gpkg"), quiet = TRUE) %>%
  st_transform(CRS_UTM)

pal_region <- colorFactor(c("#1b7837", "#762a83"), levels = c("inside", "outside"))

# Maps ----
for (i in seq_len(nrow(perims))) {
  fire_row  <- perims[i, ]
  lbl       <- fire_row$label
  fire_year <- fire_row$year
  fire_sfc  <- st_sfc(st_geometry(fire_row)[[1]], crs = CRS_UTM)
  fsites    <- sites_sf[sites_sf$fire_label == lbl, ]

  # Post-fire reburns that overlap this fire's footprint
  rb_hits <- all_fires_utm[
    all_fires_utm$year > fire_year &
      lengths(st_intersects(all_fires_utm, fire_sfc)) > 0, ]

  ref_sfc <- if (nrow(fsites) > 0) st_transform(fsites, CRS_WGS) else
             st_transform(st_sf(geometry = fire_sfc), CRS_WGS)
  bb <- st_bbox(ref_sfc)

  m <- leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]) %>%
    addControl(sprintf("<b>%s</b> — %d pairs", lbl, nrow(fsites) / 2L), position = "topright") %>%
    addPolygons(data = st_transform(st_sf(geometry = fire_sfc), CRS_WGS),
                color = "black", weight = 2, fillOpacity = 0)

  if (nrow(rb_hits) > 0) {
    rb_clip <- suppressWarnings(
      st_intersection(rb_hits, st_buffer(fire_sfc, 1000)) %>%
        st_transform(CRS_WGS)
    )
    if (nrow(rb_clip) > 0) {
      yrs    <- sort(unique(rb_clip$year))
      pal_rb <- colorFactor(
        colorRampPalette(c("#fee5d9", "#a50f15"))(length(yrs)),
        domain = yrs
      )
      m <- m %>%
        addPolygons(data = rb_clip,
                    fillColor = ~pal_rb(year), fillOpacity = 0.55,
                    color = NA, weight = 0,
                    popup = ~paste0("Reburn: ", year)) %>%
        addLegend("bottomleft", pal = pal_rb, values = rb_clip$year,
                  title = "Post-fire reburn", labFormat = labelFormat(big.mark = ""))
    }
  }

  if (nrow(fsites) > 0)
    m <- m %>%
      addPolygons(data = st_transform(fsites, CRS_WGS),
                  color = ~pal_region(region), weight = 2,
                  fillColor = ~pal_region(region), fillOpacity = 0.3,
                  popup = ~site_id) %>%
      addLegend("bottomright", pal = pal_region, values = fsites$region, title = "Site")

  print(m)
  message(sprintf("%s (%d): %d pairs placed | %d post-fire reburn years — %s",
                  lbl, fire_year, nrow(fsites) / 2L, nrow(rb_hits),
                  if (nrow(rb_hits) > 0) paste(sort(unique(rb_hits$year)), collapse = ", ")
                  else "none"))
}
