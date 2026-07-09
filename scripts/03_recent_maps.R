# ─────────────────────────────────────────────────────────────────────
# 03_recent_maps.R — Interactive per-fire site maps
# ─────────────────────────────────────────────────────────────────────
# Navigate maps with the Viewer back/forward arrows.
# Requires outputs from 02_recent_sites.R (run that first).
# Input:  output/site_selection/sampled_sites.gpkg
#         output/site_selection/fire_perimeters_candidates.gpkg
#         output/site_selection/all_fires_utm.rds
# ─────────────────────────────────────────────────────────────────────

source(here::here("scripts", "00_utils.R"))

RELAX_PAST_REBURN <- c("quail_2012", "johnson_2024")

sites_sf     <- st_read(file.path(OUT_RECENT, "sampled_sites.gpkg"), quiet = TRUE) %>%
  st_transform(CRS_UTM)

perims       <- st_read(file.path(OUT_RECENT, "fire_perimeters_candidates.gpkg"), quiet = TRUE) %>%
  mutate(label = norm_label(label)) %>%
  filter(label %in% unique(sites_sf$fire_label)) %>%
  filter(!duplicated(label)) %>%
  st_transform(CRS_UTM)

all_fires_utm <- readRDS(file.path(OUT_RECENT, "all_fires_utm.rds"))

pal_region <- colorFactor(c("#1b7837", "#762a83"),
                          levels = c("inside", "outside"))

for (lbl in perims$label) {
  fire_row  <- perims[perims$label == lbl, ]
  fire_sfc  <- st_sfc(st_geometry(fire_row)[[1]], crs = CRS_UTM)
  fire_year <- fire_row$year
  fsites    <- sites_sf[sites_sf$fire_label == lbl, ]

  rb_hits <- all_fires_utm[all_fires_utm$year != fire_year & touches(all_fires_utm, fire_sfc), ]
  if (lbl %in% RELAX_PAST_REBURN) rb_hits <- rb_hits[rb_hits$year > fire_year, ]

  ref_sfc <- if (nrow(fsites) > 0) st_transform(fsites, CRS_WGS) else
             st_transform(st_sf(geometry = fire_sfc), CRS_WGS)
  bb <- st_bbox(ref_sfc)

  m <- leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]) %>%
    addControl(paste0("<b>", lbl, "</b>"), position = "topright") %>%
    addPolygons(data = st_transform(st_sf(geometry = fire_sfc), CRS_WGS),
                color = "black", weight = 2, fillOpacity = 0)

  if (nrow(rb_hits) > 0) {
    rb_wgs <- st_transform(suppressWarnings(st_intersection(rb_hits, st_buffer(fire_sfc, 500))), CRS_WGS)
    pal_yr <- colorFactor(colorRampPalette(c("#fee5d9", "#a50f15"))(length(unique(rb_wgs$year))),
                          domain = rb_wgs$year)
    m <- m %>%
      addPolygons(data = rb_wgs, fillColor = ~pal_yr(year), fillOpacity = 0.5,
                  color = NA, weight = 0, popup = ~as.character(year)) %>%
      addLegend("bottomleft", pal = pal_yr, values = rb_wgs$year, title = "Reburn year")
  }

  if (nrow(fsites) > 0)
    m <- m %>%
      addPolygons(data = st_transform(fsites, CRS_WGS),
                  color = ~pal_region(region), weight = 2,
                  fillColor = ~pal_region(region), fillOpacity = 0.25,
                  popup = ~site_id) %>%
      addLegend("bottomright", pal = pal_region, values = fsites$region, title = "Site type")

  print(m)
}
