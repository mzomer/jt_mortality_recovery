# ─────────────────────────────────────────────────────────────────────
# 07_historical_sites.R — Paired plot placement for confirmed
#                         historical fires
# ─────────────────────────────────────────────────────────────────────
# Requires output from 06_historical_perimeters.R:
#   output/historical/historical_fire_perimeters_candidates.gpkg
#   output/historical/all_fires_utm.rds
#
# Output: output/historical/historical_confirmed_fires.kmz    (for GEP marker placement)
#         output/historical/historical_sampled_sites.gpkg
#         output/historical/historical_sites_coordinates.csv
# ─────────────────────────────────────────────────────────────────────

source(here::here("scripts", "00_utils.R"))

# Parameters ----
N_HIST_PAIRS <- 10     # max border pairs per fire
MAX_PAIR_SEP <- 4 * (sqrt(30^2 + 30^2) + 5)   # ~190 m
SEED         <- 42
MARKERS_KML  <- here("data", "sites", "historical_confirmed_fires_markers.kml")

# Confirmed fires — edit after GEP review of candidates KMZ ----
CONFIRMED_HIST_FIRES <- c(
  "joshua_1989",
  "blackbrush_1987",
  "mary_1984",
  "beacon_1993",
  "bunker_1993",
  "joshua_1994"
)

# Load confirmed candidates ----
perims_hist <- st_read(file.path(OUT_HIST, "historical_fire_perimeters_candidates.gpkg"),
                       quiet = TRUE) %>%
  mutate(label = norm_label(label)) %>%
  filter(label %in% CONFIRMED_HIST_FIRES) %>%
  distinct(label, .keep_all = TRUE) %>%
  st_transform(CRS_UTM)

message(sprintf("%d / %d confirmed fires found in candidates",
                nrow(perims_hist), length(CONFIRMED_HIST_FIRES)))
missing <- setdiff(CONFIRMED_HIST_FIRES, perims_hist$label)
if (length(missing) > 0)
  warning("Not found in candidates GPKG: ", paste(missing, collapse = ", "))

# Save confirmed KMZ for Google Earth Pro marker placement ----
kml_tmp  <- tempfile(fileext = ".kml")
kmz_path <- file.path(OUT_HIST, "historical_confirmed_fires.kmz")

perims_hist %>%
  st_transform(CRS_WGS) %>%
  mutate(Name        = label,
         Description = sprintf("%s (%d) — %d ac | JT: %s%%",
                               fire_name, year, round(acres),
                               round(pct_jt * 100))) %>%
  select(Name, Description) %>%
  st_write(kml_tmp, driver = "KML", delete_dsn = FALSE, quiet = TRUE)
zip(zipfile = kmz_path, files = kml_tmp, flags = "-j")
message(sprintf("Saved → output/historical/historical_confirmed_fires.kmz (%d fires)", nrow(perims_hist)))

# Load markers — point placemarks only (KML also contains fire perimeter polygons) ----
markers_all  <- st_read(MARKERS_KML, quiet = TRUE) %>% st_transform(CRS_UTM)
markers_hist <- markers_all[as.character(st_geometry_type(markers_all)) == "POINT", ]
message(sprintf("%d point markers loaded from KML", nrow(markers_hist)))

# Load supporting data ----
all_fires_utm    <- readRDS(file.path(OUT_HIST, "all_fires_utm.rds"))
jt_presence_utm  <- load_jt_presence() %>% st_transform(CRS_UTM)

evt_utm_path <- here("data", "vegetation_nps", "LF2025_EVT_CONUS", "Tif", "LF2025_EVT_jt_utm.tif")
if (!file.exists(evt_utm_path)) {
  message("Projecting EVT raster to UTM (first run only, ~1-2 min)...")
  evt_utm <- project(
    rast(here("data", "vegetation_nps", "LF2025_EVT_CONUS", "Tif", "LF2025_EVT_jt_extent.tif")),
    CRS_UTM, method = "near"
  )
  writeRaster(evt_utm, evt_utm_path, overwrite = TRUE)
} else {
  evt_utm <- rast(evt_utm_path)
}

# Helper functions ----

reburn_mask_hist <- function(fire_geom, fire_year) {
  fire_sfc <- st_sfc(fire_geom, crs = CRS_UTM)
  hits <- all_fires_utm[all_fires_utm$year > fire_year & touches(all_fires_utm, fire_sfc), ]
  if (nrow(hits) == 0) return(NULL)
  st_union(hits) %>% st_make_valid()
}

nudge_box_hist <- function(xy, existing, fire_sfc) {
  b      <- make_box(xy)
  ex_sfc <- if (length(existing) > 0) do.call(c, existing) else NULL
  ok_pos <- function(bx) fully_inside(bx, fire_sfc) && (is.null(ex_sfc) || !touches(bx, ex_sfc))
  if (ok_pos(b)) return(b)
  if (!is.null(ex_sfc) && touches(b, ex_sfc)) {
    conflicts <- existing[vapply(existing, function(e) lengths(st_intersects(b, e)) > 0, logical(1))]
    ctrs <- st_coordinates(st_centroid(do.call(c, conflicts)))
    dx <- xy[1] - mean(ctrs[, 1]); dy <- xy[2] - mean(ctrs[, 2])
  } else {
    bnd <- st_cast(st_boundary(fire_sfc), "MULTILINESTRING")
    np  <- st_nearest_points(st_sfc(st_point(xy), crs = CRS_UTM), bnd)
    pp  <- st_coordinates(np)[2, ]
    dx  <- xy[1] - pp[1]; dy <- xy[2] - pp[2]
  }
  d <- sqrt(dx^2 + dy^2)
  if (d == 0) { dx <- 1; dy <- 0; d <- 1 }
  for (step in seq(5, PLOT_W, by = 5)) {
    b2 <- make_box(c(xy[1] + dx/d * step, xy[2] + dy/d * step))
    if (ok_pos(b2)) return(b2)
  }
  NULL
}

box_to_record_hist <- function(box_sfc, site_id, region, pair_id, type,
                                fire_label, fire_year, fire_name, fire_date) {
  box_wgs <- st_transform(box_sfc, CRS_WGS)
  bb  <- st_bbox(box_wgs)
  ctr <- st_coordinates(st_transform(st_centroid(box_sfc), CRS_WGS))
  list(
    site_id    = site_id,              pair_id   = pair_id,
    fire_label = fire_label,           fire_year = fire_year,
    fire_name  = trimws(fire_name),    fire_date = as.character(fire_date),
    type       = type,                 region    = region,
    lat        = round(ctr[2], 7),     lon       = round(ctr[1], 7),
    min_lon    = round(bb["xmin"], 7), min_lat   = round(bb["ymin"], 7),
    max_lon    = round(bb["xmax"], 7), max_lat   = round(bb["ymax"], 7),
    geometry   = box_sfc[[1]]
  )
}

process_hist_fire <- function(fire_row) {
  label     <- fire_row$label
  fire_geom <- st_geometry(fire_row)[[1]]
  fire_sfc  <- st_sfc(fire_geom, crs = CRS_UTM)
  rb        <- reburn_mask_hist(fire_geom, fire_row$year)
  perim_line <- st_cast(st_boundary(fire_sfc), "MULTILINESTRING")

  # Assign markers to this fire by spatial containment
  # Outside markers use a 1 km search buffer — larger than MAX_PAIR_SEP — because
  # outside markers may be placed further from the perimeter than inside ones.
  in_fire   <- st_within(markers_hist, fire_sfc, sparse = FALSE)[, 1]
  near_buf  <- st_buffer(fire_sfc, 1000)
  near      <- lengths(st_intersects(markers_hist, near_buf)) > 0
  m_in      <- markers_hist[in_fire, ]
  m_out     <- markers_hist[!in_fire & near, ]

  if (nrow(m_in) == 0) {
    fire_bb <- st_bbox(fire_sfc)
    message(sprintf("  %s: no inside markers found (near_buf caught %d outside markers | fire bbox: %.4f,%.4f – %.4f,%.4f)",
                    label, sum(near),
                    fire_bb["xmin"], fire_bb["ymin"], fire_bb["xmax"], fire_bb["ymax"]))
    return(list())
  }
  if (nrow(m_out) == 0) {
    message(sprintf("  %s: no outside markers found", label)); return(list())
  }

  # Place inside boxes — nudge inward from perimeter if marker lands on edge
  iboxes          <- list()
  n_in_nudge      <- 0L
  n_in_reburn     <- 0L
  n_in_evt        <- 0L
  for (k in seq_len(nrow(m_in))) {
    xy <- st_coordinates(m_in[k, ])[1, 1:2]
    b  <- nudge_box_hist(xy, iboxes, fire_sfc)
    if (is.null(b))                        { n_in_nudge  <- n_in_nudge  + 1L; next }
    if (!is.null(rb) && touches(b, rb))    { n_in_reburn <- n_in_reburn + 1L; next }
    if (!evt_suitable(b, evt_utm))         { n_in_evt    <- n_in_evt    + 1L; next }
    iboxes <- c(iboxes, list(b))
  }

  # Place outside boxes at marker locations; nudge away from fire if box overlaps perimeter
  oboxes          <- list()
  n_out_nudge     <- 0L
  n_out_reburn    <- 0L
  n_out_evt       <- 0L
  n_out_jt        <- 0L
  for (k in seq_len(nrow(m_out))) {
    xy <- st_coordinates(m_out[k, ])[1, 1:2]
    b  <- make_box(xy)
    if (touches(b, fire_sfc)) {
      bnd <- st_cast(st_boundary(fire_sfc), "MULTILINESTRING")
      np  <- st_nearest_points(st_sfc(st_point(xy), crs = CRS_UTM), bnd)
      pp  <- st_coordinates(np)[2, ]
      dx  <- xy[1] - pp[1]; dy <- xy[2] - pp[2]
      d   <- sqrt(dx^2 + dy^2)
      if (d == 0) { dx <- 1; dy <- 0; d <- 1 }
      nudged <- FALSE
      for (step in seq(5, PLOT_W, by = 5)) {
        b2 <- make_box(c(xy[1] + dx/d * step, xy[2] + dy/d * step))
        if (!touches(b2, fire_sfc)) { b <- b2; nudged <- TRUE; break }
      }
      if (!nudged) { n_out_nudge  <- n_out_nudge  + 1L; next }
    }
    if (!is.null(rb) && touches(b, rb))   { n_out_reburn <- n_out_reburn + 1L; next }
    if (!evt_suitable(b, evt_utm))        { n_out_evt    <- n_out_evt    + 1L; next }
    if (!touches(b, jt_presence_utm))     { n_out_jt     <- n_out_jt    + 1L; next }
    oboxes <- c(oboxes, list(b))
  }

  if (length(iboxes) == 0 || length(oboxes) == 0) {
    message(sprintf("  %s: no valid boxes — in(nudge=%d reburn=%d evt=%d) out(nudge=%d reburn=%d evt=%d jt=%d)",
                    label, n_in_nudge, n_in_reburn, n_in_evt,
                    n_out_nudge, n_out_reburn, n_out_evt, n_out_jt))
    return(list())
  }

  # Pair inside-outside by nearest centroid distance
  in_ctrs  <- do.call(rbind, lapply(iboxes, function(b) st_coordinates(st_centroid(b))[1, 1:2]))
  out_ctrs <- do.call(rbind, lapply(oboxes, function(b) st_coordinates(st_centroid(b))[1, 1:2]))

  # Sort inside boxes closest-to-perimeter first
  dists_to_perim <- vapply(iboxes, function(b)
    as.numeric(st_distance(st_centroid(b), perim_line)), numeric(1))
  iord <- order(dists_to_perim)

  pairs      <- list()
  used_out   <- rep(FALSE, length(oboxes))
  n_too_far  <- 0L
  n_burn_hist <- 0L

  other_fires <- all_fires_utm[all_fires_utm$year != fire_row$year &
                                 touches(all_fires_utm, fire_sfc), ]

  for (j in iord) {
    b_in  <- iboxes[[j]]
    xy_in <- in_ctrs[j, ]

    avail <- which(!used_out)
    if (length(avail) == 0) break
    dists   <- sqrt((out_ctrs[avail, 1] - xy_in[1])^2 + (out_ctrs[avail, 2] - xy_in[2])^2)
    nearest <- avail[which.min(dists)]
    sep     <- dists[which.min(dists)]

    b_out <- oboxes[[nearest]]

    if (sep > MAX_PAIR_SEP) {
      # Try nudging outside box toward the inside box to close the gap
      xy_out <- out_ctrs[nearest, ]
      dx <- xy_in[1] - xy_out[1]; dy <- xy_in[2] - xy_out[2]
      d  <- sqrt(dx^2 + dy^2)
      nudged <- FALSE
      if (d > 0) {
        ex_out_sfc <- if (length(pairs) > 0)
          do.call(c, lapply(pairs, function(p) p$b_out)) else NULL
        for (step in seq(5, d, by = 5)) {
          b2 <- make_box(c(xy_out[1] + dx/d * step, xy_out[2] + dy/d * step))
          if (touches(b2, fire_sfc)) break
          if (!is.null(ex_out_sfc) && touches(b2, ex_out_sfc)) next
          ctr2 <- st_coordinates(st_centroid(b2))[1, 1:2]
          if (sqrt((ctr2[1] - xy_in[1])^2 + (ctr2[2] - xy_in[2])^2) <= MAX_PAIR_SEP) {
            b_out <- b2; nudged <- TRUE; break
          }
        }
      }
      if (!nudged) { n_too_far <- n_too_far + 1L; next }
    }

    b_in_sfc  <- st_sfc(b_in[[1]],  crs = CRS_UTM)
    b_out_sfc <- st_sfc(b_out[[1]], crs = CRS_UTM)

    if (nrow(other_fires) > 0) {
      in_hits  <- vapply(other_fires$year, function(yr)
        touches(b_in_sfc,  st_sfc(st_union(st_geometry(other_fires[other_fires$year == yr, ])), crs = CRS_UTM)),
        logical(1))
      out_hits <- vapply(other_fires$year, function(yr)
        touches(b_out_sfc, st_sfc(st_union(st_geometry(other_fires[other_fires$year == yr, ])), crs = CRS_UTM)),
        logical(1))
      if (!all(in_hits == out_hits)) { n_burn_hist <- n_burn_hist + 1L; next }
    }

    used_out[nearest] <- TRUE
    pairs <- c(pairs, list(list(b_in = b_in, b_out = b_out)))
  }

  n_in_fail  <- n_in_nudge  + n_in_reburn  + n_in_evt
  n_out_fail <- n_out_nudge + n_out_reburn + n_out_evt + n_out_jt
  message(sprintf("%-28s  %d pairs  markers(in=%d out=%d)  in_fail=%d(nudge=%d reburn=%d evt=%d)  out_fail=%d(nudge=%d reburn=%d evt=%d jt=%d)  too_far=%d burn_hist=%d",
                  label, length(pairs),
                  nrow(m_in), nrow(m_out),
                  n_in_fail,  n_in_nudge,  n_in_reburn,  n_in_evt,
                  n_out_fail, n_out_nudge, n_out_reburn, n_out_evt, n_out_jt,
                  n_too_far, n_burn_hist))

  records <- list()
  for (j in seq_along(pairs)) {
    pid <- sprintf("%s_pair%02d", label, j)
    records <- c(records, list(
      box_to_record_hist(pairs[[j]]$b_in,  sprintf("%s_inside_%02d",  label, j),
                         "inside", pid, "border_pair",
                         label, fire_row$year, fire_row$fire_name, fire_row$fire_date),
      box_to_record_hist(pairs[[j]]$b_out, sprintf("%s_outside_%02d", label, j),
                         "outside", pid, "border_pair",
                         label, fire_row$year, fire_row$fire_name, fire_row$fire_date)
    ))
  }
  records
}

# Place plots ----
all_hist_sites <- unlist(
  lapply(seq_len(nrow(perims_hist)), function(i) process_hist_fire(perims_hist[i, ])),
  recursive = FALSE
)

# Build output data frame ----
df_cols <- c("site_id", "pair_id", "fire_label", "fire_year", "fire_name", "fire_date",
             "type", "region", "lat", "lon", "min_lon", "min_lat", "max_lon", "max_lat")
sites_df  <- dplyr::bind_rows(lapply(all_hist_sites, function(x) as.data.frame(x[df_cols])))
site_geoms <- st_sfc(lapply(all_hist_sites, `[[`, "geometry"), crs = CRS_UTM)

# Per-site burn history
sites_df$site_other_burn_years <- vapply(seq_len(nrow(sites_df)), function(i) {
  box  <- site_geoms[i]
  fyear <- sites_df$fire_year[i]
  hits  <- all_fires_utm[all_fires_utm$year != fyear &
                           lengths(st_intersects(all_fires_utm, box)) > 0, ]
  if (nrow(hits) == 0) "" else paste(sort(unique(hits$year)), collapse = ";")
}, character(1))

# Save outputs ----
sites_gdf <- st_sf(sites_df, geometry = site_geoms) %>% st_transform(CRS_WGS)

st_write(sites_gdf,
         file.path(OUT_HIST, "historical_sampled_sites.gpkg"),
         layer = "sampled_sites", delete_layer = TRUE, quiet = TRUE)
write.csv(sites_df,
          file.path(OUT_HIST, "historical_sites_coordinates.csv"),
          row.names = FALSE)

message(sprintf("Saved %d sites (%d pairs) across %d historical fires",
                nrow(sites_df), nrow(sites_df) / 2,
                length(unique(sites_df$fire_label))))
