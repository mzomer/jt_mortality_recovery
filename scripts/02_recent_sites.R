# ─────────────────────────────────────────────────────────────────────
# 02_recent_sites.R — Place 60×60 m field plots within confirmed fires
# ─────────────────────────────────────────────────────────────────────
# Each fire gets two plot types:
#   Border pairs (N_PAIRS): matched inside/outside the fire perimeter,
#     placed MARGIN metres from the perimeter edge so the full 60×60 m
#     box fits cleanly on each side. Outside plots are in JT presence
#     cells. Neither box can overlap ground burned in a different fire.
#   Inside plots: KML-marker locations (MARKER_FIRES) or random draws
#     from the valid inside area. Used for mortality/recovery assessment.
# ─────────────────────────────────────────────────────────────────────

source(here::here("scripts", "00_utils.R"))

# Parameters ----
N_PAIRS       <- 5     # border pairs per fire
N_RANDOM      <- 10    # random inside plots per fire (non-marker fires)
CANDIDATE_SPACING <- 25    # metres between candidate points along the shrunk perimeter
MAX_PAIR_SEP  <- 4 * MARGIN   # ~190 m max inside-outside separation
SEED          <- 42

CONFIRMED_FIRES <- c(
  "quail_2012", "cahuila_2017", "star_2019",
  "coles_flat_2020", "dome_2020", "ivanpah_2020", "nadeau_2020",
  "mojave_2021", "pond_2021",
  "sheep_2022", "elk_trail_2022",
  "york_2023", "geology_2023", "boulevard_2023",
  "cal_2024", "johnson_2024"
)

MARKER_FIRES <- c(
  "quail_2012", "cahuila_2017", "nadeau_2020", "sheep_2022",
  "boulevard_2023", "cal_2024", "johnson_2024",
  "elk_trail_2022", "coles_flat_2020"
)
# quail_2012: one past-fire record (1973, CalFire, unnamed) overlaps the perimeter
# johnson_2024: two past-fire records (1942, 1960, "Outside Origin #1") overlap the perimeter
# Both are pre-satellite era — vegetation fully recovered, no effect on imagery or standing dead
RELAX_PAST_REBURN <- c("quail_2012", "johnson_2024")

# Manual outside-box shifts (UTM metres, applied after placement).
# Use when a computed outside box lands in an obstacle not captured by the EVT centroid check.
# Format: list("site_id" = c(dx_m, dy_m))  positive x = east, positive y = north
MANUAL_BOX_SHIFTS <- list(
  "cahuila_2017_outside_01" = c(-10, -30),  # lava field on E; -30 S clears perimeter overlap
  "cahuila_2017_outside_02" = c(-30, 0)     # lava field on E side
)

# Reference coordinates: random inside plots for these fires are sorted by distance
# to the ref point so chosen sites cluster near a known accessible area
REF_COORDS <- list(
  york_2023       = c(lat = 35 + 19/60 + 5.59/3600,  lon = -(115 + 10/60 + 53.05/3600)),
  coles_flat_2020 = c(lat = 36 +  5/60 + 53.46/3600, lon = -(117 + 36/60 +  8.19/3600))
)

# Load data ----
perims <- st_read(file.path(OUT_RECENT, "fire_perimeters_candidates.gpkg"), quiet = TRUE) %>%
  mutate(label = norm_label(label)) %>%
  filter(label %in% CONFIRMED_FIRES) %>%
  distinct(label, .keep_all = TRUE) %>%   # drop duplicate cross-border perimeters
  st_transform(CRS_UTM)

# Cache the UTM-projected EVT raster — projection is slow; save once and reload
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

jt_presence <- load_jt_presence() %>% st_transform(CRS_UTM)

calfire_raw <- suppressWarnings(
  st_read(here("data", "fire_boundaries", "fire25_1.gdb"), layer = "firep25_1", quiet = TRUE)
)
nv_raw <- suppressWarnings(
  st_read(here("data", "fire_boundaries",
               "Nevada_Wildland_Fire_History_2313768665656203223",
               "Wildland_Fire_History.shp"), quiet = TRUE)
) %>% st_make_valid()

all_fires_utm <- bind_rows(
  calfire_raw[as.character(st_geometry_type(calfire_raw)) %in% c("POLYGON", "MULTIPOLYGON"), ] %>%
    st_transform(CRS_WGS) %>% st_set_geometry("geometry") %>%
    mutate(year = as.integer(YEAR_)) %>% filter(!is.na(year)) %>% select(year),
  nv_raw[as.character(st_geometry_type(nv_raw)) %in% c("POLYGON", "MULTIPOLYGON"), ] %>%
    st_transform(CRS_WGS) %>%
    mutate(year = as.integer(Fire_Year)) %>% filter(!is.na(year)) %>% select(year)
) %>% st_transform(CRS_UTM)

# Fire perimeters are named "FIRE NAME (YYYY-MM-DD)"; point markers never have parentheses
markers_raw <- st_read(here("data", "sites", "recent_confirmed_fires_markers.kml"), quiet = TRUE) %>%
  filter(!grepl("\\(", Name)) %>%
  st_transform(CRS_UTM) %>%
  mutate(label = norm_label(sub("_\\d+$", "", Name)))
markers <- markers_raw %>% filter(label %in% MARKER_FIRES) %>% select(label)

# Helper functions ----

# Union of all fire perimeters overlapping this fire that burned in a different year.
# relax_past = TRUE (quail, johnson): only exclude future reburns — their past records
# are from 1942–1973 (pre-satellite), too old to affect vegetation or imagery.
reburn_mask <- function(fire_geom, fire_year, relax_past = FALSE) {
  fire_sfc <- st_sfc(fire_geom, crs = CRS_UTM)
  hits <- all_fires_utm[touches(all_fires_utm, fire_sfc), ]
  hits <- if (relax_past)
    hits[hits$year > fire_year, ]
  else
    hits[hits$year != fire_year, ]
  if (nrow(hits) == 0) return(NULL)
  st_union(hits) %>% st_make_valid()
}

# Place a marker box at xy, nudging away from existing boxes and inward from the
# perimeter if needed. Returns NULL if no valid position found (caller drops it).
nudge_box <- function(xy, existing, fire_sfc) {
  b      <- make_box(xy)
  ex_sfc <- if (length(existing) > 0) do.call(c, existing) else NULL
  ok_pos <- function(bx) fully_inside(bx, fire_sfc) && (is.null(ex_sfc) || !touches(bx, ex_sfc))
  if (ok_pos(b)) return(b)
  # Nudge direction: away from conflicting boxes (overlap) or toward fire centroid (boundary)
  if (!is.null(ex_sfc) && touches(b, ex_sfc)) {
    conflicts <- existing[vapply(existing, function(e) lengths(st_intersects(b, e)) > 0, logical(1))]
    ctrs <- st_coordinates(st_centroid(do.call(c, conflicts)))
    dx <- xy[1] - mean(ctrs[, 1]); dy <- xy[2] - mean(ctrs[, 2])
  } else {
    # Nudge directly inward: away from the nearest boundary point.
    # Using centroid is wrong when the fire is elongated (e.g. quail's south centroid
    # points away from northern-lobe markers that just need a short inward push).
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

# Clip a geometry (sfg) to exclude reburned ground, then normalise topology.
# st_buffer(sfc, 0) is a GEOS trick to fix broken topology left by st_difference.
make_area <- function(geom_sfg, rb) {
  sfc <- st_sfc(geom_sfg, crs = CRS_UTM)
  if (!is.null(rb)) {
    d <- suppressWarnings(st_difference(sfc, rb))
    if (length(d) > 0 && !all(st_is_empty(d))) sfc <- as_poly(d)
  }
  st_buffer(sfc, 0)
}


# Random inside plot placement — pre-filter then place:
#   1. st_sample() draws points directly from the valid polygon (fast; no bbox rejection)
#   2. EVT checked in one vectorised terra::extract call across all candidates
#   3. if ref_xy given, candidates sorted nearest-first so sites cluster near access point
#   4. loop through survivors: accept boxes that fit fully inside and don't overlap each other
sample_random_plots <- function(valid_area, n, ref_xy = NULL, existing = list()) {
  pts <- st_sample(valid_area, size = max(n * 50L, 300L))
  if (length(pts) == 0) return(list())

  coords <- st_coordinates(pts)
  v      <- terra::extract(evt_utm, coords)
  vals   <- v[, ncol(v)]
  ok     <- is.na(vals) | !(vals %in% EVT_EXCLUDE)
  pts    <- pts[ok];  coords <- coords[ok, , drop = FALSE]
  if (length(pts) == 0) return(list())

  if (!is.null(ref_xy)) {
    d      <- (coords[, 1] - ref_xy[1])^2 + (coords[, 2] - ref_xy[2])^2
    ord    <- order(d)
    pts    <- pts[ord];  coords <- coords[ord, , drop = FALSE]
  }

  chosen <- existing
  for (i in seq_along(pts)) {
    if (length(chosen) >= length(existing) + n) break
    b <- make_box(coords[i, 1:2])
    if (!fully_inside(b, valid_area)) next
    if (!touches(b, jt_presence))     next
    if (length(chosen) > 0 && touches(b, do.call(c, chosen))) next
    chosen <- c(chosen, list(b))
  }
  if (length(chosen) > length(existing)) chosen[(length(existing) + 1):length(chosen)] else list()
}

# Border pair placement:
#   The fire perimeter is shrunk inward by MARGIN (~47 m = half box diagonal + 5 m).
#   Candidate points are placed every CANDIDATE_SPACING metres along this shrunk boundary.
#   Each candidate becomes the centre of the inside box — because the candidate is MARGIN inside
#   the real perimeter, the full 60×60 m box fits within it by construction.
#   The matching outside box is placed by projecting from the candidate to the nearest point
#   on the real perimeter, then stepping MARGIN beyond it — same logic, other side.
#   Note: we check boxes against the original fire_sfc, not valid_inside, because candidates
#   sit ON valid_inside's edge and a box centred there would straddle it (always fail).
#   Shared burn history: for every other-year fire touching the study fire perimeter,
#   the inside and outside boxes must have the same overlap status (both in or both out).
#   This ensures pairs have matched pre-fire burn history regardless of reburn complexity.
#   For RELAX_PAST_REBURN fires (quail, johnson), only FUTURE fires are checked — their
#   pre-satellite past fires are too old to affect imagery and are intentionally ignored.
sample_border_pairs <- function(fire_geom, valid_inside, n, rb, other_fires = NULL, ref_xy = NULL) {
  fire_sfc   <- st_sfc(fire_geom, crs = CRS_UTM)
  perim_line <- st_cast(st_sfc(st_boundary(fire_geom), crs = CRS_UTM), "LINESTRING")
  inner_ring <- tryCatch(st_cast(st_boundary(valid_inside), "LINESTRING"), error = function(e) NULL)
  if (is.null(inner_ring) || st_is_empty(inner_ring[[1]])) return(list())

  coords_in <- st_coordinates(st_line_sample(inner_ring, density = 1 / CANDIDATE_SPACING))[, 1:2]

  diag <- integer(7); names(diag) <- c("in_outside_perim","in_reburn","in_evt","out_in_fire","out_no_jt","out_evt","out_burn_hist")
  candidates <- list()
  for (k in seq_len(nrow(coords_in))) {
    cx_in <- coords_in[k, 1]
    cy_in <- coords_in[k, 2]
    b_in  <- make_box(c(cx_in, cy_in))

    if (!fully_inside(b_in, fire_sfc)) { diag["in_outside_perim"] <- diag["in_outside_perim"] + 1L; next }
    if (!is.null(rb) && touches(b_in, rb)) { diag["in_reburn"] <- diag["in_reburn"] + 1L; next }
    if (!evt_suitable(b_in, evt_utm))      { diag["in_evt"]    <- diag["in_evt"]    + 1L; next }

    nearest  <- st_nearest_points(st_sfc(st_point(c(cx_in, cy_in)), crs = CRS_UTM), perim_line)
    perim_pt <- st_coordinates(nearest)[2, ]
    dx   <- perim_pt[1] - cx_in
    dy   <- perim_pt[2] - cy_in
    dist <- sqrt(dx^2 + dy^2)
    if (dist == 0) next

    cx_out <- perim_pt[1] + (dx / dist) * MARGIN
    cy_out <- perim_pt[2] + (dy / dist) * MARGIN
    b_out  <- make_box(c(cx_out, cy_out))

    if (touches(b_out, fire_sfc))       { diag["out_in_fire"] <- diag["out_in_fire"] + 1L; next }
    if (!touches(b_out, jt_presence))   { diag["out_no_jt"]   <- diag["out_no_jt"]   + 1L; next }
    if (!evt_suitable(b_out, evt_utm))  { diag["out_evt"]      <- diag["out_evt"]      + 1L; next }
    if (!is.null(other_fires) && nrow(other_fires) > 0) {
      in_hits  <- lengths(st_intersects(other_fires, b_in))  > 0
      out_hits <- lengths(st_intersects(other_fires, b_out)) > 0
      if (!identical(in_hits, out_hits)) { diag["out_burn_hist"] <- diag["out_burn_hist"] + 1L; next }
    }

    sep <- sqrt((cx_in - cx_out)^2 + (cy_in - cy_out)^2)
    candidates <- c(candidates, list(list(b_in = b_in, b_out = b_out, sep = sep)))
  }

  # Drop candidates whose inside-outside separation exceeds the max allowed
  candidates <- candidates[vapply(candidates, `[[`, numeric(1), "sep") <= MAX_PAIR_SEP]

  # Pick pairs: sort by distance to ref point if provided, otherwise by separation
  if (!is.null(ref_xy)) {
    ref_dists <- vapply(candidates, function(cand) {
      xy <- st_coordinates(st_centroid(cand$b_in))[1, 1:2]
      (xy[1] - ref_xy[1])^2 + (xy[2] - ref_xy[2])^2
    }, numeric(1))
    candidates <- candidates[order(ref_dists)]
  } else {
    candidates <- candidates[order(vapply(candidates, `[[`, numeric(1), "sep"))]
  }
  chosen_in <- list(); chosen_out <- list(); pairs <- list()
  for (cand in candidates) {
    if (length(pairs) >= n) break
    if (length(chosen_in) > 0 &&
        (touches(cand$b_in,  do.call(c, chosen_in)) ||
         touches(cand$b_out, do.call(c, chosen_out)))) next
    chosen_in  <- c(chosen_in,  list(cand$b_in))
    chosen_out <- c(chosen_out, list(cand$b_out))
    pairs <- c(pairs, list(cand))
  }
  attr(pairs, "diag") <- if (length(pairs) < n)
    sprintf("  (%d candidates, %d valid): in_outside_perim=%d in_reburn=%d in_evt=%d | out_in_fire=%d out_no_jt=%d out_evt=%d out_burn_hist=%d | kept=%d",
            nrow(coords_in), length(candidates), diag[1], diag[2], diag[3], diag[4], diag[5], diag[6], diag[7], length(pairs))
  pairs
}

box_to_record <- function(box_sfc, site_id, region, pair_id, type,
                           fire_label, fire_year, fire_name, fire_date) {
  box_wgs <- st_transform(box_sfc, CRS_WGS)
  bb  <- st_bbox(box_wgs)
  ctr <- st_coordinates(st_centroid(box_wgs))
  list(
    site_id    = site_id,              pair_id   = pair_id,
    fire_label = fire_label,           fire_year = fire_year,
    fire_name  = trimws(fire_name),    fire_date = as.character(fire_date),
    region     = region,               type      = type,
    lat        = round(ctr[2], 7),     lon       = round(ctr[1], 7),
    min_lon    = round(bb["xmin"], 7), min_lat   = round(bb["ymin"], 7),
    max_lon    = round(bb["xmax"], 7), max_lat   = round(bb["ymax"], 7),
    geometry   = box_sfc[[1]]
  )
}

process_fire <- function(fire_row) {
  label     <- fire_row$label
  fire_geom <- st_geometry(fire_row)[[1]]
  rb        <- reburn_mask(fire_geom, fire_row$year, relax_past = label %in% RELAX_PAST_REBURN)

  inner_sfg    <- st_buffer(st_sfc(fire_geom, crs = CRS_UTM), -MARGIN)[[1]]
  valid_inside <- make_area(inner_sfg, rb)

  if (st_is_empty(valid_inside[[1]])) {
    message(label, ": valid inside is empty — skipped")
    return(list())
  }

  # Other-year fires that overlap this perimeter — checked for shared burn history in pairs.
  # For RELAX fires (quail, johnson): only future fires are checked; their pre-satellite
  # past fires (1973/1942/1960) are too old to affect imagery and are intentionally ignored.
  # For all other fires: every other-year fire is checked.
  {
    fire_sfc_tmp <- st_sfc(fire_geom, crs = CRS_UTM)
    h <- all_fires_utm[all_fires_utm$year != fire_row$year &
                         touches(all_fires_utm, fire_sfc_tmp), ]
    if (label %in% RELAX_PAST_REBURN) h <- h[h$year > fire_row$year, ]
    other_fires <- if (nrow(h) > 0) h else NULL
  }
  # Marker fires: JTs are concentrated where markers were placed, not evenly distributed.
  # Candidates scan the whole perimeter and find pairs in JT-free areas → skip for marker fires.
  ref_ll  <- REF_COORDS[[label]]
  ref_utm <- if (!is.null(ref_ll)) {
    st_coordinates(
      st_transform(st_sfc(st_point(c(ref_ll["lon"], ref_ll["lat"])), crs = CRS_WGS), CRS_UTM)
    )[1, ]
  } else NULL
  pairs <- if (label %in% MARKER_FIRES) list() else
    sample_border_pairs(fire_geom, valid_inside, N_PAIRS, rb, other_fires, ref_xy = ref_utm)

  ex_in <- unlist(lapply(pairs, function(p) list(p$b_in)), recursive = FALSE)

  if (label %in% MARKER_FIRES) {
    fm    <- markers[markers$label == label, ]
    ip    <- st_within(fm, st_sfc(fire_geom, crs = CRS_UTM), sparse = FALSE)[, 1]
    ip    <- !is.na(ip) & ip
    n_outside_perim <- sum(!ip)
    crd   <- st_coordinates(fm[ip, ])
    fire_sfc_tmp <- st_sfc(fire_geom, crs = CRS_UTM)
    placed <- ex_in
    iboxes <- list()
    n_nudge_fail <- 0L
    n_evt_fail   <- 0L
    for (j in seq_len(nrow(crd))) {
      b <- nudge_box(crd[j, 1:2], placed, fire_sfc_tmp)
      if (is.null(b))              { n_nudge_fail <- n_nudge_fail + 1L; next }
      if (!evt_suitable(b, evt_utm)) { n_evt_fail <- n_evt_fail   + 1L; next }
      placed <- c(placed, list(b))
      iboxes <- c(iboxes, list(b))
    }
    n_pre_rb <- length(iboxes)
    rb_fires <- if (!is.null(rb)) {
      fire_sfc_tmp2 <- st_sfc(fire_geom, crs = CRS_UTM)
      all_fires_utm[all_fires_utm$year != fire_row$year & touches(all_fires_utm, fire_sfc_tmp2), ]
    } else NULL
    if (!is.null(rb))
      iboxes <- iboxes[!vapply(iboxes, function(b) touches(b, rb), logical(1))]
    n_rb_drop <- n_pre_rb - length(iboxes)
    rb_yrs <- if (!is.null(rb_fires) && nrow(rb_fires) > 0)
      paste(sort(unique(rb_fires$year)), collapse = ", ") else "none"
    message(sprintf("  markers loaded=%d | outside_perim=%d nudge_fail=%d evt_fail=%d reburn_drop=%d (fires: %s) | placed=%d",
                    nrow(fm), n_outside_perim, n_nudge_fail, n_evt_fail, n_rb_drop, rb_yrs, length(iboxes)))

    # All marker fires: use the N_PAIRS closest markers to the perimeter as pair anchors.
    # Outside placement uses convex-hull direction (avoids concavity re-entry on jagged
    # perimeters). When the outside box overlaps a previous one, slide it tangentially
    # along the perimeter exit direction until it finds clear ground.
    if (length(iboxes) > 0) {
      perim_q   <- st_union(st_sfc(st_boundary(fire_geom), crs = CRS_UTM))
      hull_line <- st_boundary(st_convex_hull(fire_sfc_tmp))
      dists     <- vapply(iboxes, function(b)
        as.numeric(st_distance(st_centroid(b), perim_q)), numeric(1))
      cand_idx   <- order(dists)[seq_len(min(N_PAIRS, length(iboxes)))]
      chosen_out <- list()
      is_pair    <- rep(FALSE, length(iboxes))
      dq <- integer(6)
      names(dq) <- c("out_too_far", "out_in_fire", "out_no_jt", "out_evt", "out_burn_hist", "out_no_slide")
      for (j in cand_idx) {
        b_in <- iboxes[[j]]
        xy   <- st_coordinates(st_centroid(b_in))[1, 1:2]

        # Strategy: try the nearest perimeter point direction first (shortest separation).
        # If that puts the outside box back inside the fire (concave notch), fall back to
        # the convex hull direction which always exits cleanly.
        np_direct <- st_nearest_points(st_sfc(st_point(xy), crs = CRS_UTM), perim_q)
        pp_direct <- st_coordinates(np_direct)[2, ]
        dx_d <- xy[1] - pp_direct[1]; dy_d <- xy[2] - pp_direct[2]
        d_d  <- sqrt(dx_d^2 + dy_d^2)

        nh   <- st_nearest_points(st_sfc(st_point(xy), crs = CRS_UTM), hull_line)
        hp   <- st_coordinates(nh)[2, ]
        dh_x <- hp[1] - xy[1]; dh_y <- hp[2] - xy[2]
        dh   <- sqrt(dh_x^2 + dh_y^2); if (dh == 0) next
        far  <- st_sfc(st_point(c(xy[1] + dh_x/dh * 5000, xy[2] + dh_y/dh * 5000)), crs = CRS_UTM)
        near <- st_nearest_points(far, perim_q)
        pp_hull <- st_coordinates(near)[2, ]
        dx_h <- xy[1] - pp_hull[1]; dy_h <- xy[2] - pp_hull[2]
        d_h  <- sqrt(dx_h^2 + dy_h^2); if (d_h == 0) next

        # Pick direct direction if it doesn't re-enter fire and stays within MAX_PAIR_SEP;
        # otherwise use hull direction.
        b_out_direct <- if (d_d > 0 && d_d + MARGIN <= MAX_PAIR_SEP) {
          cx_d <- pp_direct[1] - dx_d/d_d * MARGIN
          cy_d <- pp_direct[2] - dy_d/d_d * MARGIN
          b_try <- make_box(c(cx_d, cy_d))
          if (!touches(b_try, fire_sfc_tmp)) b_try else NULL
        } else NULL

        if (!is.null(b_out_direct)) {
          pp <- pp_direct; dx <- dx_d; dy <- dy_d; d <- d_d
        } else {
          pp <- pp_hull;   dx <- dx_h; dy <- dy_h; d <- d_h
        }

        if (d + MARGIN > MAX_PAIR_SEP)     { dq["out_too_far"] <- dq["out_too_far"] + 1L; next }
        cx0 <- pp[1] - dx/d * MARGIN
        cy0 <- pp[2] - dy/d * MARGIN
        b_out <- make_box(c(cx0, cy0))
        if (touches(b_out, fire_sfc_tmp))  { dq["out_in_fire"] <- dq["out_in_fire"] + 1L; next }
        if (!touches(b_out, jt_presence))  { dq["out_no_jt"]   <- dq["out_no_jt"]   + 1L; next }
        if (!evt_suitable(b_out, evt_utm)) { dq["out_evt"]      <- dq["out_evt"]      + 1L; next }
        if (!is.null(other_fires) && nrow(other_fires) > 0) {
          in_hits  <- lengths(st_intersects(other_fires, b_in))  > 0
          out_hits <- lengths(st_intersects(other_fires, b_out)) > 0
          if (!identical(in_hits, out_hits)) {
            dq["out_burn_hist"] <- dq["out_burn_hist"] + 1L
            conflict_yrs <- other_fires$year[in_hits != out_hits]
            message(sprintf("    burn_hist mismatch: fire(s) from year(s) %s",
                            paste(sort(unique(conflict_yrs)), collapse = ", ")))
            next
          }
        }
        # Slide tangentially if this outside box overlaps a previous one
        if (length(chosen_out) > 0 && touches(b_out, do.call(c, chosen_out))) {
          tang <- c(-dy/d, dx/d)  # unit tangent to perimeter at exit point
          found <- FALSE
          for (step in seq(CANDIDATE_SPACING, 3L * PLOT_W, by = CANDIDATE_SPACING)) {
            for (sgn in c(1L, -1L)) {
              b_try <- make_box(c(cx0 + sgn * tang[1] * step, cy0 + sgn * tang[2] * step))
              if (!touches(b_try, fire_sfc_tmp) &&
                  touches(b_try, jt_presence) &&
                  evt_suitable(b_try, evt_utm) &&
                  !touches(b_try, do.call(c, chosen_out))) {
                b_out <- b_try; found <- TRUE; break
              }
            }
            if (found) break
          }
          if (!found) { dq["out_no_slide"] <- dq["out_no_slide"] + 1L; next }
        }
        pairs      <- c(pairs, list(list(b_in = b_in, b_out = b_out, sep = d + MARGIN)))
        chosen_out <- c(chosen_out, list(b_out))
        is_pair[j] <- TRUE
      }
      message(sprintf(
        "  marker anchors: %d tried | dist=[%.0f,%.0f]m | out_too_far=%d out_in_fire=%d out_no_jt=%d out_evt=%d out_burn_hist=%d out_no_slide=%d | added=%d",
        length(cand_idx), min(dists[cand_idx]), max(dists[cand_idx]),
        dq["out_too_far"], dq["out_in_fire"], dq["out_no_jt"], dq["out_evt"], dq["out_burn_hist"],
        dq["out_no_slide"], sum(is_pair)))
      iboxes <- iboxes[!is_pair]
    }
  } else {
    iboxes <- sample_random_plots(valid_inside, N_RANDOM, ref_xy = ref_utm, existing = ex_in)
  }

  message(sprintf("%-22s  %d pairs   %d inside", label, length(pairs), length(iboxes)))
  if (!is.null(attr(pairs, "diag"))) message(attr(pairs, "diag"))
  if (label %in% MARKER_FIRES) {
    if (nrow(fm) == 0)
      message("  → no KML markers found")
    else if (n_pre_rb == 0)
      message(sprintf("  → %d marker(s) loaded, none within perimeter", nrow(fm)))
    else if (length(iboxes) == 0)
      message(sprintf("  → %d marker(s) in perimeter, all removed by reburn mask", n_pre_rb))
  }

  records <- list()
  for (j in seq_along(pairs)) {
    pid <- sprintf("%s_pair%02d", label, j)
    records <- c(records, list(
      box_to_record(pairs[[j]]$b_in,  sprintf("%s_inside_%02d",  label, j), "inside",  pid, "border_pair",
                    label, fire_row$year, fire_row$fire_name, fire_row$fire_date),
      box_to_record(pairs[[j]]$b_out, sprintf("%s_outside_%02d", label, j), "outside", pid, "border_pair",
                    label, fire_row$year, fire_row$fire_name, fire_row$fire_date)
    ))
  }
  offset <- length(pairs)
  for (j in seq_along(iboxes)) {
    records <- c(records, list(
      box_to_record(iboxes[[j]], sprintf("%s_inside_%02d", label, offset + j),
                    "inside", NA_character_, "interior",
                    label, fire_row$year, fire_row$fire_name, fire_row$fire_date)
    ))
  }
  records
}

# Apply a UTM (dx, dy) shift to a site record, updating geometry + WGS84 coords.
apply_box_shift <- function(rec, dx, dy) {
  old_geom <- st_sfc(rec$geometry, crs = CRS_UTM)
  old_ctr  <- st_coordinates(st_centroid(old_geom))[1, 1:2]
  new_box  <- make_box(c(old_ctr[1] + dx, old_ctr[2] + dy))
  new_wgs  <- st_transform(new_box, CRS_WGS)
  bb  <- st_bbox(new_wgs)
  ctr <- st_coordinates(st_centroid(new_wgs))
  rec$geometry <- new_box[[1]]
  rec$lat     <- round(ctr[2], 7)
  rec$lon     <- round(ctr[1], 7)
  rec$min_lon <- round(bb["xmin"], 7)
  rec$min_lat <- round(bb["ymin"], 7)
  rec$max_lon <- round(bb["xmax"], 7)
  rec$max_lat <- round(bb["ymax"], 7)
  rec
}

# Place plots ----
set.seed(SEED)
all_sites <- unlist(
  lapply(seq_len(nrow(perims)), function(i) process_fire(perims[i, ])),
  recursive = FALSE
)

# Apply manual shifts
for (i in seq_along(all_sites)) {
  shift <- MANUAL_BOX_SHIFTS[[all_sites[[i]]$site_id]]
  if (!is.null(shift)) {
    all_sites[[i]] <- apply_box_shift(all_sites[[i]], shift[1], shift[2])
    message(sprintf("  manual shift applied: %s  dx=%.0f dy=%.0f m",
                    all_sites[[i]]$site_id, shift[1], shift[2]))
  }
}

# Interactive maps are in 03_recent_maps.R — run that script to view sites per fire.

# Save outputs ----
df_cols  <- c("site_id", "pair_id", "fire_label", "fire_year", "fire_name", "fire_date",
              "type", "region", "lat", "lon", "min_lon", "min_lat", "max_lon", "max_lat")
sites_df <- dplyr::bind_rows(lapply(all_sites, function(x) as.data.frame(x[df_cols])))

# Per-site burn history: years of OTHER fires whose perimeter intersects this plot box.
# All years included (no age filter) so the user can decide relevance.
site_geoms <- st_sfc(lapply(all_sites, `[[`, "geometry"), crs = CRS_UTM)
sites_df$site_other_burn_years <- vapply(seq_len(nrow(sites_df)), function(i) {
  box  <- site_geoms[i]
  fyear <- sites_df$fire_year[i]
  hits <- all_fires_utm[all_fires_utm$year != fyear &
                          lengths(st_intersects(all_fires_utm, box)) > 0, ]
  if (nrow(hits) == 0) "" else paste(sort(unique(hits$year)), collapse = ";")
}, character(1))


sites_gdf <- st_sf(sites_df,
                   geometry = site_geoms) %>%
  st_transform(CRS_WGS)


st_write(sites_gdf, file.path(OUT_RECENT, "sampled_sites.gpkg"),
         layer = "sampled_sites", delete_layer = TRUE, quiet = TRUE)
st_write(sites_gdf, file.path(OUT_RECENT, "shapefiles", "sampled_sites.shp"),
         delete_layer = TRUE, quiet = TRUE)
write.csv(sites_df, file.path(OUT_RECENT, "recent_sites_coordinates.csv"), row.names = FALSE)

#save intermediate file of all fire perimeters in UTM for use in 03_recent_maps.R (faster than reprojection)
saveRDS(all_fires_utm, file.path(OUT_RECENT, "all_fires_utm.rds"))
