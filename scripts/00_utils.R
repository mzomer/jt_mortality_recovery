# ─────────────────────────────────────────────────────────────────────
# 00_utils.R — Shared setup, paths, parameters, and helper functions
# ─────────────────────────────────────────────────────────────────────

library(sf)
library(terra)
library(dplyr)
library(here)
library(magrittr)
library(leaflet)
library(htmlwidgets)
library(shiny)
library(bslib)
library(DT)
library(stringr)
library(purrr)


sf_use_s2(FALSE)

# Paths ----
RAW        <- here("data")
OUT_RECENT <- here("output", "recent")
OUT_HIST   <- here("output", "historical")
dir.create(OUT_RECENT, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_HIST,   recursive = TRUE, showWarnings = FALSE)

# Coordinate systems ----
CRS_UTM <- "EPSG:32611"   # UTM Zone 11N — metres
CRS_WGS <- "EPSG:4326"   # WGS84 lat/lon

# Fire selection parameters ----
YEAR_MIN   <- 2011
YEAR_MAX   <- 2024
MIN_ACRES  <- 40
MIN_JT_PCT <- 0.60

# Plot geometry ----
PLOT_W    <- 60
PLOT_H    <- 60
HALF_DIAG <- sqrt((PLOT_W / 2)^2 + (PLOT_H / 2)^2)
MARGIN    <- HALF_DIAG + 5   # ~47 m inward/outward buffer from perimeter

# EVT exclusion codes (developed, urban, roads, rock, cliff, playa, water) ----
EVT_EXCLUDE <- c(
  7292,
  7295,
  7296, 7297, 7298, 7299, 7300,
  7910:7914,
  7930:7934,
  9008,
  9033,
  9147,
  9151,
  9153,
  9213,
  9246
)

# Helper functions ----

# Normalise fire label: lowercase, non-alphanumeric → underscore, trim leading/trailing underscores
# e.g. "COLES FLAT_2020" → "coles_flat_2020"
norm_label <- function(x) gsub("(^_|_$)", "", gsub("[^a-z0-9]+", "_", tolower(x)))

load_jt_presence <- function() {
  yubr <- st_read(file.path(RAW, "Esque_distribution/YUBR_Presence_Cells/YUBR_Presence_Cells.shp"),
                  quiet = TRUE)
  yuja <- st_read(file.path(RAW, "Esque_distribution/YUJA_Presence_Cells/YUJA_Presence_Cells.shp"),
                  quiet = TRUE)
  bind_rows(yubr, yuja) %>%
    st_transform(CRS_UTM) %>%
    st_union() %>%
    st_make_valid() %>%
    st_transform(CRS_WGS)
}

# PLOT_SIZE × PLOT_SIZE square (sfc, UTM) centred on xy = c(x, y)
make_box <- function(xy) {
  xy <- as.numeric(xy); h <- PLOT_W / 2
  st_sfc(st_polygon(list(rbind(
    c(xy[1] - h, xy[2] - h), c(xy[1] + h, xy[2] - h),
    c(xy[1] + h, xy[2] + h), c(xy[1] - h, xy[2] + h),
    c(xy[1] - h, xy[2] - h)
  ))), crs = CRS_UTM)
}

# TRUE if box centroid is on suitable (non-excluded) EVT land cover
evt_suitable <- function(box, evt) {
  xy  <- st_coordinates(st_centroid(box))[, 1:2, drop = FALSE]
  v   <- terra::extract(evt, xy)
  val <- v[1, ncol(v)]   # last column is always the value (works across terra versions)
  is.na(val) || !(val %in% EVT_EXCLUDE)
}

# Short spatial predicates returning plain logicals
fully_inside <- function(a, b) lengths(st_within(a, b)) > 0
touches      <- function(a, b) lengths(st_intersects(a, b)) > 0

# Force a (possibly GEOMETRYCOLLECTION) difference result to clean polygons
as_poly <- function(g) {
  g <- st_make_valid(g)
  if (any(st_geometry_type(g) == "GEOMETRYCOLLECTION"))
    g <- st_collection_extract(g, "POLYGON")
  st_make_valid(g)
}

pct_overlap <- function(a, b) {
  if (!inherits(a, "sfc")) a <- st_sfc(a, crs = st_crs(b))
  int <- suppressWarnings(st_intersection(a, b))
  if (length(int) == 0 || all(st_is_empty(int))) return(0)
  as.numeric(st_area(int)) / as.numeric(st_area(a))
}
