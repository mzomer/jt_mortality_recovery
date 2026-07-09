# ─────────────────────────────────────────────────────────────────────
# 10_historical_kmz.R — Build KMZ from historical fire sampled sites
# ─────────────────────────────────────────────────────────────────────
# Produces a KMZ with fire perimeter polygons, coloured site squares
# (red = inside, blue = outside), label badges, and three fly-to tours
# for Google Earth Pro fieldwork planning.
# Input:  output/historical/historical_sites_coordinates.csv
#         output/historical/historical_fire_perimeters_candidates.gpkg
# Output: output/historical/historical_sites.kmz
# ─────────────────────────────────────────────────────────────────────

source(here::here("scripts", "00_utils.R"))

PAUSE_FIRE <- 8    # seconds per fire in Tour 1
PAUSE_SITE <- 10   # seconds per site in Tours 2 & 3

# KML colours (AABBGGRR) ----
COL_INSIDE    <- "ff0000ff"
COL_OUTSIDE   <- "ffff0000"
COL_FIRE_LINE <- "ff0088ff"
STYLE_COLORS  <- c(inside = COL_INSIDE, outside = COL_OUTSIDE)

# Load inputs ----
sites_df <- read.csv(file.path(OUT_HIST, "historical_sites_coordinates.csv"),
                     stringsAsFactors = FALSE)

perims <- st_read(file.path(OUT_HIST, "historical_fire_perimeters_candidates.gpkg"),
                  quiet = TRUE) %>%
  mutate(label = norm_label(label)) %>%
  filter(label %in% unique(sites_df$fire_label)) %>%
  filter(!duplicated(label)) %>%
  st_transform(CRS_WGS)

# PNG label badge using base R graphics -----
make_label_png <- function(text) {
  tmp <- tempfile(fileext = ".png")
  w <- max(300, nchar(text) * 13 + 40)
  h <- 52
  png(tmp, width = w, height = h, bg = "white", res = 96)
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
  plot.new()
  plot.window(xlim = c(0, w), ylim = c(0, h))
  rect(2, 2, w - 2, h - 2, col = "white", border = "#303030", lwd = 3)
  text(w / 2, h / 2, text, cex = 1.4, col = "black", family = "sans", font = 2)
  dev.off()
  readBin(tmp, "raw", file.info(tmp)$size)
}

# KML helpers -----
fire_range_m <- function(acres) {
  side <- sqrt(acres * 4046.856)
  max(500, min(side * 1.5 / (2 * tan(pi / 6)), 80000))
}

lookat_kml <- function(lon, lat, range_m, today) {
  sprintf(paste0("<LookAt>",
    "<longitude>%.7f</longitude><latitude>%.7f</latitude>",
    "<altitude>0</altitude><range>%.0f</range>",
    "<tilt>0</tilt><heading>0</heading>",
    "<altitudeMode>relativeToGround</altitudeMode>",
    "<gx:TimeStamp><when>%s</when></gx:TimeStamp>",
    "</LookAt>"), lon, lat, range_m, today)
}

ring_coords <- function(r) {
  sprintf("%.7f,%.7f,0 %.7f,%.7f,0 %.7f,%.7f,0 %.7f,%.7f,0 %.7f,%.7f,0",
          r$min_lon, r$min_lat, r$max_lon, r$min_lat,
          r$max_lon, r$max_lat, r$min_lon, r$max_lat,
          r$min_lon, r$min_lat)
}

geom_to_kml_polygons <- function(geom) {
  polys <- tryCatch(st_cast(geom, "POLYGON"), error = function(e) geom)
  paste(sapply(seq_along(polys), function(i) {
    coords <- st_coordinates(polys[i])[, 1:2]
    coord_str <- paste(apply(coords, 1, function(xy)
      sprintf("%.7f,%.7f,0", xy[1], xy[2])), collapse = " ")
    sprintf(paste0("<Polygon><tessellate>1</tessellate>",
      "<outerBoundaryIs><LinearRing>",
      "<coordinates>%s</coordinates>",
      "</LinearRing></outerBoundaryIs></Polygon>"), coord_str)
  }), collapse = "")
}

# Build KMZ -----
build_kmz <- function(sites_df, perims, out_path) {
  today  <- format(Sys.Date(), "%Y-%m-%d")
  images <- list()
  L      <- character(0)
  ap     <- function(...) L <<- c(L, paste0(...))

  ap('<?xml version="1.0" encoding="UTF-8"?>')
  ap('<kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2">')
  ap('<Document>')
  ap('  <name>JT Historical Fires 1984 - 1994</name><open>1</open>')

  # Styles
  for (region in names(STYLE_COLORS)) {
    col <- STYLE_COLORS[[region]]
    ap('  <Style id="', region, '_poly">',
       '<LineStyle><color>', col, '</color><width>8</width></LineStyle>',
       '<PolyStyle><fill>0</fill></PolyStyle></Style>')
    ap('  <Style id="', region, '_icon">',
       '<IconStyle><scale>0</scale></IconStyle>',
       '<LabelStyle><scale>0</scale></LabelStyle></Style>')
  }
  ap('  <Style id="fire_perim">',
     '<LineStyle><color>', COL_FIRE_LINE, '</color><width>3</width></LineStyle>',
     '<PolyStyle><fill>0</fill><outline>1</outline></PolyStyle></Style>')

  # Fire perimeters folder
  ap('  <Folder id="fires_folder"><name>Fire perimeters</name><open>1</open><visibility>1</visibility>')
  for (i in seq_len(nrow(perims))) {
    r    <- perims[i, ]
    geom <- st_geometry(r)
    cx   <- st_coordinates(st_centroid(geom))[1]
    cy   <- st_coordinates(st_centroid(geom))[2]
    rng  <- fire_range_m(r$acres)

    ap('    <Placemark id="fire_', r$label, '">')
    ap('      <name>', trimws(r$fire_name), ' (', as.character(r$fire_date), ')</name>')
    ap('      <description>', round(r$acres), ' ac</description>')
    ap('      <styleUrl>#fire_perim</styleUrl>')
    ap('      ', lookat_kml(cx, cy, rng, today))
    if (inherits(geom[[1]], "MULTIPOLYGON") || inherits(geom[[1]], "POLYGON")) {
      ap('      <MultiGeometry>', geom_to_kml_polygons(geom), '</MultiGeometry>')
    }
    ap('    </Placemark>')
  }
  ap('  </Folder>')

  site_tour_pts <- list()

  emit_site <- function(s) {
    region <- s$region
    pm_id  <- gsub("[^a-zA-Z0-9_]", "_", s$site_id)
    clat   <- (s$min_lat + s$max_lat) / 2
    clon   <- (s$min_lon + s$max_lon) / 2
    box_h  <- s$max_lat - s$min_lat
    pt_lon <- s$max_lon
    pt_lat <- s$max_lat + box_h * 0.06

    pair_str <- sub(paste0(s$fire_label, "_"), "", s$site_id)
    text     <- paste0(trimws(s$fire_name), " · ", s$fire_year,
                       " · ", pair_str)
    img_name <- paste0("images/", pm_id, ".png")
    images[[img_name]] <<- make_label_png(text)

    ap('    <Placemark><name></name>')
    ap('      <styleUrl>#', region, '_poly</styleUrl>')
    ap('      ', lookat_kml(clon, clat, 108, today))
    ap('      <Polygon><tessellate>1</tessellate><outerBoundaryIs><LinearRing>',
       '<coordinates>', ring_coords(s), '</coordinates>',
       '</LinearRing></outerBoundaryIs></Polygon>')
    ap('    </Placemark>')

    ap('    <Placemark id="', pm_id, '"><name></name>')
    ap('      <styleUrl>#', region, '_icon</styleUrl>')
    ap('      <Style><IconStyle><scale>2.0</scale>',
       '<Icon><href>', img_name, '</href></Icon>',
       '<hotSpot x="0.5" y="0" xunits="fraction" yunits="fraction"/>',
       '</IconStyle><LabelStyle><scale>0</scale></LabelStyle></Style>')
    ap('      <Point><coordinates>', sprintf("%.7f,%.7f,0", pt_lon, pt_lat), '</coordinates></Point>')
    ap('    </Placemark>')

    site_tour_pts <<- c(site_tour_pts,
                        list(list(lat = clat, lon = clon, id = pm_id,
                                  fire   = s$fire_label,
                                  pair   = if (!is.na(s$pair_id)) s$pair_id else s$site_id,
                                  region = region)))
  }

  # Border pairs folder
  ap('  <Folder><name>Border pairs</name><open>1</open><visibility>1</visibility>')
  for (i in seq_len(nrow(sites_df))) emit_site(sites_df[i, ])
  ap('  </Folder>')

  # Sort tour sites by fire then pair then region
  site_tour_pts <- site_tour_pts[order(
    sapply(site_tour_pts, `[[`, "fire"),
    sapply(site_tour_pts, `[[`, "pair"),
    sapply(site_tour_pts, `[[`, "region")
  )]
  all_pm_ids <- sapply(site_tour_pts, `[[`, "id")

  flyto_playlist <- function(show_labels) {
    vis <- if (show_labels) "1" else "0"
    for (pm_id in all_pm_ids) {
      ap('      <gx:AnimatedUpdate><gx:duration>0</gx:duration><Update>',
         '<Change><Placemark targetId="', pm_id, '">',
         '<visibility>', vis, '</visibility>',
         '</Placemark></Change></Update></gx:AnimatedUpdate>')
    }
    for (pt in site_tour_pts) {
      ap('      <gx:FlyTo><gx:duration>3.5</gx:duration><gx:flyToMode>smooth</gx:flyToMode>')
      ap('        ', lookat_kml(pt$lon, pt$lat, 108, today))
      ap('      </gx:FlyTo>')
      ap('      <gx:Wait><gx:duration>', PAUSE_SITE, '</gx:duration></gx:Wait>')
    }
  }

  # Tour 1: fire perimeters
  ap('  <gx:Tour><name>Tour 1 — Fire perimeters (by year)</name><gx:Playlist>')
  for (pm_id in all_pm_ids) {
    ap('      <gx:AnimatedUpdate><gx:duration>0</gx:duration><Update>',
       '<Change><Placemark targetId="', pm_id, '">',
       '<visibility>0</visibility>',
       '</Placemark></Change></Update></gx:AnimatedUpdate>')
  }
  for (i in seq_len(nrow(perims))) {
    r    <- perims[i, ]
    geom <- st_geometry(r)
    cx   <- st_coordinates(st_centroid(geom))[1]
    cy   <- st_coordinates(st_centroid(geom))[2]
    rng  <- fire_range_m(r$acres)
    ap('      <gx:FlyTo><gx:duration>4.0</gx:duration><gx:flyToMode>smooth</gx:flyToMode>')
    ap('        ', lookat_kml(cx, cy, rng, today))
    ap('      </gx:FlyTo>')
    ap('      <gx:Wait><gx:duration>', PAUSE_FIRE, '</gx:duration></gx:Wait>')
  }
  ap('  </gx:Playlist></gx:Tour>')

  # Tour 2: paired sites with labels
  ap('  <gx:Tour><name>Tour 2 — Paired sites (with labels)</name><gx:Playlist>')
  flyto_playlist(show_labels = TRUE)
  ap('  </gx:Playlist></gx:Tour>')

  # Tour 3: paired sites no labels
  ap('  <gx:Tour><name>Tour 3 — Paired sites (no labels)</name><gx:Playlist>')
  flyto_playlist(show_labels = FALSE)
  ap('  </gx:Playlist></gx:Tour>')

  ap('</Document></kml>')

  # Write KMZ (zip of doc.kml + images/)
  kml_str <- paste(L, collapse = "\n")
  tmp_dir  <- tempfile()
  dir.create(file.path(tmp_dir, "images"), recursive = TRUE)
  writeLines(kml_str, file.path(tmp_dir, "doc.kml"), useBytes = FALSE)
  for (nm in names(images)) {
    writeBin(images[[nm]], file.path(tmp_dir, nm))
  }

  old_wd <- setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)
  zip(out_path, files = c("doc.kml", paste0("images/", basename(names(images)))),
      flags = "-r9X")
  setwd(old_wd)
  unlink(tmp_dir, recursive = TRUE)

  message(sprintf("Saved KMZ → %s  (%d sites, %d fires)",
                  out_path, nrow(sites_df), nrow(perims)))
}

build_kmz(
  sites_df,
  perims,
  out_path = file.path(OUT_HIST, "historical_sites.kmz")
)
