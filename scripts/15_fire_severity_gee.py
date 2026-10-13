#!/usr/bin/env python3
# -----------------------------------------------------------------------
# 15_fire_severity_gee.py — burn severity (dNBR, RBR) from Sentinel-2 and
# Landsat via Google Earth Engine.
#
# For each confirmed fire: samples ~2000 random pixels across the whole
# perimeter (for the full severity distribution), and extracts the mean
# value at each of the project's sampling sites (for per-site severity),
# from whichever of Sentinel-2 / Landsat actually has usable imagery for
# that fire's era. Historical fires (pre-2015) and quail_2012 therefore
# only ever produce Landsat rows -- Sentinel-2 did not exist yet.
#
# dNBR = NBR_pre - NBR_post; RBR = dNBR / (NBR_pre + 1.001) (Parks et al.
# 2014) -- RBR is used alongside plain dNBR because it performs better
# than dNBR in low-productivity/arid systems like Mojave desert shrubland.
#
# Post-fire scene must be dated 5-20 days after the fire date -- the 5-day
# buffer guards against catching the fire still smoldering, since the only
# date we have per fire is a single recorded date, not a confirmed out/
# containment date. Falls back to a wider window only if nothing usable is
# found in that span.
#
# Setup (first time only):
#   python3 -m venv .venv
#   source .venv/bin/activate
#   pip install -r requirements.txt
#
# Run:
#   source .venv/bin/activate
#   python scripts/15_fire_severity_gee.py                  # all 22 fires
#   python scripts/15_fire_severity_gee.py --fire cahuila_2017   # one fire, for testing
#
# Reads:
#   output/fire_boundaries_confirmed_shp/fire_boundaries_confirmed.shp
#   output/recent/recent_sites_coordinates.csv
#   output/historical/historical_sites_coordinates.csv
#
# Writes:
#   output/fire_severity/fire_severity_pixels.csv  (whole-perimeter samples)
#   output/fire_severity/site_severity.csv          (per-site means)
# -----------------------------------------------------------------------

import argparse
import json
from datetime import date, datetime, timedelta, timezone
from pathlib import Path

import ee
import geopandas as gpd
import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
PERIMETERS_PATH = ROOT / "output" / "fire_boundaries_confirmed_shp" / "fire_boundaries_confirmed.shp"
RECENT_SITES_PATH = ROOT / "output" / "recent" / "recent_sites_coordinates.csv"
HISTORICAL_SITES_PATH = ROOT / "output" / "historical" / "historical_sites_coordinates.csv"
OUT_DIR = ROOT / "output" / "fire_severity"

EE_CREDENTIALS_PATH = Path.home() / ".config" / "earthengine" / "credentials"

# Scene-selection windows, as day offsets from the fire date.
PRE_WINDOW = (-365, -16)
POST_WINDOW_INITIAL = (5, 20)
POST_WINDOW_EXTENDED = (16, 40)  # fallback only, if nothing clear in the initial window

CLOUD_THRESHOLDS = [20, 50, 100]  # percent; tried in order, loosest last
MIN_POST_SCENES = 3  # below this, also try the extended window and keep whichever composites more scenes

NUM_PIXELS_PER_FIRE = 2000
SAMPLE_SEED = 42

S2_START = date(2015, 6, 23)  # first usable Sentinel-2 SR data


# ---- Earth Engine setup ----

def init_ee():
    with open(EE_CREDENTIALS_PATH) as f:
        project = json.load(f)["project"]
    ee.Initialize(project=project)


# ---- band prep: cloud-mask, scale to reflectance, rename to common NIR/SWIR2 ----

def _mask_s2(image):
    scl = image.select("SCL")
    # 3 = cloud shadow, 8/9 = cloud medium/high probability, 10 = cirrus
    mask = scl.neq(3).And(scl.neq(8)).And(scl.neq(9)).And(scl.neq(10))
    return image.updateMask(mask)


def _mask_landsat(image):
    qa = image.select("QA_PIXEL")
    # Collection 2 QA_PIXEL bits: 1 dilated cloud, 2 cirrus, 3 cloud, 4 cloud shadow
    mask = (qa.bitwiseAnd(1 << 1).eq(0)
            .And(qa.bitwiseAnd(1 << 2).eq(0))
            .And(qa.bitwiseAnd(1 << 3).eq(0))
            .And(qa.bitwiseAnd(1 << 4).eq(0)))
    return image.updateMask(mask)


def _prep_s2(image):
    masked = _mask_s2(image)
    out = masked.select(["B8", "B12"]).divide(10000).rename(["NIR", "SWIR2"])
    return (out
            .set("CLOUD_PCT", image.get("CLOUDY_PIXEL_PERCENTAGE"))
            .set("SENSOR", "Sentinel-2")
            .set("SATELLITE", "Sentinel-2")
            .set("system:time_start", image.get("system:time_start")))


def _prep_landsat(nir_band):
    # returns a per-image prep function closed over which raw band is "NIR"
    # (SR_B5 for OLI/L8-L9, SR_B4 for TM/ETM+/L4-L5-L7) -- SWIR2 is SR_B7 for all.
    def prep(image):
        masked = _mask_landsat(image)
        out = (masked.select([nir_band, "SR_B7"])
               .multiply(0.0000275).add(-0.2)
               .rename(["NIR", "SWIR2"]))
        return (out
                .set("CLOUD_PCT", image.get("CLOUD_COVER"))
                .set("SENSOR", "Landsat")
                .set("SATELLITE", image.get("SPACECRAFT_ID"))
                .set("system:time_start", image.get("system:time_start")))
    return prep


def s2_collection(geom, start, end):
    if end < S2_START:
        return None
    start = max(start, S2_START)
    return (ee.ImageCollection("COPERNICUS/S2_SR_HARMONIZED")
            .filterBounds(geom)
            .filterDate(start.isoformat(), end.isoformat())
            .map(_prep_s2))


def landsat_collection(geom, start, end):
    oli = (ee.ImageCollection("LANDSAT/LC09/C02/T1_L2")
           .merge(ee.ImageCollection("LANDSAT/LC08/C02/T1_L2")))
    tm = (ee.ImageCollection("LANDSAT/LE07/C02/T1_L2")
          .merge(ee.ImageCollection("LANDSAT/LT05/C02/T1_L2"))
          .merge(ee.ImageCollection("LANDSAT/LT04/C02/T1_L2")))
    col = (oli.filterBounds(geom).filterDate(start.isoformat(), end.isoformat()).map(_prep_landsat("SR_B5"))
           .merge(tm.filterBounds(geom).filterDate(start.isoformat(), end.isoformat()).map(_prep_landsat("SR_B4"))))
    return col


SENSORS = {
    "Sentinel-2": {"collection_fn": s2_collection, "scale": 10},
    "Landsat": {"collection_fn": landsat_collection, "scale": 30},
}


# ---- scene selection ----

def _window(center, start_offset, end_offset):
    d0 = center + timedelta(days=start_offset)
    d1 = center + timedelta(days=end_offset)
    return (d0, d1) if d0 <= d1 else (d1, d0)


def _ms_to_date_str(ms):
    return datetime.fromtimestamp(ms / 1000, tz=timezone.utc).date().isoformat()


def build_mosaic(collection):
    """Returns (mosaic_image, [date_str, ...], n_scenes), or (None, [], 0) if
    the collection is empty.

    A single "clearest scene" is not enough: one scene's footprint can
    intersect a fire's perimeter without covering every part of it -- and
    sometimes that gap lands exactly where the sampling sites are, even
    while the scene still contributes plenty of valid pixels elsewhere in
    the perimeter. Compositing every scene that passes the cloud threshold
    (clearest pixel wins wherever scenes overlap) fills those gaps.
    """
    if collection is None:
        return None, [], 0
    if collection.size().getInfo() == 0:
        return None, [], 0
    for threshold in CLOUD_THRESHOLDS:
        filtered = collection.filter(ee.Filter.lte("CLOUD_PCT", threshold))
        n = filtered.size().getInfo()
        if n > 0:
            # sort descending so the clearest (lowest CLOUD_PCT) image is
            # last -- mosaic() takes each pixel from the last image in the
            # collection that has a valid (unmasked) value there.
            mosaic = filtered.sort("CLOUD_PCT", False).mosaic()
            times = filtered.aggregate_array("system:time_start").getInfo()
            dates = sorted({_ms_to_date_str(t) for t in times})
            return mosaic, dates, n
    # CLOUD_PCT missing/null on every image in the collection: composite them all anyway
    times = collection.aggregate_array("system:time_start").getInfo()
    dates = sorted({_ms_to_date_str(t) for t in times})
    return collection.mosaic(), dates, collection.size().getInfo()


def select_pre_post(geom, fire_date, collection_fn):
    pre_start, pre_end = _window(fire_date, *PRE_WINDOW)
    pre_img, pre_dates, pre_n = build_mosaic(collection_fn(geom, pre_start, pre_end))
    if pre_img is None:
        return None

    post_start, post_end = _window(fire_date, *POST_WINDOW_INITIAL)
    post_img, post_dates, post_n = build_mosaic(collection_fn(geom, post_start, post_end))
    if post_img is None or post_n < MIN_POST_SCENES:
        # too few scenes to composite over cloud/sensor gaps -- union with
        # the extended window (not just switch to it) so scenes from BOTH
        # windows composite together; comparing scene counts alone can tie
        # (e.g. 2 scenes in each window) while still missing different
        # parts of the perimeter, so always prefer the union once triggered.
        ext_start, ext_end = _window(fire_date, *POST_WINDOW_EXTENDED)
        union_start, union_end = min(post_start, ext_start), max(post_end, ext_end)
        union_img, union_dates, union_n = build_mosaic(collection_fn(geom, union_start, union_end))
        if union_img is not None:
            post_img, post_dates, post_n = union_img, union_dates, union_n
    if post_img is None:
        return None

    return {
        "pre_img": pre_img, "pre_dates": pre_dates, "pre_n": pre_n,
        "post_img": post_img, "post_dates": post_dates, "post_n": post_n,
    }


# ---- severity computation ----

def severity_stack(pre_img, post_img):
    nbr_pre = pre_img.normalizedDifference(["NIR", "SWIR2"]).rename("nbr_pre")
    nbr_post = post_img.normalizedDifference(["NIR", "SWIR2"]).rename("nbr_post")
    dnbr = nbr_pre.subtract(nbr_post).rename("dnbr")
    rbr = dnbr.divide(nbr_pre.add(1.001)).rename("rbr")
    return nbr_pre.addBands([nbr_post, dnbr, rbr])


# ---- per-fire processing ----

def process_fire(fire_label, fire_name, fire_year, fire_date, geom, sites_for_fire):
    pixel_rows = []
    site_rows = []

    for sensor_name, cfg in SENSORS.items():
        selection = select_pre_post(geom, fire_date, cfg["collection_fn"])
        if selection is None:
            print(f"  [{sensor_name}] no usable pre/post imagery for {fire_label} -- skipped")
            continue

        pre_dates = ";".join(selection["pre_dates"])
        post_dates = ";".join(selection["post_dates"])
        print(f"  [{sensor_name}] pre={pre_dates} ({selection['pre_n']} scene(s)) "
              f"post={post_dates} ({selection['post_n']} scene(s))")

        stack = severity_stack(selection["pre_img"], selection["post_img"])
        scale = cfg["scale"]

        # whole-perimeter random pixel sample
        sample = stack.sample(
            region=geom, scale=scale, numPixels=NUM_PIXELS_PER_FIRE,
            seed=SAMPLE_SEED, geometries=True,
        ).getInfo()
        for feat in sample["features"]:
            lon, lat = feat["geometry"]["coordinates"]
            props = feat["properties"]
            pixel_rows.append({
                "fire_label": fire_label, "fire_name": fire_name, "fire_year": fire_year,
                "sensor": sensor_name,
                "pre_dates": pre_dates, "pre_n_scenes": selection["pre_n"],
                "post_dates": post_dates, "post_n_scenes": selection["post_n"],
                "lon": lon, "lat": lat,
                "nbr_pre": props.get("nbr_pre"), "nbr_post": props.get("nbr_post"),
                "dnbr": props.get("dnbr"), "rbr": props.get("rbr"),
            })

        # per-site means, batched into one reduceRegions call
        if sites_for_fire:
            site_features = [
                ee.Feature(
                    ee.Geometry.Rectangle([s["min_lon"], s["min_lat"], s["max_lon"], s["max_lat"]]),
                    {"site_id": s["site_id"]},
                )
                for s in sites_for_fire
            ]
            reduced = stack.reduceRegions(
                collection=ee.FeatureCollection(site_features),
                reducer=ee.Reducer.mean(), scale=scale,
            ).getInfo()
            for feat in reduced["features"]:
                props = feat["properties"]
                site_rows.append({
                    "site_id": props.get("site_id"), "fire_label": fire_label,
                    "sensor": sensor_name,
                    "pre_dates": pre_dates, "pre_n_scenes": selection["pre_n"],
                    "post_dates": post_dates, "post_n_scenes": selection["post_n"],
                    "nbr_pre": props.get("nbr_pre"), "nbr_post": props.get("nbr_post"),
                    "dnbr": props.get("dnbr"), "rbr": props.get("rbr"),
                })

    return pixel_rows, site_rows


def fire_geometry(row):
    return ee.Geometry(json.loads(gpd.GeoSeries([row.geometry]).to_json())["features"][0]["geometry"])


def reconstruct_fire_label(fire_name, fire_year):
    return f"{fire_name.strip().lower().replace(' ', '_')}_{int(fire_year)}"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--fire", help="only process this fire_label (for testing), e.g. cahuila_2017")
    args = parser.parse_args()

    init_ee()

    perimeters = gpd.read_file(PERIMETERS_PATH)
    sites = pd.concat(
        [pd.read_csv(RECENT_SITES_PATH), pd.read_csv(HISTORICAL_SITES_PATH)],
        ignore_index=True,
    )

    OUT_DIR.mkdir(parents=True, exist_ok=True)

    all_pixel_rows = []
    all_site_rows = []

    for _, fire in perimeters.iterrows():
        fire_label = reconstruct_fire_label(fire["fire_name"], fire["year"])
        if args.fire and fire_label != args.fire:
            continue

        fire_date = pd.to_datetime(fire["fire_date"]).date()
        sites_for_fire = sites[sites["fire_label"] == fire_label].to_dict("records")
        if not sites_for_fire:
            print(f"WARNING: no sites matched fire_label '{fire_label}' "
                  f"(from shapefile fire_name='{fire['fire_name']}', year={fire['year']}) -- check the join")

        print(f"{fire_label} (fire_date={fire_date}, {len(sites_for_fire)} sites)")
        geom = fire_geometry(fire)

        pixel_rows, site_rows = process_fire(
            fire_label, fire["fire_name"], int(fire["year"]), fire_date, geom, sites_for_fire,
        )
        all_pixel_rows.extend(pixel_rows)
        all_site_rows.extend(site_rows)

    pd.DataFrame(all_pixel_rows).to_csv(OUT_DIR / "fire_severity_pixels.csv", index=False)
    pd.DataFrame(all_site_rows).to_csv(OUT_DIR / "site_severity.csv", index=False)
    print(f"\nWrote {len(all_pixel_rows)} pixel rows and {len(all_site_rows)} site rows to {OUT_DIR}")


if __name__ == "__main__":
    main()
