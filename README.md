# Joshua Tree Fire Recovery — Plot Sampling

Field plot sampling design for a study of Joshua tree (*Yucca brevifolia*) mortality and post-fire recovery across wildfires in the Mojave Desert (California and Nevada), 1984–2024.

## Study design

Two cohorts of fires:

- **Recent fires (2011–2024)** — 60 × 60 m border pairs (inside/outside fire perimeter) plus interior plots, sampled from fires with confirmed Joshua tree presence. Pre- and post-fire satellite imagery is available for all plots.
- **Historical fires (1984–1994)** — border pairs only. Pre-fire imagery unavailable; the unburned outside plot at the fire boundary serves as a proxy for pre-fire conditions, and plot locations were selected manually from Google Earth imagery.

Each border pair consists of a matched inside plot (burned) and outside plot (unburned reference) placed at the fire perimeter. Pairs are matched for burn history, land cover, and Joshua tree presence. See [methods_sampling.md](methods_sampling.md) for full placement rules, exclusion criteria, and fire-specific exceptions.

## Repository structure

```
scripts/        R scripts (numbered in run order)
data/sites/     Hand-placed KML markers for fires with patchy JT distribution
output/
  recent/       Sampling outputs for recent fires
  historical/   Sampling outputs for historical fires
  collect_earth/ Collect Earth project bundle and reference CSVs
```

## Scripts

| Script | Description |
|--------|-------------|
| `00_utils.R` | Shared constants, packages, helper functions |
| `01_recent_perimeters.R` | Filter and validate recent fire perimeters |
| `02_recent_sites.R` | Place border pairs and interior plots for recent fires |
| `03_recent_maps.R` | Diagnostic maps of recent fire sampling |
| `04_recent_kmz.R` | Export KMZ for Google Earth field review |
| `05_recent_annotate.R` | Add pre/post-fire imagery dates and site notes |
| `06_historical_perimeters.R` | Filter and validate historical fire perimeters |
| `07_historical_sites.R` | Place border pairs for historical fires |
| `08_historical_maps.R` | Diagnostic maps of historical fire sampling |
| `09_historical_annotate.R` | Add imagery dates and site notes for historical fires |
| `10_historical_kmz.R` | Export KMZ for Google Earth field review |
| `11_export_boundaries.R` | Export confirmed fire perimeters (GeoJSON + shapefile) |
| `12_collect_earth_export.R` | Build Collect Earth `.cep` bundle for visual census |

## Key outputs

| File | Description |
|------|-------------|
| `output/recent/recent_sites_coordinates.csv` | All recent plot locations with metadata |
| `output/historical/historical_sites_coordinates.csv` | All historical plot locations |
| `output/recent/recent_sites.kmz` | Google Earth visualization — recent fires |
| `output/historical/historical_sites.kmz` | Google Earth visualization — historical fires |
| `output/collect_earth/jt_fire_recovery_en_*.cep` | Collect Survey Designer export (source for script 12) |
| `output/collect_earth/jt_fire_recovery.cep` | Patched Collect Earth project (import into CE desktop) |
| `output/collect_earth/placemark.csv` | Plot list for CE upload |
| `output/collect_earth/fire_imagery_dates.csv` | Per-fire imagery date reference |

## Data collection

Field data (Joshua tree counts, shrub cover) is collected visually from satellite imagery using [Collect Earth](https://www.openforis.org/tools/collect-earth.html) + Google Earth Pro. Each reviewer imports `jt_fire_recovery.cep`, reviews imagery at each plot, and exports their data as CSV from Collect Earth's Data Management tab. 

## Source data (not tracked — download separately)

| Dataset | Source | Path |
|---------|--------|------|
| CAL FIRE fire perimeters | [CAL FIRE FRAP](https://www.fire.ca.gov/what-we-do/fire-resource-assessment-program/fire-perimeters) | `data/fire_boundaries/fire25_1.gdb` |
| MTBS fire perimeters | [MTBS](https://www.mtbs.gov/direct-download) | `data/fire_boundaries/mtbs_perimeter_data/` |
| Nevada Wildland Fire History | BLM National Fire Perimeters (FPER), Bureau of Land Management | `data/fire_boundaries/Nevada_Wildland_Fire_History_*/` |
| Joshua Tree NP boundary | NPS | `data/fire_boundaries/JoshuaTree/` |
| Mojave NP boundary | NPS | `data/fire_boundaries/Mojave/` |
| LANDFIRE EVT 2025 | [LANDFIRE](https://landfire.gov/) | `data/vegetation_nps/LF2025_EVT_CONUS/` |
| Joshua tree presence (YUBR/YUJA) | Esque et al. | `data/Esque_distribution/` |


