# Field Plot Sampling Methods — JT Fire Recovery Study

## Overview

Each confirmed fire receives two types of 60 × 60 m plots:

- **Border pairs** — matched inside/outside plot pairs placed at the fire perimeter, used to compare burned vs. unburned conditions at the edge.
- **Inside plots** — plots in the fire interior, used to assess mortality and recovery across the burned area.

---

## Plot geometry

All plots are 60 × 60 m squares (axis-aligned in UTM Zone 11N).  
A buffer margin of `MARGIN = √(30² + 30²) + 5 ≈ 47 m` is applied inward and outward from the fire perimeter so that the full plot fits cleanly on each side without straddling the boundary.

---

## Border pair placement

### Standard fires (no KML markers)

Candidate plot-center locations are sampled systematically every 25 m along the fire perimeter, set back ~47 m inward so the full inside plot fits within the burned area. For each candidate, a paired outside plot is placed ~47 m beyond the perimeter directly opposite. Candidate pairs are filtered by the exclusion criteria below. Up to 5 non-overlapping pairs are retained per fire, preferring pairs closest to a reference access coordinate when one is specified, otherwise preferring the shortest inside–outside separation. Pairs more than ~190 m apart (centre to centre) are discarded.

### Marker fires (KML markers present)

For fires where Joshua tree distribution is patchy, Google Earth imagery review identified confirmed JT locations and KML markers were placed there. The systematic candidate-point approach is bypassed for these fires because it samples the whole perimeter and can land in JT-free areas.

Instead:
1. Each marker defines the centre of a 60 × 60 m inside plot (shifted slightly inward if it falls on the perimeter).
2. The 5 marker plots closest to the perimeter are selected as pair candidates.
3. The outside plot is placed ~47 m beyond the fire perimeter, directly away from the inside plot.
4. If an outside plot overlaps a previously placed outside plot, it is shifted along the perimeter until a clear position is found.
5. The same exclusion criteria and 190 m separation limit apply.

---

## Inside plot placement

### Marker fires
Marker plots not used as pair insides become inside plots. No random sampling is added.

### Non-marker fires
10 inside plots are drawn randomly from the valid interior area (fire perimeter minus any prior-burn mask). If a reference coordinate is set, plots are sorted by distance to that point so they cluster near the accessible area.

---

## Exclusion criteria

| Check | Applies to | Rule |
|---|---|---|
| **Within fire boundary** | Inside plots | Plot must fall entirely within the fire perimeter |
| **Prior burn history** | Inside plots | Plot must not overlap ground burned in a different fire year (see exceptions below) |
| **Land cover** | Inside and outside plots | Plot centroid must not fall on developed, urban, road, rock, cliff, playa, or water EVT classes |
| **JT presence** | Inside and outside plots | Plot must overlap at least one Joshua tree presence cell (YUBR or YUJA). Fire selection required ≥60% perimeter overlap with JT presence, but that does not guarantee any specific plot lands on JT. For marker fires, inside plots are exempt — the imagery marker itself confirms JT presence. |
| **Matched burn history** | Outside plots (pairs) | For every other-year fire overlapping the study perimeter, the inside and outside plot must have the same overlap status — ensuring pairs have matched pre-fire burn history |
| **Separation** | Outside plots (pairs) | Inside–outside centre distance ≤ ~190 m |
| **Non-overlapping** | All plots | No two inside plots may overlap; no two outside plots may overlap |

---

## Fire-specific exceptions

### Past burn records ignored: `quail_2012`, `johnson_2024`

Both fires have pre-1975 burn records covering most of their perimeter area and excluding those areas would eliminate most viable plot locations. We decided to ignore these historical fires, as they do not interfere with imagery interpretation and there has been a long recovery time. Only reburns occurring after the study fire year are applied for the prior-burn exclusion for these two fires.

### Marker fires: `quail_2012`, `cahuila_2017`, `nadeau_2020`, `sheep_2022`, `boulevard_2023`, `cal_2024`, `johnson_2024`

Joshua trees are not evenly distributed across these fires. Google Earth imagery review identified specific areas with visible JT presence; KML markers were placed at those confirmed locations. The systematic candidate-point approach is bypassed for these fires in favour of marker-anchored pairs (see above), ensuring pairs and inside plots fall where JTs actually occur.

### Fires with certain patches of joshua trees: `york_2023`, `coles_flat_2020`

These fires are large and from manual reference, certain areas of the fire have joshua trees. A reference coordinate was specified for each. Both border pairs and inside plots are sorted by distance to this coordinate so that selected plots cluster near the portion of the fire with Joshua trees.

---

## Parameters

| Parameter | Value | Description |
|---|---|---|
| Plot size | 60 × 60 m | Plot dimensions |
| Placement buffer (`MARGIN`) | ≈ 47 m | `√(30² + 30²) + 5` — half-diagonal of plot + 5 m |
| Target border pairs | 5 | Per fire |
| Target inside plots | 10 | Per non-marker fire |
| Candidate spacing | 25 m | Along perimeter |
| Max inside–outside separation | ~190 m | Centre to centre |
| Random seed | 42 | For reproducibility |

---

## Output

`output/recent/sites_coordinates.csv` — recent fires, one row per plot.  
`output/historical/historical_sites_coordinates.csv` — historical fires, border pairs only, same schema.

| Column | Description |
|---|---|
| `site_id` | Unique plot identifier (e.g. `quail_2012_inside_05`, `quail_2012_outside_05`) |
| `pair_id` | Shared by inside + outside plot of a pair (e.g. `quail_2012_pair05`); blank for interior plots |
| `fire_label` | Fire identifier (e.g. `quail_2012`) |
| `fire_year` | Year of the study fire |
| `fire_name` | Fire name |
| `fire_date` | Exact ignition date (YYYY-MM-DD) |
| `type` | `border_pair` or `interior` |
| `region` | `inside` or `outside` (position relative to fire perimeter) |
| `lat`, `lon` | Plot centroid (WGS84) |
| `min_lon`, `min_lat`, `max_lon`, `max_lat` | Plot corners (WGS84) |
| `site_other_burn_years` | Years of other fires whose perimeter intersects this plot (semicolon-separated; empty if none) |
| `imagery_pre_fire_date` | Available pre-fire imagery dates (YYYY-MM-DD; semicolon-separated if multiple options) |
| `imagery_post_fire_date` | Available post-fire imagery dates (YYYY-MM-DD; semicolon-separated if multiple options) |
| `imagery_notes` | Fire-level notes on imagery availability or quality |
| `site_notes` | Plot-level notes from imagery review (e.g. "unburned", "burned?") |

---

---

## Historical fires (1984–1994)

A parallel sampling design covers older fires for the long-term recovery chronosequence.  Only **border pairs** are placed (no interior plots); the unburned outside plot serves as the pre-fire density reference.

### Candidate selection

Fire perimeters are drawn from CalFire, the Nevada Wildland Fire History database, and MTBS (for CA federal-land fires not in CalFire). Fires must be ≥ 40 acres and have ≥ 30% overlap with the JT presence layer (relaxed from 60% for recent fires, because older perimeters are less precise and JT distribution in this era is less well mapped).

### Plot placement

Because pre-fire imagery is unavailable for 1984–1994 fires, plot locations are selected manually rather than algorithmically. Google Earth imagery review identified locations with visible Joshua trees; KML markers were placed for both inside and outside plots at each candidate pair location. This avoids a selection bias that would arise from placing plots where JTs are currently visible post-fire: the outside marker confirms JTs were present at the boundary pre-fire, so the paired inside plot is a fair recovery measurement regardless of current state.

Inside boxes are placed at marker locations and nudged inward if the marker falls on the perimeter edge. Outside boxes are placed directly at marker locations and nudged outward if they overlap the perimeter. Pairs are matched by nearest-centroid distance, prioritising inside markers closest to the perimeter; all valid pairs are kept.

The reburn mask covers only **post-fire burns** (`year > fire_year`): pre-fire burn history is handled by the matched burn-history check on each pair, ensuring both inside and outside plots share the same prior-fire exposure.

### Fires excluded from sampling

| Fire | Reason |
|---|---|
| `jumbled_1993` | Entire interior reburned by subsequent fires — no valid inside area remained after applying the post-fire reburn mask. Detected automatically during placement. |
| `paul_1987` | Completely reburned in 1999 — no unburned recovery signal available from present-day imagery. Removed after GEP review. |

### Parameters

| Parameter | Value |
|---|---|
| Year range | 1984–1994 (Landsat 5 TM era; ≥ 30 years before 2024) |
| Min JT overlap | 30% |
| Border pairs | all valid (no fixed cap) |
| Max inside–outside separation | ~190 m |

---

## Implementation

Recent fires: `scripts/02_recent_sites.R` (placement), `scripts/04_recent_kmz.R` (KMZ export), `scripts/05_recent_annotate.R` (imagery dates and site notes).  
Historical fires: `scripts/07_historical_sites.R` (placement), `scripts/09_historical_annotate.R` (imagery dates and site notes), `scripts/10_historical_kmz.R` (KMZ export).  
Boundaries: `scripts/11_export_boundaries.R` (GeoJSON + shapefile of all confirmed fire perimeters for RBR workflow).  
Collect Earth: `scripts/12_collect_earth_export.R` (Collect Earth project bundle for Open Foris Collect — sampling CSV, survey schema, plot geometry, and balloon display card).  
Outputs: `output/recent/`, `output/historical/`, `output/fire_boundaries_confirmed.geojson`, `output/fire_boundaries_confirmed.zip`, `output/collect_earth/jt_fire_recovery.cep`.
