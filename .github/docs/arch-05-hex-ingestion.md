# Arch-05: Hexagonal Data Ingestion Pipeline

## Summary

This document defines a **developer-oriented, pre-deployment data preparation pipeline** that enables PTI app developers to construct app data from custom shapefiles by querying hexagonal (H3) indicator data from an online API, aggregating it to the user's polygons, and outputting the standard `shp_dta` + `inp_dta` structures that the existing PTI app consumes.

**Key principle**: This pipeline runs _before_ deployment. The app itself ships with pre-built data and works entirely offline — no live API calls at runtime.

---

## Motivation

Currently, preparing data for a PTI app requires:
1. Preparing shapefiles manually as a hierarchical `.rds` list.
2. Constructing an Excel metadata file with indicator values pre-joined to each admin level.

This is labor-intensive. The new pipeline automates this by:
- Accepting raw shapefiles (one per admin level, or a single file).
- Identifying H3 hexagons (resolution 7 or 6) that cover those polygons.
- Fetching indicator values for those hexagons from an API.
- Aggregating hex-level values to polygon-level using configurable strategies (population-weighted by default).
- Producing the standard data structures ready for `launch_pti()`.

---

## Pipeline Overview

```
Developer's machine (pre-deployment)
─────────────────────────────────────────────────────────────────────

  ┌──────────────────┐
  │  Raw shapefiles  │  (one or more .shp / .gpkg / sf objects)
  └────────┬─────────┘
           │
           ▼
  ┌──────────────────────────────┐
  │  1. ingest_shapes()          │  Validate, standardize, build hierarchy
  └────────┬─────────────────────┘
           │
           ▼
  ┌──────────────────────────────┐
  │  2. map_hexagons_to_shapes() │  Polyfill each polygon → H3 hex IDs
  └────────┬─────────────────────┘
           │
           ▼
  ┌──────────────────────────────┐
  │  3. fetch_hex_data()         │  Query API for indicator values
  └────────┬─────────────────────┘  (placeholder/agnostic interface)
           │
           ▼
  ┌──────────────────────────────┐
  │  4. aggregate_hex_to_shapes()│  Pop-weighted (or other) aggregation
  └────────┬─────────────────────┘
           │
           ▼
  ┌──────────────────────────────┐
  │  5. build_pti_data()         │  Assemble shp_dta + inp_dta
  └────────┬─────────────────────┘
           │
           ▼
  ┌──────────────────────────────┐
  │  Standard PTI app data       │  .rds files ready for deployment
  └──────────────────────────────┘

Deployed app (runtime)
─────────────────────────────────────────────────────────────────────

  launch_pti(shp_dta, inp_dta)  ← loads pre-built .rds, works offline
```

---

## Step 1: `ingest_shapes()`

### Purpose
Accept one or more shapefiles, validate them, standardize column naming, and produce the `shp_dta` list structure.

### Inputs
- One or more file paths (`.shp`, `.gpkg`, `.geojson`) or `sf` objects.
- Optional: explicit hierarchy mapping table (a data frame relating admin codes across levels).
- Optional: admin level labels (e.g., `c("Province", "District")`).

### Behavior
1. Read each file into an `sf` tibble.
2. Validate required columns: each layer must have an `admin{N}Pcod` and `admin{N}Name` column, plus parent `admin{N-1}Pcod` for child layers.
3. **Hierarchy validation** (formal/columnar — no spatial overlay):
   - Check that child layers' parent codes (`admin{N-1}Pcod`) exist in the parent layer.
   - Build the mapping table by joining on shared `Pcod` columns (same logic as existing `get_mt()`).
   - Error if hierarchy is broken (orphan children, missing parents).
4. Standardize naming: `"admin{N}_{Label}"` (e.g., `"admin1_Province"`, `"admin2_District"`).
5. Add `admin0_Country` layer if not provided (dissolved union of finest level).
6. Compute `area` column for each polygon.

### Output
- `shp_dta`: named list of `sf` tibbles — identical structure to the existing sample data.

### Validation rules
| Check                                             | Error if           |
| ------------------------------------------------- | ------------------ |
| Column `admin{N}Pcod` present in each layer       | Missing            |
| Column `admin{N}Name` present in each layer       | Missing            |
| Parent codes in child reference valid parent rows | Orphan codes found |
| Geometries are valid (`st_is_valid`)              | Invalid geometries |
| CRS is set (preferably WGS84 / EPSG:4326)         | Missing CRS        |

---

## Step 2: `map_hexagons_to_shapes()`

### Purpose
For each polygon in the finest admin level, compute all H3 hexagons at the target resolution that intersect it.

### Inputs
- `shp_dta` (from step 1).
- `resolution`: integer, default `7`. Also supports `6` for coarser coverage.

### Behavior
1. Identify the finest admin level from `shp_dta` (highest `admin{N}` number).
2. For each polygon in that level, use H3 polyfill to get all hex IDs at the given resolution.
3. Return a mapping table.

### Output
- A tibble with columns: `hex_id`, `admin{N}Pcod` (finest level code), plus all parent codes from the mapping table.

### Notes
- Use the `h3jsr` package (or `h3o`) for hex operations.
- Border hexagons (partially overlapping) are included — their contribution is handled in aggregation via area or population weighting.

---

## Step 3: `fetch_hex_data()`

### Purpose
Query an external API to retrieve indicator values for a set of H3 hex IDs.

### Interface (API-agnostic placeholder)

```r
fetch_hex_data <- function(hex_ids,
                           indicators = NULL,
                           api_config = list(),
                           ...) {

  # Placeholder: actual implementation depends on the data API

  # Returns a tibble: hex_id × indicator columns (+ population column)
  stop("Not yet implemented. Provide a concrete API backend.")
}
```

### Design decisions
- **API-agnostic**: The function signature is fixed, but the backend is pluggable. Concrete implementations (e.g., World Bank DEC hex store, local parquet files) will be added later.
- **Batch-friendly**: Must handle thousands of hex IDs efficiently (batched requests, pagination).
- **Always returns population**: The population variable is always fetched (needed for weighted aggregation), even if not explicitly requested.
- **Returns a tibble**: Columns are `hex_id`, `population`, and one column per requested indicator.

### Future considerations
- Authentication / API keys via `api_config`.
- Rate limiting and retry logic.
- A local-file backend for testing (e.g., parquet or CSV with hex data).

---

## Step 4: `aggregate_hex_to_shapes()`

### Purpose
Aggregate hex-level indicator values to polygon-level values using a configurable strategy.

### Inputs
- `hex_data`: tibble from `fetch_hex_data()`.
- `hex_map`: tibble from `map_hexagons_to_shapes()`.
- `shp_dta`: the standardized shapes (for all admin levels).
- `strategy`: aggregation method — one of `"pop_weighted"` (default), `"area_weighted"`, `"simple_mean"`, `"sum"`.
- `pop_var`: name of the population column in `hex_data` (default `"population"`).

### Behavior

1. Join `hex_data` to `hex_map` on `hex_id`.
2. For each polygon (finest admin level), aggregate each indicator:
   - **`pop_weighted`**: $\bar{x}_p = \frac{\sum_i x_i \cdot w_i}{\sum_i w_i}$ where $w_i$ is population of hex $i$.
   - **`area_weighted`**: Same formula but $w_i$ is area of hex overlap with the polygon.
   - **`simple_mean`**: Unweighted mean of hex values within the polygon.
   - **`sum`**: Sum of hex values within the polygon.
3. Produce a data frame: `admin{N}Pcod` × indicator columns at the finest level.
4. Optionally, aggregate upward to coarser levels using the mapping table (population-weighted roll-up).

### Output
- A named list of tibbles — one per admin level — with columns: `admin{N}Pcod`, `admin{N}Name`, parent codes, `year`, and indicator value columns.

---

## Step 5: `build_pti_data()`

### Purpose
Assemble the final `inp_dta` list (metadata structure) from aggregated values, and confirm `shp_dta` is ready.

### Inputs
- Aggregated data from step 4.
- `shp_dta` from step 1.
- `indicator_config`: a data frame or list defining indicator metadata (var_code, var_name, var_description, pillar assignments, etc.). Can be auto-generated with defaults if not provided.

### Behavior
1. Build `$metadata` tibble with required columns:
   - `var_code`, `var_name`, `var_description`, `var_order`, `var_units`
   - `spatial_level` (which admin level tab this indicator belongs to)
   - `pillar_group`, `pillar_name`, `pillar_description`
   - `fltr_exclude_pti`, `fltr_exclude_explorer`, `fltr_overlay_pti`, `fltr_overlay_explorer`, `legend_revert_colours` (all default `FALSE`)
2. Build `$general` tibble: `tibble(country = "...")`.
3. Build `$admin{N}_{Label}` tibbles: admin codes + names + year + indicator values.
4. Return `inp_dta` list.

### Output
- `inp_dta`: named list identical in structure to what `fct_template_reader()` produces.
- Optionally save both `shp_dta` and `inp_dta` as `.rds` files for app deployment.

---

## End-to-End Example Usage

```r
library(devPTIpack)
library(sf)

# ─── Step 1: Load and validate shapes ───────────────────────────────
shp_dta <- ingest_shapes(
 list(
   admin1 = list(path = "data-raw/provinces.gpkg", label = "Province"),
   admin2 = list(path = "data-raw/districts.gpkg", label = "District")
 )
)

# ─── Step 2: Map hexagons ───────────────────────────────────────────
hex_map <- map_hexagons_to_shapes(shp_dta, resolution = 7)

# ─── Step 3: Fetch hex data from API ───────────────────────────────
hex_data <- fetch_hex_data(
 hex_ids = unique(hex_map$hex_id),
 indicators = c("nightlights", "poverty_rate", "connectivity"),
 api_config = list(endpoint = "https://hex-api.example.org/v1")
)

# ─── Step 4: Aggregate to polygons ─────────────────────────────────
aggregated <- aggregate_hex_to_shapes(
 hex_data = hex_data,
 hex_map = hex_map,
 shp_dta = shp_dta,
 strategy = "pop_weighted",
 pop_var = "population"
)

# ─── Step 5: Build app data ────────────────────────────────────────
inp_dta <- build_pti_data(
 aggregated = aggregated,
 shp_dta = shp_dta,
 indicator_config = tibble::tibble(
   var_code = c("nightlights", "poverty_rate", "connectivity"),
   var_name = c("Night Lights", "Poverty Rate", "Connectivity Index"),
   pillar_name = c("Economic", "Welfare", "Infrastructure")
 ),
 country_name = "Example Country",
 year = 2024
)

# ─── Save for deployment ───────────────────────────────────────────
saveRDS(shp_dta, "my-pti-app/app-data/shapes.rds")
saveRDS(inp_dta, "my-pti-app/app-data/metadata.rds")

# ─── Deploy as usual ──────────────────────────────────────────────
# In app.R:
# shp_dta <- readRDS("app-data/shapes.rds")
# inp_dta <- readRDS("app-data/metadata.rds")
# launch_pti(shp_dta = shp_dta, inp_dta = inp_dta)
```

---

## File Structure (new files)

| File                                      | Purpose                                                  |
| ----------------------------------------- | -------------------------------------------------------- |
| `R/fct_hex_ingest_shapes.R`               | `ingest_shapes()` — shape validation and standardization |
| `R/fct_hex_mapping.R`                     | `map_hexagons_to_shapes()` — H3 polyfill logic           |
| `R/fct_hex_fetch.R`                       | `fetch_hex_data()` — API-agnostic placeholder + backends |
| `R/fct_hex_aggregate.R`                   | `aggregate_hex_to_shapes()` — aggregation strategies     |
| `R/fct_hex_build_data.R`                  | `build_pti_data()` — assemble final inp_dta              |
| `tests/testthat/test-hex-ingest-shapes.R` | Tests for shape ingestion                                |
| `tests/testthat/test-hex-mapping.R`       | Tests for hex mapping                                    |
| `tests/testthat/test-hex-aggregate.R`     | Tests for aggregation                                    |
| `tests/testthat/test-hex-build-data.R`    | Tests for data assembly                                  |

---

## Dependencies (new)

| Package            | Purpose                                      |
| ------------------ | -------------------------------------------- |
| `h3jsr` (or `h3o`) | H3 hex indexing and polyfill                 |
| `sf`               | Spatial operations (already a dependency)    |
| `httr2` or `httr`  | API requests (for `fetch_hex_data` backends) |

---

## Scope & Non-Goals (v1)

### In scope
- Developer-facing functions for pre-deployment data prep.
- H3 resolution 7 and 6.
- Population-weighted aggregation (primary), plus simple mean, area-weighted, and sum.
- Columnar hierarchy validation (no spatial overlay in v1).
- Output identical to existing `fct_template_reader()` / `get_shape()` structures.
- The deployed app loads pre-built data, works fully offline.

### Not in scope (future)
- Live in-app API querying at runtime.
- Shiny module for interactive shape upload + on-the-fly data fetching.
- Spatial hierarchy inference via polygon containment/overlay.
- Automatic pillar/weighting scheme suggestions.
- Incremental data updates (re-fetch only changed indicators).

---

## Integration with Existing Pipeline

The new pipeline's outputs plug directly into the existing app infrastructure:

```
                    ┌─────────────────────────────────┐
                    │  NEW: Hex ingestion pipeline     │
                    │  (developer, pre-deployment)     │
                    └──────────────┬──────────────────┘
                                   │
                                   ▼
              shp_dta (list of sf)  +  inp_dta (list of tibbles)
                                   │
           ┌───────────────────────┼───────────────────────────┐
           │                       │                           │
           ▼                       ▼                           ▼
    EXISTING: get_shape()    EXISTING: fct_template_reader()   │
    (bypassed — data         (bypassed — data already in       │
     already prepared)        correct format)                  │
           │                       │                           │
           └───────────┬───────────┘                           │
                       ▼                                       │
              launch_pti(shp_dta, inp_dta)  ◄──────────────────┘
                       │
                       ▼
              Existing PTI calc + visualization pipeline
              (mod_calc_pti2 → mod_plot_pti2 → leaflet)
```

No changes are required to the existing calculation or visualization modules.
