# Step 2 — Zonal stats (optional)

> **This step is optional.** Use it when you need to extract zonal
> statistics from raster data (e.g. satellite imagery, gridded
> population, accessibility surfaces, night-time lights) before building
> your metadata Excel in Step 3.
>
> The template ships `02a-user-zonal-stats.qmd` as a stub. It is **not**
> rendered by `00-master.R` — run it manually whenever you need to
> recompute. Add `02b-…qmd`, `02c-…qmd` etc. for additional extracts.

## When you need zonal stats

Reach for this step when one of your indicators is only available as a
raster surface and you need polygon-level aggregates to plug into Step
3. Typical cases:

- Population-weighted poverty rate by district from a gridded poverty
  raster.
- Mean night-time-lights intensity per province as a proxy for economic
  activity.
- Average travel time to the nearest health facility per polygon.
- Land-cover composition (% forest, % cropland) per admin unit.

Skip this step entirely if every indicator you plan to ship already
arrives as a tabular per-polygon value.

## Common tools

Both of the following work well; pick whichever fits your existing
workflow:

- [`exactextractr`](https://isciences.gitlab.io/exactextractr/) — fast
  zonal stats with fractional pixel coverage. Best for
  population-weighted means and areas of binary masks.
- [`terra::extract()`](https://rspatial.github.io/terra/reference/extract.html)
  — general-purpose raster sampling. Slower than `exactextractr` for
  large rasters but more flexible across summary functions.

Both produce one numeric column per indicator, indexed by polygon
position; you’ll attach the result to the polygon’s `admin<N>Pcod`.

## Output format

The output of this step is a tidy table — one row per polygon, one
column per indicator — keyed by `admin<N>Pcod`. **Load the admin layer
from `app-data/shapes.rds`** (the Step 1 output), not from the raw
GeoJSON — the `.rds` carries the validated `admin<N>Pcod` + the full
parent cascade, so your zonal output will join cleanly back to every
other PTI input.

``` r

#| eval: false
library(sf)
library(exactextractr)
library(dplyr)
library(writexl)

# Load the cleaned admin layer from Step 1's output.
my_shp <- readRDS("app-data/shapes.rds")
adm2   <- my_shp$admin2_District

# Load the raster you want to summarise.
r <- terra::rast("path/to/your-raster.tif")

# Compute the zonal mean per polygon (fractional pixel coverage).
adm2$mean_value <- exact_extract(r, adm2, fun = "mean")

# Reshape to the columns Step 3 expects: admin<N>Pcod + one column per indicator.
my_zonal <- adm2 |>
  st_drop_geometry() |>
  transmute(
    admin2Pcod   = admin2Pcod,
    my_indicator = mean_value
  )

# Save next to the rest of your raw inputs so Step 3 can pick it up.
write_xlsx(my_zonal, "sample-data/my-zonal-stats-adm2.xlsx")
```

The package convention is one Excel sheet per admin level; if you
produce multiple admin levels in this step, write each into its own
sheet before Step 3 ingests them.

## Hex-level extraction (optional)

If your Step 1 produced an `admin9_Hexagon` layer (Step 1 §F), zonal
extraction works the same way — the hex cells are just another admin
layer with `admin9Pcod` as the key:

``` r

#| eval: false
adm9 <- my_shp$admin9_Hexagon
adm9$mean_value <- exact_extract(r, adm9, fun = "mean")

my_zonal_hex <- adm9 |>
  st_drop_geometry() |>
  transmute(
    admin9Pcod   = admin9Pcod,
    my_indicator = mean_value
  )
```

A note on choice of summary function for hex cells: H3-6 cells have
near-uniform area (~36 km²), so the *area-weighted* mean differs little
from a plain `"mean"`. **Population-weighted** aggregation differs more
substantially and is usually what you want for human-development
indicators (poverty, literacy). Pick the `fun` argument deliberately —
`exact_extract()` accepts a `weights` raster for population weighting.

## How the output feeds into Step 3

Step 3 (the metadata workbook) expects one sheet per admin level —
`admin<N>_<HumanName>` — containing the indicator columns referenced by
the `metadata` sheet. The output of this step is one of those sheets (or
one input that feeds into one of them).

A typical workflow is:

1.  Run this step once per raster indicator.
2.  Merge the resulting tidy tables into your draft metadata workbook by
    `admin<N>Pcod` (left-join on the relevant admin sheet).
3.  Add a row to the workbook’s `metadata` sheet for the new indicator.
4.  Validate in Step 3.

The full column contract for Step 3 admin sheets lives in [Data
preparation reference
§2.4](https://worldbank.github.io/devPTIpack/articles/dataprep.md).
Refer there for required types, the parent P-code cascade, and what
[`validate_metadata()`](https://worldbank.github.io/devPTIpack/reference/validate_metadata.md)
checks.

## Next

When your zonal-stats outputs are ready, continue with [Step 3 —
Metadata
Excel](https://worldbank.github.io/devPTIpack/articles/build-pti-3-metadata.md).
