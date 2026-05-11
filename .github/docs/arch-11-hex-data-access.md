# arch-11 — Hexagonal Data Access Pipeline

> **Status:** Planning. Owner: EBukin (with Claude).
>
> **Supersedes:** arch-05 (hex ingestion pipeline — original high-level sketch).
>
> **Depends on:** arch-10 (`make_hex_grid()`, `make_admin_lookup()`).
>
> **Scope:** Registry-driven hexagonal data access, aggregation to admin
> polygons, and metadata Excel generation — the full Step 4 implementation.

---

## Problem Statement

Building a PTI app today requires labour-intensive manual data preparation:
developers must locate indicator datasets, download them, join them to
administrative polygons, and construct the metadata Excel file by hand. For
gridded / hexagonal data sources (population, night-time lights, poverty
estimates, connectivity indices) this process is repeated per country, per
project, with no standardisation.

The hex data access pipeline eliminates this by giving deployers a
registry-driven, reproducible workflow: browse available indicators, declare
which ones to use, fetch pre-computed H3 hex-level values from online parquet
endpoints, aggregate to the project's admin polygons, and produce the standard
`metadata-hex.xlsx` that Step 5's `compile_pti_data()` already consumes.

---

## Solution

A set of exported R functions that form a five-phase developer workflow
(discover → configure → fetch → aggregate → build metadata), backed by a
package-shipped YAML registry encoding all known indicator sources, their
column mappings, time dimensions, aggregation defaults, and display metadata.

The deployer's Step 4 script becomes five explicit function calls — no wrapper
function — so the optional user-parquet merge step slots in naturally between
fetch and aggregate. A master switch (`INCLUDE_HEX_IN_APP`) in `00-master.R`
controls whether hex-level polygons ship with the deployed app or only
admin-level aggregates are included.

---

## User Stories

1. As a PTI deployer, I want to browse all available hex-level indicators in
   one place, so that I can decide which ones to include in my country's app
   without searching external data catalogues.

2. As a PTI deployer, I want to select indicators by canonical name and
   specify target years globally, so that I can configure the entire hex data
   pipeline in a few lines of code.

3. As a PTI deployer, I want the system to automatically find the nearest
   available year when my exact requested year is missing, so that I get the
   best available data without manual year-by-year lookups.

4. As a PTI deployer, I want to be warned (not silently surprised) when the
   system substitutes a different year than I requested, so that I can decide
   whether the substitution is acceptable for my analysis.

5. As a PTI deployer, I want the system to error if no data is available
   within 7 years of my requested year, so that I don't unknowingly use
   wildly stale data.

6. As a PTI deployer, I want to query live parquet endpoints for the actual
   available time periods of a variable, so that I can make informed year
   selections even when the registry hint is outdated.

7. As a PTI deployer, I want to fetch hex data for only the hexagons covering
   my country's boundaries (from `shapes.rds`), so that I don't download
   global data unnecessarily.

8. As a PTI deployer, I want population to always be fetched automatically
   from the registry source that declares it, so that population-weighted
   aggregation works without extra configuration.

9. As a PTI deployer, I want to control the aggregation strategy per
   indicator — choosing weight (population / area / none) and function
   (mean / median / sum / min / max) independently — so that each indicator
   is aggregated in a way that is statistically appropriate.

10. As a PTI deployer, I want a `.default` aggregation strategy that applies
    to all indicators I haven't explicitly configured, so that I only need to
    specify overrides for exceptions.

11. As a PTI deployer, I want hex data aggregated directly from hex level to
    each admin level independently (hex→admin1, hex→admin2, not
    admin2→admin1), so that each level uses the primary hex data without
    compounding aggregation error.

12. As a PTI deployer, I want hex-derived indicators to appear on every
    admin-level sheet in the output Excel (admin1, admin2, etc.), so that
    the Data Explorer tab and PTI calculation both have access to the values
    at every level.

13. As a PTI deployer, I want to merge my own local parquet data with the
    registry fetch output before aggregation, so that I can include
    project-specific indicators alongside the standard ones.

14. As a PTI deployer, I want clear documentation of the local parquet
    contract (long format, no population column, user renames columns), so
    that I can prepare my file correctly on the first attempt.

15. As a PTI deployer, I want `build_hex_metadata()` to auto-populate
    metadata fields (pillar, description, units, legend direction) from the
    registry for registry variables, so that I only need to supply metadata
    for my custom local indicators.

16. As a PTI deployer, I want to override registry-default metadata fields
    (e.g. `pillar_name`, `legend_revert_colours`) for my specific deployment,
    so that I can customise the app without modifying the package.

17. As a PTI deployer, I want the system to warn me when my hex grid exceeds
    5,000 cells and prompt whether to include hex-level data in the deployed
    app, so that I don't accidentally ship a slow app.

18. As a PTI deployer, I want a master switch (`INCLUDE_HEX_IN_APP`) in
    `00-master.R` that controls whether hex polygons and hex-level data
    sheets are included in the app, so that I can choose performance over
    resolution explicitly.

19. As a PTI deployer, I want all hex-sourced indicator values to still be
    available at admin levels even when hex polygons are excluded from the
    app, so that excluding hexes does not lose data — only display
    granularity.

20. As a PTI deployer, I want temporal indicators to expand into one metadata
    row per resolved year (e.g. `nightlights_2018`, `nightlights_2022`),
    with display names generated from a glue template, so that each year
    appears as a distinct indicator in the dashboard.

21. As a PTI deployer, I want `spatial_level` in the metadata sheet to always
    point to the finest available level for each variable (hex level for hex
    data), so that the PTI calculation engine uses the most granular source.

22. As a PTI deployer, I want population to appear as a visible indicator in
    the metadata sheet and on the map, so that users can see population
    distribution alongside other indicators.

23. As a PTI deployer, I want progress bars during fetch (per variable) and
    aggregation (per admin level × variable), so that I can track pipeline
    progress for large countries.

24. As a PTI deployer, I want to build my hex grid at H5 resolution for
    display while fetching pre-computed H6 data, with the system
    transparently expanding H5→H6 children, fetching at H6, and
    aggregating back to H5, so that I can choose display resolution
    independently of data resolution.

25. As a PTI deployer, I want the system to reject requests for finer-than-
    source resolution (e.g. H7 grid with H6 data) with a clear error, so
    that I don't get silently empty results.

26. As a PTI deployer, I want the system to handle large hex ID sets (>10k)
    by partitioning requests with a warning and progress tracking, so that
    network fetches don't fail on payload size.

27. As a PTI deployer, I want NA values in hex data treated as 0 during
    aggregation (except when all hexes in a polygon are NA, which yields
    NA with a warning), so that partial coverage does not propagate missing
    values unnecessarily.

28. As a PTI deployer, I want the output `metadata-hex.xlsx` to be validated
    by `validate_metadata()` before the function returns, so that I can
    trust the output will work with `compile_pti_data()`.

29. As a package maintainer, I want the registry YAML to be the single source
    of truth for known indicator sources, so that adding a new variable is
    a YAML edit + package release — no code changes.

30. As a package maintainer, I want a `registry_version` field captured in
    the output Excel's `general` sheet, so that deployers can trace which
    registry produced their data.

31. As a package maintainer, I want removed registry variables to cause an
    immediate error when referenced by `use_hex_vars()`, so that deployers
    learn about deprecations early rather than getting silent failures.

---

## Implementation Decisions

### Registry

- **`inst/hex_vars_registry.yaml`** is the canonical catalogue of known hex
  indicator sources. Updated by package maintainers. Users receive updates on
  package install.

- The YAML is structured as:

  ```yaml
  registry_version: "1.0.0"

  sources:
    pti_hex_store:
      label: "PTI Pre-computed Hex Data"
      path: "https://..."
      hex_col: "h3_index"
      pop_col: "pop_gpw_2020"
      variables:
        nightlights:
          source_col: "ntl_mean"
          var_name: "Night Lights ({year})"
          var_description: "..."
          var_units: "nW/cm²/sr"
          pillar_group: 2
          pillar_name: "Economic Activity"
          legend_revert_colours: false
          time_col: "year"
          available_years: [2013, ..., 2022]   # hint only
          weight: "none"
          fun: "mean"
          fltr_exclude_pti: false
        population:
          source_col: "pop_gpw_2020"
          var_name: "Population"
          ...
          weight: "none"
          fun: "sum"
  ```

- `available_years` is a **documentation hint** shown in `list_hex_vars()`
  output. `get_available_years()` queries the live parquet for ground truth.
  The YAML never gates or filters access to years.

- Removed variables cause an immediate error on `use_hex_vars()`. No soft
  deprecation mechanism in v1.

- No authentication. Sources are publicly accessible.

### Variable descriptors

- **`pti_hex_var()`** is an S3 constructor for per-variable configuration.
  Contains: `source_col`, `canonical_name`, `var_name` (glue template for
  temporal vars), `var_description`, `var_units`, `pillar_group`,
  `pillar_name`, `legend_revert_colours`, `fltr_exclude_pti`, `time_col`,
  `years`, `weight` (`"pop"` / `"area"` / `"none"`),
  `fun` (`"mean"` / `"median"` / `"sum"` / `"min"` / `"max"`).

- **`pti_hex_source()`** is an S3 constructor for one parquet endpoint.
  Contains: `path`, `hex_col`, `vars` (list of `pti_hex_var`), `pop_var`
  (`pti_hex_var` for population or `NULL`), `label`. Exactly one source
  across the full source list must declare `pop_var`.

- When the same `canonical_name` appears in multiple sources, all occurrences
  are suffixed with the source `label` (e.g. `poverty_rate__DEC`,
  `poverty_rate__local`). Each becomes a distinct variable with its own
  metadata row.

### Year resolution

- `years` is a **global** argument on `use_hex_vars()`, applying to all
  temporal variables. Per-variable override in `pti_hex_var()` still
  accepted.

- Non-temporal variables are always returned regardless of `years`.

- When an exact requested year is absent, the **nearest available year** is
  used. On equidistant tie, the **later year** is preferred.

- **Maximum tolerance: 7 years.** Beyond that, the system errors with a
  message listing the variable, requested year, and available years.

- Warnings list all requested→resolved year pairs per variable.

- `var_code` suffix in output reflects the **resolved** (truth) year, not
  the requested year.

- `years = NULL` + interactive session → `cli` prints available years +
  `utils::menu()` prompt. Non-interactive → error with actionable message.

### Fetching

- **`fetch_hex_data(hex_ids, vars)`** reads from online (or in exceptional
  cases, local) parquet endpoints via `arrow::open_dataset()` with
  predicate pushdown on the hex ID column.

- Only hexagons matching `hex_ids` are read — no global download.

- `hex_ids` come from `hex_layer$admin9Pcod` (output of arch-10's
  `make_hex_grid()` + `make_admin_lookup()`).

- Population is always fetched, always the first indicator column after
  `hex_id`.

- Temporal variables: fetched in long format internally, pivoted wide
  before returning. Output columns: `<canonical_name>_<resolved_year>`.

- **Resolution bridge:** if `hex_ids` are at a coarser resolution (e.g. H5)
  than the source data (H6), `fetch_hex_data()` transparently expands each
  H5 ID to its H6 children via `h3_to_children()`, fetches at H6, and
  aggregates back to H5 using the variable's `weight`/`fun` strategy.
  Finer-than-source resolution (e.g. H7 grid with H6 data) → error.

- **Large hex sets (>10k):** warn and partition into batches. Progress bar
  per variable (not per batch of hex IDs).

- **Network errors:** check and report clearly. No retry in v1.

- **Hive-partitioned parquet:** warn/error that it is not supported unless
  support is trivially simple to add.

- Returns a tibble: `hex_id | population | <indicator columns>`.

### Aggregation

- **`aggregate_hex_to_shapes(hex_data, hex_layer, shp_dta, strategy)`**

- `hex_layer` provides all grouping keys (parent Pcod columns assigned by
  `make_admin_lookup()`). The mapping is `st_drop_geometry(hex_layer)` —
  no separate mapping table.

- Aggregation is always **hex → each admin level directly**. Never
  chained (never admin2→admin1). Each admin level uses primary hex data.

- `strategy` is a named list with `.default`:
  ```r
  strategy = list(
    .default    = c(weight = "pop",  fun = "mean"),
    nightlights = c(weight = "none", fun = "mean"),
    conflict    = c(weight = "none", fun = "sum")
  )
  ```

- Strategy resolution for temporal variables: strip `_<year>` suffix from
  `var_code` to get the stem, look up stem in `strategy`, fall back to
  `.default`.

- **Weight × Fun are orthogonal.** All 15 combinations are valid:
  - `weight`: `"pop"` (population-weighted), `"area"` (area-weighted from
    `make_hex_grid()`'s `area` column), `"none"` (unweighted / plutocratic).
  - `fun`: `"mean"`, `"median"`, `"sum"`, `"min"`, `"max"`.

- **NA handling:**
  - `NA` population → treated as 0.
  - `NA` indicator value → treated as 0.
  - All hexes in a polygon are `NA` for an indicator → result is `NA` +
    cli warning naming the polygon and variable.

- Errors if `weight = "pop"` is requested but `population` column is
  absent from `hex_data`, or `weight = "area"` but `area` is absent.

- Progress bar: per admin level × variable.

- Returns: named list of tibbles, one per admin level. Each tibble has
  `admin<N>Pcod`, `admin<N>Name`, parent Pcods, `population` (aggregated),
  and indicator columns.

### Metadata Excel output

- **`build_hex_metadata(aggregated, shp_dta, indicator_config, country_name, output_path, include_hex)`**

- Registry variables → metadata fields auto-populated from YAML. Printed
  via `cli` for user verification.

- Non-registry variables → must be declared in `indicator_config`. Error
  if missing.

- User can override registry defaults via `indicator_config` — their values
  take precedence, with a warning.

- Temporal variables expand to one `metadata` row per resolved year:
  `var_code = <stem>_<year>`, `var_name` from glue template.

- All user-chosen resolved years: `fltr_exclude_pti = FALSE` by default
  (user explicitly chose them).

- `spatial_level` for hex-derived variables = the hex level name
  (e.g. `admin9_Hexagon`).

- Population appears as a visible indicator (row in `metadata` sheet,
  column in admin sheets).

- `registry_version` captured in the `general` sheet.

- **Hex inclusion gating (`include_hex` argument):**

  - `include_hex = TRUE` → `admin9_Hexagon` sheet included in output.
  - `include_hex = FALSE` → hex sheet omitted. Only aggregated admin
    sheets written.
  - `include_hex` not set and hex layer >5,000 cells:
    - Interactive → cli warning + prompt: "Hex grid has {n} cells
      (>5,000). App performance may suffer. Include hex level? [y/N]".
      Default N.
    - Non-interactive → `FALSE`. Cli warning: "Hex grid has {n} cells.
      Hex-level data excluded. Set `include_hex = TRUE` to override."
  - In both cases, aggregated admin-level data is **always** written.

- Output validated with `validate_metadata()` before returning. Errors on
  fail.

- `year` column in admin sheets: **not used**. Temporal indicators are
  separate columns with year in the `var_code` suffix. The legacy `year`
  column contract is not updated.

### `00-master.R` integration

```r
# ── Hex data configuration ──────────────────────────────────────────
# Set to TRUE to include hex-level polygons in the deployed app.
# When FALSE (default), hex data is aggregated to admin levels only —
# hex polygons are NOT shipped with the app. Recommended when the hex
# grid exceeds ~5,000 cells.
INCLUDE_HEX_IN_APP <- FALSE
```

This variable is passed to:

- **Step 1 (`01-shapes.qmd`):** if `FALSE`, `admin9_Hexagon` is removed
  from `shp_dta` before `saveRDS()`. The hex layer object is retained in
  the Quarto environment for Step 4 to use.

- **Step 4 (`04-hex-data.qmd`):** passed to `build_hex_metadata()` as
  `include_hex`.

- **Step 5 (`05-compile.qmd`):** no change needed — if the hex sheet is
  absent from `metadata-hex.xlsx`, `compile_pti_data()` merges only what
  is present.

### Local user parquet — DIY path (documented, not automated)

Users who have project-specific indicators in local parquet follow a
documented recipe between the fetch and aggregate steps:

1. Load only relevant hexes: `arrow::open_dataset() |> filter(hex_col %in% hex_ids) |> collect()`.
2. Rename columns to chosen canonical names.
3. Pivot long→wide to match `fetch_hex_data()` output shape (`<canonical>_<year>` columns).
4. `full_join()` with registry fetch output on `hex_id`.
5. Supply metadata for custom vars in `indicator_config`.

**Contract for user parquet:**

| Requirement   | Rule                                                                     |
| ------------- | ------------------------------------------------------------------------ |
| Format        | Single file or partitioned directory readable by `arrow::open_dataset()` |
| Hex column    | Any name — user renames to `hex_id` before merge                         |
| Time column   | Long format: one row per hex × year. Column name = user's choice         |
| Population    | Absent. Always from registry                                             |
| Value columns | Numeric. One per variable. Any name — user renames before merge          |
| Uniqueness    | No duplicate `hex_id` per time period                                    |

**Early collision warning:** `aggregate_hex_to_shapes()` checks for `.x`/`.y`
suffixed columns (symptom of a bad `full_join`) and warns with an actionable
message about canonical name collision between user and registry data.

### Resolution constraints

- Pre-computed data is at **H6**. No H7.
- `make_hex_grid()` supports resolution 5, 6, or 7 (arch-10), but this
  pipeline's `fetch_hex_data()` constrains to **H6 or coarser** (H5).
- H5 hex grid + H6 data: transparent expansion via `h3_to_children()`,
  fetch at H6, aggregate back to H5.
- H7 or finer → error: "Pre-computed data is at H6. Use resolution 6 or 5."
- Large countries at H6: warn user if >5,000 cells and recommend H5 for
  display or setting `INCLUDE_HEX_IN_APP <- FALSE`.

---

## Testing Decisions

### What makes a good test

Tests verify **external behaviour through the public interface** — inputs in,
outputs out, side effects observed. No testing of internal data structures or
private helper implementation details. Tests should break when behaviour
changes, not when implementation is refactored.

### Modules to test

| Module               | Test file                   | Key assertions                                                                                                                                                                                                                                                                                                                                          |
| -------------------- | --------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Registry reader      | `test-hex-registry.R`       | YAML parses to expected structure. `list_hex_vars()` returns correct columns and filters. `use_hex_vars()` resolves registry entries. Duplicate canonical names get source-label suffix. Unknown canonical name errors immediately.                                                                                                                     |
| Year resolver        | `test-hex-year-resolver.R`  | Exact match returns exact. Nearest year preference (later on tie). 7-year max tolerance error. Warning messages list requested→resolved pairs. Non-temporal var unaffected by `years`. `NULL` years in non-interactive session errors.                                                                                                                  |
| Hex data fetcher     | `test-hex-fetch.R`          | Returns tibble with `hex_id` + `population` + indicator columns. Temporal vars pivoted wide with `_<year>` suffix. Only requested hex IDs returned (mock with local parquet fixture). H5→H6 resolution bridge: H5 input, H6 fixture, output at H5. Finer-than-source errors.                                                                            |
| Hex aggregator       | `test-hex-aggregate.R`      | All `weight × fun` combinations produce correct numeric result on a small fixture. `.default` fallback works. Stem-based strategy lookup strips `_<year>`. NA value → 0. NA population → 0. All-NA polygon → `NA` + warning. Each admin level aggregated directly from hex (not chained).                                                               |
| Hex metadata builder | `test-hex-build-metadata.R` | Output xlsx passes `validate_metadata()`. Registry vars auto-populated. Non-registry vars require `indicator_config`. Temporal vars expand to one row per year. `spatial_level` = hex level. Population appears as indicator. `registry_version` in general sheet. `include_hex = FALSE` omits hex sheet. >5k cell prompt in non-interactive → `FALSE`. |
| Source adapter       | `test-hex-source.R`         | `pti_hex_source()` validates required fields. `pti_hex_var()` validates weight/fun combinations. Exactly-one `pop_var` enforced across sources.                                                                                                                                                                                                         |

### Prior art

Existing test patterns in `tests/testthat/` use `testthat` with the bundled
`ukr_shp` and `ukr_mtdt_full` fixtures via `helper-test-data.R`. The hex
tests will follow the same pattern, adding small synthetic hex fixtures
(a handful of H6 cells covering Rwanda) as `.rds` files in
`tests/testthat/fixtures/`.

---

## Out of Scope

- **Live in-app API querying at runtime.** The deployed app is fully offline.
- **Shiny module for interactive shape upload + on-the-fly data fetching.**
- **Spatial hierarchy inference via polygon containment/overlay** (arch-10
  uses centroid-in-polygon; no polygon-polygon intersection).
- **Automatic pillar/weighting scheme suggestions.**
- **Incremental data updates** (re-fetch only changed indicators).
- **Retry logic / backoff for network failures** (v1: error on failure).
- **Hive-partitioned parquet support** (warn/error unless trivially simple).
- **Authentication on parquet endpoints** (sources are public).
- **Soft deprecation of removed registry variables** (v1: hard error).
- **`run_hex_pipeline()` wrapper** — dropped; Step 4 template is the recipe.
- **`make_hex_grid()` and `make_admin_lookup()`** — specified in arch-10.
- **App-side Leaflet performance for >5k hex polygons** — managed by
  `INCLUDE_HEX_IN_APP` switch, not by rendering optimisation in v1.

---

## Further Notes

### Relationship to arch-05 and arch-10

This document (arch-11) supersedes arch-05's original high-level sketch.
The function names, signatures, and data flow defined here are authoritative.
Arch-05 remains as historical context.

Arch-10 specifies `make_hex_grid()` and `make_admin_lookup()` — the Step 1
geometry functions that produce the hex layer and cascade. Arch-11 consumes
their output; it does not duplicate or modify their spec.

### File locations (new files)

| File                                       | Purpose                                                         |
| ------------------------------------------ | --------------------------------------------------------------- |
| `R/fct_hex_registry.R`                     | `list_hex_vars()`, `use_hex_vars()`, `get_available_years()`    |
| `R/fct_hex_source.R`                       | `pti_hex_source()`, `pti_hex_var()` S3 constructors             |
| `R/fct_hex_fetch.R`                        | `fetch_hex_data()`, year resolver, resolution bridge (internal) |
| `R/fct_hex_aggregate.R`                    | `aggregate_hex_to_shapes()`                                     |
| `R/fct_hex_build_metadata.R`               | `build_hex_metadata()`                                          |
| `inst/hex_vars_registry.yaml`              | Canonical variable catalogue                                    |
| `tests/testthat/test-hex-registry.R`       | Registry reader tests                                           |
| `tests/testthat/test-hex-year-resolver.R`  | Year resolution tests                                           |
| `tests/testthat/test-hex-fetch.R`          | Fetch tests                                                     |
| `tests/testthat/test-hex-aggregate.R`      | Aggregation tests                                               |
| `tests/testthat/test-hex-build-metadata.R` | Metadata builder tests                                          |
| `tests/testthat/test-hex-source.R`         | Source adapter tests                                            |
| `tests/testthat/fixtures/`                 | Small synthetic hex fixtures                                    |

### Dependencies (new)

| Package          | Purpose                                                                  |
| ---------------- | ------------------------------------------------------------------------ |
| `h3jsr` or `h3o` | H3 child expansion for resolution bridge (deferred; shared with arch-10) |
| `arrow`          | Parquet reading with predicate pushdown                                  |
| `yaml`           | Registry YAML parsing                                                    |
| `glue`           | Var name templates for temporal variables                                |
| `cli`            | Progress bars, warnings, prompts                                         |
| `writexl`        | Metadata Excel output (already a dependency)                             |

### End-to-end deployer script (Step 4)

```r
library(devPTIpack)

# ── Already done in Step 1 ───────────────────────────────────────────
my_shp <- readRDS("app-data/shapes.rds")
hex_layer <- my_shp$admin9_Hexagon
hex_ids   <- hex_layer$admin9Pcod

# ── A. Discover ──────────────────────────────────────────────────────
list_hex_vars()
get_available_years(vars = NULL, "nightlights")

# ── B. Configure ─────────────────────────────────────────────────────
vars <- use_hex_vars(
  "nightlights",
  "poverty_rate",
  "population",
  years = c(2015, 2022)
)

# ── C. Fetch ─────────────────────────────────────────────────────────
hex_data <- fetch_hex_data(hex_ids = hex_ids, vars = vars)

# ── C′. Optional: merge user parquet ─────────────────────────────────
my_data <- arrow::open_dataset("data-raw/conflict.parquet") |>
  dplyr::filter(h3id %in% hex_ids) |>
  dplyr::collect() |>
  dplyr::rename(hex_id = h3id) |>
  tidyr::pivot_wider(
    names_from  = year,
    values_from = conflict_events,
    names_prefix = "conflict_"
  )

hex_data <- dplyr::full_join(hex_data, my_data, by = "hex_id")

# ── D. Aggregate ─────────────────────────────────────────────────────
aggregated <- aggregate_hex_to_shapes(
  hex_data  = hex_data,
  hex_layer = hex_layer,
  shp_dta   = my_shp,
  strategy  = list(
    .default    = c(weight = "pop",  fun = "mean"),
    nightlights = c(weight = "none", fun = "mean"),
    population  = c(weight = "none", fun = "sum"),
    conflict    = c(weight = "none", fun = "sum")
  )
)

# ── E. Build metadata Excel ──────────────────────────────────────────
build_hex_metadata(
  aggregated       = aggregated,
  shp_dta          = my_shp,
  indicator_config = tibble::tibble(
    canonical_name        = "conflict",
    var_name              = "Conflict Events ({year})",
    var_description       = "ACLED conflict event count.",
    var_units             = "count",
    pillar_group          = 4,
    pillar_name           = "Security",
    legend_revert_colours = TRUE,
    fltr_exclude_pti      = FALSE
  ),
  country_name = "Rwanda",
  output_path  = "app-data/metadata-hex.xlsx",
  include_hex  = INCLUDE_HEX_IN_APP
)
```

### Data flow diagram

```
shapes.rds (Step 1 / arch-10)
  │
  ├─ hex_layer$admin9Pcod ──► hex_ids
  │
  ├─ list_hex_vars()         ──► console: available indicators
  ├─ get_available_years()   ──► console: live year list
  │
  ├─ use_hex_vars(..., years) ──► resolved var selection
  │                                 ├─ nearest-year substitution
  │                                 ├─ 7-year max tolerance
  │                                 └─ later-year on tie
  │
  ├─ fetch_hex_data(hex_ids, vars)
  │   ├─ parquet predicate pushdown (hex_col ∈ hex_ids)
  │   ├─ transparent H5→H6 expansion if needed
  │   ├─ long→wide pivot for temporal vars
  │   ├─ population always first after hex_id
  │   └─ progress bar per variable
  │
  ├─ [optional: user parquet merge via full_join]
  │
  ├─ aggregate_hex_to_shapes(hex_data, hex_layer, shp_dta, strategy)
  │   ├─ st_drop_geometry(hex_layer) → grouping keys
  │   ├─ hex → admin<N> directly (never chained)
  │   ├─ weight × fun per variable (stem-based lookup)
  │   ├─ NA → 0; all-NA → NA + warning
  │   └─ progress bar per admin level × variable
  │
  └─ build_hex_metadata(aggregated, shp_dta, indicator_config, ...)
      ├─ registry vars: auto-populate metadata from YAML
      ├─ non-registry vars: use indicator_config
      ├─ temporal vars: expand to one row per year
      ├─ include_hex gating (>5k prompt)
      ├─ registry_version in general sheet
      └─ validate_metadata() before return
            │
            ▼
      app-data/metadata-hex.xlsx
            │
            ▼
      compile_pti_data() (Step 5) ──► metadata.xlsx + shapefiles.zip
            │
            ▼
      launch_pti() (runtime, offline)
```
