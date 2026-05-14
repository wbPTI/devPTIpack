# arch-12 — Hex Registry Catalog Expansion (Space2Stats)

> **Status:** Planning.
>
> **Depends on:** arch-11 (hex data access pipeline — fully landed on `main`).
>
> **Scope:** Expand `inst/hex_vars_registry.yaml` from 1 real indicator
> to the full WB Space2Stats catalog (~108 columns across 6 collections),
> adding a REST backend for climate time series.

---

## 1. Problem Statement

`inst/hex_vars_registry.yaml` v0.1.0 ships with one real indicator
(`flood_exposure_15cm_1in100`) plus the internal `population` variable.
A deployer running the arch-11 pipeline today has no meaningful choice
of hex-derived indicators: they must bring all their own data via the
local-parquet merge step.

The WB Space2Stats Database (https://space2stats.ds.io, open access,
MIT-licensed, unauthenticated) publishes pre-computed H3 Level-6 summary
statistics for the entire globe across 6 thematic collections. Expanding
the registry to cover this catalog turns the PTI hex pipeline into a
one-stop discovery-and-fetch workflow for country deployers.

---

## 2. Space2Stats Catalog Inventory

All collections are at **H3 Level 6** (~36 km² per cell, ~3.67M cells
globally). Sources accessed at https://space2stats.ds.io/docs and
https://worldbank.github.io/DECAT_Space2Stats/.

| Collection ID | Theme | Columns | Temporal | Backend plan |
|---|---|---|---|---|
| `space2stats_population_2020` | Demographics | ~18 WorldPop columns (total pop + age/sex pyramid) | Static snapshot (2020) | parquet |
| `urbanization_ghssmod` | Urbanization | GHS-SMOD degree-of-urbanisation share columns | Static snapshot | parquet |
| `nighttime_lights` | Economic activity | VIIRS annual NTL (per-year columns, 2012–2024) | Annual | parquet |
| `flood_exposure_15cm_1in100` | Climate hazards | Already integrated as `wb_flood_exposure` | Static | parquet ✓ done |
| `builtarea_ghsl` | Infrastructure | GHSL built-up area (per-year columns 1975–2030) | Decadal | parquet |
| `climate` | Climate hazards | Drought SPEI, cyclone, landslide, fires (static) + SPI time series | Mixed | parquet + REST |

**Asset URL discovery** is the first task (Issue A) — the exact HTTPS paths to
the per-collection parquet files must be confirmed from the STAC catalog at
https://worldbank.github.io/DECAT_Space2Stats/readme.html and the WB data
catalog. This document will be updated with confirmed URLs once Issue A lands.

---

## 3. Backend Selection Rationale

### Parquet (primary, 5 collections)

The current `fetch_hex_data()` path reads a parquet file at an HTTPS URL via
`arrow::open_dataset()` with predicate pushdown on the `hex_id` column. This
is efficient (only rows matching the project's hex IDs are read), network-call
once per fetch, and deterministic. All static and temporal-columnar collections
fit this model cleanly.

### REST API (fallback, climate time series only)

The Space2Stats FastAPI (`https://space2stats.ds.io`) exposes:
- `POST /summary_by_hexids` — returns one row per hex for a subset of fields.
- `POST /timeseries_by_hexids` — returns long-format time series for one or
  more hex IDs and one or more time periods.

Materialising the full SPI time series as a parquet file would require either
(a) downloading the global parquet and re-partitioning it, or (b) depending on
a stable DDH download link that the WB may update without notice. The REST
approach fetches only the hex IDs and time periods the deployer actually needs,
with pagination handled transparently by `hex_fetch_source_rest()`.

Both backends return data in the same wide-columnar shape after the
pivot step in `hex_fetch_source()`, so `fetch_hex_data()` and
`aggregate_hex_to_shapes()` are unaware of the backend used.

---

## 4. YAML Schema Additions (Issue F only)

Issues B–E are **YAML-only** PRs: they add new entries under `sources:` using
the existing schema. The existing `backend: "parquet"` behaviour is the
implicit default — no new fields are introduced until Issue F.

Issue F adds three optional source-level keys:

```yaml
sources:
  wb_climate_rest:
    backend: "rest"                          # NEW. Default "parquet" when absent.
    api_root: "https://space2stats.ds.io"   # NEW. Required when backend = "rest".
    collection: "climate"                   # NEW. Required when backend = "rest".
    # path and hex_col are irrelevant when backend = "rest".
    pop_var: ~
    variables:
      spi_3month:
        source_col: "spi_3"
        var_name: "SPI-3 Drought Index ({year})"
        var_description: |
          3-month Standardised Precipitation Index (SPI-3).
          Negative values indicate drought; positive indicate wet conditions.
        var_units: "dimensionless"
        pillar_group: ~
        pillar_name: "Climate hazards"
        legend_revert_colours: true
        time_col: "month"
        available_years: ~
        weight: "area"
        fun: "mean"
        fltr_exclude_pti: false
```

The `backend` field applies to the **source** (all variables under that source
share the same backend). A future arch could support per-variable backends if
needed.

---

## 5. Code Changes (Issue F)

All changes are additive and backward-compatible with the parquet path.

### `R/fct_hex_registry.R`

- `read_hex_registry()`: accept `backend`, `api_root`, `collection` in the
  per-source block. Treat `path` and `hex_col` as optional when
  `backend == "rest"`.
- `pti_hex_source()` constructor: add `backend`, `api_root`, `collection` slots
  (defaulting to `"parquet"`, `NA`, `NA`).
- `pti_hex_var()` constructor: propagate `backend`, `api_root`, `collection` from
  the source so `fetch_hex_data()` can dispatch without re-reading the source map.

### `R/fct_hex_fetch.R`

- `hex_split_by_source()` (line ~140): also split by `backend` within each
  source group (currently moot since all sources are parquet; needed once a
  REST source exists alongside a parquet source).
- `hex_fetch_source()` (line 186): switch on `attr(grp[[1]], "backend")`:
  - `"parquet"` → existing arrow path (unchanged).
  - `"rest"` → call new internal `hex_fetch_source_rest()`.
- New internal `hex_fetch_source_rest(grp, fetch_ids)`:
  - Determines whether `time_col` is set (→ `timeseries_by_hexids` endpoint vs
    `summary_by_hexids`).
  - Chunks `fetch_ids` into batches of 5,000 and POSTs to
    `<api_root>/<endpoint>`.
  - Collects and binds response pages.
  - Pivots long → wide (`<canonical>_<year>`) to match the parquet return shape.
- `get_available_years()` (line ~411): for `backend == "rest"` sources, call
  `<api_root>/fields` and filter for the matching collection and field instead of
  opening a parquet.

### `R/fct_hex_vars.R`

- `use_hex_vars()`: propagate `backend`, `api_root`, `collection` into each
  returned `pti_hex_var` (no behavioural change for parquet vars).

### Tests

| File | Change |
|---|---|
| `tests/testthat/test-hex-registry.R` | Add fixture with `backend: "rest"` entry; assert `backend`, `api_root`, `collection` slots land correctly on `pti_hex_var` objects. |
| `tests/testthat/test-hex-fetch.R` | Add REST-backend dispatch test using `with_mock` / `local_mocked_bindings` around `hex_fetch_source_rest()`. |
| `tests/testthat/test-hex-fetch-rest.R` | New file — unit tests for `hex_fetch_source_rest()`: chunking (>5,000 IDs splits into multiple POSTs), pagination (multi-page response binds correctly), pivot shape matches parquet-path output columns. |

---

## 6. Implementation Order

```
Issue A  Discover + document Space2Stats asset URLs   (research, no code)
    │
    ├─► B  population_2020     (YAML-only, can run parallel with C/D/E)
    ├─► C  urbanization_ghssmod
    ├─► D  nighttime_lights
    └─► E  builtarea_ghsl
                │
                └─► F  REST backend + climate static columns   (code PR)
                            │
                            └─► G  Climate time series via REST   (YAML-only)
```

B–E can be opened and reviewed in parallel once A lands. F depends on the
URL inventory from A and on the parquet-backend issues being fully green.

---

## 7. Definition of Done

- [ ] Issue A: all 5 static collection URLs confirmed and recorded in this doc.
- [ ] Issue B: `list_hex_vars()` returns WorldPop population + age/sex columns.
- [ ] Issue C: `list_hex_vars()` returns GHS-SMOD urbanisation variables.
- [ ] Issue D: `list_hex_vars()` returns VIIRS NTL variables (2012–2024).
- [ ] Issue E: `list_hex_vars()` returns GHSL built-area variables.
- [ ] Issue F: `backend: "rest"` parses correctly; climate static columns
      fetchable; `R CMD check` 0/0/0; `test-hex-fetch-rest.R` green.
- [ ] Issue G: `get_available_years("spi_3month")` returns live SPI month list.
- [ ] End-to-end: Rwanda (`rwa_shp`, 507 H3-6 cells) pipeline run fetching
      one variable from each new collection completes without warnings.
- [ ] `list_hex_vars()` returns ≥ 100 variables (vs. 2 today).

---

## 8. What This Spec Does Not Cover

- Self-hosted Space2Stats mirror (future work if WB infra reliability is needed).
- Authenticated / private datasets (Space2Stats is open).
- Partitioned-parquet directory support — promoted to a prerequisite issue if
  Issue A discovers a dataset that needs it.
- Datasets not at H3-6 (none currently in Space2Stats; guarded by a test in F
  that asserts `source_res == 6L` for every REST source).
