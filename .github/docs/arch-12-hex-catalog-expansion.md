# arch-12 — Hex Registry Catalog Expansion (Space2Stats)

> **Status:** In progress — §A URL discovery complete (2026-05-14).
>
> **Depends on:** arch-11 (hex data access pipeline — fully landed on `main`).
>
> **Scope:** Expand `inst/hex_vars_registry.yaml` from 1 real indicator
> to the full WB Space2Stats catalog (~120 fields across 6 thematic
> collections), adding a REST backend for all non-flood collections.

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

**Discovery date:** 2026-05-14. Source: `GET https://space2stats.ds.io/fields`
plus WB Data Catalog cross-check.

All collections are at **H3 Level 6** (~36 km² per cell, ~3.67M cells
globally). The hex ID column is uniformly named **`hex_id`** across all
data.

### Key architectural finding

The existing `flood_exposure_15cm_1in100.parquet` (DDH URL) contains **only**
`hex_id`, `pop`, and `pop_flood` — 3 columns, 3.67M rows. The other ~117
fields returned by `/fields` are **not** present in that file. No separate
public HTTPS parquet URLs were found for any other Space2Stats collection.

**All 120 fields are accessible via the REST API** at `https://space2stats.ds.io`
using the `/summary_by_hexids` endpoint (POST a list of hex IDs + field names,
receive a columnar JSON response). This makes the REST adapter the primary
(not fallback) access path for all new collections.

### Complete field inventory from `/fields`

```
# Flood / population (existing source wb_flood_exposure):
pop, pop_flood, pop_flood_pct

# Nighttime lights (VIIRS, annual 2012–2024):
sum_viirs_ntl_2012, sum_viirs_ntl_2013, sum_viirs_ntl_2014,
sum_viirs_ntl_2015, sum_viirs_ntl_2016, sum_viirs_ntl_2017,
sum_viirs_ntl_2018, sum_viirs_ntl_2019, sum_viirs_ntl_2020,
sum_viirs_ntl_2021, sum_viirs_ntl_2022, sum_viirs_ntl_2023,
sum_viirs_ntl_2024

# Built-up area (GHSL, decadal 1975–2030):
sum_built_area_m_1975, sum_built_area_m_1980, sum_built_area_m_1985,
sum_built_area_m_1990, sum_built_area_m_1995, sum_built_area_m_2000,
sum_built_area_m_2005, sum_built_area_m_2010, sum_built_area_m_2015,
sum_built_area_m_2020, sum_built_area_m_2025, sum_built_area_m_2030

# GHS-SMOD urbanisation (static):
ghs_11_count, ghs_12_count, ghs_13_count,   # Rural cluster / low / very low
ghs_21_count, ghs_22_count, ghs_23_count,   # Suburban / dense urban / urban centre (non-pop)
ghs_30_count,                                # Water
ghs_total_count,
ghs_11_pop, ghs_12_pop, ghs_13_pop,
ghs_21_pop, ghs_22_pop, ghs_23_pop,
ghs_30_pop, ghs_total_pop

# Population demographics (WorldPop, annual projections 2015–2030):
sum_pop_2015, sum_pop_2016, sum_pop_2017, sum_pop_2018, sum_pop_2019,
sum_pop_2020, sum_pop_2021, sum_pop_2022, sum_pop_2023, sum_pop_2024,
sum_pop_2025, sum_pop_2026, sum_pop_2027, sum_pop_2028,
sum_pop_2029, sum_pop_2030

# Population age/sex pyramid (WorldPop, 2025 snapshot):
sum_f_00_2025, sum_f_01_2025, sum_f_05_2025, sum_f_10_2025,
sum_f_15_2025, sum_f_20_2025, sum_f_25_2025, sum_f_30_2025,
sum_f_35_2025, sum_f_40_2025, sum_f_45_2025, sum_f_50_2025,
sum_f_55_2025, sum_f_60_2025, sum_f_65_2025, sum_f_70_2025,
sum_f_75_2025, sum_f_80_2025, sum_f_85_2025, sum_f_90_2025,
sum_m_00_2025, sum_m_01_2025, sum_m_05_2025, sum_m_10_2025,
sum_m_15_2025, sum_m_20_2025, sum_m_25_2025, sum_m_30_2025,
sum_m_35_2025, sum_m_40_2025, sum_m_45_2025, sum_m_50_2025,
sum_m_55_2025, sum_m_60_2025, sum_m_65_2025, sum_m_70_2025,
sum_m_75_2025, sum_m_80_2025, sum_m_85_2025, sum_m_90_2025,
sum_f_2025, sum_m_2025   # aggregate male/female totals

# Climate hazards (static snapshots):
fires_density_mean,
cy_frequency_mean,                    # cyclone frequency
landslide_susceptibility_mean_2023,
drought_spei_1_5_rp100_mean           # SPEI 1-in-100-year drought
```

**SPI time series** (monthly, via `/timeseries_by_hexids`) is confirmed
available through the API but not listed in the flat `/fields` endpoint —
it is returned by the separate timeseries endpoint and is the only true
time-series variable. Field name TBD (issue G).

### Revised collection grouping

| Collection | Pillar | Fields used | Format | Backend |
|---|---|---|---|---|
| `space2stats_population_2020` | Demographics | `sum_pop_*`, `sum_f_*_2025`, `sum_m_*_2025` | Wide (years in col name) | REST |
| `urbanization_ghssmod` | Urbanization | `ghs_*_count`, `ghs_*_pop` | Static | REST |
| `nighttime_lights` | Economic activity | `sum_viirs_ntl_*` | Wide (years in col name) | REST |
| `flood_exposure_15cm_1in100` | Climate hazards | `pop_flood`, `pop_flood_pct` | Static | parquet ✓ done |
| `builtarea_ghsl` | Infrastructure | `sum_built_area_m_*` | Wide (years in col name) | REST |
| `climate` | Climate hazards | `fires_density_mean`, `cy_frequency_mean`, `landslide_susceptibility_mean_2023`, `drought_spei_1_5_rp100_mean` | Static | REST |
| `climate_timeseries` | Climate hazards | SPI monthly (TBD) | Time series | REST (timeseries endpoint) |

---

## 3. Backend Selection Rationale (Revised)

### Original plan vs. reality

The original arch-12 plan assumed most collections would be accessible as
separate HTTPS parquet files (one per collection), with REST used only for
the climate SPI time series.

**Reality:** No public HTTPS parquet URLs exist for any collection other than
`flood_exposure_15cm_1in100`. All other fields are only accessible via the
Space2Stats REST API.

### REST API (primary for all new additions)

`POST https://space2stats.ds.io/summary_by_hexids`

Request body:
```json
{
  "hex_ids": ["866d0ce0fffffff", "866d0ce4fffffff", ...],
  "fields": ["sum_viirs_ntl_2020", "sum_viirs_ntl_2022", "ghs_total_pop"]
}
```

Response: JSON with one row per hex_id plus the requested fields.

- **Chunking**: The API likely has a per-request hex ID limit. `hex_fetch_source_rest()` will POST in batches of 5,000 hex IDs.
- **Field selection**: Each variable's `source_col` is the exact field name (or a template for temporal columns).
- **Return shape**: Responses are already in wide format — no pivoting needed for static columns.

### Temporal handling (wide-format columns)

The Space2Stats REST API returns temporal data **already wide** — years are
embedded in column names (e.g. `sum_viirs_ntl_2020`), not in a separate
`year` column. This differs from the long-format parquet design assumed in
arch-11.

**Solution:** Introduce a `source_col_template` YAML field as an alternative
to `source_col`. When set, the resolver substitutes `{year}` from the user's
`years =` argument:

```yaml
# In the YAML:
nightlights:
  source_col_template: "sum_viirs_ntl_{year}"  # {year} replaced at fetch time
  available_years: [2012, 2013, ..., 2024]
  time_col: ~   # no long-format time column; year is in the col name
```

When the deployer calls `use_hex_vars("nightlights", years = c(2020, 2022))`:
1. The resolver confirms 2020 and 2022 are in `available_years`.
2. The REST adapter requests fields `["sum_viirs_ntl_2020", "sum_viirs_ntl_2022"]`.
3. The adapter renames them `nightlights_2020`, `nightlights_2022` before returning.

For non-temporal (static) variables, `source_col` continues to be used as-is.

### Parquet (retained for existing `wb_flood_exposure` only)

The existing `flood_exposure_15cm_1in100.parquet` DDH URL works and its
data is already in the registry. No change — the parquet backend is NOT
extended to other sources.

---

## 4. YAML Schema Additions (Issues B–G)

### New source-level fields (added in Issue F)

```yaml
sources:
  wb_space2stats_api:
    backend: "rest"                          # NEW. Default "parquet" when absent.
    api_root: "https://space2stats.ds.io"   # NEW. Required when backend = "rest".
    hex_col: "hex_id"
    pop_var: ~
    variables:
      ...
```

### New variable-level fields (added in Issue F)

```yaml
variables:
  nightlights:
    source_col_template: "sum_viirs_ntl_{year}"  # NEW. Mutually exclusive with source_col.
    available_years: [2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,
                      2021, 2022, 2023, 2024]
    time_col: ~                              # null — year is in col name, not a separate col
    ...
```

The existing `source_col` (exact field name) continues to work for static variables.
`source_col_template` is only used when `time_col: ~` and years are embedded
in the column name.

### Example — all sub-sources under one `wb_space2stats_api` source

All new variables (issues B–G) will live under a single source entry
`wb_space2stats_api`, keeping the registry minimal:

```yaml
sources:
  wb_flood_exposure:          # existing — unchanged
    ...

  wb_space2stats_api:         # new — covers all REST collections
    backend: "rest"
    api_root: "https://space2stats.ds.io"
    hex_col: "hex_id"
    pop_var: ~
    variables:
      # Issue B — Demographics
      pop_2020:
        source_col: "sum_pop_2020"
        var_name: "Population (2020)"
        pillar_name: "Demographics"
        ...
      pop_female_2025:
        source_col: "sum_f_2025"
        ...
      # Issue C — Urbanization
      urban_pop:
        source_col: "ghs_30_pop"    # urban centre pop (GHS-SMOD class 30)
        pillar_name: "Urbanization"
        ...
      # Issue D — Nighttime lights
      nightlights:
        source_col_template: "sum_viirs_ntl_{year}"
        available_years: [2012, 2013, 2014, 2015, 2016, 2017,
                          2018, 2019, 2020, 2021, 2022, 2023, 2024]
        pillar_name: "Economic activity"
        ...
      # Issue E — Built-up area
      builtarea:
        source_col_template: "sum_built_area_m_{year}"
        available_years: [1975, 1980, 1985, 1990, 1995, 2000,
                          2005, 2010, 2015, 2020, 2025, 2030]
        pillar_name: "Infrastructure"
        ...
      # Issue F — Climate static
      fires_density:
        source_col: "fires_density_mean"
        pillar_name: "Climate hazards"
        ...
      cyclone_frequency:
        source_col: "cy_frequency_mean"
        ...
      landslide_susceptibility:
        source_col: "landslide_susceptibility_mean_2023"
        ...
      drought_spei:
        source_col: "drought_spei_1_5_rp100_mean"
        legend_revert_colours: true
        ...
      # Issue G — Climate time series (SPI)
      # spi_3month: source_col_template TBD (from timeseries endpoint)
```

---

## 5. Code Changes (Issue F — the only code-bearing issue)

> **Dependency change:** Issues B–E are YAML-only PRs but now **depend on Issue F**
> (REST backend) being merged first, because their variables need the REST
> dispatch path in `hex_fetch_source()`.

### `R/fct_hex_registry.R`

- `read_hex_registry()`:
  - Accept `backend`, `api_root` in the per-source block.
  - Accept `source_col_template` in the per-variable block (validated as a
    glue template containing `{year}`).
  - Treat `path` and `hex_col.path` as optional when `backend == "rest"`.
- `pti_hex_source()`: add `backend` slot (default `"parquet"`), `api_root` slot.
- `pti_hex_var()`: propagate `backend`, `api_root`, and `source_col_template`
  (alongside existing `source_col`) from the source/variable.

### `R/fct_hex_vars.R`

- `use_hex_vars()`: when a var has `source_col_template`, replace `{year}` with
  each resolved year to build the per-year request field names.
  Store them in a new `resolved_cols` slot on `pti_hex_var`.

### `R/fct_hex_fetch.R`

- `hex_split_by_source()` (~line 140): split by `(source_id, backend)`.
- `hex_fetch_source()` (line 186): switch on `attr(grp[[1]], "backend")`:
  - `"parquet"` → existing arrow path (unchanged).
  - `"rest"` → new `hex_fetch_source_rest()`.
- New `hex_fetch_source_rest(grp, fetch_ids)`:
  - Collects all `source_col` / `resolved_cols` values from the group.
  - Chunks `fetch_ids` into batches of 5,000.
  - POSTs to `<api_root>/summary_by_hexids` with `{"hex_ids": batch, "fields": cols}`.
  - Binds response pages; renames response columns to canonical names
    (or `<canonical>_<year>` for template variables).
  - Returns same shape as the parquet path (one row per hex, canonical column names).
- `get_available_years()` (~line 411): for `backend == "rest"` sources,
  call `<api_root>/fields` and filter matching the `source_col_template` pattern
  to extract the set of available years.

### Tests

| File | Change |
|---|---|
| `tests/testthat/test-hex-registry.R` | Fixture with `backend: "rest"` + `source_col_template`; assert slots land on `pti_hex_var`. |
| `tests/testthat/test-hex-vars.R` | Add test that `use_hex_vars("nightlights", years = c(2020, 2022))` produces a `pti_hex_var` with `resolved_cols = c("sum_viirs_ntl_2020", "sum_viirs_ntl_2022")`. |
| `tests/testthat/test-hex-fetch.R` | REST dispatch test with `local_mocked_bindings` on `hex_fetch_source_rest`. |
| `tests/testthat/test-hex-fetch-rest.R` (new) | Unit tests: chunking >5,000 IDs, response binding, column rename, template variable pivot shape. |

---

## 6. Implementation Order (Revised)

```
Issue A  URL discovery + this doc update              ← done (2026-05-14)
    │
    └─► F  REST backend + climate static + YAML schema additions
                │
                ├─► B  pop_2020 + age/sex pyramid vars (YAML-only, depends on F)
                ├─► C  GHS-SMOD urbanisation vars     (YAML-only, depends on F)
                ├─► D  nighttime lights vars           (YAML-only, depends on F)
                ├─► E  built-up area vars              (YAML-only, depends on F)
                └─► G  SPI time series vars            (YAML-only, depends on F)
```

Issues B–E and G can be reviewed in parallel once F lands. F is the
only PR that modifies R code.

---

## 7. Definition of Done

- [x] Issue A: field inventory complete, architecture revised (this PR).
- [ ] Issue F: `backend: "rest"` + `source_col_template` parse correctly;
      REST dispatch works; climate static columns fetchable; `R CMD check` 0/0/0;
      `test-hex-fetch-rest.R` green.
- [ ] Issue B: `list_hex_vars()` returns WorldPop population and age/sex columns.
- [ ] Issue C: `list_hex_vars()` returns GHS-SMOD urbanisation variables.
- [ ] Issue D: `list_hex_vars()` returns VIIRS NTL variables (2012–2024).
- [ ] Issue E: `list_hex_vars()` returns GHSL built-area variables.
- [ ] Issue G: `get_available_years("spi_3month")` returns the live SPI period list.
- [ ] End-to-end: Rwanda (`rwa_shp`, 507 H3-6 cells) pipeline run fetching
      one variable from each new collection completes without warnings.
- [ ] `list_hex_vars()` returns ≥ 100 variables (vs. 2 today).

---

## 8. What This Spec Does Not Cover

- Self-hosted Space2Stats mirror.
- Authenticated / private datasets (Space2Stats is open).
- Partitioned-parquet directory support (not needed — REST is the access path).
- Datasets not at H3-6 (none currently in Space2Stats).
- Full age/sex pyramid registration (40 columns) — Issues B's PR decides how
  many columns to expose; registering all 40 is valid but may clutter
  `list_hex_vars()`. Suggestion: register `sum_f_2025` / `sum_m_2025`
  (aggregate totals) plus a handful of representative age brackets.
