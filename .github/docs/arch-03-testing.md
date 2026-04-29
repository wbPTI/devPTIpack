# Testing Framework for Post-Cleanup `devPTIpack`

> Testing plan for active functions retained after architecture cleanup.
> Designed as **console-first** — verify all logic without launching Shiny,
> then layer on module-level and UI-level tests.

---

## Current State

| Metric                             | Value    |
| ---------------------------------- | -------- |
| Exported functions                 | ~97      |
| Functions with real tests          | ~14      |
| **Estimated coverage**             | **~14%** |
| Dead tests (commented/placeholder) | 2 files  |

**Strongest coverage:** PTI calc pipeline (weighting, scoring, aggregation), shape utilities.
**Largest gaps:** Validation, Shiny modules, plotting/legend, data export.

---

## Testing Strategy: Three Tiers

```
┌─────────────────────────────────────────────────────┐
│ Tier 3: UI Integration (shinytest2)                │  ← user tests manually
│   • Full app end-to-end                            │     after Tier 1+2 pass
│   • Weights → Calculate → Map renders             │
└─────────────────────────────────────────────────────┘
┌─────────────────────────────────────────────────────┐
│ Tier 2: Module Servers (shiny::testServer)          │  ← automated
│   • Reactive chains                                │
│   • Module I/O contracts                           │
└─────────────────────────────────────────────────────┘
┌─────────────────────────────────────────────────────┐
│ Tier 1: Pure Functions (testthat, no Shiny)         │  ← automated, fast
│   • All calc/data/validation/legend functions       │
│   • Run in console, CI-friendly                    │
└─────────────────────────────────────────────────────┘
```

**Rule:** Tier 1 must pass completely before running Tier 2.
Tier 2 must pass completely before user tests Tier 3 (UI).

---

## Test Data Fixtures

All tests use the bundled package data:

```r
data("ukr_shp", package = "devPTIpack")
data("ukr_mtdt_full", package = "devPTIpack")
```

Additional fixtures needed (create in `tests/testthat/fixtures/`):

| Fixture                   | Purpose                                                   |
| ------------------------- | --------------------------------------------------------- |
| `single_admin_shapes.rds` | Shapes with only 1 admin level — edge case                |
| `empty_weights.rds`       | `weights_clean` list where all weights = 0                |
| `all_na_data.rds`         | Admin data where all indicator values are NA              |
| `minimal_metadata.xlsx`   | Smallest valid metadata file (1 indicator, 1 admin level) |

---

## Tier 1: Pure Function Tests (Console Mode)

### 1.1 Calculation Pipeline — `test-calc-pipeline.R`

> **Moved to dedicated document:** See [`arch-02.01-testing-calc-pipeline.md`](arch-02.01-testing-calc-pipeline.md)
>
> Covers ~71 test cases across three levels:
> - **Level A:** Unit tests for all 11 pipeline functions
> - **Level B:** Integration tests for adjacent function pairs
> - **Level C:** End-to-end tests via `run_pti_pipeline()` orchestrator
> - **Edge cases:** Single admin, negative weights, overflow, determinism

---

### 1.2 Indicator List & Data Cleaning — `test-indicators-list.R`

#### `get_indicators_list(dta, fltr_var)`

| Test Case                  | Input                                  | Expected                         |
| -------------------------- | -------------------------------------- | -------------------------------- |
| Normal extraction          | `ukr_mtdt_full`                        | Tibble with 10 columns, > 0 rows |
| All excluded               | Modified metadata (all TRUE in filter) | 0-row tibble                     |
| Non-existent filter column | `fltr_var = "bogus"`                   | Error or graceful empty          |
| admin_levels_years nested  | Any                                    | Each row has nested tibble       |
| Vars not in data sheets    | Extra metadata row                     | Excluded (no data)               |
| No year column             | Data without `year` col                | Uses "all" as year placeholder   |

---

### 1.3 I/O & Template Reader — `test-template-reader.R`

#### `fct_template_reader(...)`

| Test Case                | Input                            | Expected                                                 |
| ------------------------ | -------------------------------- | -------------------------------------------------------- |
| Valid xlsx               | Sample metadata xlsx             | Named list with `$metadata`, `$weights_clean`, `$admin*` |
| `fltr_*` columns coerced | File with "TRUE"/"FALSE" strings | Logical type in output                                   |
| Extra sheets preserved   | File with custom sheet           | Appears in output list                                   |
| Empty admin sheet        | Sheet with no valid var_codes    | Excluded from output                                     |
| No weights_table sheet   | File without it                  | `$weights_clean = NULL`                                  |

#### `fct_convert_weight_to_clean(dta)`

| Test Case               | Input                | Expected                                         |
| ----------------------- | -------------------- | ------------------------------------------------ |
| Normal conversion       | Sample weights_table | Named list of tibbles with `var_code` + `weight` |
| Single weight scheme    | 1 `ws1..` prefix     | 1-element list                                   |
| Extract names correctly | `ws1..My Weights`    | Name = "My Weights"                              |

---

### 1.4 Validation — `test-validators.R`

#### `validate_geometries(existing_shapes)` / `validate_single_geom(focus_geom, full_geom)`

| Test Case            | Input                           | Expected             |
| -------------------- | ------------------------------- | -------------------- |
| Valid 3-level shapes | `ukr_shp`                       | No errors, no aborts |
| Non-sf object        | `list(admin1_X = data.frame())` | Abort                |
| Missing Pcod column  | sf without `admin1Pcod`         | Abort                |
| Duplicate Pcods      | Modified sf with dups           | Warning              |
| NA in Pcod           | Modified sf                     | Warning              |
| Wrong geometry type  | LINESTRING sf                   | Warning              |
| Orphan child Pcods   | Child Pcod not in parent        | Warning              |
| Root level (admin0)  | Single top-level                | Skips parent check   |

---

### 1.5 Legend & Palette — `test-legend-palette.R`

#### `legend_map_satelite(leg_vals, n_groups, ...)`

| Test Case                | Input                            | Expected                                   |
| ------------------------ | -------------------------------- | ------------------------------------------ |
| Normal continuous        | `c(1, 5, 3, 7, 2)`, `n_groups=3` | `$pal` is function, `$our_labels` length 3 |
| All NA                   | `c(NA, NA, NA)`                  | Single "No data" label                     |
| Single unique value      | `c(5, 5, 5)`                     | Constant color function                    |
| Binary 0/1               | `c(0, 1, 0, 1)`                  | `colorFactor` with 2 levels                |
| Color reversal           | `legend_revert_colours = TRUE`   | Palette reversed vs FALSE                  |
| n_groups > unique values | 10 bins for 3 values             | Clamped appropriately                      |
| Categorical              | `is_categorical = TRUE`          | Factor-based palette                       |
| Mixed NA + values        | `c(1, NA, 3)`                    | Labels include "No data"                   |

#### `recode_val_base(our_breaks, our_labels_category, no_data_label)`

| Test Case     | Input                          | Expected                            |
| ------------- | ------------------------------ | ----------------------------------- |
| Normal recode | `c(0,5,10)`, `c("Low","High")` | Function maps 3 → "Low", 7 → "High" |
| NA input      | Any, value=NA                  | Returns `no_data_label`             |
| Single break  | `c(5)`                         | Patched to avoid cut() error        |

---

### 1.6 Plot Data Helpers — `test-plot-helpers.R`

#### `preplot_reshape_wghtd_dta(wghtd_dta)`

| Test Case              | Input                       | Expected                                                           |
| ---------------------- | --------------------------- | ------------------------------------------------------------------ |
| Normal reshape         | `structure_pti_data` output | Flat list with `$pti_dta`, `$pti_codes`, `$admin_level` per scheme |
| Single weight scheme   | 1-scheme input              | 1-element per admin level                                          |
| Output has sf geometry | Any                         | `$pti_dta` inherits "sf"                                           |

#### `filter_admin_levels(dta, to_fltr)`

| Test Case           | Input       | Expected                  |
| ------------------- | ----------- | ------------------------- |
| `"all"`             | Any         | Returns input unchanged   |
| Specific level      | `"admin1"`  | Only admin1 elements kept |
| Non-matching filter | `"admin99"` | Returns NULL              |
| NULL filter         | `NULL`      | Returns NULL              |

#### `add_legend_paras(dta, nbins)`

| Test Case    | Input              | Expected                                  |
| ------------ | ------------------ | ----------------------------------------- |
| Normal       | Valid preplot data | Each element has `$leg` with palette info |
| NULL input   | `NULL`             | Returns NULL                              |
| Custom nbins | `nbins = 3`        | Legend has ≤ 3 groups                     |

#### `complete_pti_labels(dta)`

| Test Case  | Input            | Expected                                |
| ---------- | ---------------- | --------------------------------------- |
| Normal     | Data with `$leg` | `pti_label` enriched with priority rank |
| NULL input | `NULL`           | Returns NULL                            |

#### `check_existing_groups(cur_grps, old_grps, priority_group)`

| Test Case      | Input          | Expected                                      |
| -------------- | -------------- | --------------------------------------------- |
| Overlap exists | Same groups    | `$out_show` = overlapping, `$out_hide` = rest |
| No overlap     | Disjoint sets  | `$out_show` = first of current                |
| Empty old      | `character(0)` | `$out_show` = first of current                |
| Empty current  | `character(0)` | `$out_show` = character(0)                    |

---

### 1.7 Admin Level Dropping — `test-drop-inval-adm.R`

#### `get_vars_un_avbil(ind_list, admin_levels)`

| Test Case        | Input                     | Expected                                 |
| ---------------- | ------------------------- | ---------------------------------------- |
| Some unavailable | indicators_list with gaps | Tibble of unavailable var × admin combos |
| All available    | Complete data             | 0-row tibble                             |
| Expected columns | Any                       | Has `var_code`, `admin_level`            |

#### `get_min_admin_wght(un_available_vars, wght_list)`

| Test Case                        | Input                          | Expected                           |
| -------------------------------- | ------------------------------ | ---------------------------------- |
| Has drops                        | Unavailable + non-zero weights | Named list of admin levels to drop |
| No drops                         | All available                  | Empty character vectors            |
| Zero-weight variable unavailable | Weight=0 for missing var       | Not flagged for drop               |

#### `drop_inval_adm(dta, adm_to_drop)`

| Test Case    | Input                   | Expected                |
| ------------ | ----------------------- | ----------------------- |
| Drop admin1  | Plot data with admin1+2 | admin1 elements removed |
| Drop nothing | Empty drop list         | Input unchanged         |
| Drop all     | All levels              | Empty list              |

---

### 1.8 Export Functions — `test-export.R`

#### `get_pti_scores_export(plotted_dta)`

| Test Case             | Input                     | Expected                                             |
| --------------------- | ------------------------- | ---------------------------------------------------- |
| Normal export         | Valid structured PTI data | Named list of tibbles (1 per admin) without geometry |
| Score columns renamed | Any                       | Human-readable column names                          |
| No geometry in output | Any valid                 | No `geometry`/`sfc` columns                          |

#### `get_pti_weights_export(wghts_dta, indic_dta)`

| Test Case        | Input                | Expected                                            |
| ---------------- | -------------------- | --------------------------------------------------- |
| Normal export    | Weights + indicators | Tibble with scheme name, var_code, var_name, weight |
| Multiple schemes | 3 schemes            | All represented                                     |

#### `fct_inp_for_exp(dta)`

| Test Case            | Input           | Expected                                 |
| -------------------- | --------------- | ---------------------------------------- |
| Normal               | `ukr_mtdt_full` | Named list with renamed columns          |
| Columns use var_name | Any             | No `var_code` column names in admin data |
| All excluded         | All filtered    | Empty output                             |

#### `fct_internal_wt_to_exp(weights_clean, indicators_list)`

| Test Case     | Input                      | Expected                                                             |
| ------------- | -------------------------- | -------------------------------------------------------------------- |
| Normal        | Valid weights + indicators | Single tibble with `var_code`, `var_name`, `weight`, `weight_scheme` |
| Empty weights | Empty list                 | 0-row tibble                                                         |

---

### 1.9 Explorer Helpers — `test-explorer-helpers.R`

#### `reshaped_explorer_dta(long_dta, ind_list)`

| Test Case       | Input                     | Expected                             |
| --------------- | ------------------------- | ------------------------------------ |
| Normal reshape  | Pivoted data + indicators | Structured list suitable for mapping |
| Single variable | 1-var ind_list            | 1 element in output                  |

#### `get_var_choices(indicators_list)`

| Test Case | Input           | Expected                                          |
| --------- | --------------- | ------------------------------------------------- |
| Normal    | indicators_list | Named vector (names = display, values = var_code) |
| Empty     | Empty tibble    | Empty named vector                                |

#### `filter_var_explorer(preplot_dta, vars)`

| Test Case           | Input                   | Expected             |
| ------------------- | ----------------------- | -------------------- |
| Single var selected | preplot_dta, 1 var_code | Filtered to that var |
| Non-matching var    | "bogus"                 | Empty/NULL           |

---

### 1.10 Map Rendering (non-interactive) — `test-map-render.R`

#### `make_ggmap(plotted_dta, selected_layer, shp_dta, ...)`

| Test Case          | Input        | Expected             |
| ------------------ | ------------ | -------------------- |
| Returns ggplot     | Valid inputs | Object inherits "gg" |
| Single admin level | 1-level data | No error             |

#### `make_gg_line_map(shp_dta, ...)`

| Test Case      | Input     | Expected             |
| -------------- | --------- | -------------------- |
| Returns ggplot | `ukr_shp` | Object inherits "gg" |

#### `plot_leaf_line_map2(leaf_map, shps_dta, show_adm_levels)`

| Test Case                | Input                   | Expected                  |
| ------------------------ | ----------------------- | ------------------------- |
| Returns leaflet          | `leaflet()` + `ukr_shp` | Object inherits "leaflet" |
| Respects show_adm_levels | Subset vector           | Only those levels drawn   |

---

### 1.11 DT Construction (non-reactive parts) — `test-dt-construction.R`

#### `prep_input_data(ind_list, ns)`

| Test Case                   | Input                              | Expected                             |
| --------------------------- | ---------------------------------- | ------------------------------------ |
| Returns tibble              | indicators_list + `identity` as ns | Tibble with widget HTML columns      |
| Has numericInput HTML       | Any                                | Cells contain `<input type="number"` |
| Row count = indicator count | Any                                | `nrow(result) == nrow(ind_list)`     |

#### `make_vis_targets_for_dt(nested_dta)`

| Test Case                       | Input             | Expected                        |
| ------------------------------- | ----------------- | ------------------------------- |
| Produces column visibility list | Nested DT data    | list with `$targets`            |
| Pillar grouping                 | Multi-pillar data | Groups reflect pillar structure |

---

## Tier 2: Module Server Tests (`testServer`)

These require `library(shiny)` and use `shiny::testServer()`.
Run after Tier 1 passes.

### 2.1 `mod_calc_pti2_server` — `test-mod-calc-pti2.R`

```r
testServer(mod_calc_pti2_server, args = list(
  shp_dta = reactive(ukr_shp),
  input_dta = reactive(ukr_mtdt_full),
  wt_dta = reactive(list(
    weights_clean = ukr_mtdt_full$weights_clean,
    indicators_list = get_indicators_list(ukr_mtdt_full)
  ))
), {
  # Trigger calculation
  session$setInputs(...)
  
  # Tests:
  # 1. calc_pti() returns a non-NULL list
  # 2. Each element has $pti_data (sf), $pti_codes, $admin_level
  # 3. pti_score column exists in $pti_data
  # 4. No crash on all-zero weights
  # 5. No crash on single-indicator weight
})
```

| Test Case                     | What to Verify                                           |
| ----------------------------- | -------------------------------------------------------- |
| Happy path                    | `calc_pti()` returns structured list with expected slots |
| All-zero weights              | Returns valid structure (all scores = 0)                 |
| Single indicator              | Works with 1-var weight list                             |
| Weight change triggers recalc | Changing `wt_dta` invalidates `calc_pti`                 |
| Deduplication                 | Same weights sent twice → no recalc                      |

---

### 2.2 `mod_DT_inputs_server` — `test-mod-dt-inputs.R`

| Test Case           | What to Verify                                                   |
| ------------------- | ---------------------------------------------------------------- |
| Initial render      | `current_values()` has all expected `var_code` rows              |
| Update from outside | Setting `update_dta()` → `current_values()` reflects new weights |
| All-zero button     | Triggers → all weights = 0                                       |
| All-one button      | Triggers → all weights = 1                                       |
| Output throttled    | Rapid changes → single debounced update                          |

---

### 2.3 `mod_drop_inval_adm` — `test-mod-drop-inval-adm.R`

| Test Case           | What to Verify                                      |
| ------------------- | --------------------------------------------------- |
| No drops needed     | All data available → output == input                |
| Admin level dropped | Missing data at admin1 → admin1 removed from output |
| Notification fired  | showNotification called when levels dropped         |

---

### 2.4 `mod_get_admin_levels_srv` — `test-mod-admin-levels.R`

| Test Case                   | What to Verify                                  |
| --------------------------- | ----------------------------------------------- |
| Default selection           | Returns expected default admin level            |
| `show_adm_levels` filtering | Only allowed levels in choices                  |
| Update on data change       | New data with different levels → choices update |

---

### 2.5 `mod_fltr_sel_var2_srv` — `test-mod-var-selector.R`

| Test Case                        | What to Verify                                  |
| -------------------------------- | ----------------------------------------------- |
| Initial choices populated        | `updateSelectInput` called with correct choices |
| Selection produces filtered data | Select var → output matches filter              |
| `add_selected` override          | External selection triggers update              |

---

### 2.6 `mod_wt_save_newsrv` — `test-mod-wt-save.R`

| Test Case                       | What to Verify                         |
| ------------------------------- | -------------------------------------- |
| Save new scheme                 | Click save → scheme appears in storage |
| Overwrite existing              | Same name + save → updated weights     |
| Button disabled when empty name | No name → save button disabled         |
| Delete scheme                   | Delete → scheme removed from storage   |

---

### 2.7 `mod_export_pti_data_server` — `test-mod-export-data.R`

| Test Case                 | What to Verify                                       |
| ------------------------- | ---------------------------------------------------- |
| Returns named list        | Valid plotted_dta + weights → list of export tibbles |
| Contains scores per admin | Each admin level has a tibble                        |
| Contains weights          | Weight scheme info included                          |

---

## Tier 3: UI Integration (Manual + shinytest2)

**Prerequisite:** Tier 1 and Tier 2 all pass.

### 3.1 Smoke Test Script (for user)

```r
# Run this in console — launches app for manual testing
library(devPTIpack)
data("ukr_shp")
data("ukr_mtdt_full")

app <- launch_pti(
  shp_dta = ukr_shp,
  inp_dta = ukr_mtdt_full,
  app_title = "Post-Cleanup Test"
)
```

### 3.2 Manual Test Checklist

| #   | Action                           | Expected Result                               |
| --- | -------------------------------- | --------------------------------------------- |
| 1   | App launches without error       | Navbar with Info, PTI, Compare, Explorer tabs |
| 2   | Navigate to PTI tab              | Map renders with default boundaries           |
| 3   | Set weight = 1 for any indicator | Map updates with colored polygons             |
| 4   | Save weight scheme "Test"        | Dropdown shows "Test"                         |
| 5   | Select different admin level     | Map re-renders at new level                   |
| 6   | Change n-bins slider             | Legend updates                                |
| 7   | Navigate to Compare tab          | Two maps render side-by-side                  |
| 8   | Navigate to Explorer tab         | Variable picker + map render                  |
| 9   | Select variable in Explorer      | Map shows indicator values                    |
| 10  | Download data (xlsx)             | File downloads, opens in Excel                |
| 11  | Download map (PNG)               | Valid PNG image                               |
| 12  | Info tab guided tour             | Cicerone tour launches                        |

### 3.3 shinytest2 Automation (future)

```r
# tests/testthat/test-app-integration.R
library(shinytest2)

test_that("launch_pti renders without error", {
  app <- AppDriver$new(
    app_dir = system.file("sample_pti", package = "devPTIpack"),
    name = "pti-smoke",
    timeout = 30000
  )
  app$expect_values()
  app$stop()
})
```

---

## Implementation Order

### Phase 1: Fixtures & Tier 1.1–1.2 (calc pipeline + indicators)

Priority: **Highest**. These are the core logic functions.
See [`arch-02.01-testing-calc-pipeline.md`](arch-02.01-testing-calc-pipeline.md) for full calc pipeline spec.

```
tests/testthat/
├── fixtures/
│   ├── setup-test-data.R    # Load ukr_shp, ukr_mtdt_full, derive common objects
│   ├── single_admin_shapes.rds
│   ├── empty_weights.rds
│   └── all_na_data.rds
├── test-calc-pipeline.R      # ~71 tests: unit + integration + e2e (see arch-02.01)
├── test-indicators-list.R    # get_indicators_list edge cases
```

### Phase 2: Tier 1.3–1.5 (I/O, validation, legend)

```
├── test-template-reader.R    # fct_template_reader, fct_convert_weight_to_clean
├── test-validators.R         # validate_geometries, validate_single_geom
├── test-legend-palette.R     # legend_map_satelite, recode_val_base
```

### Phase 3: Tier 1.6–1.11 (helpers, export, explorer, DT)

```
├── test-plot-helpers.R       # preplot_reshape, filter_admin_levels, add_legend_paras, check_existing_groups
├── test-drop-inval-adm.R    # get_vars_un_avbil, get_min_admin_wght, drop_inval_adm
├── test-export.R             # get_pti_scores_export, get_pti_weights_export, fct_inp_for_exp, fct_internal_wt_to_exp
├── test-explorer-helpers.R   # reshaped_explorer_dta, get_var_choices, filter_var_explorer
├── test-map-render.R         # make_ggmap, make_gg_line_map, plot_leaf_line_map2
├── test-dt-construction.R    # prep_input_data, make_vis_targets_for_dt
```

### Phase 4: Tier 2 (module servers)

```
├── test-mod-calc-pti2.R
├── test-mod-dt-inputs.R
├── test-mod-drop-inval-adm.R
├── test-mod-admin-levels.R
├── test-mod-var-selector.R
├── test-mod-wt-save.R
├── test-mod-export-data.R
```

### Phase 5: Tier 3 (UI — manual + automated)

```
├── test-app-integration.R    # shinytest2
```

---

## Shared Test Setup Helper

Create `tests/testthat/helper-test-data.R`:

```r
# Loaded automatically by testthat before each test file

# Sample data
data("ukr_shp", package = "devPTIpack")
data("ukr_mtdt_full", package = "devPTIpack")

# Derived objects (used by most tests)
test_indicators <- get_indicators_list(ukr_mtdt_full)
test_mt <- get_mt(ukr_shp)
test_adm_levels <- get_adm_levels(test_mt)
test_clean_geoms <- clean_geoms(ukr_shp)
test_pivoted <- pivot_pti_dta(ukr_mtdt_full, test_indicators)
test_weighted <- get_weighted_data(
  ukr_mtdt_full$weights_clean,
  test_pivoted,
  test_indicators
)
test_scored <- get_scores_data(test_weighted)
test_expanded <- expand_adm_levels(test_scored[[1]], test_mt)
test_merged <- merge_expandedn_adm_levels(test_expanded)
```

---

## Per-Function Test Map (Active Functions Only)

| File                         | Function                      | Test File                   | Tier |
| ---------------------------- | ----------------------------- | --------------------------- | ---- |
| `calc_pti_helpers.R`         | `get_mt`                      | `test-calc-pipeline.R`      | 1    |
| `calc_pti_helpers.R`         | `get_adm_levels`              | `test-calc-pipeline.R`      | 1    |
| `calc_pti_helpers.R`         | `pivot_pti_dta`               | `test-calc-pipeline.R`      | 1    |
| `calc_pti_helpers.R`         | `clean_geoms`                 | `test-calc-pipeline.R`      | 1    |
| `calc_pti_helpers.R`         | `get_weighted_data`           | `test-calc-pipeline.R`      | 1    |
| `calc_pti_helpers.R`         | `get_scores_data`             | `test-calc-pipeline.R`      | 1    |
| `calc_pti_expander.R`        | `expand_adm_levels`           | `test-calc-pipeline.R`      | 1    |
| `calc_pti_expander.R`        | `merge_expandedn_adm_levels`  | `test-calc-pipeline.R`      | 1    |
| `calc_pti_expander.R`        | `agg_pti_scores`              | `test-calc-pipeline.R`      | 1    |
| `calc_pti_expander.R`        | `label_generic_pti`           | `test-calc-pipeline.R`      | 1    |
| `calc_pti_expander.R`        | `generic_pti_glue`            | `test-calc-pipeline.R`      | 1    |
| `calc_pti_expander.R`        | `structure_pti_data`          | `test-calc-pipeline.R`      | 1    |
| `dta_cleaners.R`             | `get_indicators_list`         | `test-indicators-list.R`    | 1    |
| `fct_template_reader.R`      | `fct_template_reader`         | `test-template-reader.R`    | 1    |
| `fct_template_reader.R`      | `fct_convert_weight_to_clean` | `test-template-reader.R`    | 1    |
| `fct_validate_metadata.R`    | `validate_metadata`           | `test-validators.R`         | 1    |
| `fct_validate_metadata.R`    | `validate_read_shp`           | `test-validators.R`         | 1    |
| `fct_validate_metadata.R`    | `validate_read_metadata`      | `test-validators.R`         | 1    |
| `validators.R`               | `validate_geometries`         | `test-validators.R`         | 1    |
| `validators.R`               | `validate_single_geom`        | `test-validators.R`         | 1    |
| `fct_legend_map_satelites.R` | `legend_map_satelite`         | `test-legend-palette.R`     | 1    |
| `fct_legend_map_satelites.R` | `recode_val_base`             | `test-legend-palette.R`     | 1    |
| `plot_pti_helpers.R`         | `preplot_reshape_wghtd_dta`   | `test-plot-helpers.R`       | 1    |
| `plot_pti_helpers.R`         | `get_current_levels`          | `test-plot-helpers.R`       | 1    |
| `plot_pti_helpers.R`         | `filter_admin_levels`         | `test-plot-helpers.R`       | 1    |
| `plot_pti_helpers.R`         | `add_legend_paras`            | `test-plot-helpers.R`       | 1    |
| `plot_pti_helpers.R`         | `complete_pti_labels`         | `test-plot-helpers.R`       | 1    |
| `plot_pti_helpers.R`         | `plot_pti_polygons`           | `test-plot-helpers.R`       | 1    |
| `plot_pti_helpers.R`         | `clean_pti_polygons`          | `test-plot-helpers.R`       | 1    |
| `plot_pti_helpers.R`         | `add_pti_poly_controls`       | `test-plot-helpers.R`       | 1    |
| `plot_pti_helpers.R`         | `clean_pti_poly_controls`     | `test-plot-helpers.R`       | 1    |
| `plot_pti_helpers.R`         | `check_existing_groups`       | `test-plot-helpers.R`       | 1    |
| `mod_drop_inval_adm.R`       | `get_vars_un_avbil`           | `test-drop-inval-adm.R`     | 1    |
| `mod_drop_inval_adm.R`       | `get_min_admin_wght`          | `test-drop-inval-adm.R`     | 1    |
| `mod_drop_inval_adm.R`       | `drop_inval_adm`              | `test-drop-inval-adm.R`     | 1    |
| `mod_export_pti_data.R`      | `get_pti_scores_export`       | `test-export.R`             | 1    |
| `mod_export_pti_data.R`      | `get_pti_weights_export`      | `test-export.R`             | 1    |
| `fct_inp_for_exp.R`          | `fct_inp_for_exp`             | `test-export.R`             | 1    |
| `fct_inp_for_exp.R`          | `fct_internal_wt_to_exp`      | `test-export.R`             | 1    |
| `mod_dta_explorer2.R`        | `reshaped_explorer_dta`       | `test-explorer-helpers.R`   | 1    |
| `mod_dta_explorer2.R`        | `get_var_choices`             | `test-explorer-helpers.R`   | 1    |
| `mod_dta_explorer2.R`        | `filter_var_explorer`         | `test-explorer-helpers.R`   | 1    |
| `mod_plot_poly_leaf.R`       | `make_ggmap`                  | `test-map-render.R`         | 1    |
| `mod_plot_poly_leaf.R`       | `make_gg_line_map`            | `test-map-render.R`         | 1    |
| `mod_plot_init_leaf.R`       | `plot_leaf_line_map2`         | `test-map-render.R`         | 1    |
| `mod_DT_inputs.R`            | `prep_input_data`             | `test-dt-construction.R`    | 1    |
| `mod_DT_inputs.R`            | `make_vis_targets_for_dt`     | `test-dt-construction.R`    | 1    |
| `mod_DT_inputs.R`            | `make_input_DT`               | `test-dt-construction.R`    | 1    |
| `mod_load_shapes.R`          | `get_shape`                   | `test-template-reader.R`    | 1    |
| `fct_create_new_pti.R`       | `create_new_pti`              | `test-template-reader.R`    | 1    |
| `fct_guide.R`                | `guide_launch_pti`            | `test-app-integration.R`    | 3    |
| `fct_helpers.R`              | `add_logo`                    | `test-app-integration.R`    | 3    |
| `mod_calc_pti2.R`            | `mod_calc_pti2_server`        | `test-mod-calc-pti2.R`      | 2    |
| `mod_wt_inp.R`               | `mod_wt_inp_server`           | `test-mod-wt-save.R`        | 2    |
| `mod_wt_inp.R`               | `mod_wt_save_newsrv`          | `test-mod-wt-save.R`        | 2    |
| `mod_wt_inp.R`               | `mod_wt_delete_newsrv`        | `test-mod-wt-save.R`        | 2    |
| `mod_wt_inp.R`               | `mod_wt_select_newsrv`        | `test-mod-wt-save.R`        | 2    |
| `mod_DT_inputs.R`            | `mod_DT_inputs_server`        | `test-mod-dt-inputs.R`      | 2    |
| `mod_plot_pti2.R`            | `mod_plot_pti2_srv`           | `test-app-integration.R`    | 3    |
| `mod_plot_poly_leaf.R`       | `mod_plot_poly_leaf_server`   | `test-app-integration.R`    | 3    |
| `mod_dta_explorer2.R`        | `mod_dta_explorer2_server`    | `test-mod-var-selector.R`   | 2    |
| `mod_dta_explorer2.R`        | `mod_fltr_sel_var2_srv`       | `test-mod-var-selector.R`   | 2    |
| `mod_ptipage_core.R`         | `mod_ptipage_newsrv`          | `test-app-integration.R`    | 3    |
| `mod_pti_comparepage.R`      | `mod_pti_comparepage_newsrv`  | `test-app-integration.R`    | 3    |
| `mod_pti_map_side_pan.R`     | `mod_get_nbins_srv`           | `test-mod-admin-levels.R`   | 2    |
| `mod_pti_map_side_pan.R`     | `mod_get_admin_levels_srv`    | `test-mod-admin-levels.R`   | 2    |
| `mod_pti_map_side_pan.R`     | `mod_map_dwnld_srv`           | `test-app-integration.R`    | 3    |
| `mod_drop_inval_adm.R`       | `mod_drop_inval_adm` (module) | `test-mod-drop-inval-adm.R` | 2    |
| `mod_export_pti_data.R`      | `mod_export_pti_data_server`  | `test-mod-export-data.R`    | 2    |
| `launch_pti.R`               | `launch_pti`                  | `test-app-integration.R`    | 3    |
| `launch_pti.R`               | `launch_pti_onepage`          | `test-app-integration.R`    | 3    |

---

## Post-Cleanup Verification Workflow

```
1. Run Tier 1 tests:
   devtools::test(filter = "calc-pipeline|indicators-list|template-reader|validators|legend-palette|plot-helpers|drop-inval|export|explorer-helpers|map-render|dt-construction")

2. Run Tier 2 tests:
   devtools::test(filter = "mod-")

3. Run R CMD check:
   devtools::check()

4. Manual UI smoke test:
   launch_pti(shp_dta = ukr_shp, inp_dta = ukr_mtdt_full)
   # Follow checklist in Section 3.2

5. Run full suite:
   devtools::test()
```

---

## Refactoring Suggestions to Improve Testability

| Current Code                                                 | Proposed Extraction                                                         | Benefit                                           |
| ------------------------------------------------------------ | --------------------------------------------------------------------------- | ------------------------------------------------- |
| `mod_plot_poly_leaf_server` diff logic (lines 30–60)         | Extract `diff_plot_layers(old, new)` → returns `{keep, remove, add}`        | Console-testable diff algorithm                   |
| `mod_wt_save_newsrv` button state machine                    | Extract `get_save_btn_state(cur_val, cur_name, existing_ws)` → returns enum | Test state transitions without Shiny              |
| `mod_get_admin_levels_srv` resolution logic                  | Extract `resolve_admin_levels(dta_levels, default, show, golem_opts)`       | Test complex conditional without reactive context |
| `mod_first_open_count_server` + `mod_tab_open_first_newserv` | Consolidate into single `mod_deferred_render_srv`                           | Reduce duplication, single test point             |
