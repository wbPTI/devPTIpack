# Legacy Architecture — `devPTIpack`

> Auto-generated review of the R package structure as of 2026-04-29.

## Package Summary

**devPTIpack** is a `golem`-based Shiny R package for computing, visualizing, and exploring **Priority Targeting Indices (PTI)** — composite scores from weighted indicators mapped across hierarchical administrative boundaries.

---

## Workspace Structure

```
devPTIpack/
├── R/                        # All package source code (~50 files)
├── data/                     # Bundled sample data (ukr_shp, ukr_mtdt_full)
├── inst/
│   ├── app/www/              # Static assets (CSS, images)
│   ├── sample_pti/           # Sample deployed app
│   └── template_pti/         # Skeleton for create_new_pti()
├── dev/                      # Development scripts, profiling, examples
├── tests/testthat/           # Unit tests
├── vignettes/                # Package vignettes
├── DESCRIPTION
├── NAMESPACE
└── _pkgdown.yml
```

---

## Architecture Layers

```
Entry Points (launch_pti / run_*)
    │
    ▼
App UI & Server (app_ui / app_server variants)
    │
    ▼
Page-level Modules (mod_ptipage_core, mod_pti_comparepage, mod_dta_explorer2)
    │
    ┌──────────────────────────────────────────┐
    ▼                                          ▼
Weights Input                           PTI Visualisation
(mod_wt_inp / mod_weights /             (mod_plot_pti2 / mod_map_pti_leaf /
 mod_DT_inputs / mod_wt_*)              mod_plot_poly_leaf / mod_plot_poly_legend)
    │                                          │
    ▼                                          ▼
PTI Calculation                        Legend & Controls
(mod_calc_pti2)                        (mod_plot_poly_legend / legend_map_satelite)
    │
    ▼
Core Calculation Helpers
(calc_pti_helpers / calc_pti_expander)
    │
    ▼
Data I/O & Validation (fct_template_reader / fct_validate_metadata / validators)
```

---

## Primary Call Chain (Modern Path)

```
launch_pti() or run_new_pti()
  └─ shinyApp(ui, server) + with_golem_options()
       └─ mod_ptipage_newsrv("pagepti", inp_dta, shp_dta)
            ├─ mod_wt_inp_server()
            │    └─ get_indicators_list()
            ├─ mod_calc_pti2_server()
            │    ├─ pivot_pti_dta()
            │    ├─ get_weighted_data()
            │    ├─ get_scores_data()          # z-score standardisation
            │    ├─ expand_adm_levels()        # up/down-scale admin levels
            │    ├─ merge_expandedn_adm_levels()
            │    ├─ agg_pti_scores()           # row-sum per polygon
            │    ├─ label_generic_pti()        # HTML popup labels
            │    └─ structure_pti_data()       # join to shapes
            └─ mod_plot_pti2_srv()
                 ├─ preplot_reshape_wghtd_dta()
                 ├─ filter_admin_levels()
                 ├─ add_legend_paras()         # calls legend_map_satelite()
                 ├─ complete_pti_labels()
                 └─ mod_plot_poly_leaf_server()
                      ├─ plot_pti_polygons()
                      └─ mod_plot_poly_legend_server()
```

---

## Two Generations of Modules

The package contains **two coexisting generations** of module code:

| Generation | Weights                           | Calculation     | Visualisation                          | Used by                                                        |
| ---------- | --------------------------------- | --------------- | -------------------------------------- | -------------------------------------------------------------- |
| **Modern** | `mod_wt_inp` / `mod_DT_inputs`    | `mod_calc_pti2` | `mod_plot_pti2` / `mod_plot_poly_leaf` | `launch_pti()`, `launch_pti_onepage()`, `app_new_pti_server()` |
| **Legacy** | `mod_weights` / `mod_new_weights` | `mod_calc_pti`  | `mod_map_pti_leaf`                     | `app_server()`, `app_server_input_simple()`, `run_pti()`       |

---

## File-by-File Function Tables

### Status Legend

- **Active** — function is called from other active code. Callers listed.
- **Legacy** — function exists but is only called from legacy/unused code paths, or not called at all.

### Documentation Legend

- **OK** — full roxygen: title, `@param`, `@return`, `@export`
- **partial** — some roxygen tags present but incomplete
- **poor** — only `@noRd` / `@export`, no description
- **no** — no roxygen block at all

---

### `app_config.R`

Golem infrastructure: locate package files and read config.

| Function             | Status                            | Docs    | ~Lines |
| -------------------- | --------------------------------- | ------- | ------ |
| `app_sys()`          | Active: used everywhere for paths | partial | 5      |
| `get_golem_config()` | Active: golem config reader       | partial | 12     |

### `app_server.R`

Multiple server variants that wire all modules together (data → weights → calc → plot).

| Function                      | Status                               | Docs | ~Lines |
| ----------------------------- | ------------------------------------ | ---- | ------ |
| `app_server()`                | Legacy: used by `run_pti()`          | poor | 65     |
| `app_server_input_simple()`   | Legacy: used by `run_dev_map_pti()`  | poor | 55     |
| `app_server_sample_pti_vis()` | Active: used by `run_dev_pti_plot()` | poor | 45     |
| `app_new_pti_server()`        | Active: used by `run_new_pti()`      | poor | 50     |

### `app_ui.R`

Navbar-based Shiny UIs with tabs for Info, PTI, Compare, and Explorer.

| Function                         | Status                                           | Docs    | ~Lines |
| -------------------------------- | ------------------------------------------------ | ------- | ------ |
| `app_ui()`                       | Legacy: used by `run_pti()`, `run_dev_map_pti()` | poor    | 40     |
| `app_server_sample_pti_vis_ui()` | Active: used by `run_dev_pti_plot()`             | poor    | 40     |
| `app_new_pti_ui()`               | Active: used by `run_new_pti()`                  | partial | 42     |
| `golem_add_external_resources()` | Active: all UIs                                  | partial | 10     |

### `run_app.R`

Shiny app launcher functions that wrap UI + server with `golem_options`.

| Function                   | Status                      | Docs    | ~Lines |
| -------------------------- | --------------------------- | ------- | ------ |
| `run_new_pti()`            | Active: primary entry point | partial | 25     |
| `run_dev_map_pti()`        | Active: dev entry point     | partial | 25     |
| `run_dev_pti_plot()`       | Active: dev entry point     | partial | 25     |
| `run_pti()`                | Legacy: older entry point   | partial | 25     |
| `run_onepage_pti()`        | Active: one-page entry      | partial | 25     |
| `run_onepage_pti_sample()` | Active: sample entry        | partial | 8      |

### `launch_pti.R`

Primary public API: assembles full multi-tab or one-page PTI apps from data objects.

| Function               | Status                                 | Docs | ~Lines |
| ---------------------- | -------------------------------------- | ---- | ------ |
| `launch_pti_onepage()` | Active: primary public API             | OK   | 60     |
| `launch_pti()`         | Active: primary public API (multi-tab) | OK   | 95     |

### `data.R`

Roxygen documentation for bundled sample datasets (Ukraine shapes and metadata).

| Function                    | Status              | Docs    | ~Lines |
| --------------------------- | ------------------- | ------- | ------ |
| `"ukr_shp"` (dataset)       | Active: sample data | OK      | 15     |
| `"ukr_mtdt_full"` (dataset) | Active: sample data | partial | 3      |

---

### `calc_pti_helpers.R` — Core Calculation Helpers

Pure functions for mapping tables, pivoting data, applying weights, and z-score standardisation.

| Function              | Status                                                            | Docs    | ~Lines |
| --------------------- | ----------------------------------------------------------------- | ------- | ------ |
| `get_mt()`            | Active: `mod_calc_pti2`, `validate_metadata`                      | partial | 22     |
| `get_adm_levels()`    | Active: `expand_adm_levels`, `mod_plot_init_leaf`                 | poor    | 10     |
| `pivot_pti_dta()`     | Active: `mod_calc_pti2`, `mod_dta_explorer2`, `validate_metadata` | partial | 30     |
| `clean_geoms()`       | Active: `mod_calc_pti2`, `validate_metadata`                      | partial | 16     |
| `get_weighted_data()` | Active: `mod_calc_pti2`, `validate_metadata`                      | partial | 22     |
| `get_scores_data()`   | Active: `mod_calc_pti2`, `validate_metadata`                      | partial | 18     |

### `calc_pti_expander.R` — Admin Level Expansion & PTI Structuring

Extrapolates PTI scores across admin levels (up/down-scaling), aggregates, labels, and structures for mapping.

| Function                       | Status                                       | Docs    | ~Lines |
| ------------------------------ | -------------------------------------------- | ------- | ------ |
| `expand_adm_levels()`          | Active: `mod_calc_pti2`                      | partial | 75     |
| `merge_expandedn_adm_levels()` | Active: `mod_calc_pti2`                      | poor    | 18     |
| `agg_pti_scores()`             | Active: `mod_calc_pti2`, `validate_metadata` | partial | 70     |
| `label_generic_pti()`          | Active: `mod_calc_pti2`, `mod_dta_explorer2` | partial | 10     |
| `generic_pti_glue()`           | Active: `label_generic_pti`                  | partial | 12     |
| `structure_pti_data()`         | Active: `mod_calc_pti2`, `mod_dta_explorer2` | poor    | 90     |

### `dta_cleaners.R` / `plot_pti_helpers.R` — duplicated file

Extracts the indicator list from metadata; determines which admin levels carry data for each variable.

| Function                | Status                                                   | Docs    | ~Lines |
| ----------------------- | -------------------------------------------------------- | ------- | ------ |
| `get_indicators_list()` | Active: `mod_wt_inp`, `mod_weights`, `mod_dta_explorer2` | partial | 85     |

> **Note**: `get_indicators_list()` is defined only in `dta_cleaners.R`. No duplication exists.

---

### `fct_template_reader.R` — Excel I/O

Reads the standardised Excel metadata/weights template and converts between weight formats.

| Function                        | Status                                                        | Docs    | ~Lines |
| ------------------------------- | ------------------------------------------------------------- | ------- | ------ |
| `fct_template_reader()`         | Active: `mod_fetch_data`, `validate_metadata`, `mod_wt_uplod` | partial | 60     |
| `fct_convert_weight_to_clean()` | Active: called by `fct_template_reader`                       | poor    | 18     |
| `fct_convert_clean_to_weight()` | Legacy: only called from old download handler                 | poor    | 18     |

### `fct_validate_metadata.R`

End-to-end testthat-based validation: reads shapes + metadata and confirms a PTI can be computed.

| Function                   | Status                                | Docs    | ~Lines |
| -------------------------- | ------------------------------------- | ------- | ------ |
| `validate_metadata()`      | Active: exported, integration test    | partial | 55     |
| `validate_read_shp()`      | Active: called by `validate_metadata` | partial | 35     |
| `validate_read_metadata()` | Active: called by `validate_metadata` | partial | 28     |

### `validators.R` — Geometry Validators

Detailed per-layer geometry checks: SF class, polygon type, Pcod/Name columns, uniqueness, hierarchy.

| Function                 | Status                                  | Docs    | ~Lines |
| ------------------------ | --------------------------------------- | ------- | ------ |
| `validate_single_geom()` | Active: called by `validate_geometries` | partial | 200    |
| `validate_geometries()`  | Active: exported utility                | partial | 20     |

### `fct_helpers.R`

Small HTML helper that builds the World Bank logo div for the navbar.

| Function     | Status               | Docs | ~Lines |
| ------------ | -------------------- | ---- | ------ |
| `add_logo()` | Active: `launch_pti` | poor | 15     |

### `fct_guide.R`

Builds the cicerone guided-tour steps that walk users through the PTI app.

| Function             | Status                       | Docs    | ~Lines |
| -------------------- | ---------------------------- | ------- | ------ |
| `guide_launch_pti()` | Active: `mod_infotab_server` | partial | 140    |

### `fct_inp_for_exp.R` — Export Formatting

Prepares data and weights for Excel export (rename columns, filter, reshape).

| Function                   | Status                        | Docs | ~Lines |
| -------------------------- | ----------------------------- | ---- | ------ |
| `fct_inp_for_exp()`        | Active: `prepare_export_data` | poor | 30     |
| `fct_internal_wt_to_exp()` | Active: `prepare_export_data` | poor | 10     |
| `fct_exp_wt_to_internal()` | Legacy: stub, no-op           | poor | 3      |

### `fct_export_data.R` — Legacy Export Helpers

Old-style DT/tibble export formatting; no longer called from active modules.

| Function                     | Status                                 | Docs | ~Lines |
| ---------------------------- | -------------------------------------- | ---- | ------ |
| `rename_one()`               | Legacy: only in `dt_wrap_exported`     | no   | 9      |
| `prepare_tibble_to_export()` | Legacy: only in `dt_wrap_exported`     | no   | 18     |
| `prepare_DT_export()`        | Legacy: only in `dt_wrap_exported`     | no   | 27     |
| `dt_wrap_exported()`         | Legacy: not called from active modules | no   | 33     |

### `fct_create_new_pti.R`

Scaffolds a new PTI app directory from the package template.

| Function           | Status                            | Docs | ~Lines |
| ------------------ | --------------------------------- | ---- | ------ |
| `create_new_pti()` | Active: user-facing scaffold tool | OK   | 45     |

### `fct_legend_map_satelites.R` — Legend Factory

Central legend engine: computes quantile breaks, colour palettes, label/radius functions for map layers.

| Function                | Status                                                             | Docs    | ~Lines |
| ----------------------- | ------------------------------------------------------------------ | ------- | ------ |
| `legend_map_satelite()` | Active: `add_legend_paras`, `mod_map_pal_srv`, `plot_single_ggmap` | partial | 200    |
| `recode_val_base()`     | Active: called inside `legend_map_satelite`                        | poor    | 25     |
| `get_pal_lab_fn()`      | Legacy: defined but never called                                   | poor    | 15     |
| `make_shapes()`         | Active: `mod_plot_legend_srv`                                      | poor    | 15     |
| `make_labels()`         | Active: `mod_plot_legend_srv`                                      | poor    | 12     |
| `zeros_after_period()`  | Legacy: defined but never called                                   | poor    | 8      |

### `render_metadata_pdf.R`

Renders the metadata Rmd template to PDF via `rmarkdown::render()`.

| Function            | Status                   | Docs | ~Lines |
| ------------------- | ------------------------ | ---- | ------ |
| `render_metadata()` | Active: exported utility | poor | 8      |

---

### `mod_ptipage_core.R` — Core PTI Page Module

Orchestrates the full PTI page: layout (two-col or box), weights → calc → plot wiring.

| Function                  | Status                                     | Docs    | ~Lines |
| ------------------------- | ------------------------------------------ | ------- | ------ |
| `mod_ptipage_twocol_ui()` | Active: `launch_pti`, `launch_pti_onepage` | OK      | 40     |
| `mod_ptipage_box_ui()`    | Active: `launch_pti_onepage` (box variant) | partial | 35     |
| `mod_ptipage_newsrv()`    | Active: `launch_pti`, `launch_pti_onepage` | OK      | 55     |

### `mod_calc_pti2.R` — Modern PTI Calculation

Reactive module that runs the full PTI pipeline: weight → score → expand → aggregate → label → structure.

| Function                 | Status                                                                          | Docs    | ~Lines |
| ------------------------ | ------------------------------------------------------------------------------- | ------- | ------ |
| `mod_calc_pti2_ui()`     | Active: empty placeholder                                                       | poor    | 5      |
| `mod_calc_pti2_server()` | Active: `mod_ptipage_newsrv`, `app_server_sample_pti_vis`, `app_new_pti_server` | partial | 55     |

### `mod_calc_pti.R` — Legacy PTI Calculation

Original monolithic PTI calculation using the old `callModule` pattern; replaced by `mod_calc_pti2`.

| Function                | Status                    | Docs    | ~Lines |
| ----------------------- | ------------------------- | ------- | ------ |
| `mod_calc_pti_ui()`     | Legacy: `app_server`      | poor    | 5      |
| `mod_calc_pti_server()` | Legacy: `app_server` only | partial | 180    |

### `mod_wt_inp.R` — Modern Weights Input

DataTable-based weight entry with save/delete/reset/upload/download of named weighting schemes.

| Function                  | Status                                                | Docs    | ~Lines |
| ------------------------- | ----------------------------------------------------- | ------- | ------ |
| `mod_wt_inp_ui()`         | Active: `mod_ptipage_twocol_ui`, `mod_ptipage_box_ui` | partial | 30     |
| `mod_wt_inp_test_ui()`    | Legacy: dev diagnostics only                          | poor    | 6      |
| `full_wt_inp_ui()`        | Active: conditional branch inside `mod_wt_inp_ui`     | poor    | 60     |
| `short_wt_inp_ui()`       | Active: conditional branch inside `mod_wt_inp_ui`     | partial | 65     |
| `mod_wt_inp_server()`     | Active: `mod_ptipage_newsrv`                          | partial | 50     |
| `mod_wt_name_newsrv()`    | Active: `mod_wt_inp_server`                           | poor    | 15     |
| `mod_wt_save_newsrv()`    | Active: `mod_wt_inp_server`                           | poor    | 90     |
| `mod_wt_delete_ui()`      | Active: `short_wt_inp_ui`                             | poor    | 10     |
| `mod_wt_delete_newsrv()`  | Active: `mod_wt_inp_server`                           | poor    | 40     |
| `mod_wt_select_newsrv()`  | Active: `mod_wt_inp_server`                           | poor    | 55     |
| `mod_wt_upd_newsrv()`     | Active: `mod_wt_inp_server`                           | poor    | 30     |
| `mod_wt_dwnload_newsrv()` | Active: `mod_wt_inp_server`                           | poor    | 25     |
| `mod_wt_uplod_newsrv()`   | Active: `mod_wt_inp_server` (stub)                    | poor    | 20     |
| `prepare_export_data()`   | Active: `mod_wt_dwnload_newsrv`                       | poor    | 55     |

### `mod_DT_inputs.R` — DataTable-Based Weights

Editable DT table that replaces the old one-numericInput-per-indicator approach.

| Function                    | Status                         | Docs    | ~Lines |
| --------------------------- | ------------------------------ | ------- | ------ |
| `mod_DT_inputs_ui()`        | Active: `mod_wt_inp_ui`        | partial | 25     |
| `mod_DT_inputs_server()`    | Active: `mod_wt_inp_server`    | partial | 60     |
| `add_two_action_btn()`      | Active: `prep_input_data`      | no      | 25     |
| `prep_input_data()`         | Active: `mod_DT_inputs_server` | no      | 90     |
| `make_vis_targets_for_dt()` | Active: `prep_input_data`      | no      | 55     |
| `make_input_DT()`           | Active: `mod_DT_inputs_server` | no      | 55     |

### `mod_weights.R` — Legacy Weights System

Old numericInput-per-indicator weights UI/server with pillar grouping; replaced by `mod_wt_inp`.

| Function                   | Status                                                    | Docs    | ~Lines |
| -------------------------- | --------------------------------------------------------- | ------- | ------ |
| `mod_weights_ui()`         | Legacy: `app_ui`                                          | partial | 30     |
| `mod_weights_server()`     | Legacy: `app_server_sample_pti_vis`, `app_new_pti_server` | poor    | 40     |
| `mod_indicarots_srv()`     | Legacy: `mod_weights_server`                              | poor    | 15     |
| `mod_gen_wt_inputs_srv()`  | Legacy: `mod_weights_server`                              | no      | 95     |
| `mod_wt_btns_srv()`        | Legacy: `mod_weights_server`                              | no      | 40     |
| `mod_wt_name_srv()`        | Legacy: `mod_weights_server`                              | no      | 18     |
| `mod_wt_select_srv()`      | Legacy: `mod_weights_server`                              | no      | 45     |
| `mod_wt_uplod_srv()`       | Legacy: `mod_weights_server`                              | no      | 25     |
| `mod_wt_delete_srv()`      | Legacy: `mod_weights_server`                              | no      | 45     |
| `mod_wt_fill_srv()`        | Legacy: `mod_weights_server`                              | no      | 20     |
| `mod_wt_save_srv()`        | Legacy: `mod_weights_server`                              | no      | 100    |
| `mod_collect_wt_srv()`     | Legacy: `mod_weights_server`                              | no      | 18     |
| `mod_download_wt_srv()`    | Legacy: `mod_weights_server`                              | no      | 35     |
| `mod_export_recoded_srv()` | Legacy: `app_server`, `app_server_input_simple`           | no      | 40     |

### `mod_new_weights.R` — Dev Weights Variants

Experimental weight UI/server variants used during development; mostly unused.

| Function                          | Status                    | Docs | ~Lines |
| --------------------------------- | ------------------------- | ---- | ------ |
| `mod_new_weights_ui()`            | Legacy: unused            | poor | 45     |
| `mod_weights_html_ui()`           | Legacy: unused            | poor | 10     |
| `mod_new_demo_weights_server()`   | Legacy: `mod_pti_onepage` | poor | 45     |
| `mod_new_weights_server_htlmwt()` | Legacy: unused            | poor | 35     |

### `mod_weights_rand.R` — Random Weights (Dev)

Dev shortcut: generates random or combinatorial weights for testing.

| Function                    | Status                            | Docs | ~Lines |
| --------------------------- | --------------------------------- | ---- | ------ |
| `mod_weights_rand_ui()`     | Legacy: `mod_weights_ui`          | poor | 10     |
| `mod_weights_rand_server()` | Legacy: `app_server_input_simple` | poor | 35     |
| `get_rand_weights()`        | Legacy: `mod_weights_rand_server` | no   | 15     |
| `get_all_weights_combs()`   | Active: `validate_metadata`       | no   | 18     |

---

### `mod_plot_pti2.R` — Modern Plot Orchestrator

Orchestrates PTI visualisation: reshape → filter admin → legend → render polygons on leaflet.

| Function                        | Status                                                     | Docs    | ~Lines |
| ------------------------------- | ---------------------------------------------------------- | ------- | ------ |
| `mod_plot_pti2_srv()`           | Active: `mod_ptipage_newsrv`, `mod_pti_comparepage_newsrv` | partial | 45     |
| `mod_plot_pti_comparison_srv()` | Legacy: `app_server_sample_pti_vis`, `app_new_pti_server`  | poor    | 15     |

### `mod_plot_poly_leaf.R` — Polygon Rendering

Diff-based leaflet polygon renderer; only redraws changed layers. Also provides ggplot2 static export.

| Function                      | Status                                                  | Docs    | ~Lines |
| ----------------------------- | ------------------------------------------------------- | ------- | ------ |
| `mod_plot_poly_leaf_server()` | Active: `mod_plot_pti2_srv`, `mod_dta_explorer2_server` | partial | 70     |
| `mod_plot_leaf_export()`      | Active: `mod_plot_poly_leaf_server`                     | partial | 40     |
| `make_ggmap()`                | Active: `mod_map_dwnld_srv`                             | partial | 55     |
| `make_ggmap_2()`              | Legacy: commented-out callers only                      | partial | 65     |
| `make_gg_line_map()`          | Active: `mod_map_dwnld_srv`                             | partial | 25     |
| `make_gg_line_map_2()`        | Legacy: commented-out callers only                      | partial | 55     |
| `make_spplot()`               | Legacy: commented-out callers only                      | partial | 30     |
| `make_sp_line_map()`          | Legacy: commented-out, empty body                       | partial | 22     |

### `mod_plot_poly_legend.R` — Legend Rendering

Dynamic legend add/remove on the leaflet map when the selected layer changes.

| Function                        | Status                                | Docs    | ~Lines |
| ------------------------------- | ------------------------------------- | ------- | ------ |
| `mod_plot_poly_legend_server()` | Active: `mod_plot_poly_leaf_server`   | partial | 30     |
| `plot_pti_legend()`             | Active: `mod_plot_poly_legend_server` | partial | 25     |
| `remove_pti_legend()`           | Active: `mod_plot_poly_legend_server` | partial | 15     |

### `mod_plot_init_leaf.R` — Map Initialisation

Creates the base leaflet map, adds boundary polylines, and flies to the shape extent.

| Function                      | Status                                                  | Docs    | ~Lines |
| ----------------------------- | ------------------------------------------------------- | ------- | ------ |
| `mod_plot_init_leaf_server()` | Active: `mod_plot_pti2_srv`, `mod_dta_explorer2_server` | partial | 65     |
| `init_leaf_and_bindigs()`     | Active: called inside `mod_plot_init_leaf_server`       | poor    | 40     |

### `plot_pti_helpers.R` — Plot Data Helpers

Reshapes structured PTI data for plotting; manages leaflet polygon groups and layer controls.

| Function                      | Status                                                  | Docs    | ~Lines |
| ----------------------------- | ------------------------------------------------------- | ------- | ------ |
| `preplot_reshape_wghtd_dta()` | Active: `mod_plot_pti2_srv`, `mod_dta_explorer2_server` | partial | 25     |
| `get_current_levels()`        | Active: `mod_plot_pti2_srv`, `mod_dta_explorer2_server` | poor    | 8      |
| `filter_admin_levels()`       | Active: `mod_plot_pti2_srv`, `mod_dta_explorer2_server` | poor    | 18     |
| `add_legend_paras()`          | Active: `mod_plot_pti2_srv`, `mod_dta_explorer2_server` | poor    | 14     |
| `complete_pti_labels()`       | Active: `mod_plot_pti2_srv`                             | partial | 16     |
| `plot_pti_polygons()`         | Active: `mod_plot_poly_leaf_server`                     | partial | 30     |
| `clean_pti_polygons()`        | Active: `mod_plot_poly_leaf_server`                     | partial | 15     |
| `add_pti_poly_controls()`     | Active: `mod_plot_poly_leaf_server`                     | partial | 35     |
| `clean_pti_poly_controls()`   | Active: `mod_plot_poly_leaf_server`                     | partial | 12     |
| `check_existing_groups()`     | Active: `add_pti_poly_controls`                         | partial | 25     |

---

### `mod_map_pti_leaf.R` — Legacy Full Map System

Old monolithic leaflet lifecycle (init, fly-to, palette, polygons, bubbles, controls, legend); replaced by `mod_plot_pti2`.

| Function                      | Status                                           | Docs | ~Lines |
| ----------------------------- | ------------------------------------------------ | ---- | ------ |
| `mod_map_pti_leaf_ui()`       | Active: used by both modern and legacy UIs       | poor | 15     |
| `mod_map_pti_leaf_srv()`      | Legacy: `app_server`, `app_server_input_simple`  | poor | 50     |
| `mod_map_pti_leaf_page_ui()`  | Legacy: `app_ui`, `app_server_sample_pti_vis_ui` | poor | 15     |
| `mod_map_pti_leaf_page_srv()` | Legacy: `app_server`                             | poor | 15     |
| `mod_init_leaf_pti_srv()`     | Legacy: `mod_map_pti_leaf_srv`                   | poor | 12     |
| `mod_flyto_leaf_srv()`        | Legacy: `mod_map_pti_leaf_srv`                   | poor | 55     |
| `mod_clean_leaf_srv()`        | Legacy: `mod_map_pti_leaf_srv` (commented body)  | poor | 10     |
| `mod_map_pal_srv()`           | Legacy: `mod_map_pti_leaf_srv`                   | no   | 30     |
| `mod_map_dta_srv()`           | Legacy: `mod_map_pti_leaf_srv`                   | no   | 90     |
| `mod_plot_map_srv()`          | Legacy: `mod_map_pti_leaf_srv`                   | no   | 165    |
| `mod_plot_controls_srv()`     | Legacy: `mod_map_pti_leaf_srv`                   | no   | 140    |
| `mod_plot_legend_srv()`       | Legacy: `mod_map_pti_leaf_srv`                   | no   | 80     |
| `mod_clean_buble_data_srv()`  | Legacy: `mod_map_pti_leaf_srv`                   | poor | 110    |

### `mod_pti_comparepage.R`

Side-by-side comparison page: renders two independent `mod_plot_pti2_srv` map instances.

| Function                       | Status               | Docs    | ~Lines |
| ------------------------------ | -------------------- | ------- | ------ |
| `mod_pti_comparepage_ui()`     | Active: `launch_pti` | partial | 20     |
| `mod_pti_comparepage_newsrv()` | Active: `launch_pti` | partial | 25     |

### `mod_dta_explorer2.R` — Data Explorer

Explorer tab: shows raw indicator values on the map with variable/admin-level/bin selectors.

| Function                      | Status                                     | Docs    | ~Lines |
| ----------------------------- | ------------------------------------------ | ------- | ------ |
| `mod_dta_explorer2_server()`  | Active: `launch_pti`, `mod_explrr_onepage` | partial | 55     |
| `mod_dta_explorer2_ui()`      | Active: `launch_pti`, `app_new_pti_ui`     | partial | 20     |
| `mod_dta_explorer2_side_ui()` | Active: `mod_dta_explorer2_ui`             | partial | 20     |
| `mod_select_var_ui()`         | Active: `mod_dta_explorer2_side_ui`        | partial | 20     |
| `mod_fltr_sel_var2_srv()`     | Active: `mod_dta_explorer2_server`         | partial | 55     |
| `reshaped_explorer_dta()`     | Active: `mod_dta_explorer2_server`         | partial | 35     |
| `get_var_choices()`           | Active: `mod_dta_explorer2_server`         | partial | 22     |
| `filter_var_explorer()`       | Active: `mod_dta_explorer2_server`         | partial | 8      |

### `mod_infotab.R`

Info/landing page with modal dialog and cicerone guided-tour trigger.

| Function               | Status               | Docs | ~Lines |
| ---------------------- | -------------------- | ---- | ------ |
| `mod_infotab_server()` | Active: `launch_pti` | poor | 80     |

### `mod_info_page.R` — Legacy Info Page

Old info page server with inline guide builder; replaced by `mod_infotab`.

| Function                 | Status                         | Docs | ~Lines |
| ------------------------ | ------------------------------ | ---- | ------ |
| `mod_info_page_ui()`     | Legacy: unused                 | poor | 10     |
| `mod_info_page_server()` | Legacy: `app_server` variants  | poor | 120    |
| `compile_guides()`       | Legacy: `mod_info_page_server` | no   | 180    |

### `mod_explorer.R` — Legacy Explorer

Original data explorer module; replaced by `mod_dta_explorer2`.

| Function                       | Status                        | Docs | ~Lines |
| ------------------------------ | ----------------------------- | ---- | ------ |
| `mod_explorer_tab_ui()`        | Legacy: `app_ui`              | poor | 20     |
| `mod_explorer_side_panel_ui()` | Legacy: `mod_explorer_tab_ui` | poor | 30     |
| `mod_explorer_server()`        | Legacy: `app_server`          | poor | 75     |
| `mod_select_var_srv()`         | Legacy: `mod_explorer_server` | poor | 35     |

### `mod_pti_onepage.R`

Standalone single-page PTI module used by `run_onepage_pti()`.

| Function                   | Status                    | Docs | ~Lines |
| -------------------------- | ------------------------- | ---- | ------ |
| `mod_pti_onepage_ui()`     | Active: `run_onepage_pti` | poor | 15     |
| `mod_pti_onepage_server()` | Active: `run_onepage_pti` | poor | 50     |

### `mod_explrr_onepage.R`

Standalone single-page data explorer; loads shapes and data internally.

| Function                      | Status                               | Docs | ~Lines |
| ----------------------------- | ------------------------------------ | ---- | ------ |
| `mod_explrr_onepage_ui()`     | Active: exported standalone explorer | poor | 8      |
| `mod_explrr_onepage_server()` | Active: exported standalone explorer | poor | 15     |

---

### `mod_pti_map_side_pan.R` — Side Panel Controls

Reusable UI/server for Nbins slider, admin-level selector, and map download buttons.

| Function                     | Status                                                  | Docs | ~Lines |
| ---------------------------- | ------------------------------------------------------- | ---- | ------ |
| `mod_leaf_side_panel_ui()`   | Active: `mod_map_pti_leaf_ui`                           | poor | 35     |
| `mod_get_nbins_ui()`         | Active: many UIs                                        | poor | 20     |
| `mod_get_nbins_srv()`        | Active: `mod_plot_pti2_srv`, `mod_dta_explorer2_server` | poor | 40     |
| `mod_get_admin_levels_ui()`  | Active: `mod_dta_explorer2_side_ui`                     | poor | 15     |
| `mod_get_admin_levels_srv()` | Active: `mod_plot_pti2_srv`, `mod_dta_explorer2_server` | poor | 130    |
| `mod_map_dwnld_ui()`         | Active: side panel UI                                   | poor | 55     |
| `mod_map_dwnld_srv()`        | Active: `mod_plot_pti2_srv`, `mod_dta_explorer2_server` | poor | 120    |

### `mod_export_pti_data.R`

Exports PTI scores and weights to a download-ready list of tibbles.

| Function                       | Status                                                      | Docs | ~Lines |
| ------------------------------ | ----------------------------------------------------------- | ---- | ------ |
| `mod_export_pti_data_server()` | Active: `app_server_sample_pti_vis`, `app_new_pti_server`   | poor | 30     |
| `get_pti_scores_export()`      | Active: `prepare_export_data`, `mod_export_pti_data_server` | no   | 30     |
| `get_pti_weights_export()`     | Active: called by `mod_export_pti_data_server`              | no   | 15     |

---

### Infrastructure Modules

### `mod_load_shapes.R`

Loads shape RDS from a file path or accepts a direct R object.

| Function                   | Status                           | Docs | ~Lines |
| -------------------------- | -------------------------------- | ---- | ------ |
| `mod_load_shapes_server()` | Legacy: old `callModule` pattern | poor | 20     |
| `mod_get_shape_srv()`      | Active: all app servers          | poor | 15     |
| `get_shape()`              | Active: `mod_get_shape_srv`      | no   | 20     |

### `mod_fetch_data.R`

Loads metadata Excel via `fct_template_reader` or accepts a direct R object.

| Function               | Status                  | Docs | ~Lines |
| ---------------------- | ----------------------- | ---- | ------ |
| `mod_fetch_data_srv()` | Active: all app servers | poor | 25     |

### `mod_dwnld_dta.R`

Generic download-link UI and xlsx/file download handlers.

| Function                         | Status                                               | Docs | ~Lines |
| -------------------------------- | ---------------------------------------------------- | ---- | ------ |
| `mod_dwnld_dta_link_ui()`        | Active: `short_wt_inp_ui`, `mod_map_dwnld_ui`        | poor | 20     |
| `mod_dwnld_dta_link_inline_ui()` | Active: variant of above                             | poor | 20     |
| `mod_dwnld_dta_xlsx_server()`    | Active: `mod_wt_dwnload_newsrv`                      | poor | 30     |
| `mod_dwnld_file_server()`        | Active: `mod_wt_dwnload_newsrv`, `mod_plot_pti2_srv` | poor | 12     |

### `mod_dwnld_local_file.R`

Download handler for a local file (metadata PDF, shapes zip); legacy unused.

| Function                        | Status                          | Docs | ~Lines |
| ------------------------------- | ------------------------------- | ---- | ------ |
| `mod_dwnld_local_file_ui()`     | Legacy: not used in active path | poor | 10     |
| `mod_dwnld_local_file_server()` | Legacy: not used in active path | poor | 15     |

### `mod_drop_inval_adm.R`

Drops admin levels where all weighted indicators are unavailable.

| Function               | Status                       | Docs | ~Lines |
| ---------------------- | ---------------------------- | ---- | ------ |
| `mod_drop_inval_adm()` | Active: `mod_plot_pti2_srv`  | poor | 60     |
| `get_vars_un_avbil()`  | Active: `mod_drop_inval_adm` | no   | 35     |
| `get_min_admin_wght()` | Active: `mod_drop_inval_adm` | no   | 20     |
| `drop_inval_adm()`     | Active: `mod_drop_inval_adm` | no   | 15     |

### `mod_first_open_count.R`

Tracks whether a tab has been opened at least once to trigger deferred rendering.

| Function                        | Status                                           | Docs | ~Lines |
| ------------------------------- | ------------------------------------------------ | ---- | ------ |
| `mod_first_open_count_server()` | Active: `mod_plot_pti2_srv`, `mod_dta_explorer2` | poor | 25     |

### `mod_tab_open.R`

Reactive that fires once when the hosting tab is first navigated to.

| Function                       | Status                       | Docs | ~Lines |
| ------------------------------ | ---------------------------- | ---- | ------ |
| `mod_tab_open_first_newserv()` | Active: `mod_ptipage_newsrv` | poor | 25     |

### `mod_waiter.R`

Shows/hides a `waiter` loading overlay while data loads.

| Function              | Status                                                | Docs | ~Lines |
| --------------------- | ----------------------------------------------------- | ---- | ------ |
| `mod_waiter_ui()`     | Active: `mod_ptipage_twocol_ui`, `mod_ptipage_box_ui` | poor | 28     |
| `mod_waiter_newsrv()` | Active: `mod_ptipage_newsrv`                          | poor | 30     |
| `make_spinner()`      | Active: `mod_waiter_ui`                               | no   | 8      |

---

### `supporting-goe-prep.R` — Dev/Metadata Plotting

Dev-only ggplot helpers for rendering admin-level maps in metadata PDFs and profiling.

| Function                   | Status                           | Docs | ~Lines |
| -------------------------- | -------------------------------- | ---- | ------ |
| `plot_admin_level_ggmap()` | Legacy: dev/metadata PDF scripts | poor | 40     |
| `plot_pti_plotslist()`     | Legacy: dev scripts              | poor | 18     |
| `gg_admin_list()`          | Legacy: dev scripts              | poor | 40     |
| `get_intercestions()`      | Legacy: dev scripts              | poor | 10     |
| `plot_single_ggmap()`      | Legacy: `plot_admin_level_ggmap` | poor | 16     |

---

## Key Design Observations

1. **Two-generation duplication**: Legacy modules (`mod_weights`, `mod_calc_pti`, `mod_map_pti_leaf`, `mod_explorer`, `mod_info_page`) coexist with modern replacements (`mod_wt_inp`, `mod_calc_pti2`, `mod_plot_pti2`, `mod_dta_explorer2`, `mod_infotab`). The legacy modules add ~1500 lines of unused code.

2. ~~**Duplicated function definitions**~~: _Verified 2026-04-29_ — `get_indicators_list()` is defined only in `dta_cleaners.R`. No duplication exists.

3. **Documentation is generally poor**: Most internal modules use `@noRd` with no parameter or return documentation. Only `launch_pti()`, `launch_pti_onepage()`, `create_new_pti()`, and dataset docs approach completeness.

4. **Comment-disabled code**: Many files contain large blocks of commented-out code (e.g., `mod_map_pti_leaf.R`, `mod_plot_poly_leaf.R`, `mod_weights.R`, `calc_pti_expander.R`), adding visual noise.

5. **Deprecated dplyr usage**: Heavy use of superseded functions: `group_by_at()`, `distinct_at()`, `rename_at()`, `filter_at()`, `summarise_all()`, `vars()`, `select_if()`. These should migrate to `across()` and `.by` syntax.

6. **Diff-based map updates**: `mod_plot_poly_leaf_server()` implements smart diff-rendering, only re-drawing changed layers — a good pattern worth preserving.

7. **`wt_dta` is the data spine**: A single reactive list carrying `weights_clean`, `indicators_list`, and all admin-level data flows through every module. Changes to this structure affect the entire pipeline.
