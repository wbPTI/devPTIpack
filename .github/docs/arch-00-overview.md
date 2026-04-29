The objective is to revise and refactor this package to a modern, clean, and well-documented structure.
To do this we follow a workflow:

1. Understand and identify active and redundant components to focus only on the active ones. IMPORTANT: identify functions and features that will be exported and those that will stay internal.
   See: [./arch-01-cleanup.md](./arch-01-cleanup.md) for the summary of the candidates for removal.

2. Thoroughly document active components only.
   See: [./arch-02-docs.md](./arch-02-docs.md) for the documentation template and [/.claude/rules/roxygen-documentation.md](../../.claude/rules/roxygen-documentation.md) for the documentation rules.

3. Adopt a TDD approach to existing code and implement tests for all features present in the package.
   See: [./arch-03-testing.md](./arch-03-testing.md) for the testing strategy and referenced guidelines.

4. Identify missing features and gaps — such as data validation functions, informative error/warning messages — that need to be developed, improved, and tested in the existing functions.

5. Implement new features through the test-driven development approach.

6. Restructure the workspace — remove `dev/`, repurpose `vignettes/` as component documentation.
   See: [./arch-04-workspace.md](./arch-04-workspace.md) for the workspace restructuring plan.

7. Develop documentation on all aspects of this package via vignettes.
   Vignettes are grouped into sections (pkgdown dropdown menus) and the list is open-ended.
   See: [./arch-04-workspace.md](./arch-04-workspace.md) for vignette groups, planned articles, and pkgdown structure.

8. Ensure clean and nice deployment of the package to GitHub as well as the package website on GitHub Pages.

9. Implement new data ingestion pathways — enabling hexagonal API data sources as inputs to the PTI pipeline. Developer-oriented pre-deployment tooling to prepare app data from custom shapefiles via H3 hex queries.
   See: [./arch-05-hex-ingestion.md](./arch-05-hex-ingestion.md) for the design plan.







# Legacy Architecture — `devPTIpack`

> Auto-generated review of the R package structure as of 2026-04-29.

## Package Summary

**devPTIpack** is a `golem`-based Shiny R package for computing, visualizing, and exploring **Priority Targeting Indices (PTI)** — composite scores from weighted indicators mapped across hierarchical administrative boundaries.

---

## Workspace Structure

### Current (pre-cleanup)

```
devPTIpack/
├── R/                        # All package source code (~50 files)
├── data/                     # Bundled sample data (ukr_shp, ukr_mtdt_full)
├── inst/
│   ├── app/www/              # Static assets (CSS, images)
│   ├── sample_pti/           # Sample deployed app
│   └── template_pti/         # Skeleton for create_new_pti()
├── dev/                      # ⛔ REMOVE — historical scratch scripts
├── tests/testthat/           # Unit tests
├── vignettes/                # Stub only (dataprep.Rmd)
├── DESCRIPTION
├── NAMESPACE
└── _pkgdown.yml
```

### Target (post-cleanup)

```
devPTIpack/
├── R/                            # Package source (modern stack only)
├── data/                         # Bundled sample data (ukr_shp, ukr_mtdt_full)
├── inst/
│   ├── app/www/                  # Static assets (CSS, images)
│   ├── sample_pti/              # Sample deployed app
│   └── template_pti/           # Skeleton for create_new_pti()
├── tests/testthat/              # Unit and integration tests
├── vignettes/                   # Component documentation (grouped on pkgdown site)
│   ├── pti-overview.Rmd              # Getting Started
│   ├── data-preparation.Rmd          # Getting Started
│   ├── calculation-pipeline.Rmd      # Methodology
│   ├── app-ui-modules.Rmd            # App Architecture
│   ├── deployment-guide.Rmd          # Getting Started
│   ├── project-history.Rmd           # Project History
│   └── ...                           # (open-ended, more added over time)
├── man/                         # Auto-generated roxygen docs
├── DESCRIPTION
├── NAMESPACE
├── _pkgdown.yml
└── .github/docs/                # Architecture decisions (not shipped)
```

See [./arch-04-workspace.md](./arch-04-workspace.md) for full details on this restructuring.

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
