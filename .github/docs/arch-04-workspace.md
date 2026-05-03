# Workspace Restructuring Plan — `devPTIpack`

> Defines the target folder layout after cleanup, the role of `vignettes/` as
> component documentation, and the removal of `dev/`.

---

## Current State (Problems)

| Folder | Issue |
|--------|-------|
| `dev/` | Contains ~14 ad-hoc development scripts (`40-DT-as-numericInput.R`, `65-new-pti-plot.R`, etc.) and a one-off Shiny app (`mapDwnldApp/`). None are referenced by package code, tests, or CI. They are undocumented scratch files that confuse new contributors. |
| `vignettes/` | Contains a single stub (`dataprep.Rmd`) with TBD content. Unused as a documentation vehicle. |
| `tests/testthat/` | Sparse coverage, no systematic structure. |

---

## Target State

```
devPTIpack/
├── R/                            # Package source (cleaned, modern stack only)
├── data/                         # Bundled sample data (ukr_shp, ukr_mtdt_full)
├── inst/
│   ├── app/www/                  # Static assets (CSS, images)
│   ├── sample_pti/              # Sample deployed app
│   └── template_pti/           # Skeleton for create_new_pti()
├── tests/testthat/              # Unit and integration tests
├── vignettes/                   # Component documentation (see below)
│   ├── pti-overview.Rmd              # What PTI is, theory, use cases
│   ├── data-preparation.Rmd          # How to prepare shapes + metadata
│   ├── calculation-pipeline.Rmd      # Internal data flow & methodology
│   ├── app-ui-modules.Rmd            # How UI modules connect
│   └── deployment-guide.Rmd          # How to deploy a PTI app
├── man/                         # Auto-generated roxygen docs
├── DESCRIPTION
├── NAMESPACE
├── _pkgdown.yml
├── README.Rmd
└── .github/docs/                # Architecture decisions (internal, not shipped)
```

**Removed:**
- `dev/` — entire folder deleted (contents are historical scratch, already captured in git history)

---

## Vignettes as Component Documentation

The `vignettes/` folder becomes the primary place for **explaining how the package works**, both externally (for PTI app developers) and internally (for package maintainers). Each vignette is a standalone article rendered on the pkgdown site.

### Vignette Groups (pkgdown dropdown menus)

Vignettes are **not** a fixed list. New articles are added as documentation grows.
The `_pkgdown.yml` organises them into dropdown sections on the package website.

#### Group: Getting Started

| Vignette | Content |
|----------|--------|
| `pti-overview.Rmd` | What PTI is, theory, use cases, links to publications |
| `data-preparation.Rmd` | Required shapes/metadata structure, validation, worked example with `ukr_shp` / `ukr_mtdt_full` |
| `deployment-guide.Rmd` | `create_new_pti()`, placing data, configuring, deploying to Connect/shinyapps.io |

#### Group: Methodology

| Vignette | Content |
|----------|--------|
| `calculation-pipeline.Rmd` | Full data transformation flow (see below) |
| *(future)* | Scoring alternatives, sensitivity analysis, etc. |

#### Group: App Architecture

| Vignette | Content |
|----------|--------|
| `app-ui-modules.Rmd` | Module tree, reactive flow, customisation hooks |
| *(future)* | Theming, CSS customisation, adding custom tabs |

#### Group: Project History

| Vignette | Content |
|----------|--------|
| `project-history.Rmd` | Chronology of PTI development, past country deployments, lessons learned |
| *(future)* | Country-specific write-ups, links to deployed apps |

---

### Key Vignette: `calculation-pipeline.Rmd`

The core methodology vignette explaining the mathematical/data flow:

```
Raw Input                    Function                        Transformation
───────────────────────────────────────────────────────────────────────────
metadata Excel         →  fct_template_reader()        →  Parsed list
shapes .rds            →  get_mt()                     →  Mapping table (Pcod cross-ref)
                       →  clean_geoms()                →  ID-only tibbles (no geometry)
parsed metadata        →  pivot_pti_dta()              →  Long format (var_code, value)
weights + long data    →  get_weighted_data()          →  Weighted values per scheme
weighted data          →  get_scores_data()            →  Z-score standardised
scored data + mt       →  expand_adm_levels()          →  Cross-level expansion
expanded data          →  merge_expandedn_adm_levels() →  Merged across source levels
merged + IDs           →  agg_pti_scores()             →  Row-sum → pti_score
aggregated             →  label_generic_pti()          →  HTML popup labels
labelled + geometries  →  structure_pti_data()         →  SF ready for leaflet
```

Key concepts to document:
- **Pivoting**: Wide → long by indicator
- **Weighting**: `value × weight`, filtering zero-weight indicators
- **Standardisation**: Per-variable z-score: $(x - \bar{x}) / \sigma$
- **Expansion**: Mapping table join — upscaling (mean aggregation) and downscaling (direct join)
- **Aggregation**: Row-sum of all standardised weighted indicators per polygon
- **Labelling**: Glue templates for interactive popups
- **Structuring**: Pivot back to wide, join to SF geometries

### Key Vignette: `app-ui-modules.Rmd`

How the Shiny modules compose into a working app:

```
launch_pti() / launch_pti_onepage()
  │
  ├─ navbarPage (tabs: Info, PTI, Compare, Explorer)
  │
  ├─ mod_ptipage_newsrv ─────────────────────────────────┐
  │    ├─ mod_wt_inp_server  (weights panel)             │
  │    │    └─ mod_DT_inputs_server (editable DT)        │
  │    ├─ mod_calc_pti2_server (calculation)             │
  │    └─ mod_plot_pti2_srv (map orchestration)          │
  │         ├─ mod_plot_poly_leaf_server (polygons)      │
  │         └─ mod_plot_poly_legend_server (legend)      │
  │                                                      │
  ├─ mod_pti_comparepage_newsrv (comparison page)        │
  │    └─ mod_plot_pti2_srv (reused)                     │
  │                                                      │
  ├─ mod_dta_explorer2_server (data explorer)            │
  │    └─ mod_plot_poly_leaf_server (reused)             │
  │                                                      │
  └─ mod_infotab_server (landing page + guide)           │
```

Key topics:
- Module ID namespacing pattern
- Reactive data flow between modules
- How `shp_dta` and `inp_dta` propagate
- Side panel controls (nbins, admin levels, downloads)

### Key Vignette: `deployment-guide.Rmd`

- Using `create_new_pti()` to scaffold
- Placing data files
- Configuring the app
- Deploying to RStudio Connect / shinyapps.io
- Generating the metadata PDF with `render_metadata()`
  *(Note: deleted in arch-01 Batch 6 as broken on shipped installs —
  `system.file("pti-metadata-pdf.Rmd", ...)` resolved to `""` because
  the Rmd lives at `inst/sample_pti/app-data/`, not the `inst/` root.
  Reintroduce as a fixed function with the corrected key when arch-04
  picks up this vignette.)*

---

## `dev/` Removal Plan

All files in `dev/` are development scratch with zero references from package code:

| File | Content | Action |
|------|---------|--------|
| `40-DT-as-numericInput.R` | DT widget prototyping | Delete — implemented in `mod_DT_inputs.R` |
| `42-wt-page-layout.R` | Weight page layout experiments | Delete — implemented in `mod_wt_inp.R` |
| `43-inp-to-exp.R` | Export format experiments | Delete — implemented in `fct_inp_for_exp.R` |
| `45-one-page-pti.R` | One-page PTI prototype | Delete — implemented in `launch_pti_onepage()` |
| `46-new-run-pti.R` | Runner experiments | Delete — implemented in `launch_pti()` |
| `60-new-pti-calc.R` | Calculation pipeline dev | Delete — implemented in `mod_calc_pti2.R` |
| `65-new-pti-plot.R` | Plot module dev | Delete — implemented in `mod_plot_pti2.R` |
| `68-new-dta-exprt.R` | Export module dev | Delete — implemented in `mod_export_pti_data.R` |
| `70-dta-explorer.R` | Explorer dev | Delete — implemented in `mod_dta_explorer2.R` |
| `75-legend-makers.R` | Legend dev | Delete — implemented in `fct_legend_map_satelites.R` |
| `85-profiling.R` | Performance profiling | Delete — one-off |
| `90-app-examples.R` | Example app configs | Delete — covered by `inst/sample_pti/` |
| `run_dev.R` | golem dev runner | Delete — replaced by `launch_pti()` |
| `run_prod.R` | golem prod runner | Delete — replaced by `launch_pti()` |
| `mapDwnldApp/` | Standalone download app | Delete — functionality in `mod_map_dwnld_srv` |

**Timing:** Delete `dev/` in Batch 1 (it has zero callers). Git history preserves everything.

---

## `_pkgdown.yml` Updates

After vignettes are written, update `_pkgdown.yml` to structure the pkgdown site.
Each `title` becomes a dropdown menu. New vignettes slot into the appropriate group.

```yaml
articles:
  - title: Getting Started
    navbar: Getting Started
    contents:
      - pti-overview
      - data-preparation
      - deployment-guide
  - title: Methodology
    navbar: Methodology
    contents:
      - calculation-pipeline
      # future methodology articles added here
  - title: App Architecture
    navbar: App Architecture
    contents:
      - app-ui-modules
      # future architecture articles added here
  - title: Project History
    navbar: Project History
    contents:
      - project-history
      # future country write-ups, retrospectives added here
```

> This structure is open-ended. As new vignettes are authored, they are added to
> the appropriate group in `_pkgdown.yml` and automatically appear in the site
> dropdown navigation.

---

## Migration Steps

1. **Batch 1 cleanup** — Delete `dev/` folder entirely
2. **Replace vignette stub** — Rename/rewrite `dataprep.Rmd` → `data-preparation.Rmd`
3. **Create vignette skeletons** — Add YAML front matter for each planned vignette
4. **Write `calculation-pipeline.Rmd`** first (most value, documents the core methodology)
5. **Write remaining vignettes** as documentation phase progresses
6. **Update `_pkgdown.yml`** to include article navigation
