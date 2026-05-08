# Documentation Architecture — `devPTIpack`

> **Source of truth** for the pkgdown website structure, vignette content plan,
> app-template project layout, and the PR sequence that delivers them.
> Supersedes the vignette/pkgdown section of `arch-04-workspace.md`.
>
> **Design status:** function signatures for new validation and data-prep
> infrastructure are **provisional** — marked ⚠ PROVISIONAL throughout.
> They must be revisited and agreed before implementation PRs open.
>
> **Revision 2 (2026-05-08):** Updated after grill session comparing plan
> against actual project state. Key changes: keep `dataprep.qmd`, defer
> `build-pti.qmd` rewrite, align template/website numbering, add PR #A2
> for template scaffold + Rwanda data, add companion issues.

---

## 1. Decisions locked

| Decision                      | Resolution                                                                                 |
| ----------------------------- | ------------------------------------------------------------------------------------------ |
| Branch base                   | `eb-docs-pkgdown` (NOT `main`)                                                             |
| Primary audience              | PTI deployers — WB staff who prepare country data and deploy the app                       |
| Deployer entry state          | Has shapefiles, needs to build metadata Excel from scratch (sometimes from zero)           |
| Writing style                 | Tutorial-style using a worked example country                                              |
| Tutorial country              | **Rwanda** — 5 Adm1 provinces, 30 Adm2 districts, clean geoBoundaries geometry             |
| Tutorial shapefiles           | 3 separate GeoJSON files: `rwa_adm0.geojson`, `rwa_adm1.geojson`, `rwa_adm2.geojson`       |
| Tutorial metadata Excel       | Fully **synthetic** Rwanda district-level indicator data                                   |
| Tutorial data in template     | `inst/template_pti/sample-data/` — raw files (GeoJSON + xlsx) copied by `create_new_pti()` |
| Tutorial data as package data | `data/rwa_shp.rda` + `data/rwa_mtdt_full.rda` — compiled R objects for `@examples`         |
| Ukraine data                  | **Kept** — `ukr_shp`/`ukr_mtdt_full` remain for tests; not deprecated                      |
| Validation app naming         | Functions: `validate_XXX()` · Shiny launchers: `app_validate_XXX()`                        |
| Validation app style          | Standalone — called like `launch_pti()`, opens browser                                     |
| Master orchestration          | `00-master.R` — sourceable R script calling `quarto::quarto_render()` per step file        |
| Template `.qmd` files         | **Bare scaffolds** — section headers + commented starter code + link to website tutorial   |
| Website vignettes             | **Instructional tutorials** — Rwanda worked examples, explanations, `eval=FALSE` code      |
| Vignette API target           | Written against **planned API** (`validate_shp()` etc.) with `eval=FALSE` blocks           |
| `02-*` files                  | PLACEHOLDER — NOT sourced by `00-master.R`; user runs manually when needed                 |
| `app-data/`                   | Holds compiled deployment-ready files; git treatment requires explicit user decision       |
| Deployment bundle             | `app.R` + `landing-page.md` + `app-data/` only — `docs/` excluded                          |
| gh-pages project docs         | Optional — flagged clearly in master and Step 6                                            |
| pkgdown articles index        | Build a PTI sub-pages appear in BOTH navbar dropdown AND articles index                    |
| `build-pti.qmd`               | **Kept as-is** until PR #F — then rewritten as Overview                                    |
| `dataprep.qmd`                | **Kept** — moved to bottom of "Build a PTI" dropdown with separator                        |
| Reference tab grouping        | Remove manual YAML groupings in PR #A; migrate to `@family` tags (companion issue §7)      |
| Step numbering                | Website: Step 0 = setup, Steps 1–6. Template: `01`–`06`. Aligned 1:1 from Step 1.          |

---

## 2. Navbar structure

```
LEFT:   About (→ index.html) | PTI Methodology ▾ | Build a PTI ▾
RIGHT:  Search | Reference | Resources | GitHub

PTI Methodology ▾                Build a PTI ▾
├─ Detailed methodology          ├─ Overview
├─ Overview paper                ├─ ───
└─ ───                           ├─ Step 0 — Setup new project
   Past Projects                 ├─ Step 1 — Shapefiles
                                 ├─ Step 2 — Zonal stats (optional)
                                 ├─ Step 3 — Metadata Excel
                                 ├─ Step 4 — HEX data
                                 ├─ Step 5 — Compile & finalise
                                 ├─ Step 6 — Deploy
                                 ├─ ───
                                 └─ Data preparation reference
```

### `_pkgdown.yml` navbar component map

```yaml
navbar:
  structure:
    left:  [about, methodology, build-pti]
    right: [search, reference, resources, github]
  components:
    about:
      text: About
      href: index.html
    methodology:
      text: PTI Methodology
      menu:
        - text: Detailed methodology
          href: articles/methodology.html
        - text: Overview paper
          href: articles/overview-paper.html
        - text: "---"
        - text: Past Projects
          href: articles/past-projects.html
    build-pti:
      text: Build a PTI
      menu:
        - text: Overview
          href: articles/build-pti.html
        - text: "---"
        - text: "Step 0 — Setup new project"
          href: articles/build-pti-0-setup.html
        - text: "Step 1 — Shapefiles"
          href: articles/build-pti-1-shapefiles.html
        - text: "Step 2 — Zonal stats (optional)"
          href: articles/build-pti-2-zonal-stats.html
        - text: "Step 3 — Metadata Excel"
          href: articles/build-pti-3-metadata.html
        - text: "Step 4 — HEX data"
          href: articles/build-pti-4-hex.html
        - text: "Step 5 — Compile & finalise"
          href: articles/build-pti-5-compile.html
        - text: "Step 6 — Deploy"
          href: articles/build-pti-6-deploy.html
        - text: "---"
        - text: Data preparation reference
          href: articles/dataprep.html
    reference:
      text: Reference
      href: reference/index.html
```

---

## 3. Vignette file map

Two distinct file sets exist: **website vignettes** (instructional tutorials) and
**template scaffolds** (bare workbooks copied by `create_new_pti()`). This section
covers website vignettes; template scaffolds are described in §5.

| File                                             | Status                                              | Content                                                    |
| ------------------------------------------------ | --------------------------------------------------- | ---------------------------------------------------------- |
| `vignettes/articles/methodology.qmd`             | existing — keep                                     | Detailed technical methodology                             |
| `vignettes/articles/overview-paper.qmd`          | **create — placeholder**                            | Policy-oriented overview; stub only for now                |
| `vignettes/articles/past-projects.qmd`           | existing — keep                                     | Country catalogue                                          |
| `vignettes/articles/build-pti.qmd`               | existing — **keep as-is until PR #F**, then rewrite | Overview (see §4.1)                                        |
| `vignettes/articles/build-pti-0-setup.qmd`       | **create — stub in PR #A, write in PR #E**          | Step 0 (see §4.2)                                          |
| `vignettes/articles/build-pti-1-shapefiles.qmd`  | **create — stub in PR #A, write in PR #E**          | Step 1 (see §4.3)                                          |
| `vignettes/articles/build-pti-2-zonal-stats.qmd` | **create — optional stub**                          | Step 2 — optional, user-defined zonal stats                |
| `vignettes/articles/build-pti-3-metadata.qmd`    | **create — stub in PR #A, write in PR #E**          | Step 3 (see §4.4)                                          |
| `vignettes/articles/build-pti-4-hex.qmd`         | **create — future feature stub**                    | Step 4 (see §4.5) — blocked by HEX API                     |
| `vignettes/articles/build-pti-5-compile.qmd`     | **create — stub in PR #A, write in PR #F**          | Step 5 (see §4.6)                                          |
| `vignettes/articles/build-pti-6-deploy.qmd`      | **create — stub in PR #A, write in PR #F**          | Step 6 (see §4.7)                                          |
| `vignettes/articles/dataprep.qmd`                | existing — **keep**                                 | Data preparation reference (moved to Build a PTI dropdown) |

### Stub content pattern

All stubs created in PR #A follow the same pattern: YAML front matter + one-line
description + section headers from §4 (no body content under headings). Exception:
Step 4 stub has a distinct "feature not ready" callout (see §4.5).

### `_pkgdown.yml` articles section

```yaml
articles:
  - title: PTI Methodology
    contents:
      - articles/methodology
      - articles/overview-paper
      - articles/past-projects
  - title: Build a PTI
    contents:
      - articles/build-pti
      - articles/build-pti-0-setup
      - articles/build-pti-1-shapefiles
      - articles/build-pti-2-zonal-stats
      - articles/build-pti-3-metadata
      - articles/build-pti-4-hex
      - articles/build-pti-5-compile
      - articles/build-pti-6-deploy
      - articles/dataprep
```

---

## 4. Vignette content plan

All website vignettes use `eval=FALSE` code blocks — code must be copy-paste
executable but is not run during pkgdown build. Steps 1 and 3 include condensed
column-requirement tables inline and link to `dataprep.qmd` for the full spec.

### 4.1 Overview (`build-pti.qmd`) — **rewrite in PR #F**

> **Note:** The current `build-pti.qmd` is a working ~400-line tutorial. It is
> kept as-is until PR #F, when Steps 0–3 have real content. The rewrite replaces
> it with a short overview:

1. What data a PTI app needs: shapefiles + metadata Excel (+ optionally HEX data)
2. Reproducible quickstart — launch app with bundled Rwanda data in < 5 lines
3. Step-by-step journey map: numbered list, one sentence per step, each links to its sub-page
4. Note that **Step 4 (HEX) is order-flexible** — can be done before or after Step 3
5. Pointer to `00-master.R` as the one-command path through all steps

### 4.2 Step 0 — Setup new project (`build-pti-0-setup.qmd`) — **write**

1. Install `devPTIpack`
2. Call `create_new_pti("path/to/my_pti")` — what it copies and why
3. Annotated folder structure (see §5)
4. Note on `00-master.R` — what it does, when to use it
5. Note on `app-data/` and git — explicit warning: decide consciously whether to track this folder
6. Note on sample data in `sample-data/` — run the template as-is first, then replace with your own

### 4.3 Step 1 — Shapefiles (`build-pti-1-shapefiles.qmd`) — **write**

Sections in order:

**A. Requirements** (condensed table + link to [Data preparation reference §1](dataprep.html))
- Column naming convention: `admin<N>Pcod` (unique polygon code), `admin<N>Name`, `area`
- Projection: WGS84 (EPSG:4326)
- Geometry simplification guidance — file size vs. rendering performance tradeoff
- Single-file simple case vs. multi-level advanced case (flagged as advanced, separate subsection)

**B. Load pre-existing official shapefiles** ⚠ PLACEHOLDER
- WB internal official shape files — function `load_official_shp(country)` ⚠ PROVISIONAL
- Marked clearly as "feature under development"

**C. Validate your shapefile**
- Call `validate_shp(shp)` ⚠ PROVISIONAL — robust replacement/extension of `validate_geometries()`
- Call `app_validate_shp(shp)` ⚠ PROVISIONAL — opens mini Shiny app for visual checking
- How to interpret validation output

**D. Fix problems & save**
- Code snippets: fix projection, rename columns, simplify geometry
- How and where to save: `app-data/shapes.rds` via `saveRDS()`
- Format: `.rds` for app use; keep original `.geojson` as source-of-truth

**E. Advanced: multiple admin levels** (flagged as advanced)
- The mapping table requirement
- Unique nested naming convention: `admin<N>_HumanName` slot names
- How `ukr_shp` demonstrates this pattern
- When to use multi-level vs. single-level

**Rwanda worked example** threads through sections A → C → D using `rwa_adm1.geojson`.
Advanced section uses `rwa_adm1.geojson` + `rwa_adm2.geojson` combined.

### 4.3a Step 2 — Zonal stats (`build-pti-2-zonal-stats.qmd`) — **optional stub**

> This step is optional. Use it when you need to extract zonal statistics
> from raster data (e.g., satellite imagery, gridded population data) before
> building your metadata Excel in Step 3.

Stub sections:
1. When you need zonal stats
2. Common tools: `exactextractr`, `terra::extract()`
3. Output format: one column per indicator, keyed by `admin<N>Pcod`
4. How the output feeds into Step 3

### 4.4 Step 3 — Metadata Excel (`build-pti-3-metadata.qmd`) — **write**

1. What the Excel file is and why it exists
2. Sheet structure — document every sheet (condensed table + link to [Data preparation reference §2](dataprep.html)):
   - `general` — country-level metadata (name, ISO code, etc.)
   - `admin<N>_*` — one sheet per admin level; columns: `admin<N>Pcod`, `admin<N>Name`, `area`, `year`, one column per `var_code`
   - `metadata` — indicator dictionary; document every column: `var_code`, `var_name`, `pillar_name`, `spatial_level`, `fltr_exclude_pti`, `fltr_exclude_explorer`, `fltr_overlay_pti`, `fltr_overlay_explorer`, `legend_revert_colours`
   - `weights_table` — optional pre-saved weighting schemes
3. Column-by-column reference table with type, valid values, description
4. Simple case walkthrough: one admin level (Adm1), 3 indicators, Rwanda synthetic data
5. Multi-level case: Adm1 + Adm2, some indicators available at both levels
6. Validate metadata:
   - `validate_metadata_full(inp_dta, shp_dta)` ⚠ PROVISIONAL — cross-validates metadata against shapefile
   - `app_validate_metadata(inp_dta, shp_dta)` ⚠ PROVISIONAL — mini Shiny app
7. How and where to save: `app-data/metadata.xlsx`

### 4.5 Step 4 — HEX data (`build-pti-4-hex.qmd`) — **future feature stub**

> ⚠ **This step depends on the HEX data API, which is under active development.**
> Content will be added when the API and integration functions are production-ready.
> This is not merely unwritten documentation — the underlying infrastructure
> does not yet exist.

Placeholder sections (stubs only):

1. What HEX data is and why it is useful
2. Fetching HEX data via the API — `fetch_hex_data(...)` ⚠ PROVISIONAL
3. Exploring available variables — `app_explore_hex(...)` ⚠ PROVISIONAL mini Shiny app
4. Matching HEX data to your shapefiles and creating/augmenting metadata:
   `create_hex_metadata(shp, vars, metadata = NULL)` ⚠ PROVISIONAL
5. Supported admin levels: Adm2, Adm3, Adm4 — separately or combined
6. **Note on ordering:** this step can be done before or after Step 3

### 4.6 Step 5 — Compile & finalise (`build-pti-5-compile.qmd`) — **write**

1. What "compile" means: combining shapes + metadata 1 + metadata 2 (HEX) into deployment-ready files
2. Run `compile_pti_data(shp_dta, inp_dta, ...)` ⚠ PROVISIONAL — validates combined inputs, produces final `app-data/` contents (design TBD in PR #D)
3. Generate the metadata PDF:
   - Fixed `render_metadata()` (broken in Batch 6 — path bug; fix is a pre-requisite for this PR)
   - Open the PDF, check it looks correct
4. Landing page: `landing-page.md` lives in project root, edit it to describe the app
   (covered here in Overview only — cross-reference from Overview article)
5. Final pre-launch checklist
6. Launch locally: `launch_pti(shp_dta, inp_dta)` — what each tab does
7. Pointer to `00-master.R` as the automated path through steps 1–5

### 4.7 Step 6 — Deploy (`build-pti-6-deploy.qmd`) — **write**

1. What gets deployed: `app.R` + `landing-page.md` + `app-data/` — nothing else
2. WB deployment targets:
   - Internal: WB Posit Connect (requires VPN / network access)
   - External: public-facing WB Posit Connect or shinyapps.io
3. Permissions setup — how to get access to WB Posit Connect, who to contact, links to internal resources
4. CLI deployment code snippets (`rsconnect::deployApp()`)
5. UI deployment walkthroughs:
   - RStudio: GIF showing Deploy button → fill in server details
   - Positron: GIF showing equivalent flow
6. Post-deployment: setting viewer permissions on Posit Connect so others can access the app
7. Optional: deploy `docs/` to GitHub Pages — how `00-master.R` handles this
8. Links to relevant WB internal Posit Connect documentation
9. Where to ask for help (GeoPov SharePoint, GitHub issues)

---

## 5. App template project structure (`inst/template_pti/`)

This is what `create_new_pti()` copies to the user's project folder. Template
`.qmd` files are **bare scaffolds** — section headers with commented starter
code and a link to the corresponding website tutorial. They are deployer
workbooks, not instructional content. The user fills in their own country-specific
workflow and runs each one.

```
my_pti_project/
│
├── README.md                       # Explains file order, links to website tutorials
├── 00-master.R                     # Sourceable R script; calls quarto_render() per step
│                                   # Skips 02-* files; uses app-data/ outputs instead
│
├── 01-shapes.qmd                   # Step 1 — load, validate, fix, save shapefiles
├── 02a-user-zonal-stats.qmd        # ⚠ OPTIONAL — user's own zonal stats extraction
│                                   #   NOT sourced by 00-master.R
│                                   #   Must be run separately before 03-metadata.qmd
│                                   #   Add 02b, 02c etc. as needed
├── 03-metadata.qmd                 # Step 3 — prepare & validate metadata Excel
├── 04-hex-data.qmd                 # Step 4 — fetch HEX, match to shapes, augment metadata
├── 05-compile.qmd                  # Step 5 — compile all inputs, generate PDF
├── 06-deploy.qmd                   # Step 6 — deploy app; optional gh-pages docs
│
├── app.R                           # Shiny app (deployed)
├── landing-page.md                 # App landing page text (deployed)
│
├── sample-data/                    # Rwanda tutorial data — replace with your own
│   ├── rwa_adm0.geojson            # Rwanda country boundary (geoBoundaries, CC-BY 4.0)
│   ├── rwa_adm1.geojson            # Rwanda 5 provinces
│   ├── rwa_adm2.geojson            # Rwanda 30 districts
│   ├── sample-metadata-adm1.xlsx   # Synthetic indicator data at Adm1 level
│   └── sample-metadata-adm1-adm2.xlsx  # Synthetic data at Adm1 + Adm2 (advanced)
│
├── app-data/                       # ⚠ THINK TWICE before adding to .gitignore
│   │                               #   May contain sensitive or large files
│   │                               #   Decide consciously: track in git or not
│   ├── shapes.rds                  # Compiled shapefile (output of 01-shapes.qmd)
│   ├── metadata.xlsx               # Clean metadata Excel (output of 03/04/05)
│   └── pti-metadata.pdf            # Generated PDF documentation (output of 05)
│
├── R/
│   └── _disable_autoload.R         # golem boilerplate
│
└── docs/                           # Quarto-rendered HTML (output of 00-master.R)
                                    # Optional: deploy to GitHub Pages
                                    # NOT deployed to Posit Connect / shinyapps.io
```

### Template scaffold content pattern

Each template `.qmd` file follows this pattern:

```markdown
---
title: "Step 1 — Prepare shapefiles"
---

<!-- For detailed instructions, see:
     https://worldbank.github.io/devPTIpack/articles/build-pti-1-shapefiles.html -->

## Load raw shapefiles

\```{r}
# library(sf)
# admin1 <- read_sf("sample-data/rwa_adm1.geojson")
\```

## Clean and validate

\```{r}
# validate_geometries(my_shp)
\```

## Save to app-data/

\```{r}
# saveRDS(my_shp, "app-data/shapes.rds")
\```
```

### Numbering alignment

Template file numbering is aligned with website step numbering (Step 1 onward).
Step 0 (Setup) has no template file — `create_new_pti()` *is* Step 0.

| Template file              | Website step                    |
| -------------------------- | ------------------------------- |
| `01-shapes.qmd`            | Step 1 — Shapefiles             |
| `02a-user-zonal-stats.qmd` | Step 2 — Zonal stats (optional) |
| `03-metadata.qmd`          | Step 3 — Metadata Excel         |
| `04-hex-data.qmd`          | Step 4 — HEX data               |
| `05-compile.qmd`           | Step 5 — Compile & finalise     |
| `06-deploy.qmd`            | Step 6 — Deploy                 |

---

## 6. New functions required (⚠ PROVISIONAL — signatures subject to revision)

> These names and signatures are a **starting point for design discussion**, not
> a committed API. Each must be designed, reviewed, and agreed before its
> implementation PR opens.

| Function                                          | Step | Type     | Notes                                                                                                                   |
| ------------------------------------------------- | ---- | -------- | ----------------------------------------------------------------------------------------------------------------------- |
| `validate_shp(shp)`                               | 2    | exported | Robust replacement/extension of `validate_geometries()`; returns structured validation report                           |
| `app_validate_shp(shp)`                           | 2    | exported | Standalone Shiny launcher for visual shapefile checking                                                                 |
| `load_official_shp(country)`                      | 2    | exported | Load WB internal official shapefiles; **TBD — future development**                                                      |
| `validate_metadata_full(inp_dta, shp_dta)`        | 3    | exported | Cross-validates metadata Excel against shapefile; extends existing `validate_metadata()`                                |
| `app_validate_metadata(inp_dta, shp_dta)`         | 3    | exported | Standalone Shiny launcher for metadata + shapefile visual cross-check                                                   |
| `fetch_hex_data(...)`                             | 4    | exported | Fetch pre-computed H3 res-6 HEX data via API; **TBD — HEX track**                                                       |
| `app_explore_hex(...)`                            | 4    | exported | Standalone Shiny launcher to explore and select HEX variables; **TBD — HEX track**                                      |
| `create_hex_metadata(shp, vars, metadata = NULL)` | 4    | exported | Match HEX data to shapefile polygons; create or augment metadata Excel; **TBD — HEX track**                             |
| `compile_pti_data(shp_dta, inp_dta, ...)`         | 5    | exported | Combine all inputs, run combined validation, write `app-data/` outputs (design TBD in PR #D)                            |
| `render_metadata(...)`                            | 5    | exported | Fixed version of the function deleted in Batch 6 (broken path); must be reintroduced with corrected `system.file()` key |

---

## 7. Companion GitHub issues

### 7.1 Reference page restructure — `@family` migration

> Out of scope for arch-09 implementation PRs. Open as a separate issue with
> this checklist:
>
> - [ ] Audit exported functions — which are public API vs. internal/golem plumbing
> - [ ] Add `@family` tags to all exported functions (drives pkgdown reference grouping)
> - [ ] Identify `@inheritParams` opportunities (shared params across `validate_*`, `launch_*`)
> - [ ] Replace complex examples with simple Rwanda-based ones where applicable
> - [ ] Audit implicit golem options parameters — consider making them explicit function arguments
> - [ ] Remove manual `reference:` groupings from `_pkgdown.yml`
>
> **Approach:** Use `@family` tags in roxygen2 to drive reference page groupings
> from source code rather than manual YAML. Current YAML groupings are removed
> in PR #A; the `@family` tags are added as part of this companion issue.

### 7.2 Rwanda package data + `@examples` migration

> Depends on PR #A2 (Rwanda data must be created first). Open as a separate
> issue:
>
> - [ ] Add `rwa_shp` and `rwa_mtdt_full` as package datasets (`data/rwa_shp.rda`, `data/rwa_mtdt_full.rda`)
> - [ ] Write `data.R` documentation for both datasets
> - [ ] Update all `@examples` blocks across exported functions to use Rwanda data
> - [ ] Keep `ukr_shp`/`ukr_mtdt_full` — no deprecation, used by test suite

---

## 8. PR sequence

| PR      | Branch                        | Scope                                                                                                                                                                                                                                  | Depends on                    |
| ------- | ----------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------- |
| **#A**  | `docs/navbar-restructure`     | `_pkgdown.yml` navbar restructure; remove manual reference groupings; create all stub `.qmd` files (YAML + section headers); `overview-paper.qmd` stub; Step 4 "feature not ready" stub; keep `build-pti.qmd` and `dataprep.qmd` as-is | —                             |
| **#A2** | `feat/template-scaffold`      | Download Rwanda GeoJSONs → `inst/template_pti/sample-data/`; generate synthetic metadata xlsx; create template `.qmd` scaffolds (commented starter code); `00-master.R`; `README.md`                                                   | —                             |
| **#B**  | `feat/validate-shp`           | Design + implement `validate_shp()` + `app_validate_shp()` with tests                                                                                                                                                                  | #A (naming convention)        |
| **#C**  | `feat/validate-metadata`      | Design + implement `validate_metadata_full()` + `app_validate_metadata()` with tests                                                                                                                                                   | #A, #B                        |
| **#D**  | `feat/compile-pti-data`       | Design + implement `compile_pti_data()` + fix `render_metadata()` with tests (design TBD)                                                                                                                                              | #B, #C                        |
| **#E**  | `docs/build-pti-steps-0-3`    | Write Steps 0, 1, 2 (stub), 3 vignettes — planned API, `eval=FALSE`, condensed tables + links to `dataprep.qmd`                                                                                                                        | #A, #A2                       |
| **#F**  | `docs/build-pti-overview-5-6` | Rewrite `build-pti.qmd` to overview; write Steps 5, 6 vignettes                                                                                                                                                                        | #D, #E                        |
| **#G**  | `feat/hex-integration`        | `create_hex_metadata()` + Step 4 vignette                                                                                                                                                                                              | parallel — blocked by HEX API |

> **Notes:**
> - #A and #A2 are **independent** — can be worked in parallel and merged in either order.
> - #E depends on **both** #A and #A2 (step vignettes reference template structure).
> - #B, #C, #D are feature PRs that require design review before opening.
> - The ⚠ PROVISIONAL function signatures in §6 must be finalised before their respective PRs.
> - `build-pti.qmd` is **not rewritten** until PR #F, so existing users always have a working tutorial.

---

## 9. Rwanda tutorial data preparation

### 9.1 Raw files for the template (PR #A2)

Rwanda GeoJSONs sourced from **geoBoundaries** (open licence CC-BY 4.0, stable API):

```r
# Run once to download and store in inst/template_pti/sample-data/
library(httr2)
base <- "https://www.geoboundaries.org/api/current/gbOpen/RWA"
for (level in c("ADM0", "ADM1", "ADM2")) {
  resp <- request(paste0(base, "/", level, "/")) |> req_perform() |> resp_body_json()
  download.file(resp$gjDownloadURL,
                paste0("inst/template_pti/sample-data/rwa_", tolower(level), ".geojson"))
}
```

Synthetic metadata Excel generated with a seeded script stored in
`inst/template_pti/data-raw/generate-synthetic-metadata.R`. Running this
script reproduces `sample-metadata-adm1.xlsx` and `sample-metadata-adm1-adm2.xlsx`
deterministically. The generated files are committed so users do not need to run
the generation script.

### 9.2 Package datasets for `@examples` (companion issue §7.2)

After the raw Rwanda data is created in PR #A2, compile it into package-level
datasets:

- `data/rwa_shp.rda` — named list of `sf` tibbles (same shape as `ukr_shp`)
- `data/rwa_mtdt_full.rda` — parsed metadata list (same shape as `ukr_mtdt_full`)

Generate these via a script in `data-raw/generate-rwa-package-data.R`.
Update `R/data.R` with roxygen documentation for both datasets.
Then update all `@examples` blocks to use `rwa_shp`/`rwa_mtdt_full`.

`ukr_shp` and `ukr_mtdt_full` remain in `data/` — both dataset pairs coexist.
Ukraine data is used by the test suite; Rwanda data is used by user-facing docs.

---

*For implementation conventions, testing standards, and changelog rules see
[`.claude/CLAUDE.md`](../../.claude/CLAUDE.md) and
[`.github/docs/arch-03-testing.md`](arch-03-testing.md).*
