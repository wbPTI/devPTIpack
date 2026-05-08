# Documentation Architecture — `devPTIpack`

> **Source of truth** for the pkgdown website structure, vignette content plan,
> app-template project layout, and the PR sequence that delivers them.
> Supersedes the vignette/pkgdown section of `arch-04-workspace.md`.
>
> **Design status:** function signatures for new validation and data-prep
> infrastructure are **provisional** — marked ⚠ PROVISIONAL throughout.
> They must be revisited and agreed before implementation PRs open.

---

## 1. Decisions locked

| Decision                | Resolution                                                                             |
| ----------------------- | -------------------------------------------------------------------------------------- |
| Branch base             | `eb-docs-pkgdown` (NOT `main`)                                                         |
| Primary audience        | PTI deployers — WB staff who prepare country data and deploy the app                   |
| Deployer entry state    | Has shapefiles, needs to build metadata Excel from scratch (sometimes from zero)       |
| Writing style           | Tutorial-style using a worked example country                                          |
| Tutorial country        | **Rwanda** — 5 Adm1 provinces, 30 Adm2 districts, clean geoBoundaries geometry         |
| Tutorial shapefiles     | 3 separate GeoJSON files: `rwa_adm0.geojson`, `rwa_adm1.geojson`, `rwa_adm2.geojson`   |
| Tutorial metadata Excel | Fully **synthetic** Rwanda district-level indicator data                               |
| Tutorial data location  | `inst/template_pti/` (copied to user project by `create_new_pti()`)                    |
| Validation app naming   | Functions: `validate_XXX()` · Shiny launchers: `app_validate_XXX()`                    |
| Validation app style    | Standalone — called like `launch_pti()`, opens browser                                 |
| Master orchestration    | `00-master.R` — sourceable R script calling `quarto::quarto_render()` per step file    |
| Step files              | Numbered `.qmd` files — rendered to `docs/` by master                                  |
| `02-*` files            | PLACEHOLDER — NOT sourced by `00-master.R`; master uses their outputs from `data-app/` |
| `data-app/`             | Holds compiled deployment-ready files; git treatment requires explicit user decision   |
| Deployment bundle       | `app.R` + `landing-page.md` + `data-app/` only — `docs/` excluded                      |
| gh-pages project docs   | Optional — flagged clearly in master and Step 6                                        |
| pkgdown articles index  | Build a PTI sub-pages appear in BOTH navbar dropdown AND articles index                |
| `build-pti.qmd`         | Rewritten as Overview — all current stub content replaced                              |
| `dataprep.qmd`          | Deleted — superseded by Build a PTI steps                                              |
| Reference tab grouping  | No custom groupings — raised as separate issue (see §7)                                |

---

## 2. Navbar structure

```
About (→ index.html) | PTI Methodology ▾ | Build a PTI ▾ | Reference
                              │                    │
                 ├─ Detailed methodology    ├─ Overview
                 ├─ Overview paper          ├─ Step 1 — Setup new project
                 └─ Past Projects           ├─ Step 2 — Shapefiles
                                            ├─ Step 3 — Metadata Excel
                                            ├─ Step 4 — HEX data ⚠ placeholder
                                            ├─ Step 5 — Compile & finalise
                                            └─ Step 6 — Deploy
```

### `_pkgdown.yml` navbar component map

```yaml
navbar:
  structure:
    left:  [about, methodology, build-pti, reference]
    right: [resources, search, github]
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
        - text: "Step 1 — Setup new project"
          href: articles/build-pti-1-setup.html
        - text: "Step 2 — Shapefiles"
          href: articles/build-pti-2-shapefiles.html
        - text: "Step 3 — Metadata Excel"
          href: articles/build-pti-3-metadata.html
        - text: "Step 4 — HEX data"
          href: articles/build-pti-4-hex.html
        - text: "Step 5 — Compile & finalise"
          href: articles/build-pti-5-compile.html
        - text: "Step 6 — Deploy"
          href: articles/build-pti-6-deploy.html
    reference:
      text: Reference
      href: reference/index.html
```

---

## 3. Vignette file map

| File                                            | Status                          | Content                         |
| ----------------------------------------------- | ------------------------------- | ------------------------------- |
| `vignettes/articles/methodology.qmd`            | existing (579 lines) — keep     | Detailed technical methodology  |
| `vignettes/articles/overview-paper.qmd`         | **create — placeholder**        | Developed separately; stub only |
| `vignettes/articles/past-projects.qmd`          | existing — keep                 | Country catalogue               |
| `vignettes/articles/build-pti.qmd`              | existing — **rewrite entirely** | Overview (see §4.1)             |
| `vignettes/articles/build-pti-1-setup.qmd`      | **create — write**              | Step 1 (see §4.2)               |
| `vignettes/articles/build-pti-2-shapefiles.qmd` | **create — write**              | Step 2 (see §4.3)               |
| `vignettes/articles/build-pti-3-metadata.qmd`   | **create — write**              | Step 3 (see §4.4)               |
| `vignettes/articles/build-pti-4-hex.qmd`        | **create — placeholder**        | Step 4 (see §4.5)               |
| `vignettes/articles/build-pti-5-compile.qmd`    | **create — write**              | Step 5 (see §4.6)               |
| `vignettes/articles/build-pti-6-deploy.qmd`     | **create — write**              | Step 6 (see §4.7)               |
| `vignettes/dataprep.qmd`                        | **delete**                      | Superseded                      |

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
      - articles/build-pti-1-setup
      - articles/build-pti-2-shapefiles
      - articles/build-pti-3-metadata
      - articles/build-pti-4-hex
      - articles/build-pti-5-compile
      - articles/build-pti-6-deploy
  - title: Additional Resources
    contents:
      - dataprep   # removed once dataprep.qmd is deleted
```

---

## 4. Vignette content plan

### 4.1 Overview (`build-pti.qmd`) — **rewrite**

1. What data a PTI app needs: shapefiles + metadata Excel (+ optionally HEX data)
2. Reproducible quickstart — launch app with bundled Rwanda data in < 5 lines
3. Step-by-step journey map: numbered list, one sentence per step, each links to its sub-page
4. Note that **Step 4 (HEX) is order-flexible** — can be done before or after Step 3
5. Pointer to `00-master.R` as the one-command path through all steps

### 4.2 Step 1 — Setup new project (`build-pti-1-setup.qmd`) — **write**

1. Install `devPTIpack`
2. Call `create_new_pti("path/to/my_pti")` — what it copies and why
3. Annotated folder structure (see §5)
4. Note on `00-master.R` — what it does, when to use it
5. Note on `data-app/` and git — explicit warning: decide consciously whether to track this folder

### 4.3 Step 2 — Shapefiles (`build-pti-2-shapefiles.qmd`) — **write**

Sections in order:

**A. Requirements**
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
- How and where to save: `data-app/shapes.rds` via `saveRDS()`
- Format: `.rds` for app use; keep original `.geojson` as source-of-truth

**E. Advanced: multiple admin levels** (flagged as advanced)
- The mapping table requirement
- Unique nested naming convention: `admin<N>_HumanName` slot names
- How `ukr_shp` demonstrates this pattern
- When to use multi-level vs. single-level

**Rwanda worked example** threads through sections A → C → D using `rwa_adm1.geojson`.
Advanced section uses `rwa_adm1.geojson` + `rwa_adm2.geojson` combined.

### 4.4 Step 3 — Metadata Excel (`build-pti-3-metadata.qmd`) — **write**

1. What the Excel file is and why it exists
2. Sheet structure — document every sheet:
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
7. How and where to save: `data-app/metadata.xlsx`

### 4.5 Step 4 — HEX data (`build-pti-4-hex.qmd`) — **placeholder**

> ⚠ This step is under active development. Content will be added when the HEX
> data API and integration functions are production-ready.

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
2. Run `compile_pti_data(shp_dta, inp_dta, ...)` ⚠ PROVISIONAL — validates combined inputs, produces final `data-app/` contents
3. Generate the metadata PDF:
   - Fixed `render_metadata()` (broken in Batch 6 — path bug; fix is a pre-requisite for this PR)
   - Open the PDF, check it looks correct
4. Landing page: `landing-page.md` lives in project root, edit it to describe the app
   (covered here in Overview only — cross-reference from Overview article)
5. Final pre-launch checklist
6. Launch locally: `launch_pti(shp_dta, inp_dta)` — what each tab does
7. Pointer to `00-master.R` as the automated path through steps 1–5

### 4.7 Step 6 — Deploy (`build-pti-6-deploy.qmd`) — **write**

1. What gets deployed: `app.R` + `landing-page.md` + `data-app/` — nothing else
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

This is what `create_new_pti()` copies to the user's project folder.

```
my_pti_project/
│
├── 00-master.R                     # Sourceable R script; calls quarto_render() per step
│                                   # Skips 02-* files; uses data-app/ outputs instead
│
├── 01-shapes.qmd                   # Step 2 — load, validate, fix, save shapefiles
├── 02a-user-zonal-stats.qmd        # ⚠ PLACEHOLDER — user's own zonal stats extraction
│                                   #   NOT sourced by 00-master.R
│                                   #   Must be run separately before 03-user-data.qmd
│                                   #   Add 02b, 02c etc. as needed
├── 03-user-data.qmd                # Step 3 — prepare & validate metadata Excel
├── 04-hex-data.qmd                 # Step 4 — fetch HEX, match to shapes, augment metadata
├── 05-compile.qmd                  # Step 5 — compile all inputs, generate PDF
├── 06-deploy.qmd                   # Step 6 — deploy app; optional gh-pages docs
│
├── app.R                           # Shiny app (deployed)
├── landing-page.md                 # App landing page text (deployed)
│
├── data-app/                       # ⚠ THINK TWICE before adding to .gitignore
│   │                               #   May contain sensitive or large files
│   │                               #   Decide consciously: track in git or not
│   ├── shapes.rds                  # Compiled shapefile (output of 01-shapes.qmd)
│   ├── metadata.xlsx               # Clean metadata Excel (output of 03/04/05)
│   └── pti-metadata.pdf           # Generated PDF documentation (output of 05)
│
└── docs/                           # Quarto-rendered HTML (output of 00-master.R)
                                    # Optional: deploy to GitHub Pages
                                    # NOT deployed to Posit Connect / shinyapps.io
```

### Tutorial data bundled in `inst/template_pti/`

| File                             | Description                                                |
| -------------------------------- | ---------------------------------------------------------- |
| `rwa_adm0.geojson`               | Rwanda country boundary (geoBoundaries)                    |
| `rwa_adm1.geojson`               | Rwanda 5 provinces (geoBoundaries)                         |
| `rwa_adm2.geojson`               | Rwanda 30 districts (geoBoundaries)                        |
| `sample-metadata-adm1.xlsx`      | Synthetic indicator data at Adm1 level                     |
| `sample-metadata-adm1-adm2.xlsx` | Synthetic indicator data at Adm1 + Adm2 (advanced example) |

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
| `compile_pti_data(shp_dta, inp_dta, ...)`         | 5    | exported | Combine all inputs, run combined validation, write `data-app/` outputs                                                  |
| `render_metadata(...)`                            | 5    | exported | Fixed version of the function deleted in Batch 6 (broken path); must be reintroduced with corrected `system.file()` key |

---

## 7. Companion GitHub issue — Reference tab restructure

> Out of scope for arch-09 implementation PRs. Open as a separate issue covering:
>
> - Audit of user-facing vs. internal functions — clarify public API surface
> - `@inheritParams` / `@describeIn` strategy — share param docs, avoid rewriting
> - Replace complex examples with simple Rwanda-based ones where applicable
> - Audit implicit golem options parameters — consider making them explicit function arguments
> - No custom reference groupings until the above audit is complete

---

## 8. PR sequence

| PR     | Branch                        | Scope                                                                                                 | Depends on                              |
| ------ | ----------------------------- | ----------------------------------------------------------------------------------------------------- | --------------------------------------- |
| **#A** | `docs/navbar-restructure`     | `_pkgdown.yml` navbar + all placeholder `.qmd` files created; `dataprep.qmd` deleted; no broken build | —                                       |
| **#B** | `feat/validate-shp`           | Design + implement `validate_shp()` + `app_validate_shp()` with tests                                 | #A (naming convention confirmed)        |
| **#C** | `feat/validate-metadata`      | Design + implement `validate_metadata_full()` + `app_validate_metadata()` with tests                  | #A, #B (shares validation report shape) |
| **#D** | `feat/compile-pti-data`       | Design + implement `compile_pti_data()` + fix `render_metadata()` with tests                          | #B, #C                                  |
| **#E** | `docs/build-pti-steps-1-3`    | Write Steps 1, 2, 3 vignettes using Rwanda tutorial data                                              | #B, #C                                  |
| **#F** | `docs/build-pti-overview-5-6` | Write Overview, Step 5, Step 6 vignettes                                                              | #D, #E                                  |
| **#G** | `feat/hex-integration`        | `create_hex_metadata()` + Step 4 vignette                                                             | parallel — unblocked by #A              |

> **Note:** #B, #C, #D are feature PRs that require design review before opening.
> The ⚠ PROVISIONAL function signatures in §6 must be finalised first.

---

## 9. Rwanda tutorial data preparation

Rwanda GeoJSONs sourced from **geoBoundaries** (open licence, stable API):

```r
# Run once to download and store in inst/template_pti/
library(httr2)
base <- "https://www.geoboundaries.org/api/current/gbOpen/RWA"
for (level in c("ADM0", "ADM1", "ADM2")) {
  resp <- request(paste0(base, "/", level, "/")) |> req_perform() |> resp_body_json()
  download.file(resp$gjDownloadURL, paste0("inst/template_pti/rwa_", tolower(level), ".geojson"))
}
```

Synthetic metadata Excel generated with a seeded script stored in
`inst/template_pti/data-raw/generate-synthetic-metadata.R`. Running this
script reproduces `sample-metadata-adm1.xlsx` and `sample-metadata-adm1-adm2.xlsx`
deterministically. The generated files are committed so users do not need to run
the generation script.

---

*For implementation conventions, testing standards, and changelog rules see
[`.claude/CLAUDE.md`](../../.claude/CLAUDE.md) and
[`.github/docs/arch-03-testing.md`](arch-03-testing.md).*
