# PTI app project

Scaffolded by `devPTIpack::create_new_pti()`. This project is a
working Rwanda PTI app -- run it as-is to see a Shiny app come up
end-to-end with sample data, then replace the Rwanda inputs with your
own country.

## File order

The numbered files form the data-prep pipeline:

| File                       | Step                                  | What it does                                                    |
| -------------------------- | ------------------------------------- | --------------------------------------------------------------- |
| `00-master.R`              | Pipeline orchestrator                 | Renders the step `.qmd` files top-to-bottom into `app-data/`.    |
| `01-shapes.qmd`            | Step 1 -- Shapefiles                  | Load + validate boundary GeoJSONs, save `app-data/shapes.rds`.   |
| `02a-user-zonal-stats.qmd` | Step 2 -- Zonal stats (optional)      | Stub. Extract raster zonal stats. Run manually if needed.       |
| `03-metadata.qmd`          | Step 3 -- Metadata Excel              | Read + validate the indicator workbook, stage `metadata-user.xlsx`. |
| `04-hex-data.qmd`          | Step 4 -- HEX data                    | Stub. Blocked by the HEX API -- coming in a later release.       |
| `05-compile.qmd`           | Step 5 -- Compile and finalise        | Stub. Merges intermediates into `metadata.xlsx` + PDF + zip.     |
| `06-deploy.R`              | Step 6 -- Deploy                      | Plain R script with `rsconnect::deployApp()` boilerplate.       |
| `app.R`                    | Shiny app entry point                 | The deployed app. Loads from `app-data/` paths.                  |
| `landing-page.md`          | App landing-page text                 | Markdown content shown on the app's About tab.                   |

Render the pipeline with:

```r
source("00-master.R")
```

## Tutorials

Each step `.qmd` file has a working chunk of Rwanda code at the top
and a link in the file header pointing to the corresponding website
tutorial. The tutorials live at:

- Step 0 -- Setup -- <https://worldbank.github.io/devPTIpack/articles/build-pti-0-setup.html>
- Step 1 -- Shapefiles -- <https://worldbank.github.io/devPTIpack/articles/build-pti-1-shapefiles.html>
- Step 2 -- Zonal stats (optional) -- <https://worldbank.github.io/devPTIpack/articles/build-pti-2-zonal-stats.html>
- Step 3 -- Metadata Excel -- <https://worldbank.github.io/devPTIpack/articles/build-pti-3-metadata.html>
- Step 4 -- HEX data -- <https://worldbank.github.io/devPTIpack/articles/build-pti-4-hex.html>
- Step 5 -- Compile -- <https://worldbank.github.io/devPTIpack/articles/build-pti-5-compile.html>
- Step 6 -- Deploy -- <https://worldbank.github.io/devPTIpack/articles/build-pti-6-deploy.html>

## Folders

```
sample-data/   Rwanda raw inputs (GeoJSONs + synthetic indicator workbooks).
data-raw/      Seeded scripts that produced sample-data/ (re-runnable).
app-data/      Pipeline outputs. THIS IS THE FOLDER YOU DEPLOY.
R/             golem boilerplate.
```

## ⚠ `app-data/` and git

`app-data/` holds the deployment-ready compiled artefacts. Decide
**consciously** whether to track this folder in git:

- **Track it** if your data is non-sensitive and small enough to live
  in a repo. Useful for reproducible reviews.
- **Ignore it** (`echo "app-data/" >> .gitignore`) if the data is
  sensitive, large, or both. This is the conservative default.

Don't let it slip through unreviewed.

## Current template state (arch-09 issue #79)

Steps 1 and 3 ship with **working Rwanda code** that produces real
outputs into `app-data/`. Steps 4 and 5 ship as **stubs** awaiting
upstream work:

- Step 4 is blocked indefinitely by the HEX data API (issue #79's
  parent design notes).
- Step 5's `compile_pti_data()` is delivered by issue #83.

`00-master.R` therefore renders steps 01 and 03 only by default; the
04 / 05 lines are commented and clearly marked. Until #83 lands, the
deployed `app.R` falls back to either:

1. Running the bundled `devPTIpack::ukr_shp` / `devPTIpack::ukr_mtdt_full`
   sample data (works out of the box -- see commented lines in `app.R`),
   or
2. Loading `app-data/shapes.rds` (produced by Step 1) plus a manually
   copied `app-data/metadata.xlsx` (`file.copy("app-data/metadata-user.xlsx",
   "app-data/metadata.xlsx", overwrite = TRUE)`).

The first option is the quickest sanity check; the second exercises
your real Rwanda inputs without yet running the multi-source merge
that #83 will add.

Validator app calls in `01-shapes.qmd` (`app_validate_shp()`) and
`03-metadata.qmd` (`app_validate_metadata()`) are commented out
pending issues #80 and #81 respectively.
