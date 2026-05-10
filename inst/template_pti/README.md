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
| `05-compile.qmd`           | Step 5 -- Compile and finalise        | Merges intermediates into `metadata.xlsx` + `pti-metadata.pdf` + `shapefiles.zip`. |
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

## Current template state

Steps 1, 3, and 5 ship with **working Rwanda code** that produces
real outputs into `app-data/`. Step 4 ships as a **stub** awaiting
the HEX data API.

`00-master.R` renders Steps 01, 03, and 05 by default; the 02a (optional
zonal stats), 04 (HEX), and 06 (deploy) lines are commented and clearly
marked.

Visual-validation app calls (`app_validate_shp()` in `01-shapes.qmd`
and `app_validate_metadata()` in `03-metadata.qmd`) are commented in
the template because `00-master.R` runs the files unattended. Uncomment
them and re-source the relevant `.qmd` interactively when you want a
visual pass over the data.
