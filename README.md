# devPTIpack

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test
coverage](https://codecov.io/gh/worldbank/devPTIpack/graph/badge.svg)](https://app.codecov.io/gh/worldbank/devPTIpack)
<!-- badges: end -->

**`devPTIpack`** is an R package for building interactive, geo-spatial
**Project Targeting Index (PTI)** dashboards using Shiny. Given
administrative boundary shapefiles and a metadata workbook of
socioeconomic indicators, it computes weighted composite scores across
admin levels and serves them in a ready-to-deploy Shiny application —
no front-end coding required.

➡ **[Start here — Build a
PTI](https://worldbank.github.io/devPTIpack/articles/build-pti.html)**

------------------------------------------------------------------------

## Quick-start contents

| Step | What you do                                       | Guide                                                                                                |
| ---- | ------------------------------------------------- | ---------------------------------------------------------------------------------------------------- |
| 0    | Install the package & set up a new project folder | [Step 0 — Setup](https://worldbank.github.io/devPTIpack/articles/build-pti-0-setup.html)             |
| 1    | Prepare and validate shapefiles                   | [Step 1 — Shapefiles](https://worldbank.github.io/devPTIpack/articles/build-pti-1-shapefiles.html)   |
| 2    | Compute zonal statistics *(optional)*             | [Step 2 — Zonal stats](https://worldbank.github.io/devPTIpack/articles/build-pti-2-zonal-stats.html) |
| 3    | Fill in the metadata Excel workbook               | [Step 3 — Metadata](https://worldbank.github.io/devPTIpack/articles/build-pti-3-metadata.html)       |
| 4    | Add HEX / hexagonal-grid data *(optional)*        | [Step 4 — HEX data](https://worldbank.github.io/devPTIpack/articles/build-pti-4-hex.html)            |
| 5    | Compile and finalise the PTI data                 | [Step 5 — Compile](https://worldbank.github.io/devPTIpack/articles/build-pti-5-compile.html)         |
| 6    | Deploy to Shiny Server / Posit Connect            | [Step 6 — Deploy](https://worldbank.github.io/devPTIpack/articles/build-pti-6-deploy.html)           |

See the [full Build-a-PTI
overview](https://worldbank.github.io/devPTIpack/articles/build-pti.html)
and the [data preparation
reference](https://worldbank.github.io/devPTIpack/articles/dataprep.html)
for more detail.

### Minimum working example

```r
# Install
# remotes::install_github("worldbank/devPTIpack")

# Launch with built-in sample data (Ukraine)
library(devPTIpack)
launch_pti_onepage(
  shp_dta  = ukr_shp,
  inp_dta  = ukr_mtdt_full,
  app_name = "Sample PTI"
)
```

------------------------------------------------------------------------

## PTI methodology

| Resource                                                                                 | Description                                           |
| ---------------------------------------------------------------------------------------- | ----------------------------------------------------- |
| [Detailed methodology](https://worldbank.github.io/devPTIpack/articles/methodology.html) | Full technical write-up of the PTI score construction |
| [Overview paper](https://worldbank.github.io/devPTIpack/articles/overview-paper.html)    | High-level conceptual overview of the PTI framework   |
| [Past projects](https://worldbank.github.io/devPTIpack/articles/past-projects.html)      | Country applications and case studies                 |

------------------------------------------------------------------------

## Past PTI projects

Deployed PTI applications and country-level repositories are maintained
under the **wbPTI GitHub organisation**:

<https://github.com/wbpti/>


------------------------------------------------------------------------

## Get help & contact

- **Bug reports and feature requests** — open an issue on GitHub:  
  <https://github.com/worldbank/devPTIpack/issues/new>

- **General questions** — reach out to the World Bank GeoPov team [geopov/](geopov/){target="_blank"}
