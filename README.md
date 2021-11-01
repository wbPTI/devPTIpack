# devPTIpack

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of `devPTIpack` is to be a platform that contains all the
development tools for PTI apps development as well as to be a package
that is used to deploy regular PTI applications.

## Installation:

    devtools::install_github("EBukin/devPTIpack")

## Creating new PTI project:

To start a new PTI project simply do the following:

    devtools::install_github("EBukin/devPTIpack")
    devPTIpack::create_new_pti("PATH-TO-THE/project_name")

And in the `PATH-TO-THE` folder, a new sub-folder will be created named
`project_name`, which will contain all the essential files for running
PTI apps.

This is how the typical PTI folder looks like.

    +-- app-data
    |   \-- pti-metadata-pdf.Rmd
    +-- app.R
    +-- irqRisksDB.Rproj
    +-- landing-page.md
    \-- R

Your goal is to add `metadata xlsx` and `shapes rds` files to the
`app-data` folder and edit `app.R` to specify Country name, and path to
the key data files.

You may need to re-render metadata PDF. To do so, knit the
`pti-metadata-pdf.Rmd` and the metadat should be generated
automatically. Remember to add path to the PDF file with the metadata in
the `app.R`.
