# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp() -- or see 06-deploy.R
# Or use the blue Publish button on top of this file.

library(shiny)
# remotes::install_github("worldbank/devPTIpack")
library(devPTIpack)

options("golem.app.prod" = TRUE)

# -----------------------------------------------------------------------
# Data sources.
#
# After running the data-prep pipeline (`source("00-master.R")`), the
# canonical inputs live under `app-data/`:
#
#   app-data/shapes.rds       -- compiled shapes list (Step 1)
#   app-data/metadata.xlsx    -- canonical metadata workbook (Step 5,
#                                produced by `compile_pti_data()`)
# -----------------------------------------------------------------------

# Option A -- load from app-data/ (default).
shp_dta <- readRDS("app-data/shapes.rds")
inp_dta <- devPTIpack::fct_template_reader("app-data/metadata.xlsx")

# Option B -- bundled package sample data (works without running 00-master.R).
# shp_dta <- devPTIpack::ukr_shp
# inp_dta <- devPTIpack::ukr_mtdt_full

devPTIpack::launch_pti(
  shp_dta         = shp_dta,
  inp_dta         = inp_dta,
  app_name        = "{{COUNTRY NAME}}",
  show_waiter     = TRUE,
  show_adm_levels = NULL,                       # e.g. c("admin1", "admin2")
  shapes_path     = "app-data/shapes.rds",
  mtdtpdf_path    = "app-data/pti-metadata.pdf" # produced by Step 5
  # pti_landing_page = "./landing-page.md"
)
