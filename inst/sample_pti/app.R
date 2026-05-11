# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

library(shiny)
library(devPTIpack)

# pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

shp_dta <- devPTIpack::get_shape(
  shapes_fldr   = "app-data/",
  shape_country = "sample-shapes",
  shape_path    = "app-data/sample-shapes.rds"
)

inp_dta <- devPTIpack::fct_template_reader("app-data/sample-metadata.xlsx")

devPTIpack::launch_pti(
  shp_dta         = shp_dta,
  inp_dta         = inp_dta,
  app_name        = "Sample country name",
  show_waiter     = FALSE,
  show_adm_levels = c("admin1", "admin2", "admin3", "admin4"),
  shapes_path     = "app-data/",
  mtdtpdf_path    = "app-data/"
)
