# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

library(shiny)
library(devPTIpack)

# pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

run_new_pti(
  pti.name = "Sample country name",
  shape_path = "app-data/sample-shapes.Rds",
  data_path = "app-data/sample-metadata.xlsx",
  metadata_path = NULL,
  show_waiter = FALSE,
  
  default_adm_level = "admin2",
  show_adm_levels = c("admin1", "admin2", "admin3", "admin4"),
  choose_adm_levels = TRUE,
  
  explorer_choose_adm = FALSE,
  explorer_default_adm = "all",
  explorer_multiple_var = FALSE,
  
  full_ui = FALSE,
  pti_landing_page = "./landing-page.md"
)
