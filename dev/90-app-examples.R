
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# Sample app with one page PTI:
library(devPTIpack)

shiny::shinyApp(
  ui = mod_pti_onepage_ui(id = "onpagepti_1"),
  server = function(input, output, session) {
    mod_pti_onepage_server(
      "onpagepti_1",
      shape_path = NULL,
      data_path = NULL,
      metadata_path = NULL,
      shape_dta = ukr_shp,
      data_dta = ukr_mtdt_full,
      demo_weights = FALSE
    )
  }
)


pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# golem::set_golem_options(explorer_multiple_var = TRUE)

library(shiny)
library(devPTIpack)

# options(explorer_multiple_var = NULL)

shiny::shinyApp(
  ui = fluidPage(shinydashboard::box(
    mod_explrr_onepage_ui(id = "onpagepti_1", multi_choice = TRUE, height = "500px")
  )),
  server = function(input, output, session) {
    mod_explrr_onepage_server(
      "onpagepti_1",
      shape_path = NULL,
      data_path = NULL,
      metadata_path = NULL,
      shape_dta = ukr_shp,
      data_dta = ukr_mtdt_full
    )
  }
)

