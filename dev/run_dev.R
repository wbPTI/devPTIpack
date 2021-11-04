options(golem.app.prod = FALSE)
options(shiny.fullstacktrace = TRUE)
options(shiny.reactlog = FALSE)
golem::detach_all_attached()
golem::document_and_reload()

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
run_new_pti(
  pti.name = "Sample country PTI",
  # shape_path = NULL,
  shape_dta = ukr_shp, 
  # data_path = NULL,
  data_dta = ukr_mtdt_full,
  metadata_path = NULL,
  show_waiter = FALSE,
  
  default_adm_level = "admin2",
  show_adm_levels = c("admin1", "admin2", "admin3", "admin4"),
  choose_adm_levels = TRUE,
  
  explorer_choose_adm = FALSE,
  explorer_default_adm = "all",
  explorer_multiple_var = TRUE,
  
  full_ui = FALSE,
  pti_landing_page = NULL
)

