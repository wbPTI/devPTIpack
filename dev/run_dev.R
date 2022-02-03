options(golem.app.prod = FALSE)
options(shiny.fullstacktrace = TRUE)
options(shiny.reactlog = TRUE)
golem::detach_all_attached()
golem::document_and_reload()

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
devPTIpack::run_new_pti(
  pti.name = "Sample PTI",
  # shape_path = "../other_countries/somalia/admin_bounds.rds",
  shape_path = "../../somalia/somPTIext/app-data/admin_bounds.rds",
  # shape_dta = ukr_shp,
  # data_path = "../other_countries/somalia/mtdt-2021-11-08-00-53-06.xlsx",
  data_path = "../../somalia/somPTIext/app-data/mtdt-2021-11-08-00-53-06.xlsx",
  # data_dta = ukr_mtdt_full,
  metadata_path = NULL,
  show_waiter = FALSE,

  default_adm_level = "admin2",
  show_adm_levels = c("admin1", "admin2", "admin3", "admin4"),
  choose_adm_levels = TRUE,
  
  explorer_show_adm =  c("admin1", "admin2", "admin3"),
  explorer_choose_adm = TRUE,
  explorer_default_adm = "all",
  explorer_multiple_var = FALSE,
  
  # explorer_choose_adm = FALSE,
  # explorer_default_adm = "all",
  # explorer_multiple_var = TRUE,

  full_ui = FALSE,
  pti_landing_page = NULL
)

# run_new_pti(
#   pti.name = "Somalia PTI",
#   shape_path = "../other_countries/somalia/admin_bounds.rds",
#   data_path = "../other_countries/somalia/mtdt-2021-11-08-00-53-06.xlsx",
#   metadata_path = "app-data/pti-metadata-pdf.pdf",
#   show_waiter = TRUE,
#   
#   default_adm_level = "admin3",
#   show_adm_levels = c("admin1", "admin2", "admin3"),
#   choose_adm_levels = TRUE,
#   
#   explorer_show_adm =  c("admin1", "admin2", "admin3", "admin4"),
#   explorer_choose_adm = FALSE,
#   explorer_default_adm = "all",
#   explorer_multiple_var = FALSE,
#   
#   full_ui = FALSE,
#   # pti_landing_page = "./landing-page.md"
# )


