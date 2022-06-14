options(golem.app.prod = FALSE)
options(shiny.fullstacktrace = TRUE)
options(shiny.reactlog = TRUE)
golem::detach_all_attached()
golem::document_and_reload()

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

options( "golem.app.prod" = FALSE)
launch_pti(
  shp_dta = ukr_shp,
  inp_dta = ukr_mtdt_full,
  app_name = "Country", 
  show_waiter = FALSE, 
  show_adm_levels = NULL,            # c("admin1", "admin2")
  shapes_path = ".", 
  mtdtpdf_path = "." ,
  dt_style = "zoom:1; height: calc(95vh - 160px);", #,
  # pti_landing_page = "./landing-page.md"
)

