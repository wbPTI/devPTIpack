# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

library(shiny)

# remotes::install_github("EBukin/devPTIpack", force = TRUE)
library(devPTIpack)

options( "golem.app.prod" = TRUE)
devPTIpack::launch_pti(
  shp_dta = devPTIpack::ukr_shp,                 # or readRDS("...")
  inp_dta = devPTIpack::ukr_mtdt_full,           # or devPTIpack::fct_template_reader("...")
  app_name = "{{COUNTRY NAME}}", 
  show_waiter = TRUE, 
  show_adm_levels = NULL,            # c("admin1", "admin2")
  shapes_path = ".", 
  mtdtpdf_path = "." #,
  # pti_landing_page = "./landing-page.md"
)

