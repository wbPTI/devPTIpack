# rethinking the weights page logic
library(tidyverse)
# library(devPTIpack)
devtools::load_all()
library(profvis)
library(DT)
library(shiny)


inp_dta <- ukr_mtdt_full
shp_dta <- ukr_shp

# Tailoring the WT page layout ===========================================
options(golem.app.prod = TRUE)

# devtools::load_all()
# launch_pti_onepage(shp_dta = ukr_shp, inp_dta = ukr_mtdt_full)


# Multitab layout ========================================================
devtools::load_all()


launch_pti(
  shp_dta = ukr_shp, #readRDS("../examplePTIapp/app-data/admin_bounds.rds"), # ukr_shp, #
  inp_dta = ukr_mtdt_full, #devPTIpack::fct_template_reader("../examplePTIapp/app-data/mtdt.xlsx") # ukr_mtdt_full #
  shapes_path = "../examplePTIapp/app-data/shapefiles.zip",
  mtdtpdf_path = "../examplePTIapp/app-data/metadata.pdf", 
  show_waiter = FALSE, 
  show_adm_levels = c("admin1", "admin2", "admin3", "admin4")
  )



# ui <-
#   navbarPage(
#     title = add_logo("App Name"),
#     collapsible = TRUE,
#     id = "tabpan",
#     selected = "Info",
#     tabPanel("Info"),
#     tabPanel(
#       "PTI",
#       use_cicerone(),
#       mod_ptipage_twocol_ui(
#         "pagepti",
#         map_height = "calc(100vh - 60px)",
#         wt_dwnld_options = c("data", "weights", "shapes", "metadata"),
#         show_waiter = TRUE
#       ) %>%
#         fluidRow()
#       ),
#     tabPanel(
#       "PTI comparison", 
#       mod_pti_comparepage_ui("page_comparepti")
#       ),
#     tabPanel(
#       "Data explorer",
#       mod_dta_explorer2_ui("explorer_page", multi_choice = FALSE, height = "calc(100vh - 60px)")
#       )
#   )
# 
# 
# 
# server <- function(input, output, session) {
#   
#   # Checking what tab is open.
#   active_tab <- reactive(input$tabpan)
#   
#   # Info tab + guide logic
#   mod_infotab_server(NULL, 
#                      tabpan_id = "tabpan", 
#                      infotab_id = "Info", 
#                      firsttab_id = "PTI", 
#                      ptitab_id = "PTI", 
#                      comparetab_id = "PTI comparison", 
#                      exploretab_id = "Data explorer")
#   
#   plt_dta <- 
#     mod_ptipage_newsrv("pagepti",
#                        inp_dta = reactive(inp_dta), 
#                        shp_dta = reactive(shp_dta), 
#                        show_adm_levels =  NULL,
#                        mtdtpdf_path = normalizePath("."),
#                        shapes_path = normalizePath("."),
#                        active_tab = active_tab,
#                        target_tabs = "PTI",
#                        # default_adm_level = NULL,
#                        show_waiter = TRUE
#                        )
#   
#   # Compare page visualization
#   mod_pti_comparepage_newsrv("page_comparepti",
#                              shp_dta = reactive(shp_dta),
#                              map_dta = plt_dta$map_dta,
#                              wt_dta = plt_dta$wt_dta,
#                              active_tab = active_tab,
#                              target_tabs = "PTI comparison",
#                              mtdtpdf_path = ".",
#                              shapes_path = ".")
#   
#   # Adding explorer
#   mod_dta_explorer2_server("explorer_page", 
#                            shp_dta = reactive(shp_dta),
#                            input_dta = reactive(inp_dta), 
#                            active_tab = function() "Data explorer",
#                            target_tabs = "Data explorer",
#                            mtdtpdf_path = ".")
# }
# 
# 
# 
# shinyApp(ui, server)


# Function for running only on PTI page  =====================================



