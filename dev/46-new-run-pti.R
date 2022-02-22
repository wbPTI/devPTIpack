# rethinking the weights page logic
library(tidyverse)
# library(devPTIpack)
devtools::load_all()
library(profvis)
library(DT)
library(shiny)


# shp_dta <- "../other_countries/south_sudan/South_Sudan.rds" %>% read_rds() #devPTIpack::ukr_shp
imp_dta <- ukr_mtdt_full
  # # # "../other_countries/south_sudan/South_Sudan--metadata-2021-11-29_v2.1.xlsx" %>%
  # # # "../other_countries/somalia/mtdt-2021-11-08-00-53-06.xlsx" %>%
  # "../other_countries/tzn/Tanzania--metadata-2022-01-28.xlsx" %>%
  # devPTIpack::fct_template_reader()


shp_dta <- ukr_shp
  # "../other_countries/tzn/Tanzania.rds" %>% read_rds()

# Generic WT page layout

# Tailoring the WT page layout ===========================================
options(golem.app.prod = FALSE)

# devtools::load_all()
# launch_pti_onepage(shp_dta = ukr_shp, imp_dta = ukr_mtdt_full)


# Multitab layout ========================================================
devtools::load_all()
launch_pti(shp_dta = ukr_shp, imp_dta = ukr_mtdt_full)

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
#                        imp_dta = reactive(imp_dta), 
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
#                            input_dta = reactive(imp_dta), 
#                            active_tab = function() "Data explorer",
#                            target_tabs = "Data explorer",
#                            mtdtpdf_path = ".")
# }
# 
# 
# 
# shinyApp(ui, server)


# Function for running only on PTI page  =====================================



