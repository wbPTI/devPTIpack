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

devtools::load_all()
launch_pti_onepage(shp_dta = ukr_shp, imp_dta = ukr_mtdt_full)

# ui <- 
#   navbarPage(
#     title = add_logo("App Name"),
#     collapsible = TRUE,
#     id = "main_sidebar",
#     selected = "Info",
#     tabPanel("Info"),
#     tabPanel("PTI",
#              mod_ptipage_twocol_ui("pagepti",
#                                    wt_dwnld_options = c("data", "weights", "shapes", "metadata"))
#              ),
#     tabPanel("PTI comparison",
#              mod_ptipage_box_ui("pagepti2",
#                                    wt_dwnld_options = c("data", "weights", "shapes", "metadata"))),
#     tabPanel("Data explorer", h3("Some content"))
#   )
# 
# 
# 
# server <- function(input, output, session) {
#   mod_ptipage_newsrv("pagepti",
#                      imp_dta = reactive(imp_dta), #ukr_mtdt_full), #imp_dta),
#                      shp_dta = reactive(shp_dta), #ukr_shp),  #shp_dta))
#                      show_adm_levels =  NULL,
#                      shapes_path = normalizePath("../other_countries/Mozambique.rds"),
#                      # active_tab = reactive(NULL),
#                      # target_tabs = NULL,
#                      # default_adm_level = NULL, 
#                      # show_adm_levels = NULL,
#                      show_waiter = FALSE,
#                      mtdtpdf_path = normalizePath("app.R")
#   )
#   mod_ptipage_newsrv("pagepti2",
#                      imp_dta = reactive(imp_dta), #ukr_mtdt_full), #imp_dta),
#                      shp_dta = reactive(shp_dta), #ukr_shp),  #shp_dta))
#                      show_adm_levels =  NULL,
#                      shapes_path = normalizePath("../other_countries/Mozambique.rds"),
#                      # active_tab = reactive(NULL),
#                      # target_tabs = NULL,
#                      # default_adm_level = NULL, 
#                      # show_adm_levels = NULL,
#                      show_waiter = FALSE,
#                      mtdtpdf_path = normalizePath("app.R")
#   )
# }
# 
# # shinyApp(ui, server)



# Function for running only on PTI page  =====================================



