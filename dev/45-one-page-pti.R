# rethinking the weights page logic
library(tidyverse)
# library(devPTIpack)
devtools::load_all()
library(profvis)
library(DT)
library(shiny)


# shp_dta <- "../other_countries/south_sudan/South_Sudan.rds" %>% read_rds() #devPTIpack::ukr_shps
imp_dta <- #ukr_mtdt_full
  # # "../other_countries/south_sudan/South_Sudan--metadata-2021-11-29_v2.1.xlsx" %>%
  # # "../other_countries/somalia/mtdt-2021-11-08-00-53-06.xlsx" %>%
  "../other_countries/tzn/Tanzania--metadata-2022-01-28.xlsx" %>%
  devPTIpack::fct_template_reader()


shp_dta <- #ukr_shp
  "../other_countries/tzn/Tanzania.rds" %>% read_rds()

# Generic WT page layout

# Tailoring the WT page layout ===========================================
options(golem.app.prod = FALSE)


# ui <- fluidPage(
#   shinyjs::useShinyjs(),
#   fluidRow(
#     column(4, 
#            mod_wt_inp_ui("input_tbl_1", full_ui = FALSE, height = "60vh", dt_style = "zoom:0.9;")
#     ),
#     column(8, 
#            mod_map_pti_leaf_ui("page_pti", height = '50vh'),
#            mod_wt_inp_test_ui("input_tbl_1")
#            # leaflet::leaflet() %>%
#            #   leaflet::addTiles() %>%
#            #   setView(-93.65, 42.0285, zoom = 12) ,
#            # absolutePanel(
#            #   mod_wt_inp_ui("input_tbl_2", dt_style = "max-height: 300px;") %>% 
#            #     div(style = "zoom:0.8;"),
#            #   top = 10, right = 75, width = 350, height = 550,
#            #   style = "!important; z-index: 1000;")
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   
#   # Weights
#   wt_dta <- mod_wt_inp_server("input_tbl_1", 
#                               input_dta = reactive(imp_dta), 
#                               plotted_dta = reactive(plotted_dta()$pre_map_dta()))
#   
#   observe({
#     req(wt_dta()$weights_clean)
#     cat("PTI: start calc ", as.character(Sys.time()), "\n" )
#   })
#   
#   # PTI calculations
#   map_dta <-
#     mod_calc_pti2_server(
#       "page_pti",
#       shp_dta = reactive(shp_dta),
#       input_dta = reactive(imp_dta),
#       wt_dta = wt_dta
#     )
# 
#   observe({
#     req(length(map_dta()) > 0)
#     cat("PTI: End calc ", as.character(Sys.time()), "\n" )
#   })
#   
#   
#   # PTI Visualization
#   plotted_dta <-
#     mod_plot_pti2_srv("page_pti",
#                       shp_dta = reactive(shp_dta), 
#                       map_dta = map_dta, 
#                       wt_dta =  wt_dta, 
#                       active_tab = reactive("PTI"),
#                       target_tabs = "PTI", 
#                       metadata_path = "metadata_path")
#   
#   observe({
#     req(plotted_dta()$pre_map_dta())
#     cat("PTI: End plot data to export ", as.character(Sys.time()), "\n" )
#     })
# }
# 
# devtools::load_all()
# shinyApp(ui, server)



# Same but in a one-page PTI module ---------------------------------------


devtools::load_all()
ui <- mod_ptipage_twocol_ui("pagepti")

server <- function(input, output, session) {
  mod_ptipage_newsrv("pagepti",
                     imp_dta = reactive(ukr_mtdt_full), #imp_dta), 
                     shp_dta = reactive(ukr_shp),  #shp_dta))
                     show_adm_levels =  NULL #c("admin1")
                     )
}

shinyApp(ui, server)

# Legend Fails when we have all zeros. Rewrite the legend. 
# Check the panel of map layer switch. for taking long to load.
# Check the gap between leaflet drawing the map and pc finishing computations. 




w# Convertin one page PTI into a module