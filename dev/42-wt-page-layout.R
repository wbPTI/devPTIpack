# rethinking the weights page logic
library(tidyverse)
# library(devPTIpack)
devtools::load_all()
library(profvis)
library(DT)
library(shiny)


# shp_dta <- "../other_countries/south_sudan/South_Sudan.rds" %>% read_rds() #devPTIpack::ukr_shps
imp_dta <- 
  # "../other_countries/south_sudan/South_Sudan--metadata-2021-11-29_v2.1.xlsx" %>%
  # "../other_countries/somalia/mtdt-2021-11-08-00-53-06.xlsx" %>%
  "../other_countries/tzn/Tanzania--metadata-2022-01-28.xlsx" %>%
  devPTIpack::fct_template_reader()


# Generic WT page layout

# Tailoring the WT page layout ===========================================
options(golem.app.prod = FALSE)
devtools::load_all()

ui <- fluidPage(
  shinyjs::useShinyjs(),
  fluidRow(
    column(4, 
           mod_wt_inp_ui("input_tbl_1", full_ui = FALSE, height = "50vh", dt_style = "zoom:0.9;")
           ),
    column(7, 
           mod_wt_inp_test_ui("input_tbl_1")
           # leaflet::leaflet() %>%
           #   leaflet::addTiles() %>%
           #   setView(-93.65, 42.0285, zoom = 12) ,
           # absolutePanel(
           #   mod_wt_inp_ui("input_tbl_2", dt_style = "max-height: 300px;") %>% 
           #     div(style = "zoom:0.8;"),
           #   top = 10, right = 75, width = 350, height = 550,
           #   style = "!important; z-index: 1000;")
    )
  )
)

server <- function(input, output, session) {
  mod_wt_inp_server("input_tbl_1", input_dta = reactive(imp_dta), 
                    shapes_path = normalizePath("../other_countries/Somalia.rds"),
                    mtdtpdf_path = normalizePath("../other_countries/Sudan-pti-metadata.pdf"))
  # mod_wt_inp_server("input_tbl_2", input_dta = reactive(imp_dta))
}

devtools::load_all()
shinyApp(ui, server)
 