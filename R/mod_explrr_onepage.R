#' explrr_onepage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @export
#'
#' @importFrom shiny NS tagList 
mod_explrr_onepage_ui <- function(id, multi_choice = FALSE, ...){
  ns <- NS(id)
  tagList(
    mod_dta_explorer2_ui(id, multi_choice, ...)
  )
}
    
#' explrr_onepage Server Functions
#'
#' @noRd 
#' @export
mod_explrr_onepage_server <- 
  function(id, shape_path = NULL, data_path = NULL, 
           metadata_path = NULL, shape_dta = NULL, data_dta = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    # Loading data
    shp_dta <- mod_get_shape_srv(NULL, shape_path = shape_path, shape_dta = shape_dta)
    input_dta <- mod_fetch_data_srv(NULL, data_fldr = "app-data/", data_path = data_path, data_dta = data_dta)
      
    # Data explorer
    mod_dta_explorer2_server(NULL, shp_dta, input_dta, 
                             active_tab = function() "Data explorer",
                             target_tabs = "Data explorer",
                             metadata_path = metadata_path)
      
  })
}
  