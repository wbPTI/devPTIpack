#' wt_inp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @export
#'
#'
#' @importFrom shiny NS tagList 
#' 
#' 
#' @examples 
#' 
#' column(
#'   5, 
#'   mod_wt_inp_ui("input_tbl_1", dt_style = "max-height: calc(70vh);"),
#'   style = "padding-right: 0px; padding-left: 5px;"
#' )
#' 
#' absolutePanel(
#'   mod_wt_inp_ui("input_tbl_2", dt_style = "max-height: 300px;") %>% 
#'     div(style = "zoom:0.8;"),
#'   top = 10, right = 75, width = 350, height = 550, style = "!important; z-index: 1000;")
#'   ) 
#' 
#' 
mod_wt_inp_ui <- function(id, full_ui = FALSE, dt_style = NULL){
  ns <- NS(id)
  
  if (full_ui) {
    controls_col <- full_wt_inp_ui(ns)
  } else {
    controls_col <- short_wt_inp_ui(ns)
  }
  
  controls_col %>% 
    div(style = "margin-bottom: 10px; width: -webkit-fill-available;") %>% 
    tagList(
      div(
        mod_DT_inputs_ui(id),
        style = str_c(dt_style, "overflow: auto;")
        )
      ) %>% 
    div(style = "max-height: calc(100vh-60); overflow: auto;")
}
    



#' wt_inp Server Functions
#'
#' @noRd 
mod_wt_inp_server <- function(id, input_dta, export_dta){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    curr_wt <- mod_DT_inputs_server(NULL, input_dta = input_dta)
    
 
  })
}
    
## To be copied in the UI
# mod_wt_inp_ui("wt_inp_ui_1")
    
## To be copied in the server
# mod_wt_inp_server("wt_inp_ui_1")


