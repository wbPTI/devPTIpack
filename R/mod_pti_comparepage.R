#' pti_comparepage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export
#'
#' @importFrom shiny NS tagList bootstrapPage
mod_pti_comparepage_ui <- function(id) {
  ns <- NS(id)  
  tagList(
    column(
      6,
      mod_map_pti_leaf_ui(ns("first_leaf"), height = "calc(100vh - 60px)", width = "100%") %>%
        div(id = "compare_left_1") %>%
        div(id = "compare_left_2"),
      style = "padding-right: 3px; padding-left: 0px;"
    ),
    column(
      6,
      mod_map_pti_leaf_ui(ns("second_leaf"), height = "calc(100vh - 60px)", width = "100%") %>%
        div(id = "compare_right_1") %>%
        div(id = "compare_right_2"),
      style = "padding-right: 0px; padding-left: 3px;"
    )
  ) %>% 
    fluidRow() %>% 
    bootstrapPage()
}
    
#' pti_comparepage Server Functions
#'
#' @param map_dta,wt_dta elements of the reactive list returned by 
#'   `mod_ptipage_newsrv` module. In practice, the `mod_ptipage_newsrv` has to run
#'   and produce `map_dta` and `wt_dta` to make this module work. 
#' @inheritParams mod_ptipage_newsrv
#' @noRd 

mod_pti_comparepage_newsrv <-
  function(id,
           shp_dta,
           map_dta,
           wt_dta,
           active_tab,
           target_tabs,
           mtdtpdf_path = ".",
           shapes_path = ".",
           show_adm_levels = NULL,
           ...){
    
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    mod_plot_pti2_srv("first_leaf",
                      shp_dta = shp_dta,
                      map_dta = map_dta,
                      wt_dta = wt_dta,
                      active_tab = active_tab,
                      target_tabs = target_tabs,
                      metadata_path = mtdtpdf_path, 
                      shapes_path = shapes_path,
                      show_adm_levels =  show_adm_levels,
                      ...)
    
    mod_plot_pti2_srv("second_leaf",
                      shp_dta = shp_dta,
                      map_dta = map_dta,
                      wt_dta = wt_dta,
                      active_tab = active_tab,
                      target_tabs = target_tabs,
                      metadata_path = mtdtpdf_path,
                      shapes_path = shapes_path,
                      show_adm_levels =  show_adm_levels,
                      ...)
  })
}
    