#' plot_poly_leaf_server UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_poly_leaf_server_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' plot_poly_leaf_server Server Function for plotting PTI polygons
#'
#' @param shp_dta reactive object with shapes
#' @param preplot_dta Clean weighted data for plotting as a reactive object
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' 
#' @export
#' @import shiny
#' @importFrom leaflet leafletProxy
mod_plot_poly_leaf_server <- function(id, preplot_dta, shp_dta, ...){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    previous_plot <- reactiveVal(NULL)
    selected_layer <- reactiveVal(NULL)
    
    observeEvent(#
      input[["leaf_id_groups"]], {
        selected_layer(input[["leaf_id_groups"]])
        # cat(selected_layer(), "\n")
        }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    observeEvent(#
      list(preplot_dta()), {
        if (!isTruthy(preplot_dta())) {
          leaflet::leafletProxy("leaf_id", deferUntilFlush = TRUE) %>%
            clean_pti_polygons(previous_plot()) %>% 
            clean_pti_poly_controls(previous_plot()) 
          previous_plot(NULL)
          
        } else {
          leaflet::leafletProxy("leaf_id", deferUntilFlush = TRUE) %>%
            clean_pti_polygons(previous_plot()) %>% 
            clean_pti_poly_controls(previous_plot()) %>% 
            plot_pti_polygons(preplot_dta()) %>%  
            add_pti_poly_controls(preplot_dta(), selected_layer()) 
          
          previous_plot(preplot_dta())
        }
        
      }, ignoreInit = FALSE, ignoreNULL = FALSE, priority = 0)
    
    # Plotting the legend
    mod_plot_poly_legend_server(NULL, preplot_dta, selected_layer)
    
    # returning selected layer
    out <- mod_plot_leaf_export(NULL, shp_dta, preplot_dta, selected_layer)
    
    out
  })
}


#' @describeIn mod_plot_poly_leaf_server complementing module ment to reproduce the map and return a simple leaflet object
#' 
#' @export
#' @import shiny
mod_plot_leaf_export <-
  function(id, shp_dta, preplot_dta, selected_layer, ...) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      leaf_out <- reactiveVal(NULL)
      
      observeEvent(#
        shp_dta(), {
          req(shp_dta())
          leaflet() %>%
            plot_leaf_line_map2(shp_dta(), get_golem_options("show_adm_levels")) %>%
            leaf_out()
        }, ignoreNULL = FALSE, ignoreInit = FALSE)
      
      observeEvent(#
        list(preplot_dta(), selected_layer()), {
          req(preplot_dta())
          req(selected_layer())
          leaflet() %>%
            plot_leaf_line_map2(shp_dta(), get_golem_options("show_adm_levels")) %>%
            plot_pti_polygons(preplot_dta()) %>%
            clearControls() %>%
            add_pti_poly_controls(preplot_dta(), selected_layer()) %>%
            plot_pti_legend(preplot_dta(), selected_layer()) %>%
            leaflet::removeLayersControl() %>%
            leaf_out()
        }, ignoreNULL = FALSE, ignoreInit = FALSE)
      
      
      # # Extending legend module
      # old_layer <- reactiveVal(NULL)
      # observeEvent(#
      #   selected_layer(), {
      #     # req(selected_layer())
      #     
      #     # Removing any old legend
      #     if (isTruthy(old_layer())) {
      #       leaf_out() %>%
      #         remove_pti_legend(map_dta(), old_layer())  %>%
      #         leaf_out()
      #       old_layer(NULL)
      #     }
      #     
      #     # Adding new legend to the map
      #     if (isTruthy(selected_layer())) {
      #       old_layer(selected_layer())
      #       leaf_out() %>%
      #         plot_pti_legend(preplot_dta(), selected_layer()) %>% 
      #         leaflet::removeLayersControl() %>%
      #         leaf_out() 
      #     }
      #     
      #   }, ignoreNULL = FALSE, ignoreInit = FALSE)
      # 
      # observe({
      #   leaf_out()
      #   browser()
      # })
      # 
      # observe({
      #   selected_layer()
      #   browser()
      #   
      #   %>%
      #     clean_pti_polygons(previous_plot()) %>% 
      #     clean_pti_poly_controls(previous_plot()) %>% 
      #     plot_pti_polygons(preplot_dta()) %>%  
      #     add_pti_poly_controls(preplot_dta(), selected_layer()) 
      # })
      
      leaf_out
    })
  }
    
## To be copied in the UI
# mod_plot_poly_leaf_server_ui("plot_poly_leaf_server_ui_1")
    
## To be copied in the server
# mod_plot_poly_leaf_server_server("plot_poly_leaf_server_ui_1")
