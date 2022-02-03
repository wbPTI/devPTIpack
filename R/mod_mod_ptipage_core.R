#' page_pti UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList column div
mod_ptipage_twocol_ui <- function(id, 
                                   map_height = "calc(95vh - 60px)",
                                   wt_height = "calc(95vh - 250px)", 
                                   dt_style = "zoom:0.9;",
                                   full_ui = FALSE){
  ns <- NS(id)
  fillPage(
    shinyjs::useShinyjs(),
    golem_add_external_resources(),
    div(
      column(4, 
             mod_wt_inp_ui(ns(NULL), 
                           full_ui = FALSE, 
                           height = wt_height, 
                           dt_style = dt_style),
             style = "padding-right: 5; padding-left: 5;"
      ),
      column(8,
             mod_map_pti_leaf_ui(ns(NULL), height = map_height),
             style = "padding-left: 0; padding-right: 0;")
    )
  )
  
}
    
#' page_pti Server Functions
#'
#' @noRd 
mod_ptipage_newsrv <- function(id, 
                                imp_dta = reactive(NULL), 
                                shp_dta = reactive(NULL)){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Inputs
    wt_dta <- mod_wt_inp_server(NULL, 
                                input_dta = imp_dta, 
                                plotted_dta = reactive(plotted_dta()$pre_map_dta()))
    
    observe({
      req(golem::get_golem_options("diagnostics"))
      req(wt_dta()$weights_clean)
      cat("PTI: start calc ", as.character(Sys.time()), "\n" )
    })
    
    # PTI calculations
    map_dta <-
      mod_calc_pti2_server(NULL,
                           shp_dta = shp_dta,
                           input_dta = imp_dta,
                           wt_dta = wt_dta)
    
    observe({
      req(golem::get_golem_options("diagnostics"))
      req(length(map_dta()) > 0)
      cat("PTI: End calc ", as.character(Sys.time()), "\n" )
    })
    
    # PTI Visualization
    plotted_dta <-
      mod_plot_pti2_srv(NULL,
                        shp_dta = shp_dta, 
                        map_dta = map_dta, 
                        wt_dta =  wt_dta, 
                        active_tab = reactive("PTI"),
                        target_tabs = "PTI", 
                        metadata_path = "metadata_path")
    
    observe({
      req(golem::get_golem_options("diagnostics"))
      req(plotted_dta()$pre_map_dta())
      cat("PTI: End plot data to export ", as.character(Sys.time()), "\n" )
    })
    
  })
}
    
## To be copied in the UI
# mod_page_pti_ui("page_pti_ui_1")
    
## To be copied in the server
# mod_page_pti_server("page_pti_ui_1")
