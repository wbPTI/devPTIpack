#' Module: UI of the one-page PTI with the two-col layout 
#'
#' @description A shiny Module.
#'
#' @param id module ID, to be the same as the ID of server module (`mod_ptipage_newsrv`)
#' @param cols vector with two values that should not exceed 12 in sum to
#'             define split of the column layout
#' @param map_height,wt_height are css heights of the map part. calc(95vh - 60px) 
#' @param dt_style additional css style passed to the DataTable with weights
#' @param full_ui TRUE/FALSE is the complete input layout has to be plotted;
#' @param ... other arguments (not in use)
#' #
#' ,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @export
#'
#' @importFrom shiny NS tagList column div
mod_ptipage_twocol_ui <- function(id, 
                                  cols = c(4,8),
                                  full_ui = FALSE,
                                  map_height = "calc(95vh - 60px)",
                                  wt_height = "calc(95vh - 250px)", 
                                  dt_style = "zoom:0.9;",
                                  ...) {
  ns <- NS(id)
  shiny::fillPage(
    shinyjs::useShinyjs(),
    golem_add_external_resources(),
    shiny::div(
      shiny::column(
        cols[[1]],
        style = "padding-right: 5; padding-left: 5;",
        mod_wt_inp_ui(
          ns(NULL),
          full_ui = FALSE,
          height = wt_height,
          dt_style = dt_style
        )
      ),
      shiny::column(#
        cols[[2]],
        style = "padding-left: 0; padding-right: 0;",
        mod_map_pti_leaf_ui(
          ns(NULL), 
          height = map_height
          )
        )
      )
    )
  }
    

#' @describeIn mod_ptipage_twocol_ui
#' 
#' @param imp_dta reactive list of tibbles with standardized PTI inputs data
#' @param shp_dta reactive list of geo-referenced tibbles with polygons shapes
#' @param show_adm_levels vector of admin levels to show and select from.
#'        if NULL - all levels are printed. If single value, e.g. 'admin2'
#'        exclusively one level is plotted unless there is not data for such level,
#'        then one level with the highest dis aggregation is plotted.
#'        If vector c("admin1", "admin4"), only level specified are plotted. If 
#'        no data exist for levels specified
#'        plotted. if
#' @param ... other parameters to pass to server logic and golem_options (see description)
#' 
#' @export
#'
#' @noRd 
mod_ptipage_newsrv <- function(id,
                               imp_dta = reactive(NULL), 
                               shp_dta = reactive(NULL),
                               default_adm_level = NULL, 
                               show_adm_levels = NULL,
                               ...){
  
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
                        metadata_path = "metadata_path",
                        default_adm_level = default_adm_level,
                        show_adm_levels = show_adm_levels)
    
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
