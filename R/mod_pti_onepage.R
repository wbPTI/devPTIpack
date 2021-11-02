#' @describeIn app_new_pti_ui onepage_pti UI
#' @export
#' @import shiny
#' @importFrom golem get_golem_options 
mod_pti_onepage_ui <-
  function(request, id) {
    ns <- NS(id)
    pages <- mod_weights_ui(id, id, full_ui = FALSE)
    tagList(golem_add_external_resources(),
            shinyjs::useShinyjs(),
            pages) %>%
      fluidPage()
  }


#' pti_onepage Server Functions
#'
#' @noRd 
mod_pti_onepage_server <-
  function(id,
           shape_path = NULL,
           data_path = NULL,
           metadata_path = NULL,
           shape_dta = NULL,
           data_dta = NULL,
           demo_weights = FALSE) {
    
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      # Info-page, guides and blackouts
      # active_tab <- reactive({NULL})
      
      # Loading data
      shp_dta <- mod_get_shape_srv(NULL, shape_path = shape_path, shape_dta = shape_dta)
      input_dta <- mod_fetch_data_srv(
        NULL,
        data_fldr = "app-data/",
        data_path = data_path,
        data_dta = data_dta
      )
      
      # Weights input page
      if (demo_weights) {
        ws_to_plot_all <- mod_new_demo_weights_server(NULL, input_dta)
      } else {
        ws_to_plot_all <- mod_new_weights_server(NULL, input_dta, export_dta, demo_weights)
      }
      
      ws_to_plot <- reactive(ws_to_plot_all()$save_ws())
      # ws_current_ws_values <- reactive(ws_to_plot_all()$current_ws_values())
      export_dta <-
        mod_export_pti_data_server(
          NULL,
          reactive(req(plotted_dta()$pre_map_dta())),
          ws_to_plot
        )
      
      # PTI calculations
      map_dta <-
        mod_calc_pti2_server(
          NULL,
          shp_dta = shp_dta,
          input_dta = input_dta,
          wt_dta = ws_to_plot
        )
      
      # PTI Visualization
      plotted_dta <-
        mod_plot_pti2_srv(
          NULL, shp_dta, map_dta, wt_dta =  ws_to_plot,
          active_tab = function() "PTI", target_tabs = "PTI",
          metadata_path = metadata_path
        )
      
      observe({
        export_dta()
        # browser()
        # ws_to_plot_all()
        # req(!all(is.na(ws_to_plot_all()$current_ws_values()$weight)))
        # waiter::waiter_hide()
      })
      
    })
  }
