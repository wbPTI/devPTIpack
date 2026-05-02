#' Server with a sample PTI plot
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import dplyr purrr stringr readr
#' @importFrom shiny reactive reactiveValues observeEvent callModule observe req isTruthy
#' @importFrom glue glue
#' @noRd
app_new_pti_server <- function(input, output, session, shape_path = NULL, data_path = NULL,
                                      metadata_path = NULL, shape_dta = NULL, data_dta = NULL) {

  # Info-page, guides and blackouts
  active_tab <- reactive({input$main_sidebar})
  mod_info_page_server(input, output, session, first_tab = "PTI")

  # Loading data
  shp_dta <- mod_get_shape_srv("data_inputs", shape_path = shape_path, shape_dta = shape_dta)
  input_dta <- mod_fetch_data_srv("data_inputs", data_fldr = "app-data/", data_path = data_path, data_dta = data_dta)

  # Weights input page
  ws_to_plot_all <- mod_weights_server("new_pti_core", input_dta, export_dta)
  ws_to_plot <- reactive(ws_to_plot_all()$save_ws())
  ws_current_ws_values <- reactive(ws_to_plot_all()$current_ws_values())
  export_dta <- mod_export_pti_data_server("new_pti_core",
                                           reactive(req(plotted_dta()$pre_map_dta())),
                                           ws_to_plot)

  # PTI calculations
  map_dta <-
    mod_calc_pti2_server("new_pti_core", shp_dta = shp_dta,
                         input_dta = input_dta, wt_dta = ws_to_plot)

  # PTI Visualization
  plotted_dta <-
    mod_plot_pti2_srv("weight_page_leaf",
                      shp_dta, map_dta, wt_dta =  ws_to_plot, active_tab = active_tab,
                      target_tabs = "PTI", metadata_path = metadata_path)

  # Compare page visualization
  mod_plot_pti_comparison_srv("compare_leaf_map",
                              shp_dta, map_dta, ws_to_plot, active_tab,
                              target_tabs = "PTI comparison", metadata_path)

  # Data explorer
  mod_dta_explorer2_server("explorer_page_leaf",
                           shp_dta, input_dta, active_tab,
                           target_tabs = "Data explorer",
                           metadata_path = metadata_path)

  observe({
    req(!all(is.na(ws_to_plot_all()$current_ws_values()$weight)))
    waiter::waiter_hide()
  })

}
