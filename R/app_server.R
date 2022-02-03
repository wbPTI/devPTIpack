#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import dplyr purrr stringr readr
#' @importFrom shiny reactive reactiveValues observeEvent callModule observe req isTruthy
#' @importFrom glue glue
#' @noRd
app_server <- function(input, output, session, shape_path = NULL, data_path = NULL, metadata_path = NULL) {
  
  # Info-page, guides and blackouts ==========================================
  
  active_tab <- reactive({input$main_sidebar})
  mod_info_page_server(input, output, session, first_tab = "PTI")
  
  # Loading data =============================================================
  # browser()
  shape_data <- mod_get_shape_srv("data_inputs", shape_path = shape_path)
  imported_data <- mod_fetch_data_srv("data_inputs", data_fldr = "app-data/", data_path = data_path)
  
  # Weights input page =======================================================
  
  ws_to_plot <-
    mod_weights_server("new_pti_core", 
                       imported_data, 
                       export_recoded_data)
  
  # Data explorer    =========================================================
  
  mod_explorer_server("data_explorer", 
                      shape_data = shape_data, 
                      imported_data = imported_data, 
                      active_tab = active_tab, 
                      target_tabs = "Data explorer", 
                      metadata_path = metadata_path)
  
  # PTI calculations =========================================================
  
  globr <- reactiveValues()
  observeEvent(input$main_sidebar, {
    globr$active_tab <- input$main_sidebar
  })
  
  observeEvent(shape_data(), globr[["new_shapes"]] <- shape_data())
  observeEvent(imported_data(), globr[["new_active_data"]] <- imported_data())
  observeEvent(ws_to_plot(), globr[["new_edited_list"]] <- ws_to_plot())
  
  callModule(mod_calc_pti_server, "new_pti_core", globr,
             shapes_list = 'new_shapes',
             # active_list = "new_active_data",
             edited_list = "new_edited_list",
             pti_raw_list = "new_pti_raw_dta",
             pti_clean_list = "new_main_page_pti")
  
  map_data <- reactive({(globr[["new_main_page_pti"]])})  
  
  # observeEvent(
  #   ws_to_plot()$weights_clean,
  #   {
  #     # req(map_data())
  #     browser()
  #   }, ignoreInit = T)
  
  
  observeEvent(
    map_data(),
    {
      # req(map_data())
      # browser()
    }, ignoreInit = T)
  
  export_recoded_data <- 
    mod_export_recoded_srv("new_pti_core", map_data, main_pallette)
  
  # PTI Visualisation ========================================================
  
  main_pallette <- 
    mod_map_pti_leaf_srv("weight_page_leaf", 
                       shape_data = shape_data,
                       map_data = map_data,
                       active_tab = active_tab, 
                       target_tabs = "PTI",
                       imported_data = imported_data,
                       metadata_path = metadata_path)
  
  # Compare page visualisation
  mod_map_pti_leaf_page_srv("compare_leaf_map", 
                            shape_data = shape_data,
                            map_data = map_data,
                            active_tab = active_tab, 
                            target_tabs = "PTI comparison",
                            imported_data = imported_data,
                            metadata_path = metadata_path)  
  
  # PTI Export and validation ================================================
  
  observe({
    req(ws_to_plot())
    waiter::waiter_hide()
  })
  
  # # callModule(profvis::profvis_server, "profiler")
}


#' The application server-side simplified for small problems
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import dplyr purrr stringr readr
#' @importFrom shiny reactive reactiveValues observeEvent callModule observe req isTruthy
#' @importFrom glue glue
#' @noRd
app_server_input_simple <- function(input, output, session, shape_path = NULL, data_path = NULL, 
                                    metadata_path = NULL, shape_dta = NULL, data_dta = NULL) {
  
  # Info-page, guides and blackouts ==========================================
  
  active_tab <- reactive({input$main_sidebar})
  mod_info_page_server(input, output, session, first_tab = "PTI")
  
  # Loading data =============================================================
  # browser()
  shape_data <- mod_get_shape_srv("data_inputs", shape_path = shape_path, shape_dta = shape_dta)
  imported_data <- mod_fetch_data_srv("data_inputs", data_fldr = "app-data/", data_path = data_path, data_dta = data_dta)
  
  # Weights input page =======================================================
  
  ws_to_plot <- mod_weights_rand_server("new_pti_core", imported_data)
  
  # observe({
  #   ws_to_plot()
  #   browser()
  # })
  # ws_to_plot <-
  #   mod_weights_server("new_pti_core",
  #                      imported_data,
  #                      export_recoded_data)
  
  # Data explorer    =========================================================
  
  # mod_explorer_server("data_explorer", 
  #                     shape_data = shape_data, 
  #                     imported_data = imported_data, 
  #                     active_tab = active_tab, 
  #                     target_tabs = "Data explorer", 
  #                     metadata_path = metadata_path)
  
  # PTI calculations =========================================================
  map_data <- 
    mod_calc_pti2_server(
      "new_pti_core",
      shp_dta = shape_data,
      input_dta = imported_data,
      wt_dta = ws_to_plot
    )
  
  export_recoded_data <-
    mod_export_recoded_srv("new_pti_core", map_data, main_pallette)
  
  # PTI Visualisation ========================================================
  
  main_pallette <-
    mod_map_pti_leaf_srv("weight_page_leaf",
                         shape_data = shape_data,
                         map_data = map_data,
                         active_tab = active_tab,
                         target_tabs = "PTI",
                         imported_data = imported_data,
                         metadata_path = metadata_path)
  
  # # Compare page visualisation
  # mod_map_pti_leaf_page_srv("compare_leaf_map",
  #                           shape_data = shape_data,
  #                           map_data = map_data,
  #                           active_tab = active_tab,
  #                           target_tabs = "PTI comparison",
  #                           imported_data = imported_data,
  #                           metadata_path = metadata_path)
  
  # PTI Export and validation ================================================
  
  observe({
    req(ws_to_plot())
    waiter::waiter_hide()
  })
  
  callModule(profvis::profvis_server, "profiler")
}

#' Server with a sample PTI plot
#' 
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import dplyr purrr stringr readr
#' @importFrom shiny reactive reactiveValues observeEvent callModule observe req isTruthy
#' @importFrom glue glue
#' @noRd
app_server_sample_pti_vis <- function(input, output, session, shape_path = NULL, data_path = NULL, 
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
                      shp_dta, map_dta, ws_to_plot, active_tab, 
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
