#' explorer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom leaflet leafletOutput
mod_explorer_tab_ui <- function(id){
  ns <- NS(id)
  
  
  tagList(
    leafletOutput(ns("leaf_id"), height = "calc(100vh - 60px)", width = "100%"),
    mod_explorer_side_panel_ui(id)
  ) %>%
    tags$div(style = "position:relative;") %>% 
    tags$div(id = "explorer_1") %>% 
    tags$div(id = "explorer_2") %>% 
    tags$div(id = "explorer_3") %>% 
    fluidRow()
             
  
}



# Side panel --------------------------------------------------------------


mod_explorer_side_panel_ui <- function(id, ...){
  ns <- NS(id)
  
  absolutePanel(
    id = "explorer_panel", 
    style = 
      "z-index:950; min-width: 350px; background-color: rgba(255,255,255,0.8);
    box-shadow: 0 0 15px rgba(0,0,0,0.2); border-radius: 5px;
    padding: 10px 10px 10px 10px;
    zoom: 0.85; transition: opacity 500ms 1s;",
    fixed = FALSE,
    draggable = FALSE, 
    left = "auto", bottom = "auto",
    width = 400,
    height = "auto",
    top = 10, right = 10,
    
    mod_select_var_ui(id),
    mod_get_nbins_ui(id, "Number of bins"),
    mod_get_admin_levels_ui(id),
    mod_map_dwnld_ui(id)
  )
}



# Explorere server logic --------------------------------------------------


    
#' explorer Server Function
#'
#' @noRd 
mod_explorer_server <- function(id, shape_data, imported_data, active_tab, target_tabs, metadata_path = NULL, ...) {
  
  plotting_map <- reactiveVal()
  
  # Initialize the map
  mod_init_leaf_pti_srv(id, shape_data)
  
  # Fly to the shape
  mod_flyto_leaf_srv(id, shape_data, active_tab, target_tabs, plotting_map)
  
  # Input-related modules
  # Variable selector module 
  explorr_data <- mod_select_var_srv(id, imported_data)
  n_bins <- mod_get_nbins_srv(id)
  
  # Cleaning data for plotting:
  poly_data <- mod_clean_buble_data_srv(id, shape_data, imported_data = explorr_data, 
                                        var_fltr = "the_one", 
                                        out_type =  "polygon")
  poly_pal <- mod_map_pal_srv(id, n_bins, poly_data)
  clean_poly_map_data <- mod_map_dta_srv(id, poly_data, poly_pal, active_tab, target_tabs)
  
  # Mapping logic for poly data.
  mod_clean_leaf_srv(id, shape_data, clean_poly_map_data)

  # Plotting mechanism
  plotted_poly_leyers <- mod_plot_map_srv(id, clean_poly_map_data, shape_data, plotting_map, legend_type = "values")
  
  # Modules for overlay bubles
  bubble_data <- mod_clean_buble_data_srv(id, shape_data, imported_data, var_fltr = "fltr_overlay_explorer")
  bubble_pal <- mod_map_pal_srv(id, n_bins, bubble_data)
  clean_bubble_map_data <- mod_map_dta_srv(id, bubble_data, bubble_pal, active_tab, target_tabs)
  plotted_bubbles_leyers <- mod_plot_map_srv(id, clean_bubble_map_data, shape_data, plotting_map)
  
  # Adding controls for switch between layers
  selected_leyers <- mod_plot_controls_srv(id, plotted_poly_leyers, 
                                           added_bubbles = plotted_bubbles_leyers, plotting_map)
  
  # Adding legend based on selected layer
  mod_plot_legend_srv(id, selected_leyers, plotting_map, legend_type = "values")
  
  # Map download logic as an image
  mod_map_dwnld_srv(id, plotting_map, metadata_path = metadata_path)
  
  observe({
    # req(map_data())
    # plotted_bubbles_leyers()
    # browser()
    # req(plotted_leyers())
    # req(selected_leyers())
    # browser()
    # clean_map_data()
    # browser()
    # req(clean_map_data())
  })
  
  
  # observe({
  #   # req(map_pal())
  #   # req(clean_map_data())
  #   # # browser()
  #   # req(plotted_leyers())
  #   # req(selected_leyers())
  #   # browser()
  #   map_data()
  #   browser()
  #   # req(clean_map_data())
  # })
  
  reactive({map_pal()})
  
}






mod_select_var_srv <- function(id, imported_data, max_item = 1, ...) {
  
  moduleServer(#
    id, #
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(#
        imported_data(),
        {
          all_vars <-
            imported_data()$metadata %>%
            filter(!fltr_exclude_explorer)
          
          valid_choices <-
            all_vars %>%
            arrange(pillar_group, var_order) %>%
            group_by(pillar_name) %>%
            tidyr::nest() %>%
            pmap(
              .f = function(pillar_name, data) {
                # browser()
                set_names(list(set_names(data$var_code, data$var_name)), pillar_name)
              }
            ) %>%
            unlist(recursive = F)
          
          shinyWidgets::updatePickerInput(
            session,
            inputId = "indicators",
            choices = valid_choices#,
            # options = list(maxItems = max_item, highlight = TRUE)
          )
          
        },
        ignoreNULL = FALSE,
        ignoreInit = FALSE)
      
      selected_var <- 
        reactive({input$indicators}) %>% 
        debounce(500)
      
      eventReactive(#
        selected_var(), {
          if (isTruthy( selected_var())) {
            out <- imported_data()
            out$metadata <- 
              out$metadata %>% 
              mutate(the_one = var_code %in%  selected_var())
            return(out)
          } else {
            return(NULL)
          }
        })
      
      
    }
  )
  
}
