#' shape_upload UI Function
#'
#' @description A shiny Module for uploading shapefiles or GeoJSON files and displaying them on a Leaflet map.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fileInput textInput selectInput actionButton renderUI fluidRow bootstrapPage uiOutput
#' @importFrom leaflet leaflet addTiles addPolygons renderLeaflet
mod_shape_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("shapefile"), "Upload Shapefile/GeoJSON:",
      accept = c(".shp", ".geojson", ".zip")
    ),
    textInput(ns("layer_name"), "Layer Name:", ""),
    uiOutput(ns("column_select")),
    actionButton(ns("add_layer"), "Add Layer"),
    mod_map_pti_leaf_ui(ns("uploaded_leaf"), height = "calc(100vh - 300px)", width = "100%")
  ) %>%
    fluidRow() %>%
    bootstrapPage()
}

#' shape_upload Server Functions
#'
#' @description A shiny Module for handling the server-side logic of the shape upload and plotting.
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive observeEvent req showNotification updateSelectInput renderUI bindCache
#' @importFrom leaflet leaflet addTiles addPolygons renderLeaflet leafletProxy clearShapes clearGroup fitBounds addLayersControl
#' @importFrom sf st_read st_geometry_type st_transform
#' @importFrom tools file_ext
#' @importFrom dplyr mutate_if
mod_shape_upload_server <- function(id, shp_dta, map_dta, wt_dta, active_tab, target_tabs, mtdtpdf_path, shapes_path, show_adm_levels) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    uploaded_file <- reactiveVal(NULL)
    user_layers <- reactiveVal(list())

    observeEvent(input$shapefile, {
      req(input$shapefile)
      uploaded_file(input$shapefile)
    })

    user_shape_data <- reactive({
      req(uploaded_file())
      
      file_info <- uploaded_file()
      file_path <- file_info$datapath
      ext <- tools::file_ext(file_path)
      
      tryCatch({
        shape_data <- if (ext == "geojson") {
          sf::st_read(file_path, quiet = TRUE)
        } else if (ext == "zip") {
          unzip_dir <- tempdir()
          unzip(file_path, exdir = unzip_dir)
          shp_files <- list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE)
          if (length(shp_files) > 0) sf::st_read(shp_files[1], quiet = TRUE) else NULL
        } else if (ext == "shp") {
          sf::st_read(file_path, quiet = TRUE)
        } else {
          NULL
        }
        
        if (is.null(shape_data)) {
          showNotification("Invalid file format. Please upload a valid shapefile or GeoJSON file.", type = "error")
          return(NULL)
        }
        
        shape_data <- shape_data %>%
          dplyr::mutate_if(is.character, as.factor)
        
        crs <- st_crs(shp_dta()[[1]])
        transformed_shape <- st_transform(shape_data, crs)
        
        return(transformed_shape)
      }, error = function(e) {
        showNotification(paste("Error processing file:", e$message), type = "error")
        NULL
      })
    }) %>% bindCache(uploaded_file())

    output$column_select <- renderUI({
      req(user_shape_data())
      col_names <- names(user_shape_data())[sapply(user_shape_data(), function(x) is.numeric(x) || is.character(x) || is.factor(x))]
      
      if (length(col_names) > 0) {
        selectInput(ns("selected_column"), "Select Column for Coloring:", choices = col_names)
      } else {
        HTML("<p>No suitable columns found for coloring. Please upload a file with numeric, character, or factor columns.</p>")
      }
    })

    observeEvent(input$add_layer, {
      req(user_shape_data(), input$layer_name, input$selected_column)

      # Instead of storing data, just update metadata
      global_layers$layer_info[[input$layer_name]] <- list(source = "shape_upload")
      new_layer <- user_shape_data()[, c(input$selected_column, attr(user_shape_data(), "sf_column"))]
      
      current_layers <- user_layers()
      current_layers[[input$layer_name]] <- new_layer
      user_layers(current_layers)
      
      showNotification(paste("Layer", input$layer_name, "added successfully"), type = "message")
      
      # Reset inputs
      updateTextInput(session, "layer_name", value = "")
      uploaded_file(NULL)
    })

    mod_plot_pti2_srv("uploaded_leaf",
                      shp_dta = shp_dta,
                      map_dta = map_dta,
                      wt_dta = wt_dta,
                      active_tab = active_tab,
                      target_tabs = target_tabs,
                      metadata_path = mtdtpdf_path,
                      shapes_path = shapes_path,
                      show_adm_levels = show_adm_levels)

    observe({
      req(length(user_layers()) > 0)
      
      map <- leafletProxy("uploaded_leaf-leaf_id")
      
      # Clear all user layers and legends
      map %>%
        addMapPane("user_shapefile", zIndex = 450) %>%
        clearControls()
      
      for (layer_name in names(user_layers())) {
        layer_data <- user_layers()[[layer_name]]
        col_name <- names(layer_data)[1]  # The first column is the selected column for coloring
        
        print(paste("Adding layer:", layer_name))
        print(paste("Column used for coloring:", col_name))
        
        tryCatch({
          map <- add_user_shapefile(map, 
                                    layer_data, 
                                    layer_data[[col_name]], 
                                    col_name, 
                                    input[["uploaded_leaf-leaf_id_zoom"]], 
                                    group = layer_name)
        }, error = function(e) {
          print(paste("Error adding layer:", layer_name, "-", e$message))
        })
      }
      
      # Add layers control outside the loop
      map %>%
        addLayersControl(
          overlayGroups = names(global_layers$layer_info),
          options = layersControlOptions(collapsed = FALSE),
          position = "bottomleft"
        )
    })
  })
}