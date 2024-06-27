#' shape_upload UI Function
#'
#' @description A shiny Module for uploading shapefiles or GeoJSON files and displaying them on a Leaflet map.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList fileInput leafletOutput selectInput actionButton uiOutput
#' @importFrom leaflet leaflet addTiles addPolygons renderLeaflet
mod_shape_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("shapefile"), "Upload Shapefile/GeoJSON:",
              accept = c(".shp", ".geojson", ".zip")),
    uiOutput(ns("column_select")),
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
#' @importFrom leaflet leaflet addTiles addPolygons renderLeaflet leafletProxy clearShapes clearGroup bringToFront fitBounds
#' @importFrom sf st_read st_geometry_type
#' @importFrom tools file_ext
#' @importFrom dplyr mutate_if
mod_shape_upload_server <- function(id, shp_dta, map_dta, wt_dta, active_tab, target_tabs, mtdtpdf_path, shapes_path, show_adm_levels) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    user_shape_data <- reactive({
      if(is.null(input$shapefile)) {
        return(NULL)
      }
      
      file_path <- input$shapefile$datapath
      ext <- tools::file_ext(file_path)
      
      tryCatch({
        shape_data <- if (ext == "geojson") {
          sf::st_read(file_path)
        } else if (ext == "zip") {
          unzip_dir <- tempdir()
          unzip(file_path, exdir = unzip_dir)
          shp_files <- list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE)
          if (length(shp_files) > 0) sf::st_read(shp_files[1]) else NULL
        } else if (ext == "shp") {
          sf::st_read(file_path)
        } else {
          NULL
        }
        
        if (is.null(shape_data)) {
          showNotification("Invalid file format. Please upload a valid shapefile or GeoJSON file.", type = "error")
          return(NULL)
        }
        
        # Convert character columns to factors
        shape_data <- shape_data %>%
          dplyr::mutate_if(is.character, as.factor)
        

        crs <- st_crs(shp_dta()[[1]])
        transformed_shape <- st_transform(shape_data, crs)
        
        return(transformed_shape)
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
        NULL
      })
    })
    
    output$column_select <- renderUI({
      req(user_shape_data())
      print(user_shape_data())
      # get a vector of column names whose values are either numeric or character
      col_names <- names(user_shape_data())[sapply(user_shape_data(), function(x) is.numeric(x) || is.character(x) || is.factor(x))]
      
      if (length(col_names) > 0) {
        selectInput(ns("user_input_col"), "Select Column for Coloring:", choices = col_names)
      } else {
        HTML("<p>No suitable columns found for coloring. Please upload a file with numeric, character, or factor columns.</p>")
      }
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

    # New observe block to handle user-uploaded shapefile
    observe({
      req(user_shape_data())
      req(input$user_input_col)
      # bounding box of the shapefile
      lat_1 <- st_bbox(user_shape_data())[["ymin"]]
      lng_1 <- st_bbox(user_shape_data())[["xmin"]]
      lat_2 <- st_bbox(user_shape_data())[["ymax"]]
      lng_2 <- st_bbox(user_shape_data())[["xmax"]]

      leafletProxy("uploaded_leaf-leaf_id") %>% 
        addMapPane("user_shapefile", zIndex = 450) %>%
        clearGroup("user_shapefile") %>%
        clearControls() %>% 
        fitBounds(lng1 = lng_1, lat1 = lat_1, lng2 = lng_2, lat2 = lat_2) %>%
        add_user_shapefile(user_shape_data(), user_shape_data()[[input$user_input_col]], input$user_input_col, input[["uploaded_leaf-leaf_id_zoom"]], group = "user_shapefile")
    })
    
    observe({
      if (is.null(input$shapefile)) {
        leafletProxy("uploaded_leaf-leaf_id") %>%
          clearGroup("user_shapefile")
      }
    })
  })
}