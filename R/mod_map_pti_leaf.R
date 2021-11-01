
# Single leaf logic -------------------------------------------------------------



#' Simple one plot map UI
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom leaflet leafletOutput
mod_map_pti_leaf_ui <- function(id, ...){
  ns <- NS(id)
  
  tagList(
    leafletOutput(ns("leaf_id"), ...),
    mod_leaf_side_panel_ui(id)
  ) %>% 
    tags$div(
      style = "position:relative;"
    )
  
}



# Central cerver logic ----------------------------------------------------


#' @noRd 
mod_map_pti_leaf_srv <- function(id, shape_data, map_data, active_tab, target_tabs, imported_data, metadata_path = NULL,  ...) {
  
  plotting_map <- reactiveVal()
  
  # Initialize the map
  mod_init_leaf_pti_srv(id, shape_data)

  # Fly to the shape
  mod_flyto_leaf_srv(id, shape_data, active_tab, target_tabs, plotting_map)
  
  # Modules set for filterring data
  # N bins module
  n_bins <- mod_get_nbins_srv(id)
  
  # observe({
  #   map_data()
  #   browser()
  # })
  map_pal <- mod_map_pal_srv(id, n_bins, map_data)
  clean_map_data <- mod_map_dta_srv(id, map_data, map_pal, active_tab, target_tabs)
  
  mod_clean_leaf_srv(id, shape_data, clean_map_data)
  
  # Plotting mechanism
  plotted_leyers <- mod_plot_map_srv(id, clean_map_data, shape_data, plotting_map)
  
  # Modules for overlay bubles 
  clean_buble_data <- mod_clean_buble_data_srv(id, shape_data, imported_data)
  bubble_pal <- mod_map_pal_srv(id, n_bins, clean_buble_data)
  clean_map_buble_data <- mod_map_dta_srv(id, clean_buble_data, bubble_pal, active_tab, target_tabs)
  plotted_bubbles_leyers <- mod_plot_map_srv(id, clean_map_buble_data, shape_data, plotting_map)
  
    # Adding controls for switch between layers
  selected_leyers <- mod_plot_controls_srv(id, plotted_leyers, plotted_bubbles_leyers, plotting_map)
  
  # Adding legend based on selected layer
  mod_plot_legend_srv(id, selected_leyers, plotting_map)
  
  # Map download logic as an image
  mod_map_dwnld_srv(id, plotting_map, metadata_path = metadata_path)
  
  observe({
    # req(map_data())
    req(plotted_bubbles_leyers())
    # # browser()
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


# Several leafs page -------------------------------------------------------------

#' Page with several similar leaflets
#'
#' @noRd 
mod_map_pti_leaf_page_ui <-
  function(id, ...) {
    ns <- NS(id)
    tagList(
      fluidRow(
        column(6, mod_map_pti_leaf_ui(ns("first_leaf"), height = "calc(100vh - 60px)", width = "100%") %>%
                 div(id = "compare_left_1") %>%
                 div(id = "compare_left_2")),
        column(6, mod_map_pti_leaf_ui(ns("second_leaf"), height = "calc(100vh - 60px)", width = "100%") %>%
                 div(id = "compare_right_1") %>%
                 div(id = "compare_right_2"))
        )
      )
  }

#' Server for a page with several similar leaflets
#'
#' @noRd 
mod_map_pti_leaf_page_srv <-
  function(id, shape_data, map_data,  active_tab, target_tabs, imported_data, metadata_path = NULL, ...) {
    # observe({
    #   req(map_data())
    #   browser()
    # })
    moduleServer(
      id, 
      function(input, output, session) {
        ns <- session$ns
        
        mod_map_pti_leaf_srv("first_leaf", shape_data, map_data, active_tab, target_tabs, imported_data, metadata_path = metadata_path, ...)
        
        mod_map_pti_leaf_srv("second_leaf", shape_data, map_data, active_tab, target_tabs, imported_data, metadata_path = metadata_path, ...)
         
      }
    )
    
  }



# Data explorer -----------------------------------------------------------



# Initialize empty leaf map  -----------------------------------------------------

#' Initialize empty leaf 
#' 
#'
#' @noRd  
#' @importFrom  leaflet renderLeaflet leaflet addTiles
#' 
mod_init_leaf_pti_srv <- function(id, shape_data) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      output[["leaf_id"]] <- leaflet::renderLeaflet({
        leaflet::leaflet() %>%
          leaflet::addTiles()
      })
    })
}

# Fly-to modual -------------------------------------------------------------



#' Fly to observer and reactivity for the map
#' @noRd 
#' @import  leaflet
#' 
mod_flyto_leaf_srv <-
  function(id, shape_data, active_tab, target_tabs, plotting_map) {
    moduleServer(#
      id,
      function(input, output, session) {
        ns <- session$ns
        
        once_opened <- reactiveVal(NULL)
        
        first_open <-
          eventReactive(active_tab(), {
            req((active_tab() %in% target_tabs) & (!active_tab() %in% once_opened()))

            once_opened() %>%
              c(., active_tab()) %>%
              unique() %>%
              once_opened(.)

            once_opened()
            # req((active_tab() %in% target_tabs))
          }, ignoreNULL = FALSE, ignoreInit = FALSE)
        
        
        observe({
          req(shape_data())
          req(first_open())
          
          bounds <- sf::st_bbox(shape_data()[[1]])
          
          dashing <- c("1", "10 10", "5 5", "1 1","0.5 0.5") %>% rep(3)
          sizing <- c(4, 2, rep(1, 10))

          leafletProxy("leaf_id") %>%
            fitBounds(bounds[[1]], bounds[[2]], bounds[[3]], bounds[[4]]) %>%
            addProviderTiles(provider = providers$CartoDB.Voyager) %>%
            addMapPane("liene1", zIndex = 401) %>%
            addMapPane("polygons", zIndex = 410) %>%
            addMapPane("bubles", zIndex = 430)  %>%
            addMapPane("points", zIndex = 440) %>%
            list() %>%
            append(shape_data()) %>%
            reduce2(seq_along(shape_data()), function(x, y, i) {
              x %>%
                addPolylines(
                  data = y,
                  color = "darkgrey",
                  dashArray = dashing[[i]],
                  stroke = TRUE,
                  weight = sizing[[i]],
                  options = pathOptions(pane = "liene1")
                )
            })
          
          isolate({
            leaflet() %>%
              fitBounds(bounds[[1]], bounds[[2]], bounds[[3]], bounds[[4]]) %>% 
              addTiles() %>%
              addProviderTiles(provider = providers$CartoDB.Voyager) %>%
              addMapPane("liene1", zIndex = 401) %>%
              addMapPane("polygons", zIndex = 410) %>%
              addMapPane("bubles", zIndex = 430)  %>%
              addMapPane("points", zIndex = 440) %>%
              plotting_map()
            
          })
        })
      })
  }

#' @noRd 
#' @import  leaflet
#' 
mod_clean_leaf_srv <-
  function(id, shape_data, clean_map_data) {
    moduleServer(#
      id,
      function(input, output, session) {
        ns <- session$ns
        
        # observe({
        #   req(shape_data())
        #   
        #   if (!isTruthy(clean_map_data())) {
        #     # browser()
        # 
        #     # bounds <- sf::st_bbox(shape_data()[[1]])
        #     dashing <- c("1", "10 10", "5 5", "1 1") %>% rep(3)
        #     sizing <- c(4, 2, 1, 1)
        # 
        #     leafletProxy("leaf_id") %>%
        #       removeLayersControl() %>% 
        #       clearControls() %>% 
        #       clearTiles() %>% 
        #       clearShapes() %>% 
        #       addProviderTiles(provider = providers$CartoDB.Voyager) %>%
        #       addMapPane("liene1", zIndex = 401) %>%
        #       addMapPane("polygons", zIndex = 410) %>%
        #       addMapPane("bubles", zIndex = 430)  %>%
        #       addMapPane("points", zIndex = 440) %>%
        #       list() %>%
        #       append(shape_data()) %>%
        #       reduce2(seq_along(shape_data()), function(x, y, i) {
        #         x %>%
        #           addPolylines(
        #             data = y,
        #             color = "darkgrey",
        #             dashArray = dashing[[i]],
        #             stroke = TRUE,
        #             weight = sizing[[i]],
        #             options = pathOptions(pane = "liene1")
        #           )
        #       })
        #     
        #   }
        #   
        # })
      })
  }



# Prepare plot data -------------------------------------------------------


mod_map_pal_srv <- function(id, n_bins, map_data) {
  moduleServer(#
    
    id,
    function(input, output, session) {
      ns <- session$ns
      eventReactive(#
        list(n_bins(), map_data())
        ,
        {
          if (!isTruthy(map_data())) {
            return(NULL)
          }
          
          req(map_data())
          req(n_bins())

          spatial_levels <- map_data() %>% map("admin_level") %>% unname %>% unlist()
          
          
          if (!isTruthy(spatial_levels)) {
            return(NULL)
          }
          req(spatial_levels)
          selected_level <- spatial_levels[length(spatial_levels)]
          data_to_plot <- map_data()[[selected_level]]
          var_names <- data_to_plot$pti_data %>% names()
          data_to_plot$pti_codes %>% 
            as.list() %>%
            imap(~{
              a <- var_names %>% magrittr::extract(str_detect(., .y))
              value_var_id <- a %>% magrittr::extract(str_detect(., "score"))
              col_values <- data_to_plot$pti_data %>% pull(value_var_id)
              # browser()
              legend_map_satelite(col_values, n_groups = n_bins(),
                                  legend_paras = data_to_plot$legend_paras)
            })
          
        })
    })
}


# Key point where we can modify appearance of the polygons on the map
# Here we can specify what spatial level to plot.
#
mod_map_dta_srv <- function(id, map_data, map_pal, active_tab, target_tabs) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      once_opened <- reactiveVal(NULL)
      
      first_open <-
        eventReactive(active_tab(), {
          # req((active_tab() %in% target_tabs) & (!active_tab() %in% once_opened()))
          # 
          # once_opened() %>% 
          #   c(., active_tab()) %>% 
          #   unique() %>% 
          #   once_opened(.)
          # 
          # once_opened()
          
          req((active_tab() %in% target_tabs))
        }, ignoreNULL = FALSE, ignoreInit = FALSE)
      
      
      
      eventReactive(#
        list(map_pal(),
             map_data(),
             first_open())
        ,
        {
          if (!isTruthy(map_data())) {
            return(NULL)
          }
          
          req(map_data())
          req(map_pal())
          req(first_open())
          
          spatial_levels <- map_data() %>% map("admin_level") %>% unname %>% unlist()
          selected_level <- #c("admin1", "admin2")
            golem::get_golem_options("admins_to_plot")
          
          if (all(is.null(selected_level))) {
            selected_level <- spatial_levels[length(spatial_levels)-1]
          }
          
          data_to_plot <- map_data()[[selected_level]]
          if (isTruthy(data_to_plot$type)) shape_type <- data_to_plot$type else  shape_type <- "polygon"
          if (isTruthy(data_to_plot$overlay)) shape_position <- data_to_plot$overlay else  shape_position <- "single"
          
          var_names <- data_to_plot$pti_data %>% names()
          out_val <- 
            data_to_plot$pti_codes %>%
            as.list() %>%
            list(.x = .,
                 .y = names(.),
                 .z = map_pal()) %>%
            pmap(
              .f =
                function(.x, .y, .z) {
                  # browser()
                  a <- var_names %>% magrittr::extract(str_detect(., .y))
                  value_var_id <- a %>% magrittr::extract(str_detect(., "score"))
                  label_var_id <- a %>% magrittr::extract(str_detect(., "label"))
                  leyer_id <- str_c(.x, " (", names(selected_level) , ")")
                  col_values <- data_to_plot$pti_data %>% select(value_var_id) %>% pull(value_var_id)
                  pal_fun <- .z$pal
                  ploy_id = str_c(leyer_id, seq(1, nrow(data_to_plot$pti_data)))
                  list(
                    leyer_id = leyer_id,
                    leyer_id_bubble = str_c(leyer_id, " "),
                    ploy_id = ploy_id,
                    ploy_id_bubble = str_c(ploy_id,"buble"),
                    poly_id_tbl =
                      data_to_plot$pti_data %>% st_drop_geometry() %>%
                      select(contains("Pcod")) %>%
                      mutate(iid = ploy_id),
                    ploy_id_remove = str_c(leyer_id, seq(1, nrow(data_to_plot$pti_data))),
                    var_name = .x,
                    var_id = .y,
                    value_var_id = value_var_id,
                    label_var_id = label_var_id,
                    pal_fun = .z$pal,
                    recode_function = .z$recode_function,
                    col_values = col_values,
                    groups_number = .z$selected_groups,
                    out_pal = .z,
                    timestamp = Sys.time(),
                    shape_type = shape_type,
                    shape_position = shape_position
                  ) %>%
                    list() %>%
                    set_names(leyer_id)
                }
            ) %>%
            unname() %>%
            unlist(recursive = F) %>%
            list() %>%
            set_names("var_data") %>%
            prepend(list(plt_data = data_to_plot$pti_data)) 
          # browser()
          out_val
        },
        label = "New plotting for maps", ignoreNULL = FALSE)
      
    })
}


# Plotting map srvr -------------------------------------------------------

mod_plot_map_srv <- function(id, clean_map_data, shape_data, plotting_map = NULL, legend_type = "priorities") {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      plotted_base_leyers <- reactiveVal(list())
      existing_base_leyers <- reactiveVal(list())
      prog <- reactiveVal()
      
      observeEvent(#
        clean_map_data(),
        {
          # browser()
          base_map <-
            leafletProxy("leaf_id",
                         data = clean_map_data()$plt_data,
                         deferUntilFlush = TRUE) %>%
            clearControls() %>%
            list() %>%
            append(plotted_base_leyers()) %>%
            reduce(function(x, y) {
              x %>%
                removeShape(layerId = y)
            })
          
          isolate({
            out <-
              plotting_map() %>%
              clearControls() %>%
              list() %>%
              append(plotted_base_leyers()) %>%
              reduce(function(x, y) {
                x %>% removeShape(layerId = y)
              })
          })
          
          plotted_base_leyers(list())
          
          if (!isTruthy(clean_map_data())) {
            existing_base_leyers(list())
          }
          
          req(clean_map_data())
          
          prog(shiny::Progress$new(style = "notification"))
          prog()$set(message = "Updating:", value = 0.05)
          length_upd <- length(clean_map_data()$var_data)
          
          isolate({
            
            out <-
              out %>%
              list() %>%
              append(clean_map_data()$var_data) %>%
              reduce(function(xx, y) {
                
                if (legend_type == "priorities") {
                  poly_values <-
                    clean_map_data()$plt_data %>%
                    sf::st_drop_geometry() %>%
                    select(y$value_var_id) %>%
                    pull(y$value_var_id) %>%
                    y$recode_function() %>%
                    as.character() %>%
                    str_c("<strong>", ., "<strong/>")
                } else {
                  poly_values <- rep("", nrow(clean_map_data()$plt_data))
                }
                
                out_map <- xx
                
                if ("polygon" %in% y$shape_type) {
                  out_map <-
                    out_map %>%
                    addPolygons(
                      data = clean_map_data()$plt_data,
                      fillColor = ~ y$pal_fun(eval(parse(
                        text = y$value_var_id
                      ))),
                      group = y$leyer_id,
                      layerId = y$ploy_id,
                      color = "white",
                      weight = 1,
                      opacity = 1,
                      dashArray = "2",
                      label = ~ map(str_c(eval(
                        parse(text = y$label_var_id)
                      ), poly_values), ~ HTML(.)),
                      fillOpacity = 1,
                      highlight = highlightOptions(
                        weight = 2,
                        color = "#666",
                        dashArray = "",
                        # fillOpacity = 0.95,
                        bringToFront = TRUE
                      ),
                      options = pathOptions(pane = "polygons")
                    ) %>%
                    hideGroup(y$leyer_id_bubble)
                }
                
                
                if ("bubble" %in% y$shape_type) {
                  labs <- 
                    clean_map_data()$plt_data %>%
                    sf::st_drop_geometry() %>%
                    select(y$value_var_id) %>%
                    pull(y$value_var_id) %>% 
                    as.character() 
                  labs[is.na(labs)] <- ""
                  
                  out_map <-
                    out_map %>%
                    addCircleMarkers(
                      data = clean_map_data()$plt_data,
                      fillColor = "#fff9e8",
                      group = y$leyer_id_bubble,
                      layerId = y$ploy_id_bubble,
                      radius = ~ y$out_pal$radius_function(eval(parse(text = y$value_var_id))),
                      color = "#6b6b6b",
                      weight = 1,
                      label = ~ map(eval(parse(
                        text = y$label_var_id
                      )), ~ HTML(.)), 
                      popup = ~ map(eval(parse(
                        text = y$label_var_id
                      )), ~ HTML(.)), 
                      stroke = TRUE,
                      fillOpacity = 0.85,
                      options = pathOptions(pane = "bubles")
                    ) %>%
                    hideGroup(y$leyer_id_bubble)
                }
                out_map
              })
            plotting_map(out)
          })
          
          base_map %>%
            list() %>%
            append(clean_map_data()$var_data) %>%
            reduce(function(x, y) {
              plotted_base_leyers() %>%
                append(set_names(nm = y$leyer_id, list(y$ploy_id))) %>%
                plotted_base_leyers()
              
              prog()$set(
                value = prog()$getValue() + (prog()$getMax() - prog()$getValue()) / (length_upd),
                detail = "map"
              )
              
              if (legend_type == "priorities") {
                poly_values <-
                  clean_map_data()$plt_data %>%
                  sf::st_drop_geometry() %>%
                  select(y$value_var_id) %>%
                  pull(y$value_var_id) %>%
                  y$recode_function() %>%
                  as.character() %>%
                  str_c("<strong>", ., "<strong/>")
              } else {
                poly_values <- rep("", nrow(clean_map_data()$plt_data))
              }
              
              
              if ("polygon" %in% y$shape_type) {
                x %>%
                  hideGroup(y$leyer_id_bubble) %>%
                  addPolygons(
                    data = clean_map_data()$plt_data,
                    fillColor = ~ y$pal_fun(eval(parse(
                      text = y$value_var_id
                    ))),
                    group = y$leyer_id,
                    layerId = y$ploy_id,
                    color = "white",
                    weight = 1,
                    opacity = 1,
                    dashArray = "2",
                    label = ~ map(str_c(eval(
                      parse(text = y$label_var_id)
                    ), poly_values), ~ HTML(.)),
                    fillOpacity = 1,
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#666",
                      dashArray = "",
                      # fillOpacity = 0.95,
                      bringToFront = TRUE
                    ),
                    options = pathOptions(pane = "polygons")
                  )
              }
              
              
              if ("bubble" %in% y$shape_type) {
                labs <- 
                  clean_map_data()$plt_data %>%
                  sf::st_drop_geometry() %>%
                  select(y$value_var_id) %>%
                  pull(y$value_var_id) %>% 
                  as.character() 
                labs[is.na(labs)] <- ""
                
                
                x %>%
                  hideGroup(y$leyer_id_bubble) %>%
                  addCircleMarkers(
                    data = clean_map_data()$plt_data,
                    fillColor = "#fff9e8",
                    group = y$leyer_id_bubble,
                    layerId = y$ploy_id_bubble,
                    radius = ~ y$out_pal$radius_function(eval(parse(text = y$value_var_id))),
                    color = "#6b6b6b",
                    weight = 1,
                    label = ~ map(eval(parse(
                            text = y$label_var_id
                          )), ~ HTML(.)), 
                    popup = ~ map(eval(parse(
                      text = y$label_var_id
                    )), ~ HTML(.)), 
                    stroke = TRUE,
                    fillOpacity = 0.85,
                    options = pathOptions(pane = "bubles")
                  )

                # x %>%
                #   hideGroup(y$leyer_id_bubble) %>%
                #   addCircleMarkers(
                #     data = clean_map_data()$plt_data,
                #     fillColor = ~ y$pal_fun(eval(parse(
                #       text = y$value_var_id
                #     ))),
                #     group = y$leyer_id_bubble,
                #     layerId = y$ploy_id_bubble,
                #     radius = ~ y$out_pal$radius_function(eval(parse(
                #       text = y$value_var_id
                #     ))),
                #     color = ~ y$out_pal$pal(max(y$out_pal$our_breaks, na.rm = TRUE)),
                #     label = ~ map(eval(parse(
                #       text = y$label_var_id
                #     )), ~ HTML(.)),
                #     stroke = TRUE,
                #     weight = 1,
                #     fillOpacity = 0.60,
                #     options = pathOptions(pane = "bubles")
                #   )
              }
              x
            })
          
          prog()$close()
          
          prog(NULL)
          clean_map_data()$var_data %>%
            unname() %>%
            existing_base_leyers()
          
        },
        label = "New plotting PTI: PLOT POLYGONS",
        ignoreNULL = FALSE)
      
      reactive({
        if(!isTruthy(existing_base_leyers())) {
          return(NULL)
        } else {
          return(existing_base_leyers())
        }
        
      })
      
    })

}

# Plotting controls srvr -------------------------------------------------------

mod_plot_controls_srv <- function(id, plotted_leyers, added_bubbles, plotting_map) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      existing_base_leyers <- reactiveVal()
      existing_base_leyers_index <- reactiveVal(character(0))
      selected_base_leyer <- reactiveVal(character(0))
      ever_existing_base_leyer <- reactiveVal(character(0))
      previous_selected_base_leyer <- reactiveVal(character(0))
      
      existing_overlay_leyers <- reactiveVal()
      existing_overlay_leyers_index <- reactiveVal(character(0))
      selected_overlay_leyer <- reactiveVal(character(0))
      previous_selected_overlay_leyer <- reactiveVal(character(0))
      
      current_existing_groups <- reactiveVal(character(0))
      ever_existed_groups <- reactiveVal(character(0))
      hide_groups <- reactiveVal(character(0))
      
      check_selected <- reactiveVal(0)
      
      # Adding map controls first =====================================
      
      observeEvent(plotted_leyers(), {
        plotted_leyers() %>% existing_base_leyers()
        plotted_leyers() %>% map_chr("leyer_id") %>% existing_base_leyers_index()
      }, ignoreNULL = FALSE)
      
      observeEvent(added_bubbles(), {
        added_bubbles() %>% existing_overlay_leyers()
        added_bubbles() %>% map_chr("leyer_id_bubble") %>% existing_overlay_leyers_index()
      }, ignoreNULL = FALSE)
      
      observe({
        new_existing_groups <- c(existing_base_leyers_index(), existing_overlay_leyers_index())
        isolate({
          new_existing_groups %>% unique() %>% current_existing_groups()
          ever_existed_groups() %>%
            c(., current_existing_groups()) %>%
            unique() %>%
            ever_existed_groups()
        })
      })
      
      
      observeEvent(#
        current_existing_groups(),
        {
        hide <- ever_existed_groups()#[!ever_existed_groups() %in% dont_hide]
        if(length(current_existing_groups()) == 0) {
          isolate({
            leafletProxy("leaf_id", deferUntilFlush = TRUE) %>%
              hideGroup(hide) %>%
              clearControls()

            if (isTruthy(plotting_map())) {
              plotting_map() %>%
                hideGroup(hide) %>%
                clearControls() %>%
                plotting_map()
            }
          })
        }

        req(length(current_existing_groups()) > 0)

        isolate({
          # if (!identical(existing_base_leyers_index(), character(0)) &
          #     !isTRUE(selected_base_leyer() %in% existing_base_leyers_index())) {
          #   existing_base_leyers_index()[[1]] %>% selected_base_leyer()
          # }

          leafletProxy("leaf_id", deferUntilFlush = TRUE) %>%
            # clearControls() %>%
            addLayersControl(
              baseGroups = existing_base_leyers_index(),
              overlayGroups = existing_overlay_leyers_index(),
              position = "bottomright",
              options = layersControlOptions(collapsed = FALSE)
            ) %>%
          #   showGroup(selected_base_leyer()) %>%
            showGroup(selected_overlay_leyer())
          if (isTruthy(plotting_map())) {
            plotting_map() %>%
              # clearControls() %>%
              addLayersControl(
                baseGroups = existing_base_leyers_index(),
                overlayGroups = existing_overlay_leyers_index(),
                position = "bottomright",
                options = layersControlOptions(collapsed = FALSE)
              ) %>%
              # showGroup(selected_base_leyer()) %>%
              showGroup(selected_overlay_leyer()) %>%
              plotting_map()
          }
          check_selected(1)

        })
      }, ignoreNULL = FALSE)
      
      
      
      # Unhiding requested leyer ======================================
      
      observeEvent({
        input[["leaf_id_groups"]]
      }, {
        
        if (!isTruthy(input[["leaf_id_groups"]])) {
          selected_base_leyer(character(0))
          selected_overlay_leyer(character(0))
        }
        
        req(isTruthy(input[["leaf_id_groups"]]))
        
        selected_bases <- input[["leaf_id_groups"]] %in% existing_base_leyers_index()
        if (any(selected_bases)) {
          input[["leaf_id_groups"]][selected_bases] %>% selected_base_leyer()
        } else {
          selected_base_leyer(character(0))
        }
        
        selected_overs <- input[["leaf_id_groups"]] %in% existing_overlay_leyers_index()
        if (any(selected_overs)) {
          input[["leaf_id_groups"]][selected_overs] %>% selected_overlay_leyer()
        } else {
          selected_overlay_leyer(character(0))
        }
        
      }, 
      ignoreNULL = FALSE)

      
      # Check what are the selected leyers if controls were updated
      
      observeEvent(#
        check_selected(), {
          req(check_selected() > 0)
          # browser()
          
          if (isTruthy(selected_base_leyer()) & 
              any(!selected_base_leyer() %in% existing_base_leyers_index())) 
            selected_base_leyer()[selected_base_leyer() %in% existing_base_leyers_index()] %>% 
            selected_base_leyer()
          
          if (!isTruthy(selected_base_leyer()) & isTruthy(existing_base_leyers_index()))
            existing_base_leyers_index()[[1]] %>% selected_base_leyer()
          
          if (isTruthy(selected_base_leyer()) & 
              all(selected_base_leyer() %in% existing_base_leyers_index())) {
            recelect <- selected_base_leyer()
            selected_base_leyer(character(0))
            selected_base_leyer(recelect)
          } 
          check_selected(0)
        })
      
      observeEvent(#
        selected_base_leyer(),
        {
          layers_remove <-
            previous_selected_base_leyer()[!previous_selected_base_leyer() %in% 
                                             selected_base_leyer()]
          
          isolate({
            if (isTruthy(plotting_map())) {
              plotting_map() %>%
                hideGroup(layers_remove) %>%
                showGroup(selected_base_leyer()) %>%
                plotting_map()
            }
          })
          selected_base_leyer() %>%
            previous_selected_base_leyer()
        })
      
      observeEvent(#
        selected_overlay_leyer(),
        {
          layers_remove <-
            previous_selected_overlay_leyer()[!previous_selected_overlay_leyer() %in% selected_overlay_leyer()]
          
          isolate({
            if (isTruthy(plotting_map())) {
              plotting_map() %>%
                hideGroup(layers_remove) %>%
                showGroup(selected_overlay_leyer()) %>%
                plotting_map()
            }
          })
          selected_overlay_leyer() %>%
            previous_selected_overlay_leyer()
        })
      
      reactive({
        list(
          selected_base_leyer = selected_base_leyer(),
          selected_overlay_leyer = selected_overlay_leyer(),
          existing_base_leyers = existing_base_leyers(),
          existing_overlay_leyers = existing_overlay_leyers()
        )
      })
      
      
    })
}






# Plotting legends srvr -------------------------------------------------------

mod_plot_legend_srv <- function(id, selected_leyers, plotting_map, legend_type = "priorities") {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      existing_base_leyers <- reactiveVal()
      selected_base_leyer <- reactiveVal()
      
      
      observe({
        selected_leyers()$selected_base_leyer %>% 
          c(., selected_leyers()$selected_overlay_leyer) %>% 
          selected_base_leyer()
        selected_leyers()$existing_base_leyers %>% 
          append(selected_leyers()$existing_overlay_leyers) %>% 
          existing_base_leyers()
      })
      
      
      observeEvent(
        list(existing_base_leyers(),
             selected_base_leyer()),
        {
          req(existing_base_leyers())
          if(!isTruthy(selected_base_leyer())) {
            # browser()
            # removeControl(all_legends)
          }
          
          main_map <-
            leafletProxy("leaf_id", deferUntilFlush = TRUE) %>%
            clearControls()
          
          isolate({
            plotting_map() %>%
              clearControls() %>%
              plotting_map()
          })
          
          existing_base_leyers() %>%
            keep(~{.x$leyer_id %in% selected_base_leyer()}) %>%
            walk( ~ {
              if (legend_type == "priorities") {
                labs <- .x$out_pal$our_labels_category
              } else {
                labs <- .x$out_pal$our_labels
              }
              # groupsnum <- max(2,  .x$groups_number)
              isolate({
                plotting_map() %>%
                  addLegend(
                    position = "bottomleft",
                    labels = labs,
                    colors =  .x$out_pal$pal(.x$out_pal$our_values),
                    opacity = 1,
                    title = .x$leyer_id
                  ) %>%
                  plotting_map()
              })
              
              main_map %>%
                addLegend(
                  position = "bottomleft",
                  labels = labs,
                  colors =  .x$out_pal$pal(.x$out_pal$our_values),
                  opacity = 1,
                  title = .x$leyer_id
                )
              
            })
          
          existing_base_leyers() %>%
            keep( ~ {
              .x$leyer_id_bubble %in% selected_base_leyer()
            }) %>%
            walk(~ {
              legend_colors <-
                make_shapes(
                  colors = "#fff9e8", #.x$out_pal$pal(.x$out_pal$our_values),
                  sizes = .x$out_pal$radius_function(.x$out_pal$our_values) * 2,
                  borders = "#6b6b6b", #.x$out_pal$pal(max(.x$out_pal$our_values, na.rm = T))
                )
              
              legend_labels <-
                make_labels(
                  sizes = .x$out_pal$radius_function(.x$out_pal$our_values) * 2,
                  labels = .x$out_pal$our_labels_bubble
                )
              
              plotting_map()  %>%
                addLegend(
                  position = "bottomleft",
                  colors = legend_colors,
                  labels = legend_labels,
                  title = .x$leyer_id,
                  layerId = .x$leg_id_poly,
                  opacity = 0.6
                )
              
              isolate({
                plotting_map()  %>%
                  addLegend(
                    position = "bottomleft",
                    colors = legend_colors,
                    labels = legend_labels,
                    title = .x$leyer_id,
                    layerId = .x$leg_id_poly,
                    opacity = 0.6
                  ) %>%
                  plotting_map()
              })
              
              main_map  %>%
                addLegend(
                  position = "bottomleft",
                  colors = legend_colors,
                  labels = legend_labels,
                  title = .x$leyer_id,
                  layerId = .x$leg_id_poly,
                  opacity = 0.6
                )
            })
          
        }, 
        label = "plot pti: PLOT legend", 
        ignoreInit = TRUE
      )
      
      
    })
}




# Mod buble data srvr ------------------------------------------------

#' Buble data serve
#' 
#' @noRd
#' @importFrom sf st_centroid
mod_clean_buble_data_srv <- function(id, shape_data, imported_data, 
                                     # map_data, clean_map_data, 
                                     var_fltr = "fltr_overlay_pti",
                                     call_fltr = function(x) (x),
                                     # admin_focus = "admin3",
                                     out_type =  "bubble") {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      eventReactive(#
        imported_data(), {
          if(!isTruthy( imported_data())) return(NULL)
          
          relevant_leyers <-
            imported_data() %>%
            magrittr::extract(names(.) %>% str_detect("admin\\d"))
          
          leyers_names <-
            relevant_leyers %>%
            names() %>%
            map( ~ {
              set_names(str_extract(.x, "admin\\d"),
                        str_replace(.x, "admin\\d_", ""))
            })
          
          # Preparing shapes for bubles
          suppressWarnings({
            if (out_type == "bubble") {
              point_locations <-
                shape_data() %>%
                magrittr::extract(names(.) %in% names(relevant_leyers)) %>%
                # magrittr::extract(names(.) %>% str_detect(admin_focus)) %>%
                map( ~ sf::st_centroid(.x))
            } else {
              point_locations <-
                shape_data() %>%
                magrittr::extract(names(.) %in% names(relevant_leyers)) 
            }
          })
          
          
          # fltr_var <- sym(var_fltr)
          # browser()
          buble_vars <-
            imported_data()$metadata %>%
            filter_at(vars((var_fltr)), any_vars(call_fltr(.)))
          
          if(nrow(buble_vars) == 0) return(NULL)
          
          req(nrow(buble_vars) > 0)
          # browser()
          imported_data() %>%
            magrittr::extract(names(.) %>% str_detect("admin\\d")) %>%
            # magrittr::extract(names(.) %>% str_detect(admin_focus)) %>%
            magrittr::set_names(., names(.) %>% str_extract_all("admin\\d")) %>%
            imap( ~ {
              out <-
                list(pti_data = select(.x, matches("Pcod"), any_of(buble_vars$var_code)))
            }) %>%
            map2(point_locations, ~ {
              # browser()
              if (length(names(.x$pti_data)) > 1) {
                by_var <-
                  names(.x$pti_data) %>%  magrittr::extract(str_detect(., "admin\\d"))
                .x$pti_data <- left_join(.y, .x$pti_data, by_var)
              } else {
                .x <- NULL
              }
              .x
            }) %>%
            map2(leyers_names,
                 ~ {
                   if (isTruthy(.x))
                     .x$admin_level <- .y
                   .x
                 }) %>%
            map( ~ {
              # browser()
              out <- .x
              if (!isTruthy(.x)) return(out)
              
              full_var_metadata <- filter(buble_vars, var_code %in% names(.x$pti_data)) 
              if (nrow(full_var_metadata) == 0) return(NULL)
              
              out$pti_codes <- set_names(full_var_metadata$var_name, full_var_metadata$var_code)
              
              list(names(out$pti_codes),
                   out$pti_codes,
                   seq_along(out$pti_codes)) %>%
                pwalk(~ {
                  name_var <- 
                    names(out$pti_data) %>%
                    magrittr::extract(str_detect(., "admin\\dName")) %>%
                    magrittr::extract2(1)
                  name_var <- sym(name_var)
                  lab_var <- str_c("pti_label..pti_ind_", ..3)
                  val_var <- str_c("pti_score..pti_ind_", ..3)
                  indicator_var = sym(..1)
                  glue_call <- "<strong>{spatial_name}</strong><br/><strong>{..2}</strong>: <strong>"
                  out$pti_data <<-
                    out$pti_data %>%
                    mutate(spatial_name = {{name_var}}) %>%
                    mutate(!!lab_var := glue(glue_call) %>%
                             str_c(ifelse(is.na({{indicator_var}}), "No data", {{indicator_var}}), 
                                   "</strong><br/>")
                           ) %>%
                    rename_at(vars(any_of(..1)), list( ~ val_var))
                  
                })
              # browser()
              
              out$pti_codes <-
                out$pti_codes %>%
                magrittr::set_names(str_c("pti_ind_", seq_along(.)))
              
              # Adding legend parameters from metadata.
              out$legend_paras <- 
                full_var_metadata %>% 
                select(matches("^legend_")) %>% 
                as.list()
              if (length(out$legend_paras) == 0) {
                out$legend_paras <- NULL
              }
              
              out$type = out_type
              out$overlay = "multiple"
              out
            })
          
        }, 
        ignoreNULL = FALSE, 
        ignoreInit = FALSE
      )

    })
  
}


















