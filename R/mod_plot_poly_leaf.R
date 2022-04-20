#' plot_poly_leaf_server UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @export
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
#' @importFrom shiny moduleServer observeEvent reactiveVal
#' @importFrom leaflet leafletProxy
mod_plot_poly_leaf_server <- function(id, preplot_dta, shp_dta, leg_type = "value", ...){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    previous_plot <- reactiveVal(NULL)
    remove_old_poly <- reactiveVal(NULL)
    add_new_poly <- reactiveVal(NULL)
    selected_layer <- reactiveVal(NULL)
    
    # compare data and remove only some
    observeEvent(
      preplot_dta(), {
        if (isTruthy(previous_plot()) && isTruthy(preplot_dta())) {
          keep_vals <-
            intersect(names(preplot_dta()), names(previous_plot())) %>%
            keep(function(.x) {
              out <- FALSE
              new_dta <- preplot_dta()[[.x]]$pti_dta$pti_score
              old_dta <- previous_plot()[[.x]]$pti_dta$pti_score
              if (isTRUE(all.equal(new_dta, old_dta)))
                out <- TRUE
              out
            }) %>%
            unlist()
          
          # Compare legends
          leg_change <- 
            keep_vals %>% 
            map_lgl(~{
              all.equal(
                preplot_dta()[[.x]]$leg$our_labels,  
                previous_plot()[[.x]]$leg$our_labels
                ) %>% 
                isTRUE()
            })
          
          keep_vals <- keep_vals[leg_change]
          
          previous_plot()[setdiff(names(previous_plot()), keep_vals)] %>% remove_old_poly()
          preplot_dta()[setdiff(names(preplot_dta()), keep_vals)] %>% add_new_poly()
          
        } else if (isTruthy(previous_plot()) && !isTruthy(preplot_dta())) {
          previous_plot() %>% remove_old_poly()
          add_new_poly(NULL)
          
        } else if (!isTruthy(previous_plot()) && isTruthy(preplot_dta())) {
          remove_old_poly(NULL)
          preplot_dta() %>% add_new_poly()
          
        } else {
          remove_old_poly(NULL)
          add_new_poly(NULL)
        }
        
      }, 
      ignoreInit = FALSE, 
      ignoreNULL = FALSE
    )
    
    observeEvent(#
      input[["leaf_id_groups"]], {
        selected_layer(input[["leaf_id_groups"]])
      }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    observeEvent(#
      preplot_dta(), {
        if (!isTruthy(preplot_dta())) {
          leaflet::leafletProxy("leaf_id", deferUntilFlush = TRUE) %>%
            clean_pti_polygons(remove_old_poly()) %>% 
            clean_pti_poly_controls(remove_old_poly()) 
          remove_old_poly(NULL)
          add_new_poly(NULL)
          previous_plot(NULL)
        } else {
          leaflet::leafletProxy("leaf_id", deferUntilFlush = TRUE) %>%
            clean_pti_polygons(remove_old_poly()) %>% 
            plot_pti_polygons(add_new_poly()) %>%  
            clean_pti_poly_controls(remove_old_poly()) %>% 
            add_pti_poly_controls(preplot_dta(), selected_layer()) 
          remove_old_poly(NULL)
          add_new_poly(NULL)
          previous_plot(preplot_dta())
        }
        
      }, ignoreInit = FALSE, ignoreNULL = FALSE)
    
    # Plotting the legend
    mod_plot_poly_legend_server(NULL, preplot_dta, selected_layer, leg_type = leg_type)
    
    # returning selected layer
    out <- mod_plot_leaf_export(NULL, shp_dta, preplot_dta, selected_layer)
    
    out
  })
}


#' @describeIn mod_plot_poly_leaf_server complementing module ment to reproduce the map and return a simple leaflet object
#' 
#' @export
#' @importFrom shiny moduleServer observeEvent
mod_plot_leaf_export <-
  function(id, shp_dta, preplot_dta, selected_layer, ...) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      leaf_out <- reactiveVal(NULL)
      
      observeEvent(#
        shp_dta(), {
          req(shp_dta())
          # make_gg_line_map(shp_dta()) %>% leaf_out()
          
          list(
            poly = FALSE,
            shp_dta = shp_dta()
          ) %>% 
            leaf_out()
          
          
        }, ignoreNULL = FALSE, ignoreInit = FALSE)
      
      observeEvent(#
        list(preplot_dta(), selected_layer()), {
          req(preplot_dta())
          req(selected_layer())
          # browser()
          
          list(
            poly = TRUE,
            preplot_dta = preplot_dta(), 
            selected_layer = selected_layer(),
            show_interval = str_detect(ns(""), "explor")
           ) %>% 
            leaf_out()
          
          # withProgress({
          #   try({
          #     map_file <- tempfile(fileext = ".png")
          #     
          #  
          #     
          #     map_file %>% leaf_out()
          #     
          #   }, silent = T)
          #   
          # },
          # min = 0,
          # value = 0.1,
          # message = "Rendering the map as an image.")
         

        }, ignoreNULL = FALSE, ignoreInit = FALSE)
      
      leaf_out
    })
  }




#' @describeIn mod_plot_poly_leaf_server Plot the map of country using GG and knowing the layer to plot.  
#' 
#' @import ggplot2 sf
#' @export
make_ggmap <- function(preplot_dta, selected_layer, show_interval = FALSE, ...) {
  
  map_to_plot <-
    preplot_dta %>%
    purrr::keep(function(.x) {
      str_c(.x$pti_codes, " (", .x$admin_level, ")") %in% selected_layer[[1]]
    }) %>%
    `[[`(1)
  
  layer_id <-
    str_c(map_to_plot$pti_codes, " (", map_to_plot$admin_level, ")")
  
  if (show_interval) {
    # browser()
    plt_dta <- 
      map_to_plot$pti_dta %>%
      mutate(
        pti_score_category = map_to_plot$leg$recode_function_intervals(pti_score),
        pti_score_category = factor(pti_score_category, 
                                    levels = map_to_plot$leg$recode_function_intervals(map_to_plot$leg$our_values))
      )
    
    col_list <- map_to_plot$leg$pal(map_to_plot$leg$our_values)
  } else {
    plt_dta <- 
      map_to_plot$pti_dta %>%
      mutate(
        pti_score_category = map_to_plot$leg$recode_function(pti_score),
        pti_score_category = factor(pti_score_category, 
                                    levels = map_to_plot$leg$our_labels_category)
      ) 
    col_list <- set_names(
      map_to_plot$leg$pal(map_to_plot$leg$our_values),
      map_to_plot$leg$our_labels_category
    )
  }
  # browser()
  plt_dta %>%
    ggplot2::ggplot() +
    ggplot2::aes(fill = pti_score_category) +
    # ggspatial::annotation_map_tile(zoomin = 0, progress = "none", interpolate = FALSE) +
    ggplot2::geom_sf() +
    ggplot2::coord_sf(crs = sf::st_crs(plt_dta), datum = sf::st_crs(plt_dta)) +
    ggplot2::scale_fill_manual(values = col_list) +
    ggplot2::labs(fill = layer_id) +
    ggplot2::theme_bw()
  
}




#' @describeIn mod_plot_poly_leaf_server Plot the map using SP pacakge
#' 
#' @import ggplot2 sf sp
#' @export
make_spplot <- function(preplot_dta, selected_layer, show_interval = FALSE, ...) {
  
  map_to_plot <-
    preplot_dta %>%
    purrr::keep(function(.x) {
      str_c(.x$pti_codes, " (", .x$admin_level, ")") %in% selected_layer[[1]]
    }) %>%
    `[[`(1)
  
  layer_id <-
    str_c(map_to_plot$pti_codes, " (", map_to_plot$admin_level, ")")
  
  if (show_interval) {
    # browser()
    plt_dta <- 
      map_to_plot$pti_dta %>%
      mutate(
        pti_score_category = map_to_plot$leg$recode_function_intervals(pti_score),
        pti_score_category = factor(pti_score_category, 
                                    levels = map_to_plot$leg$recode_function_intervals(map_to_plot$leg$our_values))
      )
    
    col_list <- map_to_plot$leg$pal(map_to_plot$leg$our_values)
  } else {
    plt_dta <- 
      map_to_plot$pti_dta %>%
      mutate(
        pti_score_category = map_to_plot$leg$recode_function(pti_score),
        pti_score_category = factor(pti_score_category, 
                                    levels = map_to_plot$leg$our_labels_category)
      ) 
    col_list <- set_names(
      map_to_plot$leg$pal(map_to_plot$leg$our_values),
      map_to_plot$leg$our_labels_category
    )
  }
  
  plt_dta %>%
    select(pti_score_category) %>%
    sf::as_Spatial() %>%
    sp::spplot(col.regions = map_to_plot$leg$pal(map_to_plot$leg$our_values))
}


#' @describeIn mod_plot_poly_leaf_server Plot the map of country using GG and knowing the layer to plot.  
#' @import ggplot2 sf
#' @export
make_gg_line_map <- function(shp_dta, ...) {
  # browser()
  dta <- 
    shp_dta %>%
    `[`(-length(.)) %>% 
    list(.x = ., .y = names(.), .z = rev(seq_along(.)) / max(seq_along(.))) %>% 
    pmap_dfr(function(...) {..1 %>% mutate(line = ..2, width = ..3)}) 
  dta %>% 
    ggplot2::ggplot() +
    ggplot2::aes(group = line, linetype = line, colour = line, size = width) +
    # ggspatial::annotation_map_tile(zoomin = 0, progress = "none", interpolate = TRUE) +
    ggplot2::geom_sf(fill = NA) +
    ggplot2::coord_sf(crs = sf::st_crs(dta), datum = sf::st_crs(dta)) +
    ggplot2::scale_colour_brewer(palette = "Dark2") + 
    ggplot2::scale_size_continuous(range = c(0.15, 1.25)) +
    ggplot2::theme_bw()  + 
    ggplot2::theme(legend.position="none")
  
}

#' @describeIn mod_plot_poly_leaf_server Plot the line of the map using SP pacakge
#' 
#' @import ggplot2 sf sp
#' @export
make_sp_line_map <- function(shp_dta, ...) {
  cols <- c("#e41a1c",
            "#377eb8",
            "#4daf4a",
            "#984ea3",
            "#ff7f00",
            "#ffff33",
            "#a65628")
  out_plt <- 
    shp_dta %>%
    `[`(-length(.)) %>% 
    `[[`(length(.)) %>% 
    sf::st_geometry() %>%
    sf::st_as_sf() %>%
    mutate(ID = row_number()) %>%
    # plot()
    # sf::st_cast("MULTILINESTRING") %>%
    sf::as_Spatial() %>%
    sp::spplot(fill = NULL, col = cols[[2]], border = cols[[2]])
  
  out_plt$legend <- NULL
  out_plt
  
}

    
## To be copied in the UI
# mod_plot_poly_leaf_server_ui("plot_poly_leaf_server_ui_1")
    
## To be copied in the server
# mod_plot_poly_leaf_server_server("plot_poly_leaf_server_ui_1")
