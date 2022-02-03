#' mod_plot_poly_legend_server implements logic of dynamically plotting legend of the selected layer/layers
#'
#' @export
#' @importFrom shiny observeEvent
#' @importFrom leaflet leafletProxy
mod_plot_poly_legend_server <- function(id, map_dta, selected_layer){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    old_layer <- reactiveVal(NULL)
    
    observeEvent(
      selected_layer(), {
        
        # Removing any old legend
        if (isTruthy(old_layer())) {
          
          leaflet::leafletProxy("leaf_id", deferUntilFlush = TRUE) %>%
            remove_pti_legend(map_dta(), old_layer())
          
          old_layer(NULL)
        }
        
        # Adding new legend to the map
        if (isTruthy(selected_layer())) {
          
          old_layer(selected_layer())
          
          leaflet::leafletProxy("leaf_id", deferUntilFlush = TRUE)  %>% 
            plot_pti_legend(map_dta(), selected_layer())
          
        }
        
      }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
  })
}

#' @describeIn mod_plot_poly_legend_server Function to plot Legend for the PTI polygons outside of shiny reactive environment
#' 
#' @export
#' @importFrom leaflet addLegend
#' @importFrom purrr reduce
#' @importFrom stringr str_c
plot_pti_legend <- function(leaf_map, map_dta, selected_layer) {
  if (isTruthy(selected_layer)) {
    leaf_map %>%
      list() %>%
      append({
        map_dta %>%
          purrr::keep(function(.x) {
            str_c(.x$pti_codes, " (", .x$admin_level, ")") %in% selected_layer
          })
      }) %>%
      purrr::reduce(function(.y, .x) {
        title <-  str_c(.x$pti_codes, " (", .x$admin_level, ")")
        layerId <-  str_c("LEGEND_", title)
        .y %>%
          leaflet::addLegend(
            position = "bottomleft",
            labels = .x$leg$our_labels,
            colors = .x$leg$pal(.x$leg$our_values),
            opacity = 1,
            title = title,
            layerId = layerId
          )
      })
  } else {
    leaf_map
  }
}


#' @describeIn mod_plot_poly_legend_server Function to plot Legend for the PTI polygons outside of shiny reactive environment
#' 
#' @export
#' @importFrom leaflet clearControls removeControl
remove_pti_legend <- function(leaf_map, map_dta, remove_layer) {
  if (isTruthy(remove_layer)) {
    leaf_map %>%
      clearControls()
    
      # list() %>%
      # append({
      #   map_dta %>%
      #     keep(function(.x) {
      #       str_c(.x$pti_codes, " (", .x$admin_level, ")") %in% remove_layer
      #     })
      # }) %>%
      # reduce(function(.y, .x) {
      #   title <-  str_c(.x$pti_codes, " (", .x$admin_level, ")")
      #   layerId <-  str_c("LEGEND_", title)
      #   .y %>% removeControl(layerId = layerId)
      # })
  } else {
    leaf_map
  }
}