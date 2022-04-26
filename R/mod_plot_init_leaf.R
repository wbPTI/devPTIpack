#' mod_plot_init_leaf_server module to initialize and fly to the leaflet map
#'
#' @param leaf_map either a `leaflet()` or a leaflet proxy object. 
#' 
#' @export
#' @importFrom shiny moduleServer observeEvent reactiveVal
#' @importFrom leaflet leaflet renderLeaflet
mod_plot_init_leaf_server <- function(id, shp_dta, ...){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    leaf_to_init <- reactiveVal()
    
    observeEvent(#
        shp_dta(),
        {
          leaflet() %>% 
            plot_leaf_line_map2(shp_dta(), get_golem_options("show_adm_levels")) %>% 
            leaf_to_init()
        },
        ignoreInit = FALSE,
        ignoreNULL = FALSE, 
        priority = 100)
    
    output[["leaf_id"]] <- leaflet::renderLeaflet({
      leaf_to_init()
    })
    
    outputOptions(output, "leaf_id", suspendWhenHidden = FALSE, priority = 1000)
    
    leaf_to_init
  })
}


#' @describeIn mod_plot_init_leaf_server Returns a leaflet or a proxy that has a line plot of administrative boundaries initialized
#' 
#' @param show_adm_levels character vector with the ids of the admin levels which could be plotted
#' 
#' @export
#' @importFrom leaflet  fitBounds addProviderTiles addMapPane addPolylines pathOptions providers
#' @importFrom sf st_bbox
plot_leaf_line_map2 <-
  function(leaf_map,
           shps_dta,
           show_adm_levels = NULL, 
           ...) {
    if (is.null(show_adm_levels)) {
      show_shps <- 
        shps_dta %>% 
        `[`(-length(.))
    } else {
      show_shps <-
        shps_dta %>%
        `[`(get_adm_levels(.) %in% show_adm_levels) %>% 
        `[`(-length(.))
    }
    
    shp_bounds <- sf::st_bbox(show_shps[[1]])
    line_dash <- c("1", "10 10", "5 5", "1 1", "0.5 0.5", "1") %>% rep(5)
    line_size <- c(4, 2, rep(1, 100))
    
    # cat(Sys.time(), " start lines \n")
    leaf_map %>%
      leaflet::fitBounds(shp_bounds[[1]], shp_bounds[[2]], shp_bounds[[3]], shp_bounds[[4]]) %>%
      leaflet::addMapPane("basetile", zIndex = 400) %>%
      leaflet::addMapPane("liene1", zIndex = 401) %>%
      leaflet::addMapPane("polygons", zIndex = 410) %>%
      leaflet::addMapPane("bubles", zIndex = 430)  %>%
      leaflet::addMapPane("points", zIndex = 440) %>%
      leaflet::addProviderTiles(
        provider = leaflet::providers$CartoDB.Voyager,
        options = pathOptions(pane = "basetile")
      ) %>%
      list() %>%
      append(show_shps) %>%
      reduce2(seq_along(show_shps), function(x, y, i) {
        x %>%
          leaflet::addPolylines(
            data = y,
            color = "darkgrey",
            dashArray = line_dash[[i]],
            stroke = TRUE,
            weight = line_size[[i]],
            options = leaflet::pathOptions(pane = "liene1")
          )
      })
    # cat(Sys.time(), " end lines \n")
  }
