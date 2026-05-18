#' Initialise the Leaflet base map and fly to the shape bounds
#'
#' Server module that builds the initial Leaflet output: starts a fresh
#' [leaflet::leaflet()], overlays the admin-boundary lines via
#' [plot_leaf_line_map2()], and stores the result in a `reactiveVal()`
#' that downstream polygon-rendering modules read on first paint. The
#' module also forces the leaflet output to render even when the
#' containing tab is currently hidden, so panning / zooming works
#' immediately on first reveal.
#'
#' @param id Character. Shiny module namespace ID.
#' @param shp_dta Reactive yielding a named list of `sf` tibbles (one per
#'   admin level). The map is re-initialised whenever this changes.
#' @param ... Unused; retained for forward compatibility.
#'
#' @return A `reactiveVal()` yielding the initialised Leaflet map. Other
#'   modules (e.g. [mod_plot_poly_leaf_server()]) chain on this to add
#'   PTI polygons.
#'
#' @importFrom shiny moduleServer observeEvent reactiveVal
#' @importFrom leaflet leaflet renderLeaflet
#' @noRd
mod_plot_init_leaf_server <- function(id, shp_dta, ...){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    leaf_to_init <- reactiveVal()

    observeEvent(
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


#' Draw admin-boundary polylines on a Leaflet map
#'
#' Adds dashed polylines for each admin level in `shps_dta` (skipping the
#' last / finest level, which is rendered as filled polygons by the
#' polygon module). Sets up the panes (`basetile` / `liene1` / `polygons`
#' / `bubles` / `points`) so subsequent polygon and label layers can be
#' z-ordered correctly. Fits the viewport to the bounding box of the
#' first (coarsest) shown level.
#'
#' @param leaf_map A [leaflet::leaflet()] map or a leaflet proxy.
#' @param shps_dta Named list of `sf` tibbles -- the same shape structure
#'   passed to [mod_plot_init_leaf_server()].
#' @param show_adm_levels Character vector or NULL. Admin levels to draw
#'   as polylines. `NULL` means all levels except the finest.
#' @param ... Unused; retained for forward compatibility.
#'
#' @return The mutated `leaf_map`, ready for downstream polygon /
#'   legend layers.
#'
#' @importFrom leaflet fitBounds addProviderTiles addMapPane addPolylines
#'   pathOptions providers addTiles
#' @importFrom sf st_bbox
#' @noRd
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

    leaf_map %>%
      leaflet::fitBounds(shp_bounds[[1]], shp_bounds[[2]], shp_bounds[[3]], shp_bounds[[4]]) %>%
      leaflet::addMapPane("basetile", zIndex = 400) %>%
      leaflet::addMapPane("liene1", zIndex = 401) %>%
      leaflet::addMapPane("polygons", zIndex = 410) %>%
      leaflet::addMapPane("bubles", zIndex = 430)  %>%
      leaflet::addMapPane("points", zIndex = 440) %>%
      addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/gsdpm/cjrc1z9u53oci2tqxvbhhnf7r/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiZ3NkcG0iLCJhIjoiY2toZjFvZ3gwMG1qODJ4cnpwaDdvenpzMiJ9.01pv2kccL9cXhxO6B-Naiw") %>%
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
  }
