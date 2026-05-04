#' Reactive legend manager for PTI polygons
#'
#' Server module that adds, replaces, and clears the Leaflet legend for
#' the currently selected PTI layer. Tracks the previously shown layer
#' in a `reactiveVal()` so a layer change first removes the old legend
#' before adding the new one. Delegates the actual draw and clear to
#' [plot_pti_legend()] and [remove_pti_legend()].
#'
#' @param id Character. Shiny module namespace ID.
#' @param map_dta Reactive yielding the structured PTI list (one element
#'   per admin level; each element a list with `pti_codes`, `admin_level`,
#'   and a `leg` palette / labels block).
#' @param selected_layer Reactive character giving the currently selected
#'   layer label (typically `"<pti_codes> (<admin_level>)"`).
#' @param leg_type One of `"priority"` (categorical priority labels) or
#'   `"value"` (numeric value labels). Forwarded to [plot_pti_legend()].
#'
#' @return No explicit return value. Called for side effects (mutates
#'   the leaflet output via `leaflet::leafletProxy()`).
#'
#' @importFrom shiny moduleServer observeEvent reactiveVal isTruthy
#' @importFrom leaflet leafletProxy
#' @noRd
mod_plot_poly_legend_server <- function(id, map_dta, selected_layer, leg_type = "priority"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    old_layer <- reactiveVal(NULL)

    observeEvent(
      list(selected_layer(), map_dta()), {

        if (isTruthy(old_layer())) {

          leaflet::leafletProxy("leaf_id", deferUntilFlush = TRUE) %>%
            remove_pti_legend(map_dta(), old_layer())

          old_layer(NULL)
        }

        if (isTruthy(selected_layer())) {

          old_layer(selected_layer())

          leaflet::leafletProxy("leaf_id", deferUntilFlush = TRUE)  %>%
            plot_pti_legend(., map_dta(), selected_layer(), leg_type = leg_type)

        }

      }, ignoreNULL = FALSE, ignoreInit = FALSE)

  })
}

#' Add a PTI legend to a Leaflet map outside the reactive environment
#'
#' Pure (non-reactive) function that appends a `leaflet::addLegend()`
#' control for each layer in `map_dta` whose `<pti_codes> (<admin_level>)`
#' label is in `selected_layer`. Each legend gets its own layerId so it
#' can be selectively removed later.
#'
#' @param leaf_map A [leaflet::leaflet()] map or proxy.
#' @param map_dta The structured PTI list (one element per admin level).
#' @param selected_layer Character vector of layer labels to draw legends
#'   for.
#' @param leg_type One of `"priority"` (categorical) or `"value"`
#'   (numeric). Selects which label set in `map_dta[[i]]$leg` to use.
#'
#' @return The mutated `leaf_map` with one legend per `selected_layer`,
#'   or `leaf_map` unchanged when `selected_layer` is empty.
#'
#' @importFrom leaflet addLegend
#' @importFrom purrr reduce keep
#' @importFrom stringr str_c
#' @importFrom shiny isTruthy
#' @noRd
plot_pti_legend <- function(leaf_map, map_dta, selected_layer, leg_type = "priority") {
  if (isTruthy(selected_layer)) {

    leg_type_2 <-
      switch (leg_type,
              priority = "our_labels_category",
              value = "our_labels")

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
            labels = .x$leg[[leg_type_2]],
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


#' Clear all PTI legends from a Leaflet map
#'
#' Companion to [plot_pti_legend()]. When `remove_layer` is truthy,
#' clears every legend control on the map (`leaflet::clearControls()`);
#' otherwise returns `leaf_map` unchanged. The per-layerId removal
#' alternative is preserved as commented code in the source for future
#' refinement.
#'
#' @param leaf_map A [leaflet::leaflet()] map or proxy.
#' @param map_dta The structured PTI list. Currently unused (the
#'   `clearControls()` path does not need it); retained for symmetry
#'   with [plot_pti_legend()] and the per-layerId removal alternative.
#' @param remove_layer Character vector of layer labels. Treated as a
#'   truthiness gate -- non-empty triggers a clear.
#'
#' @return The mutated `leaf_map` with controls cleared, or `leaf_map`
#'   unchanged.
#'
#' @importFrom leaflet clearControls removeControl
#' @importFrom shiny isTruthy
#' @noRd
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
