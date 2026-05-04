#' Map container UI for PTI page modules
#'
#' Composes a Leaflet output and the [mod_leaf_side_panel_ui()] side panel
#' into a single positioned container. Used by [mod_ptipage_twocol_ui()],
#' [mod_ptipage_box_ui()], and [mod_pti_comparepage_ui()] to render the
#' map area within a PTI page layout.
#'
#' @param id Character. Shiny module namespace ID. Wired to the matching
#'   server-side module.
#' @param side_width Numeric. Side-panel width in pixels.
#' @param side_ui Tag list or NULL. Additional UI inserted at the top of
#'   the side panel (passed through to [mod_leaf_side_panel_ui()]).
#' @param map_dwnld_options Character vector of download options to expose
#'   in the side panel; passed to [mod_leaf_side_panel_ui()].
#' @param ... Further arguments forwarded to `leaflet::leafletOutput()`
#'   (e.g. `height`, `width`).
#'
#' @return A `shiny::tags$div` containing the leaflet output and the side
#'   panel, positioned for absolute-panel overlay.
#'
#' @importFrom shiny NS tagList tags
#' @importFrom leaflet leafletOutput
#' @export
#'
#' @examples
#' \dontrun{
#' # Inside a Shiny UI:
#' mod_map_pti_leaf_ui("page1", side_width = 350)
#' }
mod_map_pti_leaf_ui <- function(id, side_width = 350, side_ui = NULL,
                                map_dwnld_options = c("shapes", "metadata"),
                                ...){
  ns <- NS(id)

  tagList(
    leafletOutput(ns("leaf_id"), ...),
    mod_leaf_side_panel_ui(id, side_width, side_ui, map_dwnld_options = map_dwnld_options)
  ) %>%
    tags$div(
      style = "position:relative;"
    )

}
