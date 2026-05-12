#' Comparison-page UI for two side-by-side PTI maps
#'
#' Returns a two-column [shiny::fluidRow()] in which each column hosts a
#' [mod_map_pti_leaf_ui()] container, allowing the user to compare two
#' weighting schemes (or admin-level renderings) on adjacent maps. Each
#' map gets its own namespace (`first_leaf` / `second_leaf`) so the two
#' instances are addressable independently from the server side.
#'
#' @param id Character. Shiny module namespace ID. Wired to the matching
#'   [mod_pti_comparepage_newsrv()] call.
#'
#' @return A `shiny::bootstrapPage()` containing the two-column compare
#'   layout.
#'
#' @importFrom shiny NS tagList bootstrapPage column fluidRow div
#' @family shiny-modules
#' @export
#'
#' @examples
#' \dontrun{
#' mod_pti_comparepage_ui("compare")
#' }
mod_pti_comparepage_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      6,
      mod_map_pti_leaf_ui(ns("first_leaf"), height = "calc(100vh - 60px)", width = "100%") %>%
        div(id = "compare_left_1") %>%
        div(id = "compare_left_2"),
      style = "padding-right: 3px; padding-left: 0px;"
    ),
    column(
      6,
      mod_map_pti_leaf_ui(ns("second_leaf"), height = "calc(100vh - 60px)", width = "100%") %>%
        div(id = "compare_right_1") %>%
        div(id = "compare_right_2"),
      style = "padding-right: 0px; padding-left: 3px;"
    )
  ) %>%
    fluidRow() %>%
    bootstrapPage()
}

#' Comparison-page server module
#'
#' Server module powering the comparison page. Instantiates two
#' [mod_plot_pti2_srv()] sub-modules (`first_leaf` / `second_leaf`)
#' against the same shapes, structured PTI data, and weights. The two
#' maps share inputs but render independently so the user can pan / zoom
#' each side separately while comparing the same underlying weighting.
#'
#' @param id Character. Shiny module namespace ID. Must match the `id`
#'   passed to [mod_pti_comparepage_ui()].
#' @param shp_dta Reactive yielding a named list of `sf` tibbles (one
#'   per admin level). Forwarded to both [mod_plot_pti2_srv()] children.
#' @param map_dta Reactive yielding the structured PTI list (output of
#'   [mod_ptipage_newsrv()]).
#' @param wt_dta Reactive yielding the weights list (output of the
#'   weights-input module).
#' @param active_tab Reactive character indicating the currently selected
#'   tab.
#' @param target_tabs Character vector of tab names this module should
#'   render for.
#' @param mtdtpdf_path Character. Filesystem path to the metadata PDF
#'   served via the side-panel download link in each map.
#' @param shapes_path Character. Filesystem path to the shapes archive
#'   served via the side-panel download link in each map.
#' @param show_adm_levels Character vector or NULL. Admin levels to
#'   expose in the side-panel selector of each map.
#' @param ... Forwarded to both [mod_plot_pti2_srv()] children.
#'
#' @return No explicit return value. Called for side effects (renders
#'   two side-by-side leaflet maps within the Shiny session).
#'
#' @importFrom shiny moduleServer
#' @family shiny-modules
#' @export
#'
#' @examples
#' \dontrun{
#' mod_pti_comparepage_newsrv(
#'   "compare",
#'   shp_dta     = shp_dta,
#'   map_dta     = map_dta,
#'   wt_dta      = wt_dta,
#'   active_tab  = active_tab,
#'   target_tabs = "Compare PTIs"
#' )
#' }
mod_pti_comparepage_newsrv <-
  function(id,
           shp_dta,
           map_dta,
           wt_dta,
           active_tab,
           target_tabs,
           mtdtpdf_path = ".",
           shapes_path = ".",
           show_adm_levels = NULL,
           ...){

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_plot_pti2_srv("first_leaf",
                      shp_dta = shp_dta,
                      map_dta = map_dta,
                      wt_dta = wt_dta,
                      active_tab = active_tab,
                      target_tabs = target_tabs,
                      metadata_path = mtdtpdf_path,
                      shapes_path = shapes_path,
                      show_adm_levels =  show_adm_levels,
                      ...)

    mod_plot_pti2_srv("second_leaf",
                      shp_dta = shp_dta,
                      map_dta = map_dta,
                      wt_dta = wt_dta,
                      active_tab = active_tab,
                      target_tabs = target_tabs,
                      metadata_path = mtdtpdf_path,
                      shapes_path = shapes_path,
                      show_adm_levels =  show_adm_levels,
                      ...)
  })
}
