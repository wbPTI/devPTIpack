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
