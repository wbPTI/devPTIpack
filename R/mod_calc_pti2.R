#' UI placeholder for the PTI calculation module
#'
#' Returns an empty `tagList()`. The calculation module has no UI of its
#' own; this placeholder exists so the UI side of the
#' [mod_calc_pti2_server()] pair can be referenced symmetrically by
#' callers that build their UI declaratively.
#'
#' @param id Character. Shiny module namespace ID.
#'
#' @return An empty `shiny::tagList()`.
#'
#' @importFrom shiny NS tagList
#' @noRd
mod_calc_pti2_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' Reactive PTI calculation engine
#'
#' Wires the full PTI calculation pipeline (pivot, weight, standardise,
#' expand across admin levels, merge, aggregate, label, restructure) as a
#' chain of reactives that recompute when the weights, metadata, or shapes
#' change. The non-reactive equivalent is [run_pti_pipeline()] -- this
#' module is the Shiny adapter that surfaces it inside a session.
#'
#' Notifies the user via `shiny::showNotification()` once a non-empty PTI
#' result is available.
#'
#' @param id Character. Shiny module namespace ID.
#' @param shp_dta Reactive yielding a named list of `sf` tibbles, one per
#'   admin level.
#' @param input_dta Reactive yielding the metadata list as returned by
#'   [fct_template_reader()] (currently unused -- the module derives
#'   metadata from `wt_dta()`; retained for forward compatibility).
#' @param wt_dta Reactive yielding a named list with elements
#'   `weights_clean`, `indicators_list`, and the per-admin metadata
#'   tibbles (typically the output of the weights-input module).
#'
#' @return A `reactive()` yielding the structured PTI list (one element
#'   per admin level; each element a list with `pti_data`, `pti_codes`,
#'   and `admin_level`).
#'
#' @importFrom shiny moduleServer reactiveVal observeEvent eventReactive
#'   reactive req observe showNotification isTruthy
#' @importFrom stringr str_detect
#' @importFrom purrr imap
#' @noRd
mod_calc_pti2_server <- function(id, shp_dta, input_dta, wt_dta){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    wt_dta_local <- reactiveVal(NULL)
    observeEvent(wt_dta(), {
      new_val <- wt_dta() %>% `[`(names(.) %>% str_detect("admin|indicators_list|weights_clean"))
      if (isTruthy(wt_dta_local())) {
        if (!identical(new_val, wt_dta_local())) {
          wt_dta_local(new_val)}
      } else {
        wt_dta_local(new_val)
      }
    })

    long_vars <- eventReactive(wt_dta_local(), {
      wt_dta_local() %>% pivot_pti_dta((.)$indicators_list)
      })

    existing_shapes <- eventReactive(shp_dta(), { shp_dta() %>% clean_geoms() })

    mt <- eventReactive(shp_dta(), { shp_dta() %>% get_mt() })


    calc_pti <-
      shiny::eventReactive(
        wt_dta_local()$weights_clean,
        {
          wt_dta_local()$weights_clean %>%
            get_weighted_data(long_vars(), indicators_list = wt_dta_local()$indicators_list) %>%
            get_scores_data() %>%
            imap(~ expand_adm_levels(.x, mt()) %>%
                   merge_expandedn_adm_levels()) %>%
            agg_pti_scores(existing_shapes()) %>%
            label_generic_pti() %>%
            structure_pti_data(shp_dta())

        }, ignoreNULL = FALSE)


    shiny::observe({
      shiny::req(length(calc_pti()) > 0)
      shiny::showNotification("PTIs are calculated and extrapolated",
                              type = "message", duration = 5)
    })

    calc_pti
  })
}
