#' Two-column UI for a PTI page
#'
#' Builds a [shiny::bootstrapPage()] that splits the viewport into a
#' weights-input column and a map column. Used by single-page apps and
#' multi-tab apps where each PTI scheme gets its own tab. The split width
#' is configurable via `cols`; the inner UIs are wired to the matching
#' [mod_ptipage_newsrv()] module via the shared `id`.
#'
#' @param id Character. Shiny module namespace ID. Must match the `id`
#'   passed to [mod_ptipage_newsrv()]. The weights-input column is
#'   rendered via `mod_wt_inp_ui()`.
#' @param cols Integer vector of length 2 summing to <= 12 -- column widths
#'   (Bootstrap grid units) for the weights-input column and the map
#'   column.
#' @param full_ui Logical. If `TRUE`, the full weights-input UI is shown;
#'   otherwise the compact variant is used. Forwarded to
#'   [mod_wt_inp_ui()].
#' @param show_waiter Logical. If `TRUE`, wraps the page in a waiter /
#'   spinner overlay shown until the first PTI calculation completes.
#' @param map_height,wt_height CSS height values for the map and weights
#'   columns respectively (e.g. `"calc(95vh - 60px)"`).
#' @param dt_style,wt_style Character or NULL. Additional CSS applied to
#'   the weights `DT::datatable` and to the surrounding weights container.
#' @param wt_dwnld_options,map_dwnld_options Character vector of download
#'   options exposed in the weights side and on the map respectively. Any
#'   subset of `c("data", "weights", "shapes", "metadata")`. `NULL` or
#'   empty means no download options.
#' @param side_width Numeric. Width in pixels of the side panel in the
#'   box variant ([mod_ptipage_box_ui()]).
#' @param ... Additional arguments forwarded to `mod_wt_inp_ui()`.
#'
#' @return A `shiny::tagList` / `shiny::bootstrapPage` wrapping the page
#'   layout, optionally wrapped by a waiter overlay.
#'
#' @importFrom shiny NS tagList column div fillPage bootstrapPage
#' @family shiny-modules
#' @export
#'
#' @examples
#' \dontrun{
#' mod_ptipage_twocol_ui("page_pti", cols = c(4, 8))
#' }
mod_ptipage_twocol_ui <- function(id,
                                  cols = c(4,8),
                                  full_ui = FALSE,
                                  show_waiter = FALSE,
                                  map_height = "calc(99vh)",
                                  wt_height = "inherit",
                                  dt_style = "zoom:0.95; height: calc(95vh - 250px);",
                                  wt_style = NULL,
                                  wt_dwnld_options = c("data", "weights", "shapes", "metadata"),
                                  map_dwnld_options = c("shapes", "metadata"),
                                  ...) {
  ns <- NS(id)

  shiny::bootstrapPage(
    shinyjs::useShinyjs(),
    golem_add_external_resources(),
    div(
      shiny::column(
        cols[[1]],
        class = "order-sm-last",
        style = "padding-right: 5px; padding-left: 5px;",
        mod_wt_inp_ui(
          ns(NULL),
          full_ui = FALSE,
          height = wt_height,
          dt_style = dt_style,
          wt_style = wt_style,
          wt_dwnld_options = wt_dwnld_options,
          ...
        )
      ),
      shiny::column(
        cols[[2]],
        class = "order-sm-first",
        style = "padding-right: 0px; padding-left: 0px;",
        mod_map_pti_leaf_ui(
          ns(NULL),
          height = map_height,
          map_dwnld_options = map_dwnld_options
        ) %>%
          div(id = "step_8_map_inspection1")
      )
    )
  ) %>%
    mod_waiter_ui(., ns(NULL), show_waiter = show_waiter)
}

#' @describeIn mod_ptipage_twocol_ui PTI page where the weights input sits
#'   inside an absolute side panel overlaid on a full-viewport map. Used
#'   when a single PTI is the only thing on screen.
#'
#' @family shiny-modules
#' @export
mod_ptipage_box_ui <- function(id,
                               full_ui = FALSE,
                               show_waiter = FALSE,
                               map_height = "calc(100vh)",
                               wt_height = "inherit",
                               dt_style = "zoom:0.9; height: calc(35vh);",
                               wt_style = "zoom:0.9;",
                               side_width = 450,
                               wt_dwnld_options = c("data", "weights", "shapes", "metadata"),
                               map_dwnld_options = NULL,
                               ...) {
  ns <- NS(id)

  wt_ui <-
    mod_wt_inp_ui(
      ns(NULL),
      full_ui = FALSE,
      height = wt_height,
      dt_style = dt_style,
      wt_style = wt_style,
      wt_dwnld_options = wt_dwnld_options,
      ...
      )

  shiny::fillPage(
    shinyjs::useShinyjs(),
    golem_add_external_resources(),
    shiny::div(mod_map_pti_leaf_ui(
      ns(NULL),
      height = map_height,
      side_ui = wt_ui,
      side_width = side_width,
      map_dwnld_options = map_dwnld_options
    )) %>%
      mod_waiter_ui(., ns(NULL), show_waiter = show_waiter)
  )
}


#' Page-level PTI server module
#'
#' Orchestrates the weights-input, PTI calculation, and map-visualisation
#' sub-modules for a single PTI page (one tab in a multi-tab app, or the
#' whole app in a single-page deployment). Wires their reactives together
#' and returns the joined results so a sibling page (e.g. a comparison
#' tab) can read the same inputs and outputs.
#'
#' @param id Character. Shiny module namespace ID. Must match the `id`
#'   passed to [mod_ptipage_twocol_ui()] or [mod_ptipage_box_ui()].
#' @param inp_dta Reactive yielding the metadata list (output of
#'   [fct_template_reader()]).
#' @param shp_dta Reactive yielding a named list of `sf` tibbles, one
#'   per admin level.
#' @param active_tab Reactive character indicating the currently selected
#'   tab. Used by the deferred-render guard so the map only renders on
#'   first open of a `target_tabs` tab.
#' @param target_tabs Character vector of tab names this module should
#'   render for.
#' @param default_adm_level Character or NULL. Default admin level shown
#'   in the side-panel selector.
#' @param show_adm_levels Character vector or NULL. Admin levels exposed
#'   in the selector. `NULL` shows all levels with data; a single value
#'   pins to that level (falling back to the most disaggregated level
#'   that has data); a vector restricts to the listed levels.
#' @param shapes_path,mtdtpdf_path Character. Filesystem paths to the
#'   shapes archive and metadata PDF served via download links.
#' @param show_waiter Logical. Whether to render the waiter overlay
#'   wrapping the page.
#' @param ... Additional arguments forwarded to nested sub-modules.
#'
#' @return A `shiny::reactiveValues` object with elements `plotted_dta`,
#'   `map_dta`, and `wt_dta`, allowing sibling modules to consume the
#'   same outputs.
#'
#' @importFrom shiny moduleServer reactive reactiveValues observe req
#'   isTruthy
#' @family shiny-modules
#' @export
#'
#' @examples
#' \dontrun{
#' mod_ptipage_newsrv(
#'   "page_pti",
#'   inp_dta     = inp_dta,
#'   shp_dta     = shp_dta,
#'   active_tab  = active_tab,
#'   target_tabs = "PTI"
#' )
#' }
mod_ptipage_newsrv <- function(id,
                               inp_dta = reactive(NULL),
                               shp_dta = reactive(NULL),
                               active_tab = reactive(NULL),
                               target_tabs = NULL,
                               default_adm_level = NULL,
                               show_adm_levels = NULL,
                               shapes_path = "",
                               mtdtpdf_path = "",
                               show_waiter = FALSE,
                               ...){

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    first_open <- mod_tab_open_first_newserv(id, active_tab, target_tabs)

    mod_waiter_newsrv(NULL, show_waiter = show_waiter, tab_opened = first_open,
      hide_invalidator = reactive({req(isTruthy(wt_dta()$curr_wt$weight))}))

    wt_dta <- mod_wt_inp_server(NULL,
                                input_dta = inp_dta,
                                plotted_dta = reactive(plotted_dta()$pre_map_dta()),
                                shapes_path = shapes_path,
                                mtdtpdf_path = mtdtpdf_path)



    observe({
      req(golem::get_golem_options("diagnostics"))
      req(wt_dta()$weights_clean)
      cat("PTI: start calc ", as.character(Sys.time()), "\n" )
    })

    map_dta <-
      mod_calc_pti2_server(NULL,
                           shp_dta = shp_dta,
                           input_dta = inp_dta,
                           wt_dta = wt_dta)

    observe({
      req(golem::get_golem_options("diagnostics"))
      req(length(map_dta()) > 0)
      cat("PTI: End calc ", as.character(Sys.time()), "\n" )
    })

    plotted_dta <-
      mod_plot_pti2_srv(NULL,
                        shp_dta = shp_dta,
                        map_dta = map_dta,
                        wt_dta =  wt_dta,
                        active_tab = active_tab,
                        target_tabs = target_tabs,
                        metadata_path = mtdtpdf_path,
                        shapes_path = shapes_path,
                        default_adm_level = default_adm_level,
                        show_adm_levels = show_adm_levels)

    observe({
      req(golem::get_golem_options("diagnostics"))
      cat("PTI: End plot data to export ", as.character(Sys.time()), "\n" )
    })

    reactiveValues(
      plotted_dta = plotted_dta,
      map_dta = map_dta,
      wt_dta = wt_dta
    )

  })
}
