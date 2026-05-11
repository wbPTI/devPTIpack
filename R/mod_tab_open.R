#' Tab-opening invalidator for custom PTI page layouts
#'
#' Returns a reactive that emits a fresh `Sys.time()` timestamp the
#' first time the user opens any tab whose name appears in
#' `target_tabs`. Subsequent visits to the same tab do not re-trigger
#' the invalidator. Used by page modules to defer expensive rendering
#' (e.g. the `mod_pti_comparepage_newsrv` map pair) until the tab is
#' actually shown for the first time.
#'
#' When both `active_tab()` and `target_tabs` are not truthy, the
#' invalidator fires once on init -- this covers the single-tab
#' (`launch_pti_onepage`) layout where there is no `navbarPage`
#' selection at all.
#'
#' @param id Character. Shiny module namespace ID.
#' @param active_tab Reactive returning the currently selected tab
#'   name (typically `reactive(input$tabpan)`). May yield `NULL` for
#'   single-tab layouts.
#' @param target_tabs Character vector of tab names whose first-open
#'   event should invalidate. May be `NULL` for single-tab layouts.
#' @param ... Currently unused; reserved for future extensions.
#'
#' @return A reactive returning either `NULL` (before any qualifying
#'   tab open) or a `POSIXct` timestamp updated on each first open.
#'
#' @importFrom shiny moduleServer reactiveVal observeEvent isTruthy
#' @family shiny-modules
#' @export
#'
#' @examples
#' \dontrun{
#' # In a server function:
#' first_open <- mod_tab_open_first_newserv(
#'   id          = "first_open",
#'   active_tab  = reactive(input$tabpan),
#'   target_tabs = c("PTI", "PTI comparison")
#' )
#' observeEvent(first_open(), {
#'   message("Tab opened for the first time at ", first_open())
#' })
#' }
mod_tab_open_first_newserv <- function(id, active_tab, target_tabs, ...){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    once_opened <- reactiveVal(NULL)
    invalidator <- reactiveVal(NULL)

    observeEvent(active_tab(), {
      if (isTruthy(active_tab()) && isTruthy(target_tabs)) {
        if (active_tab() %in% target_tabs &&
            !active_tab() %in% once_opened()) {
          once_opened() %>%
            c(., active_tab()) %>%
            unique() %>%
            once_opened(.)
          invalidator(Sys.time())
        }
      } else if (!isTruthy(active_tab()) && !isTruthy(target_tabs)) {
        invalidator(Sys.time())
      }
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    invalidator

  })
}
