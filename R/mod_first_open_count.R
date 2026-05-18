#' First-open invalidator (legacy variant of mod_tab_open_first_newserv)
#'
#' Internal sibling of `mod_tab_open_first_newserv`. Differs in two
#' ways: (1) the both-NULL branch is the truthy default rather than
#' guarded, so the invalidator fires on init when both `active_tab`
#' and `target_tabs` are `NULL`; (2) used internally by page modules
#' that need a per-namespace open count without forcing the EXPORTED
#' contract on consumers.
#'
#' @param id Character. Shiny module namespace ID.
#' @param active_tab Reactive returning the currently selected tab
#'   name. May yield `NULL`.
#' @param target_tabs Character vector of tab names. May be `NULL`.
#' @param ... Currently unused; reserved for future extensions.
#'
#' @return A reactive returning either `NULL` or a `POSIXct` timestamp
#'   updated on each qualifying first-open event.
#'
#' @importFrom shiny moduleServer reactiveVal observeEvent
#' @noRd
mod_first_open_count_server <- function(id, active_tab, target_tabs, ...){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    once_opened <- reactiveVal(NULL)
    invalidator <- reactiveVal(NULL)

    observeEvent(active_tab(), {
      if ((is.null(active_tab()) && all(is.null(target_tabs))) ||
          (active_tab() %in% target_tabs && !active_tab() %in% once_opened())) {
        once_opened() %>%
          c(., active_tab()) %>%
          unique() %>%
          once_opened(.)
        invalidator(Sys.time())
      }
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    invalidator

  })
}
