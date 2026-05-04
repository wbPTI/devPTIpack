#' Waiter UI wrapper
#'
#' Internal UI helper that wraps a child `ui` in a div carrying the
#' `waiter::useWaiter()` machinery. When `show_waiter = TRUE`, calls
#' `waiter::waiter_show_on_load()` so the spinner is visible during
#' app boot. The spinner caption defaults to
#' `"<pti.name golem option> Loading..."`.
#'
#' @param ui Shiny UI tag(s) to wrap inside the waiter div.
#' @param id Character. Shiny module namespace ID. May be `NULL` for
#'   single-instance use.
#' @param show_waiter Logical. If `TRUE`, the spinner is shown
#'   immediately on app load.
#' @param ... Currently unused; reserved for future extensions.
#'
#' @return A `shiny.tag` -- the input `ui` wrapped in a div with a
#'   `pti-waiter` namespaced ID.
#'
#' @importFrom shiny NS tagList
#' @importFrom waiter useWaiter waiter_show_on_load
#' @importFrom golem get_golem_options
#' @importFrom stringr str_c
#' @noRd
mod_waiter_ui <- function(ui, id = NULL, show_waiter = FALSE, ...){
  ns <- NS(id)

  spinner <-
    golem::get_golem_options("pti.name") %>%
    as.character() %>%
    str_c(., " Loading...") %>%
    make_spinner( )

  div(
    id = ns("pti-waiter"),
    waiter::useWaiter(),
    if (show_waiter)
      waiter::waiter_show_on_load(html = spinner),
    ui
  )
}

#' Waiter show/hide server
#'
#' Internal server module paired with `mod_waiter_ui`. Shows the
#' waiter when `tab_opened()` becomes truthy (typical caller wires
#' this to a `mod_tab_open_first_newserv()` invalidator) and hides it
#' when `hide_invalidator()` becomes truthy. Both observers are
#' guarded by `req(show_waiter)` so the module is a no-op when the
#' caller opted out at UI construction.
#'
#' @param id Character. Shiny module namespace ID.
#' @param show_waiter Logical. Must match the value passed to
#'   `mod_waiter_ui`; when `FALSE`, this server is a no-op.
#' @param tab_opened Reactive. When truthy, triggers `Waiter$show()`.
#' @param hide_invalidator Reactive. When truthy, triggers
#'   `Waiter$hide()`.
#'
#' @return Called for side effects (registers two `observe()` blocks
#'   that show/hide a `waiter::Waiter` instance).
#'
#' @importFrom shiny moduleServer observe req
#' @importFrom waiter Waiter waiter_hide
#' @importFrom golem get_golem_options
#' @importFrom stringr str_c
#' @noRd
mod_waiter_newsrv <- function(id,
                              show_waiter = FALSE,
                              tab_opened = reactive(NULL),
                              hide_invalidator = reactive(NULL)){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    app_name <-
      golem::get_golem_options("pti.name") %>%
      as.character() %>%
      str_c(., " Loading PTI module...")
    if (identical(character(0), app_name)) app_name <- "Loading PTI module"
    spinner <- app_name %>% make_spinner()

    w <- waiter::Waiter$new(id = ns("pti-waiter"), html = spinner)

    observe({
      req(show_waiter)
      req(tab_opened())
      w$show()
    }, priority = 100)

    observe({
      req(show_waiter)
      req(hide_invalidator())
      w$hide()
      waiter::waiter_hide()
    })
  })
}

#' Build the chasing-dots spinner tag
#'
#' Internal helper that combines `waiter::spin_chasing_dots()` with a
#' caption span. Used by both `mod_waiter_ui` and `mod_waiter_newsrv`
#' so the on-load and per-tab spinners look identical.
#'
#' @param spin_text Character. Caption rendered below the spinner in
#'   white text. Empty string by default.
#'
#' @return A `shiny.tag.list` with the spinner SVG and a `<span>`
#'   caption.
#'
#' @importFrom shiny tagList
#' @importFrom waiter spin_chasing_dots
#' @noRd
make_spinner <- function(spin_text = "") {
  shiny::tagList(
    waiter::spin_chasing_dots(),
    tags$br(),
    spin_text %>% tags$span(style = "color:white;")
  )
}
