#' @describeIn mod_waiter_newsrv waiter UI wrapper
#'
#' @description user interface for the waiter
#'
#' @param ui user interface to wrap in the waiter
#' @param id ns identifier
#' @param show_waiter starts waiter on app's start.
#'
#' 
#' @importFrom shiny NS tagList 
#' @importFrom waiter useWaiter waiter_show_on_load
#' @importFrom golem get_golem_options
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
    
#' waiter Server Functions
#' 
#' @description This server function stops waiter based on the change in the 
#'   invalidating reactive parameter.
#'   
#'   `hide_invalidator` reactive. to invalidate and hide the waiter.
#'   `tab_opened` reactive. if qualifies under `req()`, a waiter is launched on the page.
#'
#' @param show_waiter logical TRUE/FALSE, indicates weather to include a waiter
#'     into the shiny app. 
#'     
#' 
#' @importFrom waiter Waiter waiter_hide
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
      # w$initialize()
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

   
#' @describeIn mod_waiter_newsrv waiter makes a spinner
#' 
#' @importFrom waiter spin_chasing_dots
make_spinner <- function(spin_text = "") {
  shiny::tagList(
    waiter::spin_chasing_dots(),
    tags$br(),
    spin_text %>% tags$span(style = "color:white;")
  )
}
