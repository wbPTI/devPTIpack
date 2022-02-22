#' waiter UI wrappere
#'
#' @description Adds waiter to any object in the.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_waiter_ui <- function(ui, id = NULL){
  ns <- NS(id)
  div(id = ns("pti-waiter"), waiter::useWaiter(), ui)
}
    
#' waiter Server Functions
#'
#' @param show_waiter logical weather to include a waiter
#' @param hade_invalidator reactive to invalidate and hide the waiter
#' @noRd 
mod_waiter_newsrv <- function(id, 
                              show_waiter = FALSE, 
                              tab_opened = reactive(NULL),
                              hade_invalidator = reactive(NULL)){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    spinner <- make_spinner(
      golem::get_golem_options("pti.name") %>%
        as.character() %>%
        str_c(., " Loading PTI module...")
      )
    
    w <- waiter::Waiter$new(id = ns("pti-waiter"), html = spinner)
    
    observe({
      req(show_waiter)
      req(tab_opened())
      w$initialize()
      w$show()
    }, priority = 100)
    
    observe({
      req(show_waiter)
      req(hade_invalidator())
      w$hide()
    })
  })
}
   
#' make a spinner
#' 
#' @noRd
make_spinner <- function(spin_text = "") {
  tagList(
    waiter::spin_chasing_dots(),
    br(),
    spin_text %>% span(style = "color:white;")
  )
}