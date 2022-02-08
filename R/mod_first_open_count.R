#' Invalidator for the first time tab opening. 
#' 
#' @details returns time stamp when the tab gets open the first time. 
#' 
#' @param active_tab,target_tabs are single character and a single/vector
#' 
#' @export
#' @importFrom shiny reactiveVal observeEvent
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