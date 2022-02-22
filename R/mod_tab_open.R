#' tab_open invalidator Server Functions
#' 
#' Checks if the `active_tab` is present in the ones specified in the `target_tabs` 
#' and invalidates the output if this is the first time such tab is being called.
#' 
#' @export
#' @noRd 
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
