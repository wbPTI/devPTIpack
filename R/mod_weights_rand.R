#' weights_rand UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList actionButton
mod_weights_rand_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("rand_weights_ui"))

}
    
#' @describeIn mod_weights_rand_ui  Computes random weights
#' 
#' @noRd
#' 
#' @export
get_rand_weights <- function(indicators_list) {
  1:(sample(2:5, 1)) %>% 
    map(~ {
    list_nm <- 
      str_c(sample(letters, 5), collapse = "", sep = "") %>%
      str_c(" ", .x)
    wght <- indicators_list %>%
      select(var_code) %>%
      mutate(weight = runif(nrow(.), -2, 2) %>% round(.))
    set_names(list(wght), list_nm)
  }) %>%
    unlist(recursive = F)
}


#' @describeIn mod_weights_rand_ui Produces all combinations of weights
#' @export
#' @noRd
get_all_weights_combs <- function(var_codes, n_items = 3) {
  n_items %>% 
    map(~{
      n_combo <- .x
      
      combn(var_codes, n_combo, simplify = F) %>% 
        map2(seq_along(.), ~{
          wt_nm <- str_c("Wght of ", n_combo, " comb no. ", .y)
          tibble(var_code = var_codes[var_codes %in% unlist(.x)]) %>%
            mutate(weight = 1) %>% 
            list() %>% 
            set_names(wt_nm)
        }) %>% 
        unlist(recursive = F)
    }) %>% 
    unlist(recursive = F)
}

    
## To be copied in the UI
# mod_weights_rand_ui("weights_rand_ui_1")
    
## To be copied in the server
# mod_weights_rand_server("weights_rand_ui_1")
