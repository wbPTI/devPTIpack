#' export_pti_data Module for preparing data for export from PTI calculations
#'
#'
mod_export_pti_data_server <- function(id, plotted_dta, weights_dta){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    reactive({
      # browser()
      req(plotted_dta())
      req(weights_dta())
      get_pti_scores_export(plotted_dta()) %>% 
        rev() %>% 
        prepend(
          list(`Weighting schemes` =
                 get_pti_weights_export(weights_dta()$weights_clean, 
                                        weights_dta()$indicators_list))
        ) %>% 
        prepend(list(Country = weights_dta()$general)) 
    })
 
  })
}
    
## To be copied in the UI
# mod_export_pti_data_ui("export_pti_data_ui_1")
    
## To be copied in the server
# mod_export_pti_data_server("export_pti_data_ui_1")


#' @describeIn mod_export_pti_data_server Prepare PTI scores for export.
#' 
#' @export
#' @importFrom sf st_drop_geometry
get_pti_scores_export <- function(plotted_dta) {
  plotted_dta %>%
    devPTIpack::get_current_levels() %>%
    map2(names(.), ~ {
      by_joint <- 
        plotted_dta %>%
        `[`(str_detect(names(.), .y)) %>% 
        map(~{.x$pti_dta %>% names()}) %>% 
        unlist() %>% unname() %>% unique() %>% 
        `[`(str_detect(., "admin\\d|spatial_name"))
      plotted_dta %>%
        `[`(str_detect(names(.), .y)) %>%
        imap( ~ {
          repl <- .x$pti_codes %>% unname()
          .x$pti_dta %>%
            sf::st_drop_geometry() %>%
            select(-contains("pti_label"),-contains("area")) %>%
            mutate(
              `pti_score - PTI Score` = pti_score,
              `pti_score - PTI Priority` = .x$leg$recode_function(pti_score)
            ) %>%
            select(-pti_score) %>%
            rename_with( ~ str_replace(., "pti_score", repl))
        }) %>%
        reduce(full_join, by = by_joint) %>%
        list() %>%
        set_names(str_c(.x, " PTI Scores"))
    }) %>%
    unname() %>%
    unlist(recursive = FALSE)
}



#' @describeIn mod_export_pti_data_server Preparing weights for export too
#' 
#' @export
get_pti_weights_export <- function(wghts_dta, indic_dta) {
  indic_dta <-
    indic_dta %>%
    select(var_code , var_name, pillar_name)
  
  wghts_dta %>%
    map2(names(.), ~ {
      new_name <- str_c("Weights - ", .y)
      indic_dta %>%
        left_join(.x, "var_code") %>%
        rename_with(~ str_replace(., "weight", new_name))
    }) %>%
    reduce(left_join, by = c("var_code", "var_name", "pillar_name")) %>%
    select(-var_code) %>%
    rename_with(~ str_replace(., "var_name", "Variable name")) %>%
    rename_with(~ str_replace(., "pillar_name", "Pillar"))
}

