#' calc_pti2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList  
mod_calc_pti2_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' calc_pti2 - second generation calc PTI module meant to simplify and straighten 
#'   the calculation process. 
#' 
#' @importFrom shiny eventReactive req moduleServer showNotification observe
#' 
#' @noRd 
mod_calc_pti2_server <- function(id, shp_dta, input_dta, wt_dta){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # 0. Data preparation 
    
    ## 0.1 All data in the long format
    long_vars <- eventReactive(wt_dta(), { 
      wt_dta() %>% pivot_pti_dta((.)$indicators_list) 
      })
    
    ## 0.2 Key ID columns of each admin level. 
    existing_shapes <- eventReactive(shp_dta(), { shp_dta() %>% clean_geoms() })
    
    ## 0.3 Mapping table for administrative units 
    mt <- eventReactive(shp_dta(), { shp_dta() %>% get_mt() })
    
    
    # Calculating PTis in one reactive expression
    calc_pti <-
      shiny::eventReactive(#
        wt_dta()$weights_clean,
        {
          wt_dta()$weights_clean %>%
            
            # 1 Weighting data according to the existing weighting scheme.
            get_weighted_data(long_vars(), indicators_list = wt_dta()$indicators_list) %>% 
          
            # 2 Calculate PTI scores
            get_scores_data() %>% 
            
            # 3 Extrapolating PTI scores between aggregation levels.
            imap(~ expand_adm_levels(.x, mt()) %>%
                   merge_expandedn_adm_levels()) %>% 
            
            # 4 Clean agg pti data
            agg_pti_scores(existing_shapes()) %>% 
            
            # 5 Providing data points with labels 
            label_generic_pti() %>% 
            
            # 6 Restructuring PTI data to the mapping needs
            structure_pti_data(shp_dta())
            
        }, ignoreNULL = FALSE)

    
    shiny::observe({
      shiny::req(length(calc_pti()) > 0)
      shiny::showNotification("PTIs are calculated and extrapolated", 
                              type = "message", duration = 5)
    })
    
    calc_pti
  })
}
    
## To be copied in the UI
# mod_calc_pti2_ui("calc_pti2_ui_1")
    
## To be copied in the server
# mod_calc_pti2_server("calc_pti2_ui_1")

