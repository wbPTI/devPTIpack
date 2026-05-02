#' new_weights server runner for generating demo weights of all data combinations
#'
#' @description A shiny Module.
#'
mod_new_demo_weights_server <- function(id, imported_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ws_to_plot2 <- reactiveVal()
    indicators <-  mod_indicarots_srv(NULL, imported_data)
    
    output$weights_fields <- renderUI({
      tagList(
        # actionButton(ns("new_weights"), "Regenerate random weights"),
        verbatimTextOutput(ns("weights_tbl"))
      )
    }) 
    
    save_ws <-
      eventReactive(#
        list(indicators(), input$new_weights),
        {
          req(indicators())
          a <- imported_data()
          a$indicators_list <- indicators()
          a$weights_clean <- 
            get_all_weights_combs(a$indicators_list$var_code, 1)[1:3] %>% 
            map(~ indicators() %>% 
                  select(var_code) %>% 
                  left_join(.x, by = "var_code"))
          a
        }, ignoreNULL = TRUE)
    
    current_ws_values <-
      eventReactive(#
        save_ws(),
        {
          save_ws()$weights_clean[[1]] 
        }, ignoreNULL = TRUE)
    
    reactive(list(save_ws = save_ws,
                  current_ws_values = current_ws_values))
  })
}



    
#' @describeIn mod_weights_ui generic new weights Server Functions
#'
#' @noRd 

mod_new_weights_server <-
  function(id, imported_data, export_recoded_data, ...) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      edited_ws <- reactiveVal()
      
      observeEvent(imported_data(), imported_data() %>% edited_ws())
      
      pti_indicators <- mod_indicarots_srv(NULL, imported_data)
      mod_gen_wt_inputs_srv(NULL, pti_indicators)
      
      # Listeners of the action buttons on the weights
      mod_wt_btns_srv(NULL, pti_indicators, dtn_id = "_set_zero", to_value = 0)
      mod_wt_btns_srv(NULL, pti_indicators, dtn_id = "_set_one", to_value = 1)
      
      # Upload WS module
      uploaded_ws <- mod_wt_uplod_srv(NULL, imported_data, pti_indicators)
      
      # Delete WS module
      deleted_ws <- mod_wt_delete_srv(NULL, edited_ws, current_ws_name)
      
      # Select WS out of existing
      selected_ws_name <- mod_wt_select_srv(NULL, deleted_ws)
      
      # Enter WS name
      current_ws_name <- mod_wt_name_srv(NULL, selected_ws_name)
      
      # Updating weights based on the selected WS
      mod_wt_fill_srv(NULL, pti_indicators, deleted_ws, selected_ws_name)
      
      # collecting currently entered values
      current_ws_values <- mod_collect_wt_srv(NULL, pti_indicators)
      
      # Save WS module
      save_ws <- mod_wt_save_srv(NULL, pti_indicators, deleted_ws,
                                 current_ws_name, current_ws_values)
      
      # observeEvent(save_ws(), {
      #   current_ws_values()
      #   browser()
      #   # save_ws() %>% edited_ws()
      # })
      
      # Download weights server
      mod_download_wt_srv(NULL, edited_ws, export_recoded_data)
      
      reactive(list(save_ws = save_ws,
                    current_ws_values = current_ws_values))
      
    })
  }


    
## To be copied in the UI
# mod_new_weights_ui("new_weights_ui_1")
    
## To be copied in the server
# mod_new_weights_server("new_weights_ui_1")
