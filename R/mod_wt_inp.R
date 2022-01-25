#' wt_inp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @export
#'
#'
#' @importFrom shiny NS tagList 
#' 
#' 
#' @examples 
#' 
#' column(
#'   5, 
#'   mod_wt_inp_ui("input_tbl_1", dt_style = "max-height: calc(70vh);"),
#'   style = "padding-right: 0px; padding-left: 5px;"
#' )
#' 
#' absolutePanel(
#'   mod_wt_inp_ui("input_tbl_2", dt_style = "max-height: 300px;") %>% 
#'     div(style = "zoom:0.8;"),
#'   top = 10, right = 75, width = 350, height = 550, style = "!important; z-index: 1000;")
#'   ) 
#' 
#' 
mod_wt_inp_ui <- function(id, full_ui = FALSE, height = "550px", dt_style, ...){
  ns <- NS(id)
  
  if (full_ui) {
    controls_col <- full_wt_inp_ui(ns)
  } else {
    controls_col <- short_wt_inp_ui(ns)
  }

  controls_col %>% 
    div(style = "margin-bottom: 10px; width: -webkit-fill-available;") %>%
    tagList(mod_DT_inputs_ui(ns(NULL), height, dt_style, ...))
  # %>%  fillCol()
}
    
mod_wt_inp_test_ui <- function(id){
  ns <- NS(id)
  
  if (is.null(options("golem.app.prod")) || !isTRUE(options("golem.app.prod")[[1]]))
    verbatimTextOutput(ns("wt_tests_out"))
}




#' wt_inp Server Functions
#'
#' @noRd 
mod_wt_inp_server <- function(id, input_dta, export_dta){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Step 1. Convert input data into the table-ready style ===================
    ind_list <- reactive({req(input_dta()) %>% get_indicators_list()})
    
    # Step 2. Init weights input and collect weights values ===================
    curr_wt <- mod_DT_inputs_server(NULL, ind_list, upd_wt)
    curr_wt_name <- mod_wt_name_newsrv(NULL, selected_ws)
  
    # Step 3. Setting a reactive value for actively used set of weights =======
    edited_ws <- reactiveValues(
      indicators_list = ind_list,
      weights_clean = reactive(NULL)
    )
    
    # Step 4. Save/Delet/Reset current weights to the list ====================
    save_ws <- mod_wt_save_newsrv(NULL, edited_ws, curr_wt, curr_wt_name)
    observeEvent(save_ws(), {edited_ws$weights_clean <- save_ws})
    
    # Step 5. Delet/Reset current weights to the list =========================
    reset_ws <- mod_wt_delete_newsrv(NULL, edited_ws, curr_wt_name)
    observeEvent(reset_ws(), {edited_ws$weights_clean <- reset_ws}, ignoreNULL = FALSE)
    
    selected_ws <- mod_wt_select_newsrv(NULL, edited_ws, curr_wt_name)
    
    # Prepare data for update ================================================
    upd_wt <- reactive({
      (curr_wt_name())
      isolate({
        if (!isTruthy(edited_ws$weights_clean())) {
          upd_to <- 
            edited_ws$indicators_list() %>% 
            select(var_code) %>% 
            mutate(weight = 0L)
        } else {
          req(curr_wt_name() %in% names(edited_ws$weights_clean()))
          upd_to <- edited_ws$weights_clean()[[curr_wt_name()]]
        }
        req(!identical(upd_to, curr_wt()))
        upd_to
      })
    })
    
    
    # TO DO!
    # == Write some logic for UPDATE WT
    # == Check reset button with simplified UI.
    # == Upload PTI - Fix
    # == Doanload Weigths and Data modules.
    
    # Step 6. Upload weights from a file =====================================
    # uploaded_ws <- mod_wt_uplod_srv(ws_name, input_dta, ind_list)
    # observeEvent(uploaded_ws(), {uploaded_ws() %>% edited_ws()}, ignoreInit = TRUE)
  
    # # # Download weights server =============================================
    # mod_download_wt_srv(id, edited_ws, reactuve(NULL))
    
    output$wt_tests_out <- renderPrint({
      list(
        edited_ws_weights_clean = edited_ws$weights_clean(),
        curr_wt_name = curr_wt_name(),
        curr_wt = curr_wt(),
        selected_ws = selected_ws()
        ) %>% 
        str(max.level = 3)
    })
    
    # reactive(list(save_ws = save_ws, 
    #               current_ws_values = current_ws_values))
 
  })
}
    
## To be copied in the UI
# mod_wt_inp_ui("wt_inp_ui_1")
    
## To be copied in the server
# mod_wt_inp_server("wt_inp_ui_1")





#' New WT names server processor.
#' 
#' @noRd
mod_wt_name_newsrv <- function(id, selected_ws) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observe({
        if (isTruthy(selected_ws())) {
          updateTextInput(session, "existing.weights.name", value = selected_ws())
        } else {
          updateTextInput(session,
                          "existing.weights.name",
                          value = "",
                          placeholder = "Specify name for a weighting scheme")
        }
      })
      
      reactive(input$existing.weights.name) %>% shiny::throttle(millis = 500)
    })
  
}



#' New WT save server processor.
#' @noRd
mod_wt_save_newsrv <- function(id, edited_ws, curr_wt, curr_wt_name) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      return_ws <- reactiveVal()
      current_btn_ui <- reactiveVal()
      output$save_btn <- renderUI({current_btn_ui()})
      
      # Activate/deactivate save button. No name specified ========
      observeEvent(
        curr_wt_name(),
        {
          if (!isTruthy(curr_wt_name())) {
            actionButton(
              ns("weights.save"),
              label = "Provide a name",
              icon = icon("exclamation-triangle"),
              class = "btn-warning",
              width = "100%"
            ) %>%
              shinyjs::disabled() %>%
              current_btn_ui()
          } else {
            actionButton(
              ns("weights.save"),
              "Save and plot PTI",
              icon = icon("save"),
              class = "btn-success",
              width = "100%"
            ) %>%
              # shinyjs::enable()%>%
              current_btn_ui()
          }
        },
        ignoreInit = FALSE,
        ignoreNULL = FALSE)
      
      
      # Activate/deactivate on delete or same input ================
      observe({
        req(edited_ws$weights_clean())
        req(curr_wt())
        req(curr_wt_name())
        # req(length(after_delete_ws()$weights_clean) > 0)

        if (curr_wt_name() %in% names(edited_ws$weights_clean())) {
          existing_values <- edited_ws$weights_clean()[[curr_wt_name()]]
          
          if (isTRUE(all_equal(existing_values, curr_wt(), convert = TRUE))) {
            actionButton(
              ns("weights.save"),
              "No changes to save",
              class = "btn-info",
              width = "100%"
            ) %>%
              shinyjs::disabled() %>%
              current_btn_ui()
          } else {
            actionButton(
              ns("weights.save"),
              "Save changes and plot PTI",
              icon = icon("save"),
              class = "btn-success",
              width = "100%"
            ) %>%
              current_btn_ui()
          }
        } else {
          actionButton(
            ns("weights.save"),
            "Save and plot new PTI",
            icon = icon("save"),
            class = "btn-success",
            width = "100%"
          ) %>%
            current_btn_ui()
        }
      })
      
      
      # Data output logic ======================================
      observeEvent(#
        input$weights.save,
        {
          out_wt <- edited_ws$weights_clean()
          out_wt[[curr_wt_name()]] <- curr_wt()
          out_wt %>% return_ws()
        })
      
      return_ws
    })
}


#' Reset WT save server processor.
#' @noRd
mod_wt_delete_newsrv <- function(id, edited_ws, curr_wt_name) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      return_ws <- reactiveVal()
      
      # hide/show
      observe({
        if (!isTruthy(edited_ws$weights_clean())) {
          shinyjs::hide(id = "weights.delete", anim = TRUE)
          shinyjs::hide(id = "weights.reset", anim = TRUE)
        } else {
          shinyjs::show(id = "weights.delete", anim = TRUE)
          shinyjs::show(id = "weights.reset", anim = TRUE)
        }
      })
      
      # Enable Disable
      observe({
        # req(curr_wt_name())
        if (curr_wt_name() %in% names(edited_ws$weights_clean())) {
          shinyjs::enable(id = "weights.delete")
          shinyjs::enable(id = "weights.reset")
        } else {
          shinyjs::disable(id = "weights.delete")
          shinyjs::disable(id = "weights.reset")
        }
      })
      
      observeEvent(#
        input$weights.delete,
        {
          out_wt <- edited_ws$weights_clean()
          out_wt[[curr_wt_name()]] <- NULL
          if (length(out_wt) == 0)
            return_ws(NULL)
          else {
            out_wt %>% return_ws()
          }
        })
      
      observeEvent(#
        input$weights.reset,
        {
          NULL %>% return_ws()
        })
      
      return_ws
    })
}


#' Select / update selected
#' @noRd
mod_wt_select_newsrv <- function(id, edited_ws, curr_wt_name) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      past_sel <- reactiveVal(list(NULL))
      curr_sel <- reactiveVal(NULL)
      curr_choices <- reactiveVal(NULL)
      
      observe({
        input$existing.weights
        isolate({
          curr_sel(input$existing.weights)
        })
      })
      
      observe({
        curr_sel()
        isolate({
          past_sel() %>% prepend(list(curr_sel())) %>% past_sel()
        })
      })
      
      # Deactivate/activate selector
      observeEvent(
        edited_ws$weights_clean(),
        {
          # browser()
          if (!isTruthy(edited_ws$weights_clean())) {
            shinyjs::hide(id = "existing.weights", anim = TRUE)
            curr_sel(NULL)
            curr_choices(NULL)
          } else {
            names(edited_ws$weights_clean()) %>% curr_choices()
            
            if (length(curr_choices()) >= 1) {
              shinyjs::show(id = "existing.weights", anim = TRUE)
              
              if (isTruthy(curr_wt_name()) &&
                  curr_wt_name() %in% curr_choices()) {
                curr_wt_name() %>% curr_sel()
                updateSelectInput(session, "existing.weights", choices = curr_choices(), selected = curr_sel())
              } else if (
                isTruthy(curr_wt_name()) &&
                !curr_wt_name() %in% curr_choices()
              ) {
                past_sel() %>% 
                  unlist() %>%
                  `[`(. %in% names(edited_ws$weights_clean())) %>%
                  `[[`(1) %>%
                  curr_sel()
                updateSelectInput(session, "existing.weights", choices = curr_choices(), selected = curr_sel())
                
              }
              
            } else {
              curr_sel(NULL)
              # curr_choices() %>% `[[`(1) %>% curr_sel()
              shinyjs::hide(id = "existing.weights", anim = TRUE)
            }
          }
        }, ignoreNULL = FALSE)
      
      # # Update curr_sel if the data changes
      # observeEvent(
      #   curr_choices(),
      #   {
      #     if (isTruthy(edited_ws$weights_clean())) {
      #       
      #       if (
      #         isTruthy(input$existing.weights) &&
      #         curr_choices() %in% names(edited_ws$weights_clean())
      #         ) {
      #         curr_sel(input$existing.weights)
      #       } else if (
      #         isTruthy(input$existing.weights) &&
      #         !input$existing.weights %in% names(edited_ws$weights_clean())
      #       ) {
      #         browser()
      #         past_sel() %>% 
      #           unlist() %>% 
      #           `[`(. %in% names(edited_ws$weights_clean())) %>% 
      #           `[[`(1) %>% 
      #           curr_sel()
      #       } else {
      #         curr_sel(NULL)
      #         curr_choices(NULL)
      #       }
      #       
      #     } else {
      #       curr_sel(NULL)
      #       curr_choices(NULL)
      #     }
      #     
      #   })
      
      # observe({
      #   updateSelectInput(session, "existing.weights", choices = curr_choices(), selected = curr_sel())
      # })
      
      curr_sel
    })
  
}


