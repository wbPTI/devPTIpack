#' wt_inp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
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
    
#' @describeIn mod_wt_inp_ui
#' 
mod_wt_inp_test_ui <- function(id){
  ns <- NS(id)
  
  if (is.null(options("golem.app.prod")) || !isTRUE(options("golem.app.prod")[[1]]))
    verbatimTextOutput(ns("wt_tests_out"))
}


#' @describeIn mod_wt_inp_ui Weights input UI full
#' 
full_wt_inp_ui <- function(ns) {
  
  # controls_col <-
  tagList(
    # width = 3,
    textInput(
      ns("existing.weights.name"),
      label = "Name your PTI",
      placeholder = "Specify name for a weighting scheme",
      value = NULL,
      width = "100%"
    ) %>%
      div(id = "step_1_name"),
    
    tagList(
      selectInput(
        ns("existing.weights"),
        label = "Select existing PTI to modify",
        choices = NULL,
        selected = NULL,
        width = "100%"
      ) %>%
        shinyjs::hidden() %>%
        div(id = "step_2_select_existing")
      ,
      uiOutput(ns("save_btn")) %>%
        div(id = "step_3_save")
      ,
      {
        actionButton(
          ns("weights.delete"),
          "Delete PTI",
          icon = icon("remove"),
          class = "btn-danger",
          width = "100%"
        )
      } %>%
        # shinyjs::disabled() %>%
        # shinyjs::hidden() %>%
        div(id = "step_4_delete")
    ) %>%
      div(id = "step_234_controls1") %>%
      div(id = "step_234_controls2") %>%
      div(id = "step_234_controls3")
    ,
    tagList(
      downloadButton(
        ns("weights.download"),
        "Download PTIs",
        icon = icon("download"),
        class = "btn-primary",
        style = "width: 100%"
      ) %>%
        shinyjs::hidden(),
      
      downloadButton(
        ns("dwnld_data"),
        "Download PTI data",
        icon = icon("download"),
        class = "btn-primary",
        style = "width: 100%"
      ), 
      
      fileInput(
        ns("weights_upload"),
        #ns("data_upload"),
        "Upload PTI",
        multiple = FALSE,
        accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        width = "100%",
        buttonLabel = "Browse...",
        placeholder = "No file selected"
      )
    ) %>%
      div(id = "step_5_downalod_upload1") %>%
      div(id = "step_5_downalod_upload2")
  )
  
}



#' @describeIn mod_wt_inp_ui  Weights input UI full
#' 
#' 
short_wt_inp_ui <- function(ns) {
  
  tagList(
    # width = 3,
    textInput(
      ns("existing.weights.name"),
      label = "Name your PTI",
      placeholder = "Specify name for a weighting scheme",
      value = NULL,
      width = "100%"
    ) %>%
      div(id = "step_1_name"),
    
    tagList(
      uiOutput(ns("save_btn")) %>%
        div(id = "step_3_save")
      ,
      {
        actionButton(
          ns("weights.reset"),
          "Reset PTI",
          icon = icon("remove"),
          class = "btn-danger",
          width = "100%"
        )
      } %>%
        # shinyjs::disabled() %>%
        # shinyjs::hidden() %>%
        div(id = "step_4_delete")
    ) %>%
      div(id = "step_234_controls1") %>%
      div(id = "step_234_controls2") %>%
      div(id = "step_234_controls3"),
    
    tagList(
      downloadButton(
        ns("weights.download"),
        "Download PTIs",
        icon = icon("download"),
        class = "btn-primary",
        style = "width: 100%"
      ) %>%
        shinyjs::hidden(),
      
      downloadButton(
        ns("dwnld_data"),
        "Download PTI data",
        icon = icon("download"),
        class = "btn-primary",
        style = "width: 100%"
      )
      
    ) %>%
      div(id = "step_5_downalod_upload1") %>%
      div(id = "step_5_downalod_upload2")
  )
  
}





#' @describeIn mod_wt_inp_ui  Server Functions
#'
mod_wt_inp_server <- function(id, input_dta, plotted_dta = reactive(NULL)){
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
    
    # Step 4. Save current weights to the list ====================
    save_ws <- mod_wt_save_newsrv(NULL, edited_ws, curr_wt, curr_wt_name)
    observeEvent(save_ws(), {edited_ws$weights_clean <- save_ws})
    
    # Step 5. Delet/Reset current weights to the list =========================
    reset_ws <- mod_wt_delete_newsrv(NULL, edited_ws, curr_wt_name)
    observeEvent(reset_ws$invalidator, {
      edited_ws$weights_clean <- reset_ws$value
    }, ignoreInit = TRUE)
    
    # Step 6. Selected WS must be reflected in the WS name field
    selected_ws <- mod_wt_select_newsrv(NULL, edited_ws, curr_wt_name)
    
    # Step 7. Update PTI on selecting another ================================
    upd_wt <- mod_wt_upd_newsrv(NULL, edited_ws, curr_wt, selected_ws)
    
    # TO DO!
    # == Upload PTI - Fix
    # == Doanload Weigths and Data modules.
    
    # Step 6. Upload weights from a file =====================================
    uploaded_ws <- mod_wt_uplod_newsrv(NULL, input_dta, ind_list)
    # observeEvent(uploaded_ws(), {uploaded_ws() %>% edited_ws()}, ignoreInit = TRUE)
  
    # Step 8. Download weights server =========================================
    mod_wt_dwnld_newsrv(NULL, edited_ws, input_dta = input_dta, 
                        plotted_dta = plotted_dta)
    
    output$wt_tests_out <- renderPrint({
      list(
        edited_ws_weights_clean = edited_ws$weights_clean(),
        curr_wt_name = curr_wt_name(),
        curr_wt = curr_wt(),
        selected_ws = selected_ws()
        ) %>% 
        str(max.level = 3)
    })
    
    # Stop any waiter if needed
    observe({
      req(!all(is.na(curr_wt()$weight)))
      waiter::waiter_hide()
    })
    
    # save_ws_out <-
    reactive({
      input_dta() %>%
        append(
          list(
            weights_clean = edited_ws$weights_clean(),
            indicators_list = edited_ws$indicators_list()
          )
        )
    })
    
    
    # edited_ws
 
  })
}
    
## To be copied in the UI
# mod_wt_inp_ui("wt_inp_ui_1")
    
## To be copied in the server
# mod_wt_inp_server("wt_inp_ui_1")





#' @describeIn mod_wt_inp_ui  New WT names server processor.
#' 
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
      
      reactive(input$existing.weights.name) %>% 
        shiny::throttle(millis = 500)
    })
  
}



#' @describeIn mod_wt_inp_ui  New WT save server processor.
#' 
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
        list(curr_wt_name(), curr_wt()),
        {
          cur_val <- curr_wt()$weight
          cur_val[is.na(cur_val)] <- 0
          if (all(cur_val == 0)) {
            actionButton(
              ns("weights.save"),
              "All weights must not be zero",
              class = "btn-warning",
              width = "100%"
            ) %>%
              shinyjs::disabled() %>%
              current_btn_ui()
          } else {
            
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

        cur_val <- curr_wt()$weight
        cur_val[is.na(cur_val)] <- 0
        if (all(cur_val == 0)) {
          actionButton(
            ns("weights.save"),
            "All weights must not be zero",
            class = "btn-warning",
            width = "100%"
          ) %>%
            shinyjs::disabled() %>%
            current_btn_ui()
        } else {
        
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


#' @describeIn mod_wt_inp_ui  Reset/Delete WT save server processor.
#' 
mod_wt_delete_newsrv <- function(id, edited_ws, curr_wt_name) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      return_ws <- reactiveValues(
        invalidator = 0,
        value = reactive(NULL)
      )
      
      # hide/show
      observe({
        if (!isTruthy(edited_ws$weights_clean())) {
          # shinyjs::hide(id = "weights.delete", anim = TRUE)
          # shinyjs::hide(id = "weights.reset", anim = TRUE)
          shinyjs::disable(id = "weights.delete")
          shinyjs::disable(id = "weights.reset")
        } else {
          # shinyjs::show(id = "weights.delete", anim = TRUE)
          # shinyjs::show(id = "weights.reset", anim = TRUE)
          shinyjs::enable(id = "weights.delete")
          shinyjs::enable(id = "weights.reset")
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
          return_ws$invalidator <- return_ws$invalidator + 1
          if (length(out_wt) == 0) {
            return_ws$value <- reactive(NULL)
          } else {
            return_ws$value <- reactive(out_wt)
          }
        })
      
      observeEvent(#
        input$weights.reset,
        {
          return_ws$invalidator <- return_ws$invalidator + 1
          return_ws$value <- reactive(NULL)
        })
      
      return_ws
    })
}


#' @describeIn mod_wt_inp_ui  Select / update selected
#' 
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
      
      curr_sel
    })
  
}



#' @describeIn mod_wt_inp_ui  Update weights in the input to the selected WS
#' 
#' 
mod_wt_upd_newsrv <- function(id, edited_ws, curr_wt, selected_wt_name) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      upd_to_reactive <- reactiveVal()
      
      observe({
        selected_wt_name()
        isolate({
          if (!isTruthy(edited_ws$weights_clean())) {
            upd_to <-
              edited_ws$indicators_list() %>%
              select(var_code) %>%
              mutate(weight = 0L)
          } else {
            req(selected_wt_name() %in% names(edited_ws$weights_clean()))
            upd_to <-
              edited_ws$weights_clean()[[selected_wt_name()]]
          }
          req(!identical(upd_to, curr_wt()))
          upd_to %>% upd_to_reactive()
        })
      })
      
      upd_to_reactive
      
    })
  
}



#' Download all weights as an excel file
#' 
#' @noRd
#' 
mod_wt_dwnld_newsrv <- function(id, 
                                edited_ws = reactiveValues(weights_clean = list(1)), 
                                input_dta = reactive(NULL), 
                                plotted_dta = reactive(NULL),
                                ...) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      out_list <- reactiveValues()
      
      observeEvent(#
        input_dta(), {
          req(input_dta())
          if (isTruthy(input_dta())) {
            input_dta() %>%
              fct_inp_for_exp() %>%
              iwalk( ~ {
                out_list[[.y]] <- .x
              })
          }
        })
      
      observeEvent(#
        edited_ws$weights_clean(),
        {
          if (isTruthy(edited_ws$weights_clean())) {
            out_list[["weights_table"]] <-
              edited_ws$weights_clean() %>%
              fct_internal_wt_to_exp(edited_ws$indicators_list())
          } else {
            out_list[["weights_table"]] <- NULL
          }
        },
        ignoreInit = TRUE,
        ignoreNULL = FALSE)
      
      prev_ploted_names <- reactiveVal(NULL)

      observeEvent(#
        plotted_dta(),
        {
          if (isTruthy(plotted_dta())) {
            plotted_dta() %>%
              get_pti_scores_export() %>%
              iwalk(~{
                prev_ploted_names() %>% 
                  append(.y) %>% unlist() %>% unique() %>% 
                  prev_ploted_names()
                out_list[[.y]] <- .x
              })
          } else {
            prev_ploted_names() %>% 
              walk(~{
                if (.x %in% names(out_list)) {
                  out_list[[.x]] <- NULL
                }
              })
            prev_ploted_names(NULL)
          }
        },
        ignoreInit = TRUE,
        ignoreNULL = FALSE)
      
      dta_dwnld <-
        reactive({
          out_list %>% reactiveValuesToList() %>% keep(isTruthy)
        })
      
      # Write export data
      output$dwnld_data <- downloadHandler(
        filename = function() {dta_dwnld()$general[1,1][[1]] %>% str_c(., "-pti-data.xlsx")},
        content = function(con) {writexl::write_xlsx(dta_dwnld(), con)}
      )
    })
}


#' @describeIn mod_wt_inp_ui  TODO: Upload WS to the server
#' 
mod_wt_uplod_newsrv <- function(id, imported_data, pti_indicators) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      return_wt <- reactiveVal()
      # observeEvent(imported_data(), imported_data() %>% return_wt())
      observeEvent(#
        input$weights_upload,
        {
          #############################
          #### To do: Warn about invalid file to upload when weights ID do not match
          # Warn about no weights to upload
          # browser()
          
          
          # Checking upload. Step 1. Checking data
          # uploaded_weights <- fct_template_reader(input$weights_upload$datapath)
          
          
          
          # uploaded_weights <- fct_template_reader(input$weights_upload$datapath)
          # out <- imported_data()
          # out$timestamp <- Sys.time()
          # out$weights_table <-
          #   uploaded_weights$weights_table %>%
          #   filter(var_code %in% pti_indicators()$var_code)
          # out$weights_clean <-
          #   uploaded_weights$weights_clean %>%
          #   imap(~{
          #     .x %>%
          #       filter(var_code %in% pti_indicators()$var_code)
          #   })
          # return_wt(out)
          
          
        },
        ignoreInit = TRUE,
        ignoreNULL = FALSE)
      reactive({return_wt})
    })
}



