#' UI Function for plotting weights controls
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param height css for height in the `dataTableOutput` function.
#' @param dt_style additional styling css for outer div where `dataTableOutput` is placed in.
#'
#' @importFrom shiny NS tagList 
#' 
#' @examples
#'\dontrun{
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
#'}
#' 
mod_wt_inp_ui <- function(id,
                          full_ui = FALSE,
                          height = "inherit",
                          dt_style = "height: 450px;",
                          wt_style = NULL,
                          wt_dwnld_options = c("data", "weights", "shapes", "metadata"),
                          ...) {
  
  ns <- NS(id)
  
  if (full_ui) {
    controls_col <- full_wt_inp_ui(ns)
  } else {
    controls_col <- short_wt_inp_ui(ns, dwnld_options = wt_dwnld_options)
  }

  controls_col %>% 
    # div(style = "margin-bottom: 10px; width: -webkit-fill-available;") %>%
    div(style = "margin-bottom: 10px; width: 100%;") %>%
    tagList(
      div(id = "step_5_modify_weights", 
          mod_DT_inputs_ui(ns(NULL), height, dt_style)
          )
      ) %>% 
    div(., wt_style = wt_style, ...)
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
      shiny::uiOutput(ns("save_btn")) %>%
        div(id = "step_3_save")
      ,
      {
        actionButton(
          ns("weights.delete"),
          "Delete PTI",
          icon = shiny::icon("remove", lib = "glyphicon"),
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
        icon = shiny::icon("download"),
        class = "btn-primary",
        style = "width: 100%"
      ) %>%
        shinyjs::hidden(),
      
      downloadButton(
        ns("dwnld_data"),
        "Download PTI data",
        icon = shiny::icon("download"),
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
#' @importFrom shiny textInput
#' @param ns namespace
#' @param dwnld_options character vector that defines what data download options
#'        are available. one or all of c("data", "weights", "shapes", "metadata").
#'        If NULL or blank, no data download options are available., 
short_wt_inp_ui <- function(ns, dwnld_options = c("data", "weights", "shapes", "metadata")) {
  
  if (length(dwnld_options) > 0) {
    dwnld_text <-
      list(
        c("data", "weights", "shapes", "metadata"),
        c("dta.download", "weights.download", "shp.files", "mtdt.files"),
        c("data", "scores", "shapes", "metadata")
      ) %>%
      pmap( ~ {
        if (..1 %in% dwnld_options)
        {
          mod_dwnld_dta_link_ui(NULL, ns(..2), ..3, prefix = NULL, suffix = NULL)
        }
        else
          NULL
      }) %>%
      purrr::keep(function(x) !is.null(x))
    
    if (length(dwnld_text) >= 2) {
      dwnld_text <- 
        c(rep(", ", max(length(dwnld_text) - 2, 0)), " and ", "") %>%
        map2(dwnld_text, ~ tagList(.y, .x))
    }
      
    dwnld_text <- 
      dwnld_text %>%
      tagList("Download PTI ", ., ".") %>% 
      tags$i() %>% 
      tags$p(style = "font-size: 12px;", 
             style = "text-align: right; margin: 0 0 0px !important;")
  } else {
    dwnld_text <- tagList()
  }
  
  tagList(
    
    textInput(
      ns("existing.weights.name"),
      label = "Name your PTI",
      placeholder = "Specify name for a weighting scheme",
      value = NULL,
      width = "100%"
    ) %>%
      div(id = "step_1_name"), 
    
    tagList(
      shiny::uiOutput(ns("save_btn"), inline = TRUE) %>% tags$span(id = "step_3_save"),
      mod_wt_delete_ui(NULL, ns("weights.reset")) %>% tags$span(id = "step_4_delete")
    ) %>%
      div(id = "step_234_controls1") %>%
      div(id = "step_234_controls2") %>%
      div(id = "step_234_controls3"),
    
    tagList(
      dwnld_text
    ) %>%
      div(id = "step_5_downalod_upload1") %>%
      div(id = "step_5_downalod_upload2")
  )
  
}





#' @describeIn mod_wt_inp_ui  Server Functions
#'
mod_wt_inp_server <- function(id, input_dta, plotted_dta = reactive(NULL), shapes_path = "", mtdtpdf_path = "") {
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
    observeEvent(reset_ws$invalidator(), {
      edited_ws$weights_clean <- reset_ws$value
    }, ignoreInit = TRUE)
    
    # Step 6. Selected WS must be reflected in the WS name field
    selected_ws <- mod_wt_select_newsrv(NULL, edited_ws, curr_wt_name)
    
    # Step 7. Update PTI on selecting another ================================
    upd_wt <- mod_wt_upd_newsrv(NULL, edited_ws, curr_wt, selected_ws, reset_ws$invalidator)
    
    # TO DO!
    # == Upload PTI - Fix
    # == Doanload Weigths and Data modules.
    
    # Step 6. Upload weights from a file =====================================
    uploaded_ws <- mod_wt_uplod_newsrv(NULL, input_dta, ind_list)
    
    # Step 8. Download weights server =========================================
    mod_wt_dwnload_newsrv(NULL, input_dta = input_dta, edited_ws = edited_ws, 
                          plotted_dta = plotted_dta, filename_glue = "{.country}",
                          shapes_path = shapes_path, mtdtpdf_path = mtdtpdf_path)
    
    output$wt_tests_out <- renderPrint({
      list(
        edited_ws_weights_clean = edited_ws$weights_clean(),
        curr_wt_name = curr_wt_name(),
        curr_wt = curr_wt(),
        selected_ws = selected_ws()
      ) %>%
        str(max.level = 3)
    })
    
    # save_ws_out <-
    reactive({
      input_dta() %>%
        append(
          list(
            weights_clean = edited_ws$weights_clean(),
            indicators_list = edited_ws$indicators_list(),
            curr_wt = curr_wt()
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
              "Modify weights",
              icon = shiny::icon("warning", lib = "glyphicon"),
              class = "btn-warning btn-xs",
              width = "63%"
            ) %>%
              shinyjs::disabled() %>%
              current_btn_ui()
          } else {
            
            if (!isTruthy(curr_wt_name())) {
              actionButton(
                ns("weights.save"),
                label = "Provide a name",
                icon = shiny::icon("warning", lib = "glyphicon"),
                class = "btn-warning btn-xs",
                width = "63%"
              ) %>%
                shinyjs::disabled() %>%
                current_btn_ui()
            } else {
              actionButton(
                ns("weights.save"),
                "Save and plot PTI",
                icon = shiny::icon("save"),
                class = "btn-success btn-xs",
                width = "63%"
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
            "Modify weights",
            icon = shiny::icon("warning", lib = "glyphicon"),
            class = "btn-warning btn-xs",
            width = "63%"
          ) %>%
            shinyjs::disabled() %>%
            current_btn_ui()
        } else {
          
          if (curr_wt_name() %in% names(edited_ws$weights_clean())) {
            existing_values <- edited_ws$weights_clean()[[curr_wt_name()]]
            
            if (isTRUE(all.equal(existing_values, curr_wt(), convert = TRUE))) {
              actionButton(
                ns("weights.save"),
                "No changes to save",
                icon = shiny::icon("warning", lib = "glyphicon"),
                class = "btn-info btn-xs",
                width = "63%"
              ) %>%
                shinyjs::disabled() %>%
                current_btn_ui()
            } else {
              actionButton(
                ns("weights.save"),
                "Save changes and plot PTI",
                icon = shiny::icon("save"),
                class = "btn-success btn-xs",
                width = "63%"
              ) %>%
                current_btn_ui()
            }
          } else {
            actionButton(
              ns("weights.save"),
              "Save and plot new PTI",
              icon = shiny::icon("save"),
              class = "btn-success btn-xs",
              width = "63%"
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


#' @describeIn mod_wt_inp_ui  Reset/Delete WT UI button
#' 

mod_wt_delete_ui <- function(id = NULL,inputId = "weights.reset", label = "Reset PTI" ) {
  ns <- NS(id)
  actionButton(
    inputId = ns(inputId), 
    label = label,
    icon = shiny::icon("remove", lib = "glyphicon"),
    class = "btn-danger btn-xs",
    width = "35%"
  )
}


#' @describeIn mod_wt_inp_ui  Reset/Delete WT save server processor.
#' 

mod_wt_delete_newsrv <- function(id, edited_ws, curr_wt_name) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      return_ws <- reactiveValues(
        invalidator = reactiveVal(0),
        value = reactiveVal(NULL)
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
          (return_ws$invalidator() + 1) %>% return_ws$invalidator()
          if (length(out_wt) == 0) {
            return_ws$value(NULL) 
          } else {
            return_ws$value(out_wt)
          }
        })
      
      observeEvent(#
        input$weights.reset,
        {
          (return_ws$invalidator() + 1) %>% return_ws$invalidator()
          return_ws$value(NULL)
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
mod_wt_upd_newsrv <- function(id, edited_ws, curr_wt, selected_wt_name, reset_invalidator) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      upd_to_reactive <- reactiveVal()
      
      observeEvent(
        selected_wt_name(), 
        {
          if (!isTruthy(edited_ws$weights_clean())) {
            upd_to <-
              edited_ws$indicators_list() %>%
              select(var_code) %>%
              mutate(weight = 0L)
          } else {
            req(selected_wt_name() %in% names(edited_ws$weights_clean()))
            upd_to <- edited_ws$weights_clean()[[selected_wt_name()]]
          }
          req(!identical(upd_to, curr_wt()))
          upd_to %>% upd_to_reactive()
        }, ignoreInit = FALSE)
      
      observeEvent(
        reset_invalidator(),
        {
          upd_to <-
            edited_ws$indicators_list() %>%
            select(var_code) %>%
            mutate(weight = 0L)
          upd_to %>% upd_to_reactive()
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      upd_to_reactive
      
    })
  
}



#' @describeIn mod_wt_inp_ui  data download module for weights page
#' 
#' @noRd
#' 
mod_wt_dwnload_newsrv <- function(id, 
                                  input_dta = reactive(NULL),
                                  edited_ws = reactiveValues(weights_clean = reactive(NULL), 
                                                             indicators_list = reactive(NULL)),
                                  plotted_dta = reactive(NULL),
                                  filename_glue = "{.country} weights.xlsx",
                                  shapes_path = NULL,
                                  mtdtpdf_path = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      exp_dta <-
        prepare_export_data(input_dta, edited_ws$weights_clean, edited_ws$indicators_list,
                            plotted_dta, filename_glue = filename_glue)
      
      # Data download server
      mod_dwnld_dta_xlsx_server(NULL, "dta.download",
                                file_name = reactive(str_c("PTI data for ", exp_dta$file_name(), ".xlsx")),
                                dta_dwnld = reactive(list(exp_dta$exp_inputs()) %>% purrr::flatten()))
      
      # weights download server
      mod_dwnld_dta_xlsx_server(NULL, "weights.download",
                                file_name = reactive(str_c("PTI weights for ", exp_dta$file_name(), ".xlsx")),
                                dta_dwnld = reactive(
                                  list(exp_dta$exp_inputs(), exp_dta$exp_wght(), exp_dta$exp_pltdta()) %>%
                                    purrr::flatten()
                                ))
      
      # Metadata PDF file download
      mod_dwnld_file_server(NULL, "mtdt.files", mtdtpdf_path)
      
      # Metadata PDF file download
      mod_dwnld_file_server(NULL, "shp.files", shapes_path)
      
    })
}


#' @describeIn mod_wt_inp_ui prepare data for data downlaod 
#' 
#' @noRd
#' 
#' 
prepare_export_data <-
  function(input_dta = reactive(NULL),
           weights_clean = reactive(NULL),
           indicators_list = reactive(NULL),
           plotted_dta = reactive(NULL),
           filename_glue = "{.country} weights.xlsx") {
    
    out_list <- reactiveValues(
      file_name = reactiveVal(NULL),
      exp_inputs = reactiveVal(NULL),
      exp_wght = reactiveVal(NULL),
      exp_pltdta = reactiveVal(NULL),
      dta = reactive(NULL)
    )
    
    observeEvent(#
      input_dta(), {
        if (isTruthy(input_dta())) {
          input_dta() %>% fct_inp_for_exp() %>% out_list$exp_inputs()
        } else {
          out_list$exp_inputs(NULL)
        }
      })
    
    observeEvent(#
      list(weights_clean(), indicators_list()),
      {
        if (isTruthy(weights_clean()) && isTruthy(indicators_list())) {
          weights_clean() %>% fct_internal_wt_to_exp(indicators_list()) %>% out_list$exp_wght() %>% 
            str_c(., " ")
        } else {
          out_list$exp_wght(NULL)
        }
      })
    
    observeEvent(#
      plotted_dta(),
      {
        if (isTruthy(plotted_dta())) {
          plotted_dta() %>%
            get_pti_scores_export() %>%
            out_list$exp_pltdta()
        } else {
          out_list$exp_pltdta(NULL)
        }
      })
    
    observeEvent(#
      input_dta(),
      {
        if (isTruthy(input_dta()$general %>% unlist() %>% `[[`(1))) {
          .country <-
            input_dta()$general %>% unlist() %>% `[[`(1) %>% as.character()
        } else {
          .country <- ""
        }
        glue(filename_glue) %>% out_list$file_name()
      })
    
    out_list
    
  }



#' @describeIn mod_wt_inp_ui  TODO: Upload WS to the server
#' 
#' @noRd
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



