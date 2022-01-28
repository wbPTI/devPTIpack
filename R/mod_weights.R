#' weights UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export
#'
#' @importFrom shiny NS tagList 
#' 
mod_weights_ui <- function(id, id_pti = "weight_page_leaf", full_ui = FALSE){
  ns <- NS(id)
  if (full_ui) {
    controls_col <- full_wt_inp_ui(ns)
  } else {
    controls_col <- short_wt_inp_ui(ns)
  }
  
  tagList(
    column(width = 3, controls_col),
    column(
      width = 9,
      style = "padding-right: 0px; padding-left: 15px; margin-bottom: 10px;",
      fluidRow(
        mod_map_pti_leaf_ui(id_pti, height = '53vh') %>%
          div(id = 'step_8_map_inspection1') %>%
          div(id = 'step_8_map_inspection2') %>% 
          div(class = 'pti-map-field')
      )
    ) ,
    fluidRow(#
      mod_weights_rand_ui(id),
      div(id = "step_5_modify_weights2", style = "left: 10px; right: 10px;"),
      div(id = "step_5_modify_weights3", style = "width: 100%"),
      ns("weights_fields") %>% 
        htmlOutput() %>% 
        div(class = "weights-field") %>% 
        column(width = 12)
      ),
    div(
      id = "step_5_modify_weights",
      style = "position: absolute; top: calc(65vh + 60px); left: 0; right: 0; bottom: 0; z-index: -99999"
    ),
    div(
      id = "step_5_modify_weights2",
      style = "position: absolute; top: calc(65vh + 60px); left: 0; right: 0; bottom: 0; z-index: -99999"
    )
  )
}


    
#' weights Server Function
#'
#' @noRd 
mod_weights_server <- function(id, imported_data, export_recoded_data){
  # ns <- session$ns
  
  edited_ws <- reactiveVal()

  observeEvent(imported_data(), imported_data() %>% edited_ws())
  
  pti_indicators <- mod_indicarots_srv(id, imported_data)
  mod_gen_wt_inputs_srv(id, pti_indicators)

  mod_wt_btns_srv(id, pti_indicators,  dtn_id = "_set_zero", to_value = 0)
  mod_wt_btns_srv(id, pti_indicators,  dtn_id = "_set_one", to_value = 1)

  # Upload WS module
  uploaded_ws <- mod_wt_uplod_srv(id, imported_data, pti_indicators)
  observeEvent(uploaded_ws(), {
    # browser()
    
    uploaded_ws() %>% edited_ws()})
  
  # Delete WS module
  deleted_ws <- mod_wt_delete_srv(id, edited_ws, current_ws_name)
  # observeEvent(deleted_ws(), {deleted_ws() %>% edited_ws()})
  
  # Select WS out of existing 
  selected_ws_name <- mod_wt_select_srv(id, deleted_ws)
  
  # Enter WS name 
  current_ws_name <- mod_wt_name_srv(id, selected_ws_name)
  
  
  # Updating weights based on the selected WS
  mod_wt_fill_srv(id, pti_indicators, deleted_ws, selected_ws_name)
  
  # collecting currently entered values
  current_ws_values <- mod_collect_wt_srv(id, pti_indicators)
  
  # Save WS module
  save_ws <- mod_wt_save_srv(id, pti_indicators, deleted_ws, current_ws_name, current_ws_values)
  observeEvent(save_ws(), {
    # browser()
    save_ws() %>% edited_ws()})
  
  # Download weights server
  mod_download_wt_srv(id, edited_ws, export_recoded_data)
  
    
  observe({
    # req(imported_data())
    # req(deleted_ws())
    # selected_ws_name()
    # req(current_ws_values())
    # browser()
  })
  
  reactive(list(save_ws = save_ws, 
                current_ws_values = current_ws_values))
}



# Generate wights inputs --------------------------------------------------

#' module that produces a list of indicators. 
mod_indicarots_srv <- function(id, imported_data, fltr_var = "fltr_exclude_pti") {
  moduleServer(#
    id,
    function(input, output, session) {
      reactive(#
        {
          req(imported_data())
          get_indicators_list(imported_data(), fltr_var)
          # 
          # imported_list <- imported_data()
          # meta_dta <-
          #   imported_list$metadata  %>%
          #   filter(!fltr_exclude_pti) %>%
          #   select(-contains("fltr")) %>% 
          #   arrange(pillar_group, var_order)
          # element_names <- imported_list %>% names()
          # 
          # pillars <-
          #   meta_dta %>%
          #   distinct_at(vars(contains("pillar"))) %>%
          #   arrange(pillar_group, pillar_name, pillar_description) %>%
          #   group_by(pillar_group) %>%
          #   filter(row_number() == 1) %>%
          #   ungroup() %>%
          #   arrange(pillar_group)
          # 
          # spatial_levels_names <-
          #   element_names %>%
          #   magrittr::extract(str_detect(., "\\D{1,}\\d{1}_")) %>%
          #   str_split("_") %>%
          #   transpose() %>%
          #   set_names(c("admin_level", "admin_level_name")) %>%
          #   map( ~ unlist(.x)) %>%
          #   as_tibble()
          # 
          # used_vars <- meta_dta$var_code %>% unique()
          # 
          # levels_with_data <-
          #   spatial_levels_names$admin_level %>%
          #   map_lgl(~ {
          #     rows <-
          #       imported_list %>%
          #       magrittr::extract(names(.) %>% str_detect(regex(.x, ignore_case = T))) %>%
          #       magrittr::extract2(1) %>%
          #       select(any_of(c("year", used_vars))) %>%
          #       length()
          #     rows != 0
          #   })
          # 
          # if (any(!levels_with_data)) {
          #   spatial_levels_names[!levels_with_data,]$admin_level %>%
          #     walk( ~ {
          #       # browser()
          #       remove <-
          #         imported_list %>%
          #         names() %>%
          #         magrittr::extract(str_detect(., .x))
          #       imported_list[[remove]] <- NULL
          #     })
          #   
          #   spatial_levels_names <-
          #     spatial_levels_names[levels_with_data, ]
          #   
          # }
          # vars_admins <-
          #   spatial_levels_names$admin_level %>%
          #   map(~ {
          #     imported_list %>%
          #       magrittr::extract(names(.) %>% str_detect(regex(.x, ignore_case = T))) %>%
          #       magrittr::extract2(1) %>%
          #       select(any_of(c("year", used_vars))) %>%
          #       tidyr::pivot_longer(
          #         cols = any_of(used_vars),
          #         names_to = "var_code",
          #         values_to = "val"
          #       ) %>%
          #       filter(!is.na(val)) %>%
          #       distinct_at(vars(any_of(c(
          #         "year", "var_code"
          #       )))) %>%
          #       mutate(admin_level = .x)
          #     
          #   }) %>%
          #   bind_rows() %>%
          #   left_join(spatial_levels_names, by = "admin_level") %>%
          #   group_by_at(vars(any_of(
          #     c("var_code", "admin_level", "admin_level_name")
          #   ))) %>%
          #   tidyr::nest() %>%
          #   mutate(years = map(data, ~ {
          #     as.vector(.x[[1]]) %>% sort() %>% unlist()
          #   })) %>%
          #   select(-data) %>%
          #   group_by(var_code) %>%
          #   tidyr::nest() %>%
          #   rename(admin_levels_years = data)
          # 
          # meta_dta %>%
          #   select(-contains("pillar_group"),-contains("pillar_descr"),-contains("spatial")) %>%
          #   semi_join(vars_admins, by = "var_code") %>%
          #   left_join(vars_admins, by = "var_code") %>%
          #   left_join(pillars, by = "pillar_name")
          # 
        })
      
    })
  
}



mod_gen_wt_inputs_srv <- function(id, pti_indicators) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      indicators_UI <-
        reactive({
          req(pti_indicators())
          # browser()
          
          box_input <-
            pti_indicators()  %>%
            arrange_at(vars(contains("pillar_group"), any_of("var_order"))) %>%
            group_by_at(vars(contains("pillar"))) %>%
            tidyr::nest() %>%
            # pull(data) %>%
            pmap( ~ {
              dots <- rlang::dots_list(...)
              out_tags <-
                dots$data %>%
                # box_input$data[[1]] %>%
                mutate(nn = row_number()) %>%
                group_by(nn) %>%
                tidyr::nest() %>%
                pull(data) %>%
                map( ~ {
                  admin_level <-
                    str_c(.x$admin_levels_years[[1]]$admin_level_name,
                          collapse = ", ")
                  numericInput(
                    ns(.x$var_code),
                    str_c(.x$var_name), #, ", ", .x$var_units) ,
                    value = 0,
                    # min = 0,
                    step = 1
                  ) %>%
                    bsplus::shinyInput_label_embed(
                      tippy::tippy(
                        # icon("info-circle"),
                        '<i class="fa fa-info-circle"></i>',
                        tooltip =
                          str_c(
                            "<p>",
                            .x$var_name,
                            "</br>",
                            shiny::markdown(.x$var_description),
                            "</br>",
                            "</br>",
                            "Admin. level: ",
                            admin_level,
                            "</br>",
                            "</p>"
                          ) %>%
                          htmltools::HTML(.),
                        #p(style = "font-size: 12px;", str_c(oneind$var_description)),
                        placement = "top",
                        theme = "light-border",
                        arrow = "round",
                        animation = "shift-away",
                        interactive = TRUE,
                        allowHTML = T
                      )
                    )
                })
              # browser()
              list(
                tags_number = length(out_tags),
                out_tags =
                  wellPanel(
                    #
                    id = if (dots$pillar_group == 1)
                      "step_5_modify_weights"
                    else
                      NULL,
                    fluidRow(
                      #
                      column(8, h4(dots$pillar_name)),
                      column(
                        2,
                        actionButton(
                          ns(str_c(
                            "pillar_", dots$pillar_group, "_set_one"
                          )),
                          "All to One",
                          class = "btn btn-primary btn-sm",
                          width = "100%",
                          style = "margin-top: 5px;"
                        )
                      ),
                      
                      column(
                        2,
                        actionButton(
                          ns(str_c(
                            "pillar_", dots$pillar_group, "_set_zero"
                          )),
                          "All to Zero",
                          class = "btn btn-primary btn-sm",
                          width = "100%",
                          style = "margin-top: 5px;"
                        )
                      )
                    ),
                    fluidRow(column(12, tagList(out_tags)))
                  )
              )
            }) %>%
            purrr::transpose()
          
          dta_tbbl <-
            tibble(
              order = seq_along(box_input$tags_number),
              tags = box_input$out_tags,
              tags_number = box_input$tags_number %>% unlist
            )
          
          if (nrow(dta_tbbl) > 2) {
            dta_tbbl <-
              dta_tbbl %>%
              dplyr::arrange(tags_number) %>%
              dplyr::mutate(cum_order = cumsum(tags_number)) %>%
              dplyr::arrange(desc(tags_number)) %>%
              dplyr::mutate(cum_rev_order = cumsum(tags_number)) %>%
              dplyr::mutate(diff = -cum_order +  cum_rev_order) %>%
              dplyr::arrange(order) %>%
              dplyr::mutate(diff = diff < 0) %>%
              dplyr::group_by(diff) %>%
              tidyr::nest() %>%
              dplyr::pull(data) %>%
              purrr::map( ~ {
                .x$tags %>%
                  column(width = 6)
              }) %>%
              fluidRow()
          } else {
            dta_tbbl <-
              dta_tbbl %>%
              mutate(diff = row_number()) %>%
              dplyr::group_by(diff) %>%
              tidyr::nest() %>%
              dplyr::pull(data) %>%
              purrr::map( ~ {
                .x$tags %>%
                  column(width = 6)
              }) %>%
              fluidRow()
          }
          
          tagList(dta_tbbl)
        })
      
      output$weights_fields <-
        renderUI({
          req(indicators_UI())
          # showNotification("PTI weights are loaded.",
          #                  type = "message",
          #                  duration = 5)
          # indicators_UI()
        })
    })
  
}


# To zero/one btn observers -----------------------------------------------

mod_wt_btns_srv <- function(id, pti_indicators, dtn_id = "", to_value = 0) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      btn_groups <- 
        reactive({
          req(pti_indicators())
          pti_indicators()%>% 
            distinct(pillar_group,  var_code) %>% 
            group_by(pillar_group) %>% 
            tidyr::nest() %>% 
            ungroup() %>% 
            as.list() %>% 
            transpose() %>% 
            map(~{
              .x$input_codes <- pull(.x$data, var_code)
              .x$data <- NULL
              .x
            })
        })
      
      change_trigger <- reactive({
        req(btn_groups())
        # browser()
        out_trigers <-
          btn_groups() %>%
          map( ~ {
            .x$triger_id <- .x$pillar_group %>%
              str_c("pillar_", ., dtn_id)
            .x$triger_value <- input[[.x$triger_id]]
            .x
          })
        if (all(map_lgl(out_trigers, ~ !is.null(.x$triger_value))))
          return(out_trigers)
        else
          return(NULL)
      })
      
      pillars_zero_values <- reactiveVal(NULL)
      
      observeEvent(#
        change_trigger(), #
        {
          # browser()
          if (!isTruthy(pillars_zero_values())) {
            change_trigger() %>% pillars_zero_values()
          }
          
          if (isTruthy(pillars_zero_values())) {
            change_trigger() %>%
              map2(pillars_zero_values(), ~ {
                if (.x$triger_value > .y$triger_value) {
                  .y$triger_value <- .x$triger_value
                  .y$input_codes %>%
                    walk( ~ updateNumericInput(session, .x, value = to_value))
                }
                .y
              }) %>%
              pillars_zero_values()
          }
        }, ignoreInit = FALSE)
      
    })
  
}

# Weight controls observers ========================================


mod_wt_name_srv <- function(id, selected_ws_name) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(#
        selected_ws_name(),
        {
          if (isTruthy(selected_ws_name())) {
            updateTextInput(session, "existing.weights.name", value = selected_ws_name())
          } else {
            updateTextInput(session, "existing.weights.name", value = "")
          }
        },
        ignoreNULL = FALSE,
        ignoreInit = TRUE)
      
      reactive(input$existing.weights.name)
    })
  
}


mod_wt_select_srv <- function(id, edited_ws) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      selected_ws_name <- reactiveVal(NULL)
      
      existing_ws <- reactive({
        # browser()
        # req(edited_ws())
        edited_ws()$weights_clean %>% names()
      })
      
      observe({
        if (!isTruthy(existing_ws())) {
          updateSelectInput(session, "existing.weights", choices = "")
          updateSelectInput(session, "existing.weights", selected = "")
          # shinyjs::disable(ns("existing.weights"), asis = TRUE)
          shinyjs::hide(ns("existing.weights"), asis = TRUE, anim = TRUE)
          isolate(selected_ws_name(NULL))
        } else {
          # shinyjs::enable(ns("existing.weights"), asis = TRUE)
          
          if (length(existing_ws()) <= 1) {
            shinyjs::hide(ns("existing.weights"), asis = TRUE, anim = TRUE)
          } else {
            shinyjs::show(ns("existing.weights"), asis = TRUE, anim = TRUE)
          }
          
          updateSelectInput(session, "existing.weights", choices = existing_ws())
          isolate({
            if (isTruthy(selected_ws_name())) {
              if (selected_ws_name() %in% existing_ws())
                updateSelectInput(session, "existing.weights", selected = selected_ws_name())
              else
                updateSelectInput(session, "existing.weights", selected = existing_ws()[[1]])
            } else {
              updateSelectInput(session, "existing.weights", selected = existing_ws()[[1]])
            }
          })
        }
        
      })
      
      observeEvent(input$existing.weights, {
        req(input$existing.weights)
        selected_ws_name(input$existing.weights)
      })
      
      reactive(selected_ws_name())
    })
  
}


# Wt upload srvr ----------------------------------------------------------

mod_wt_uplod_srv <- function(id, imported_data, pti_indicators) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      return_wt <- reactiveVal()
      observeEvent(imported_data(), imported_data() %>% return_wt())
      observeEvent(#
        input$weights_upload,
        {
          uploaded_weights <- fct_template_reader(input$weights_upload$datapath)
          out <- imported_data()
          out$timestamp <- Sys.time()
          out$weights_table <- 
            uploaded_weights$weights_table %>% 
            filter(var_code %in% pti_indicators()$var_code)
          out$weights_clean <- 
            uploaded_weights$weights_clean %>% 
            imap(~{
              .x %>% 
                filter(var_code %in% pti_indicators()$var_code)
            })
          return_wt(out)
        }, 
        ignoreInit = TRUE, 
        ignoreNULL = FALSE)
      reactive({return_wt()})
    })
}


# Wt delete srvr ----------------------------------------------------------


mod_wt_delete_srv <- function(id, edited_ws, current_ws_name) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      return_wt <- reactiveVal()
      
      observeEvent(edited_ws(), {
        edited_ws() %>% return_wt()
        if (!isTruthy(edited_ws()$weights_clean)) {
          shinyjs::hide(id = "weights.delete", anim = TRUE)
          shinyjs::hide(id = "weights.reset", anim = TRUE)
        } else {
          shinyjs::show(id = "weights.delete", anim = TRUE)
          shinyjs::show(id = "weights.reset", anim = TRUE)
        }
      }, 
      ignoreNULL = FALSE, 
      ignoreInit = FALSE)
      
      observe({
        req(current_ws_name())
          if (current_ws_name() %in% names(edited_ws()$weights_clean)) {
            # shinyjs::show(id = "weights.delete", anim = TRUE)
            shinyjs::enable(id = "weights.delete")
            shinyjs::enable(id = "weights.reset")
          } else {
            # shinyjs::hide(id = "weights.delete", anim = TRUE)
            shinyjs::disable(id = "weights.delete")
            shinyjs::disable(id = "weights.reset")
          }
        })
      
      observeEvent(#
        input$weights.delete,
        {
          req(input$weights.delete)
          req(current_ws_name() %in% names(return_wt()$weights_clean))
          out <- return_wt() 
          out$weights_clean[[current_ws_name()]] <- NULL
          if (length(out$weights_clean) == 0) {
            out$weights_clean <- NULL
            shinyjs::hide(id = "weights.delete", anim = TRUE)
          } 
          return_wt(out)
        }, 
        ignoreInit = TRUE)
      
      observeEvent(#
        input$weights.reset,
        {
          req(input$weights.reset)
          # browser()
          req(current_ws_name() %in% names(return_wt()$weights_clean))
          out <- return_wt() 
          out$weights_clean <- NULL
          shinyjs::hide(id = "weights.reset", anim = TRUE)
          # if (length(out$weights_clean) == 0) {
          #   out$weights_clean <- NULL
          # } 
          return_wt(out)
        }, 
        ignoreInit = TRUE)
      
      reactive({return_wt()})
    })
}


# Wt fill srvr ----------------------------------------------------------

mod_wt_fill_srv <- function(id, pti_indicators, edited_ws, selected_ws_name) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(#
        selected_ws_name(), {
          if (isTruthy(edited_ws()$weights_clean)) {
            if (selected_ws_name() %in% names(edited_ws()$weights_clean)) {
              edited_ws()$weights_clean[[selected_ws_name()]] %>% 
                pwalk( ~ updateNumericInput(session, inputId = .x, value = .y))
            }
          } else {
            pti_indicators()$var_code %>% 
              walk( ~ updateNumericInput(session, inputId = .x, value = 0))
          }
        }, 
        ignoreNULL = FALSE, 
        ignoreInit = TRUE)
      
    })
}

# WT Save srvr ----------------------------------------------------------------

mod_wt_save_srv <- function(id, pti_indicators, after_delete_ws, current_ws_name, current_ws_values) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      current_btn_ui <- reactiveVal()
      save_type <- reactiveVal()
      return_wt <- reactiveVal()
      
      
      observeEvent(after_delete_ws(), {
        out <- after_delete_ws() 
        out$indicators_list <- pti_indicators()
        return_wt(out)
      }, 
      ignoreNULL = FALSE, 
      ignoreInit = FALSE)
      
      # No name provided
      observeEvent(
        current_ws_name(),
        {
        if (!isTruthy(current_ws_name())) {
          actionButton(
            ns("weights.save"),
            "Provide name for your PTIs",
            icon = icon("exclamation-triangle"),
            class = "btn-warning",
            width = "100%"
          ) %>%
            shinyjs::disabled() %>% 
            current_btn_ui()
        } else {
          actionButton(
            ns("weights.save"),
            "Save and plot your PTI",
            icon = icon("save"),
            # class = "btn-success",
            width = "100%"
          ) %>%
            shinyjs::disabled()%>% 
            current_btn_ui()
        }
      }, 
      ignoreInit = FALSE, 
      ignoreNULL = FALSE)
      
      
      observe({
        req(after_delete_ws())
        req(current_ws_name())
        req(current_ws_values())
        req(length(after_delete_ws()$weights_clean) > 0)
        
        if (current_ws_name() %in% names(after_delete_ws()$weights_clean)) {
          existing_values <- after_delete_ws()$weights_clean[[current_ws_name()]]
          if (isTRUE(all_equal(existing_values, current_ws_values(), convert = TRUE))) {
            actionButton(
              ns("weights.save"),
              "No changes to save",
              # icon = icon("exclamation-triangle"),
              class = "btn-info",
              width = "100%"
            ) %>%
              shinyjs::disabled() %>%
              current_btn_ui()
          } else {
            actionButton(
              ns("weights.save"),
              "Save changes and plot existing PTI",
              icon = icon("save"),
              class = "btn-success",
              width = "100%"
            ) %>%
              current_btn_ui()
            save_type("save_change")
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
          save_type("save_new")
        }
        
      })
      
      
      observe({
        req(after_delete_ws())
        req(current_ws_name())
        req(current_ws_values())
        req(length(after_delete_ws()$weights_clean) == 0)
        
        save_type("save_first")
        
        actionButton(
          ns("weights.save"),
          "Save and plot PTI",
          icon = icon("save"),
          class = "btn-success",
          width = "100%"
        ) %>% 
          current_btn_ui()
      })
      
      
      observeEvent(#
        input$weights.save,
        {
          out <- return_wt()
          
          out$indicators_list <- pti_indicators()
          if (save_type() %in% c("save_first", "save_new")) {
            out$weights_clean <-
              out$weights_clean %>%
              append(set_names(list(current_ws_values()), current_ws_name()))
          } else  if (save_type() %in% c("save_change")) {
            out$weights_clean[[current_ws_name()]] <- (current_ws_values())
          }
          out %>%  return_wt()
        })
      
      output$save_btn <- renderUI({current_btn_ui()})
      
      reactive(return_wt())
    })
}

# WT Collect srvr ------------------------------------------------------------


mod_collect_wt_srv <- function(id, pti_indicators) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      reactive({
        if (isTruthy(pti_indicators())) {
          # browser()
          pti_indicators()$var_code %>%
            map_dfr(~ {
              tibble(var_code = .x,
                     weight =  ifelse(!isTruthy(input[[.x]]), NA_real_, input[[.x]]))
            }) %>%
            return()
        } else {
          return(NULL)
        }
      })
    })
}



# WT download srvr ------------------------------------------------------------


mod_download_wt_srv <- function(id, edited_ws, export_recoded_data) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(
        edited_ws(), {
          
          if (length(edited_ws()$weights_clean) == 0 | !isTruthy(edited_ws()$weights_clean)| !isTruthy(edited_ws())) {
            shinyjs::hide("weights.download", anim = TRUE)
          }
          
          
          if (length(edited_ws()$weights_clean) > 0 & isTruthy(edited_ws()$weights_clean)) {
            shinyjs::show("weights.download", anim = TRUE)
          }
          
        }, ignoreNULL = FALSE, ignoreInit = FALSE)
      
      
      # observeEvent(
      #   input$weights.download, {
      #     
      #     browser()
      #     
      #   }, ignoreNULL = FALSE, ignoreInit = TRUE)
      
      output$weights.download <- downloadHandler(
        filename = function() {
          paste(edited_ws()$general$country,
                '--pti-data-and-weights--',
                Sys.Date(),
                '.xlsx',
                sep = '')
        },
        content = function(con) {
          # 
          # # browser()
          # if (isTruthy(edited_ws()$weights_clean)) {
          #   out_data <- edited_ws()
          #   out_data$weights_table  <-
          #     out_data$weights_clean %>%
          #     fct_convert_clean_to_weight()
          #   out_data$weights_clean <- NULL
          # } else {
          #   out_data <- edited_ws()
          #   out_data$weights_table <-
          #     tibble(var_code = edited_ws()$metadata$var_code)
          #   out_data$weights_clean <- NULL
          # }
          # 
          # if (isTruthy(export_recoded_data())) {
          #   # browser()
          #   out_data <- 
          #     export_recoded_data() # %>% 
          #     # unlist(recursive = F) %>% 
          #     # prepend(out_data)
          #   
          # } 
          # # browser()
          # out_data$indicators_list <- NULL
          # out_data$current_weight <- NULL
          # out_data$point_data <- NULL
          # out_data$timestamp <- NULL
          # out_data$pti_scores_Zones <- NULL
          # out_data$weights_table  <- 
          #   out_data$weights_table %>% 
          #   left_join(out_data$metadata %>% 
          #               select(var_code, var_name), "var_code") %>% 
          #   select(contains("var_"), everything())
          # out_all <- 
          #   out_data %>% 
          #   magrittr::extract(!str_detect(names(out_data), "admin")) %>% 
          #   magrittr::extract(!str_detect(names(out_data), "metadata")) %>% 
          #   magrittr::extract(!str_detect(names(out_data), "point_data")) %>% 
          #   keep(~!is.null(.x))
          
          writexl::write_xlsx(export_recoded_data(), con)
          
        }
      )
    })
}



mod_export_recoded_srv <- function(id, map_data, main_pallette) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      
      reactive({
        req(map_data())
        req(main_pallette())
        isolate({
          names_and_levels <-
            map_data() %>% names() %>% magrittr::extract(str_detect(., "admin"))
          
          if (isTruthy(map_data())) {
            # browser()
            out_data <-
              # globr[[pti_clean_list]] %>%
              map_data() %>%
              imap(~ {
                # browser()
                # one_pti <- .x$pti_data
                
                new_name <-
                  .x$admin_level %>% names() %>%
                  str_c("pti_scores_", .)
                
                .x$pti_data %>%
                  st_drop_geometry() %>%
                  select(-contains("pti_label")) %>%
                  list() %>%
                  append(list(
                    pti_names = names(.x$pti_codes),
                    pti_values = .x$pti_codes
                  ) %>%
                    transpose()) %>%
                  reduce2(main_pallette(), function(xx, yy, zz) {
                    # browser()
                    new_var <- str_c(yy$pti_values, "_priority")
                    from_var <- sym(yy$pti_values)
                    xx %>%
                      rename_at(vars(contains(yy$pti_names)), list(~ yy$pti_values)) %>%
                      mutate(!!new_var :=  zz$recode_function(!!from_var) %>% as.character())
                  }) %>%
                  list() %>%
                  set_names(nm = new_name)
              }) %>%
              unname() %>%
              unlist(recursive = F)
            
          }
          
        })
        
      })
      
    })
  
}
