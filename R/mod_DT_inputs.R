#' dtNumInputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList div
#' @importFrom DT dataTableOutput
#' @importFrom glue glue
mod_DT_inputs_ui <- function(id, height = NULL, dt_style = NULL, ...){
  ns <- NS(id)
  if(is.null(height)) height <- "550px"
  
  DT::dataTableOutput(ns('wghts_dt'), height = height)  %>%
    shiny::tagList(shiny::tags$style(
      shiny::HTML(
        ".dtcenter .form-group {margin-bottom: 0px !important};
        .dtcenter {text-align: -webkit-center;};
        .dtcustom {padding: 2px 3px !important;};
        .td {padding: 2px'};
        "
      )
    )) %>%
    shiny::tagList(
      # .,
      # if (is.null(options("golem.app.prod")) || !isTRUE(options("golem.app.prod")[[1]]))
      #   shiny::verbatimTextOutput(ns('wghts_dt_values'))
      ) %>% 
    div(style = dt_style, ...)
}

#' dtNumInputs Server Functions
#'
#' @importFrom DT renderDataTable
#' @noRd 
mod_DT_inputs_server <- function(id, ind_list, update_dta = reactive(NULL)){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # # Step 1. Convert input data into the table-ready style ========================
    # ind_list <- reactive({req(input_dta()) %>% get_indicators_list()})
    
    # Step 2. Generate inputs UI and render it. ========================
    ind_DT <- reactive({ind_list() %>% make_input_DT(ns = ns)})
    
    # Step 3. Render the table and debugging data ======================
    output$wghts_dt <- DT::renderDataTable(
      ind_DT(),
      server = FALSE
    )
    
    # Step 4. make update buttons work ==================================
    mod_wt_btns_srv(NULL, ind_list,  dtn_id = "_set_zero", to_value = 0)
    mod_wt_btns_srv(NULL, ind_list,  dtn_id = "_set_one", to_value = 1)
    
    # Step 5. Collect current values ======================================
    current_values <- 
      mod_collect_wt_srv(NULL, ind_list) %>% 
      throttle(500)
    
    # Step 6. Updated values based on some pre-loaded para ===================
    observe({
      req(update_dta())
      update_dta() %>% 
        pwalk(~ {
          # browser()
          updateNumericInput(session = session, inputId = ..1, value = ..2)
        })
    })
    
    
    # observeEvent(update_dta(), {
    #   # browser()
    #   update_dta() %>% 
    #     pwalk(~ {
    #       browser()
    #       updateNumericInput(session = session, inputId = ..1, value = ..2)
    #       })
    # }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # Return diagnostic data.
    output$wghts_dt_values = renderPrint({
      current_values() %>% glimpse()
    })
    
    current_values
    
  })
}
    
## To be copied in the UI
# mod_DT_inputs_ui("dtNumInputs_ui_1")
    
## To be copied in the server
# mod_dtNumInputs_server("dtNumInputs_ui_1")

#' Adds an action buttons for resetting pillar-specific variables
#' @noRd
add_two_action_btn <- function(id, ns) {
  
  btn_one <- 
    str_c("pillar_", id, "_set_one") %>%
    ns() %>%
    actionButton(.,
                 "All 1",
                 class = "btn-primary btn-xs",
                 width = "40%",
                 style = "margin-top: 0px;")
  
  btn_two <- 
    str_c("pillar_", id, "_set_zero") %>%
    ns() %>%
    actionButton(.,
                 "All 0",
                 class = "btn-primary btn-xs",
                 width = "40%",
                 style = "margin-top: 0px; padding: 1px;")
  
  tagList(btn_one, btn_two)
  
}


#' convert ind_list to the data frame with the UI suitable for render as a data table.
#' 
#' @noRd
#' @importFrom glue glue
prep_input_data <- function(ind_list, ns) {
  ind_list %>% 
    mutate(
      var_adm_levels = map_chr(admin_levels_years, ~ {str_c(.x$admin_level_name, collapse = ", ")}),
      var_years = map_chr(admin_levels_years, ~ {
        if (length(.x$years) > 0 && !(identical(logical(0), unlist(.x$years)) | 
                                      identical(numeric(0), unlist(.x$years)))) {
          str_c(.x$years, collapse = ", ")
        } else {
          NA_character_
        }
      }) %>% 
        ifelse(is.na(.), ., str_c("Available year(s): ", ., "</br>")),
      var_description = map(var_description, ~shiny::markdown(.x)),
      tooltip_text = 
        glue("<strong>{var_name}</strong><p>{var_description}</p>",
             "Admin level(s): {var_adm_levels}</br>{var_years}",
             "Available year(s): {var_adm_levels}</br>"),
      var_description = tooltip_text,
      pillar_description = as.character(pillar_description),
      pillar_description = map(pillar_description, ~shiny::markdown(.x)),
      pillar_description = glue("<strong>{pillar_name}</strong><p>{pillar_description}</p>")
    ) %>% 
    select(-tooltip_text, -var_adm_levels, -var_years) %>% 
    arrange(pillar_group, var_order) %>% 
    group_by(pillar_group, pillar_name, pillar_description) %>% 
    nest() %>% 
    pmap_dfr(.f = function(...){
      dts <- rlang::dots_list(...)
      # browser()
      tibble(var_name = dts$pillar_name,
             var_code = dts$pillar_group, 
             var_description = dts$pillar_description,
             type = "pillar") %>% 
        mutate(across(everything(), ~as.character(.))) %>% 
        bind_rows(
          dts$data %>% 
            mutate(type = "variable") %>% 
            mutate(across(everything(), ~as.character(.)))
          ) %>% 
        select(var_name, var_code, var_description, type) %>% 
        mutate(pillar = 
                 c(dts$pillar_name, dts$pillar_description) %>% 
                 str_replace_na("") %>% 
                 str_c(sep = " ", collapse = " "))
    }) %>% 
    mutate(
      ui = case_when(
        type == "pillar" ~
          map_chr(var_code, ~{
            add_two_action_btn(.x, ns = ns) %>% as.character()
          } ),
        type == "variable" ~ 
          map_chr(var_code, ~{
            numericInput(ns(.x), label = NULL, value = 0, step = 1, width = "100%") %>% 
              as.character()
          }),
        FALSE ~ ""
      ) 
    )
  # %>% 
  #   mutate(
  #     var_name = 
  #       map2(var_description, var_name,                   
  #            ~{
  #              tagList(
  #                .y,  
  #                tippy::tippy(
  #                  '<i class="fa fa-info-circle"></i>',
  #                  tooltip = .x %>% htmltools::HTML(.),
  #                  placement = "top",
  #                  theme = "light-border",
  #                  arrow = "round",
  #                  animation = "shift-away",
  #                  interactive = TRUE,
  #                  allowHTML = TRUE
  #                ) 
  #              )
  #            })
  #   )
}


#' helper to define visible and invisible targets for the inputs datatable parameters
#' 
#' @noRd
make_vis_targets_for_dt <- function(nested_dta) {
  # Getting columns that are visible and invisible
  visible_vars <-
    names(nested_dta) %>%
    set_names(seq_along(.)-1, .) %>%
    `[`(names(.) %in% c("var_name", "ui"))
  
  invisible_vars <-
    names(nested_dta) %>%
    set_names(seq_along(.)-1, .) %>%
    `[`(!names(.) %in% c("var_name", "ui"))
  
  # browser()
  visible_targets <-
    visible_vars %>%
    unname() %>%
    list(
      c("55%", rep("45%", length(.)-1)),
      c("150px", rep("100px", length(.)-1))
    ) %>% 
    pmap(.,~{list(targets=c(..1), visible=TRUE, width = ..2
                  # , `max-width` = ..3
                  )})
  
  visible_targets[[length(visible_targets)]]$className <- c("dtcustom dtcenter")
  visible_targets[[1]]$className <- c("dtcustom")
  
  invisible_targets <-
    invisible_vars%>%
    unname() %>%
    c() 
  
  invisible_targets <- 
    list(targets=c(invisible_targets), visible=FALSE, searchable = TRUE, width="0px")
  
  colnames <- 
    nested_dta %>% 
    names() %>% 
    setNames(., rep("", length(.)))
  
  list(
    columnDefs = append(list(invisible_targets), visible_targets),
    colnames = colnames
  )
  
}


#' Preparing DT-ready table based on indicators list
#' 
#' @param ind_list list of tibbles based on stadradised PTI inputs prepares with
#'   `get_indicators_list()`
#'   
#' @description  More, on who we wrote it. Some help with css 
#'   http://live.datatables.net/qocanadu/44/edit
#'   We used scrollResize from https://datatables.net/blog/2017-12-31
#'   
#' @importFrom DT datatable formatStyle styleEqual
#' @importFrom htmlwidgets JS
#' @noRd
make_input_DT <- function(ind_list, ns = function(x) x, width = "100%", height = "100%", scrollY="450px") {
  
  nested_dta <- prep_input_data(ind_list, ns = ns)
  targets_dta <- make_vis_targets_for_dt(nested_dta)
  
  nested_dta %>% 
    datatable( 
      width = width,
      height = height,
      escape = FALSE, 
      selection = 'none',
      fillContainer = F,
      rownames = NULL,
      colnames = NULL,
      # extensions = c('Scroller'),
      plugins = c('scrollResize'),
      options = list(
        dom = 'ft',
        bPaginate = FALSE,
        columnDefs = targets_dta$columnDefs,
        ordering = FALSE,
        autoWidth = F,
        
        # scrollResize potions
        paging = FALSE,
        scrollResize = TRUE, 
        scrollY =  100,
        scrollCollapse = TRUE,
        
        headerCallback = JS(
          "function(thead, data, start, end, display){
          $('th', thead).css('display', 'none');
          }"
        )
      #   paging = TRUE,
      #   
      #   columnDefs = targets_dta$columnDefs,
      #   # deferRender = TRUE,
      #   scrollY = scrollY,
      #   # scrollX = FALSE,
      #   scroller = TRUE,
      #   # scrollCollapse = TRUE
      ),
      callback = JS("table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());")
    ) %>% 
    formatStyle(
      'type',
      target = 'row',
      backgroundColor = styleEqual("pillar", c('lightgray')),
      fontWeight = styleEqual("pillar", c('bold')),
    ) 
  # %>% 
  #   tagList(
  #     tags$style(HTML(".dataTables_scrollBody{position: relative; overflow: auto; width: 100%; max-height: 90% !important; height: auto!important;}"))
  #   )
  #   div(class = "DTcontainer",
  #       style = "display: flex;flex-direction: column;
  #       height: 100%;width: 100%;padding: 16px;")
  # 
}
