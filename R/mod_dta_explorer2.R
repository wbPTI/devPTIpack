#' dta_explorer2 module for plotting data explorer map based on the PTI inputs.
#'
#' @export
#' @import shiny
#' 
mod_dta_explorer2_server <- 
  function(id, shp_dta, input_dta, active_tab, target_tabs, 
           metadata_path = NULL,  ...) {
    
    # Check if the tab is opened at first
    first_open <- mod_first_open_count_server(id, active_tab, target_tabs)
    
    # Extracting PTI 
    indicators_list <- mod_indicarots_srv(id, input_dta, "fltr_exclude_explorer")
    
    # Clean explorer data
    pre_map_dta_1 <- reactive({
      req(indicators_list())
      input_dta() %>% 
        pivot_pti_dta(indicators_list()) %>% 
        reshaped_explorer_dta(indicators_list()) %>% 
        structure_pti_data(shp_dta()) %>% 
        preplot_reshape_wghtd_dta() 
    })
    
    var_choices <- reactive({req(indicators_list()) %>% get_var_choices()})
    
    # N bins, selected admin levels and choose variables modules
    pre_map_dta_2 <- mod_fltr_sel_var2_srv(id, pre_map_dta_1, var_choices, first_open)
    
    sel_adm_levels <- mod_get_admin_levels_srv(id, pre_map_dta_2,
                                               def_adm_opt = "explorer_default_adm",
                                               show_adm_opt = "explorer_show_adm",
                                               choose_adm_opt = "explorer_choose_adm")
    n_bins <- mod_get_nbins_srv(id)
    
    # Computing legend based on
    pre_map_dta_3 <- reactive({
      req(sel_adm_levels())
      req(n_bins())
      req(first_open())
      pre_map_dta_2() %>%
        filter_admin_levels(sel_adm_levels()) %>%
        add_legend_paras(nbins = n_bins()) %>%
        # complete_pti_labels() %>%
        rev()
    })
    
    # Initialize the map and fly to it.
    init_leaf <- mod_plot_init_leaf_server(id, shp_dta)
    
    # Plotting of the map
    out_leaf <- mod_plot_poly_leaf_server(id, pre_map_dta_3, shp_dta)
    
    # Map download server functions
    mod_map_dwnld_srv(id, out_leaf, metadata_path = metadata_path)
    
    # Data download 
    reactive({
      list(pre_map_dta = pre_map_dta_3, init_leaf = init_leaf)
    })
    
  }

#' @describeIn mod_dta_explorer2_server data explorer page UI
#'
#' @import shiny 
#' @importFrom leaflet leafletOutput
mod_dta_explorer2_ui <- function(id, multi_choice, ...){
  ns <- NS(id)
  tagList(
    leafletOutput(ns("leaf_id"), width = "100%", ...),
    mod_dta_explorer2_side_ui(id, multi_choice)
  ) %>%
    tags$div(style = "position:relative;") %>% 
    tags$div(id = "explorer_1") %>% 
    tags$div(id = "explorer_2") %>% 
    tags$div(id = "explorer_3") %>% 
    fluidRow() %>% 
    tagList(
      golem_add_external_resources(),
      shinyjs::useShinyjs()
      )
}


#' @describeIn mod_dta_explorer2_server panel with the N bins selector
#'
#' @import shiny 
mod_dta_explorer2_side_ui <- function(id, multi_choice, ...){
  ns <- NS(id)
  
  absolutePanel(
    id = "nbins_panel", 
    fixed = FALSE,
    draggable = FALSE, 
    left = "auto", bottom = "auto",
    width = 400,
    height = "auto",
    top = 10, right = 10,
    
    mod_select_var_ui(id, multi_choice),
    mod_get_admin_levels_ui(id),
    mod_get_nbins_ui(id, "Number of bins"),
    mod_map_dwnld_ui(id)
  )
}


#' @describeIn mod_dta_explorer2_server UI of the variables selector in Nbins side panel
#' 
#' @import shiny 
#' @importFrom shinyWidgets pickerInput pickerOptions
mod_select_var_ui <- function(id, multi_choice = NULL) {
  ns <- NS(id)
  
  explorer_multiple_var <- golem::get_golem_options("explorer_multiple_var")
  if (is.null(explorer_multiple_var)) explorer_multiple_var <- multi_choice
  if (is.null(explorer_multiple_var)) explorer_multiple_var <- FALSE
  
  tagList(
    shinyWidgets::pickerInput(
      ns("indicators"),
      "Select an indicator",
      NULL, NULL,
      multiple = explorer_multiple_var,
      width = "100%",
      options = shinyWidgets::pickerOptions(dropdownAlignRight  = TRUE,
                                            liveSearch = TRUE,
                                            maxOptions = 3)
    ) 
  )
  
}


#' @describeIn mod_dta_explorer2_server server logic for filtering variables
#' 
#' @import shiny 
#' @importFrom shinyWidgets pickerInput pickerOptions
mod_fltr_sel_var2_srv <- function(id, preplot_dta, choices, first_open, ...) {
  
  moduleServer(#
    id, #
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(#
        choices(),
        {
          shinyWidgets::updatePickerInput(
            session,
            inputId = "indicators",
            choices = choices() %>% map(~.x %>% names()), 
            selected = NULL
          )
        },
        ignoreNULL = TRUE,
        ignoreInit = FALSE)
      
      observeEvent(#
        first_open(),
        {
          req(first_open())
          shinyWidgets::updatePickerInput(session, inputId = "indicators",
                                          selected = choices() %>% map(~.x %>% names()) %>% unlist() %>% `[[`(1)
          )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE)
      
      selected_var <- 
        reactive({input$indicators}) %>% 
        debounce(500)
      
      eventReactive(#
        selected_var(), {
          out <- NULL
          if (isTruthy(selected_var())) {
            out <- 
              preplot_dta() %>% 
              filter_var_explorer(selected_var())
          } 
          out
        })
      
    }
  )
  
}


#' @describeIn mod_dta_explorer2_server Reshapes loaded data for the needs of explorer
#' 
#' @export 
#' @importFrom purrr map
reshaped_explorer_dta <- function(long_dta, ind_list) {
  out <- 
    long_dta %>%
    map(~ {
      adm_level <-
        names(.x) %>%
        `[`(str_detect(., 'admin\\d')) %>%
        max() %>%
        str_extract(., 'admin\\d')
      
      id_var <- str_c(adm_level, "Pcod")
      nm_var <- str_c(adm_level, "Name")
      
      .x %>%
        left_join(ind_list %>% select(var_code, var_name), by = "var_code") %>%
        select(
          one_of(id_var),
          pti_score = value,
          pti_name = var_name,
          spatial_name = !!sym(nm_var)
        )
    }) %>%
    label_generic_pti(
      str_c(
        "<strong>{spatial_name}</strong>",
        "<br/>Variable: <strong>{ifelse(is.na(pti_name), 'No data', pti_name)}</strong>",
        "<br/>Value: <strong>{ifelse(is.na(pti_score), 'No data', scales::label_number(accuracy = 0.00001)(pti_score))}</strong>",
        "<br/>",
        collapse = ""
      )
    ) 
  
  new_name <- str_extract(names(out), "admin\\d")
  names(out) <- new_name
  out
}

#' @describeIn mod_dta_explorer2_server Extracts choices of variables to select from in the visualization page
#' @export
#' @importFrom purrr pmap set_names
#' @importFrom tidyr nest
#' @importFrom dplyr arrange group_by
get_var_choices <- function(indicators_list) {
  indicators_list %>%
    dplyr::arrange(pillar_group, var_order) %>%
    dplyr::group_by(pillar_name) %>%
    tidyr::nest() %>%
    pmap(
      .f = function(pillar_name, data) {
        purrr::set_names(list(
          purrr::set_names(data$var_code, data$var_name)), pillar_name)
      }
    ) %>%
    unlist(recursive = F)
}


#' @describeIn mod_dta_explorer2_server filter explorer data
#' 
#' @export
#' @importFrom purrr map_lgl
filter_var_explorer <- function(preplot_dta, vars) {
  preplot_dta %>%
    `[`(purrr::map_lgl(.,  ~ {
      .x$pti_codes %in% vars | .x$pti_codes %in% names(vars)
    }))
}



