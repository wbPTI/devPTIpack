#' infotab Server Functions
#'
#' @noRd 
mod_infotab_server <-
  function(id,
           tabpan_id = "tabpan",
           ptitab_id = "PTI",
           comparetab_id = NULL,
           exploretab_id = NULL,
           infotab_id = "Info",
           howtab_id =  "How it works?",
           firsttab_id = infotab_id) {
    moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Landing page UI
    land_page <- get_golem_options("pti_landing_page")
    if (is.null(land_page)) {
      land_page <- app_sys('app/generic-landing-page.HTML') %>% readLines() %>% htmltools::HTML() %>% tagList()
    } else if (str_detect(land_page, regex("md$|rmd$", ignore_case = T))) {
      land_page <- land_page %>% readLines() %>% shiny::markdown() %>% htmltools::HTML() %>% tagList()
    } else if (str_detect(land_page, regex("html$", ignore_case = T))) {
      land_page <- land_page %>% readLines() %>% htmltools::HTML() %>% tagList()
    }
    
    active_tab <- reactiveValues(current = NULL, previous = firsttab_id)
    
    guide <- guide_launch_pti(
      tabpanel_id = tabpan_id, 
      ptitab_id = ptitab_id, 
      comparetab_id = comparetab_id, 
      exploretab_id = exploretab_id)$init(session = session)
    
    run_guid_click <- reactiveVal(0)
    help_tab_click <- reactiveVal(0)
    guid_from_step <- reactiveVal(1)
    
    
    # Get back to the valid from info ----------------------
    observeEvent(#
      input[[tabpan_id]], #
      {
        if (input[[tabpan_id]] == infotab_id) {
          
          updateNavbarPage(
            session = session,
            inputId = tabpan_id,
            selected =  active_tab$previous
          )
          
          modalDialog(
            land_page,
            title = as.character(golem::get_golem_options("pti.name")),
            size = "m",
            easyClose = TRUE,
            footer =  tagList(
              modalButton("Close"),
              actionButton(
                ns("tour"),
                "Tour me around!",
                class = "btn-success",
                icon = icon(name = "arrow-circle-right")
              )
            )
          ) %>%
            showModal()
          
        } else if (input[[tabpan_id]] == howtab_id) {
          
          updateNavbarPage(
            session = session,
            inputId = tabpan_id,
            selected =  active_tab$previous
          )
          
          new_val <- run_guid_click() + 1
          run_guid_click(new_val)
          
        } else {
          active_tab$previous <- input[[tabpan_id]]
        }
        
      })
    
    
    # Start from info buton ---------------------
    observeEvent(input$tour, {
      removeModal(session = getDefaultReactiveDomain())
      new_val <- run_guid_click() + 1
      run_guid_click(new_val)
    })
    
    # Start from Get help -------------------------
    get_help <- reactive({
      list(help_tab_click(), run_guid_click())
    }) %>%
      debounce(150)
    
    # Universal restart guide ------------------------
    observeEvent(get_help(), {
      req(any(unlist(get_help()) != 0))
      removeModal(session = getDefaultReactiveDomain())
      guide$start(session = session, step = guid_from_step())
      
    })
  })
}
    
## To be copied in the UI
# mod_infotab_ui("infotab_ui_1")
    
## To be copied in the server
# mod_infotab_server("infotab_ui_1")
