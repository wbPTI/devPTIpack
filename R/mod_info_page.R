#' info_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_info_page_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    


#' info_page Server Function
#'
#' @noRd
#' @import shiny
#' @importFrom htmltools HTML
mod_info_page_server <-
  function(input, output, session, first_tab) {
    ns <- session$ns
    
    active_tab <- reactiveValues(current = NULL, previous = first_tab)
    
    guide <- compile_guides()$init(session = session)
    run_guid_click <- reactiveVal(0)
    help_tab_click <- reactiveVal(0)
    guid_from_step <- reactiveVal(1)
    
    land_page <- get_golem_options("pti_landing_page")
    if (is.null(land_page)) {
      land_page <- app_sys('app/generic-landing-page.HTML') %>% readLines() %>% htmltools::HTML() %>% tagList()
    } else if (str_detect(land_page, regex("md$|rmd$", ignore_case = T))) {
      land_page <- land_page %>% readLines() %>% shiny::markdown() %>% htmltools::HTML() %>% tagList()
    } else if (str_detect(land_page, regex("html$", ignore_case = T))) {
      land_page <- land_page %>% readLines() %>% htmltools::HTML() %>% tagList()
    }
    
    # Get back to the valid tab observer from info ----------------------
    observeEvent(#
      input$main_sidebar, #
      {
        if (input$main_sidebar == "Info") {
          updateNavbarPage(
            session = session,
            inputId = "main_sidebar",
            selected =  active_tab$previous
          )
          
          showModal(
            modalDialog(
              land_page,
              title = as.character(golem::get_golem_options("pti.name")),
              size = "m",
              easyClose = TRUE,
              footer =  tagList(
                modalButton("Close"),
                actionButton(
                  "tour",
                  "Tour me around!",
                  class = "btn-success",
                  icon = icon(name = "arrow-circle-right")#,
                  # class = "disabled"
                )
              )
            )
          )
          
        } else if (input$main_sidebar == "How it works?") {
          updateNavbarPage(
            session = session,
            inputId = "main_sidebar",
            selected =  active_tab$previous
          )
          new_val <- run_guid_click() + 1
          run_guid_click(new_val)
        } else {
          active_tab$previous <- input$main_sidebar
        }
        
      })
    
    
    # Start from info buton ---------------------
    observeEvent(input$tour, {
      removeModal(session = getDefaultReactiveDomain())
      new_val <- run_guid_click() + 1
      run_guid_click(new_val)
    })
    
    # Next click -------------------------------
    # observeEvent(input$apps_guide_cicerone_next,
    #              {
    #                if (input$apps_guide_cicerone_next$previous == names(spatialPovertyExplorer::gifs_base[[7]])) {
    #                  # browser()
    #                  updateNavbarPage(session = session,
    #                                   inputId = "selected_tab",
    #                                   selected =  "Analysis")
    #                  
    #                  removeModal(session = getDefaultReactiveDomain())
    #                  guid_from_step(8)
    #                  new_val <- run_guid_click() + 1
    #                  run_guid_click(new_val)
    #                }
    #              })
    # 
    # # Previous click -------------------------------
    # observeEvent(input$apps_guidecicerone_previous,
    #              {
    #                if (input$apps_guidecicerone_previous$previous ==  "country_change") {
    #                  updateNavbarPage(session = session,
    #                                   inputId = "selected_tab",
    #                                   selected =  "Map")
    #                  
    #                  removeModal(session = getDefaultReactiveDomain())
    #                  guid_from_step(7)
    #                  new_val <- run_guid_click() + 1
    #                  run_guid_click(new_val)
    #                }
    #              })
    
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
    
  }




#' Compile guides functoin
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom htmltools HTML
#' @import cicerone
compile_guides <- function() {
  
  out_cice <- 
    Cicerone$new(#
    id = "apps_guide",
    opacity = 0.35)$step(
      el = "step_1_name",
      title = "Specify PTI name",
      description =
        glue(tags$image(src = "www/010-name.gif", class = "popoverimg")%>%
               as.character(),
             "<br/><br/>If you have just loaded the app, provide here the name for your first PTI.") %>%
        htmltools::HTML(),
      position = "right-top",
      show_btns = TRUE,
      on_highlight_started = str_c(
        'function() {
        $("#main_sidebar a[data-value=\'PTI\']").tab(\'show\');
      }')
    )$step(
      el = "step_5_modify_weights", 
      is_id = TRUE,
      title = "Modify weights",
      description = 
        glue(tags$image(src = "www/018-change-weights.gif", class = "popoverimg")%>%
             as.character(),
             "<br/><br/>One can modify weights manually one-by-one or using shortcuts.", 
           ) %>%
        htmltools::HTML(),
      position = "top-center",
      show_btns = TRUE
    )$step(
      el = "step_3_save", 
      # names(spatialPovertyExplorer::gifs_base[[2]]),
      title = "Save PTI",
      description = 
          glue(tags$image(src="www/010-name.gif", class = "popoverimg") %>% as.character(),
               "<br/><br/>As soon as PTI name is specified, \"Save\" button will be activated: <br/><br/>", 
               tags$image(src="www/guide_save.GIF", class = "smallimg") %>% as.character(),
               "<br/>Press it to save your PTI. Only after saving PTI the map will be reloaded.", 
               
               ) %>% 
        htmltools::HTML(),
      position = "right-top",
      show_btns = TRUE
    )$step(
      el = "step_8_map_inspection1",
      title = "Inspect the Map",
      description =
        htmltools::HTML(
        glue("Inspect the map by switching between PTIs and by changing the number of bins: <br/>", 
             tags$image(src = "www/025-map.gif", class = "popoverimg") %>% as.character(),
        )
      ),
      position = "mid-center",
      show_btns = TRUE
    )$step(
      el = "step_234_controls1", 
      title = "Save new PTIs under different name",
      description = 
        htmltools::HTML(
          glue(
            tags$image(src = "www/012-2-name-switch.gif", class = "popoverimg") %>% as.character(),
            "<br/>You may create multiple PTIs and \"Select existing PTI to modify\" <br/>menu will appear. 
            Use it to switch between PTIs and <br/>modify selected one.
            <br/> Always remember to press \"Save changes and plot existing PTI\"  <br/>button after you have modified weights.<br/>",
               tags$image(src="www/guide_save_changes.GIF",  
                          style = "max-width: 250px; display:block; margin-left:auto; margin-right:auto;") %>% as.character()
          )
        ),
      position = "right-top",
      show_btns = TRUE
    )$step(
      el = "step_234_controls2", 
      title = "Delete PTI",
      description = 
        htmltools::HTML(
          glue(tags$image(src = "www/015-delete.gif", class = "popoverimg") %>% as.character(),
               "<br/>You may also \"Delete\" existing PTIs one by one.  <br/>Select existing PTI and press \"Delete PTI\":",
               tags$image(src="www/guide_delete_selected.GIF",  
                          style = "max-width: 250px; display:block; margin-left:auto; margin-right:auto;") %>% as.character(),
               " <br/>Delete button is disabled when name of the PTI was modified  <br/>(or there were no PTIs saved)."
          )
        ),
      position = "right-top",
      show_btns = TRUE
    )$step(
      el = "step_5_downalod_upload1",
      title = "Download PTI",
      description =
        htmltools::HTML(
          glue("You may download current PTIs in a single Excel file. <br/>",
               "At least on PTI has to be saved beforehand <br/>(otherwise download button will remain hidden):<br/>",
               tags$image(src="www/guide_download.GIF",  
                          style = "max-width: 250px; display:block; margin-left:auto; margin-right:auto;") %>% 
                 as.character()
          )
        ),
      position = "right-top",
      show_btns = TRUE
    )$step(
      el = "step_5_downalod_upload2",
      title = "Upload PTI",
      description =
        htmltools::HTML(
          glue("Finally, you may upload previously downloaded PTI <br/>data to the app and continue modifying it:<br/>",
               tags$image(src="www/guide_upload.GIF",  
                          style = "max-width: 250px; display:block; margin-left:auto; margin-right:auto;") %>% 
                 as.character()
          )
        ),
      position = "right-top",
      show_btns = TRUE
    )$step(
      el = "main_sidebar",
      title = "Switch between tabs",
      description =
        htmltools::HTML(
          glue("Use tabs panel to switch between tabs.")
        ),
      position = "bottom-center",
      show_btns = TRUE,
      on_highlight_started = str_c(
        'function() {
        $("#main_sidebar a[data-value=\'PTI\']").tab(\'show\');
      }')
    )
  
  out_cice <- 
    out_cice$step(
      el = "#main_sidebar li:nth-child(3)", 
      is_id = FALSE,
      title = "Compare multiple PTIs side-by-side.",
      description =
        htmltools::HTML(
          glue("When two or more PTIs are specified, they could be compared side-by-side.")
        ),
      position = "bottom-left",
      show_btns = TRUE,
      on_highlight_started = str_c(
        'function() {
        $("#main_sidebar a[data-value=\'PTI comparison\']").tab(\'show\');
      }')
    )$step(
      el = "compare_left_1", 
      is_id = TRUE,
      title = "Select different PTIs and bins numbers.",
      description =
        tags$image(src = "www/030-compare.gif", class = "popoverimg")%>%
        as.character() %>%
        htmltools::HTML(),
      position = "mid-center",
      show_btns = TRUE
    )
  
  
  out_cice <- 
    out_cice$step(
      el = "#main_sidebar li:nth-child(4)", 
      is_id = FALSE,
      title = "Explore actual data behind PTI",
      description =
        htmltools::HTML(
          glue("On the explorer tab, one can visualise data that is used for construction PTIs.<br/> 
               It is also possible to show locations of different projects.")
        ),
      position = "bottom-left",
      show_btns = TRUE
    )$step(
      el = "explorer_1", 
      is_id = TRUE,
      title = "Explore data",
      description =
        tags$image(src = "www/035-explorer.gif", class = "popoverimg")%>%
        as.character() %>%
        htmltools::HTML(),
      position = "mid-center",
      show_btns = TRUE,
      on_highlight_started = str_c(
        'function() {
        $("#main_sidebar a[data-value=\'Data explorer\']").tab(\'show\');
      }')
    )
  
  out_cice
  

}
