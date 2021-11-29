#' pti_map_side_pan UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList absolutePanel
mod_leaf_side_panel_ui <- function(id, ...){
  ns <- NS(id)
  
  absolutePanel(
    id = "nbins_panel", 
    style = 
      "z-index:950; min-width: 250px; background-color: rgba(255,255,255,0.8);
    box-shadow: 0 0 15px rgba(0,0,0,0.2); border-radius: 5px;
    padding: 10px 10px 10px 10px;
    zoom: 0.85; transition: opacity 500ms 1s;",
    fixed = FALSE,
    draggable = FALSE, 
    left = "auto", bottom = "auto",
    width = 200,
    height = "auto",
    top = 10, right = 10,
    
    mod_get_nbins_ui(id),
    mod_get_admin_levels_ui(id),
    mod_map_dwnld_ui(id)
  )
}



# Get n_bins ---------------------------------------------------------------

#' Get number of bins UI
#' 
#' @noRd 
#'
#' @importFrom shiny NS tagList numericInput
mod_get_nbins_ui <- function(id, label = "Number of bins") {
  ns <- NS(id)
  numericInput(
    ns("nbins_number"),
    label = tags$span(
      label,
      tags$br(),
      tags$i(style = "font-size:10px; font-weight: 100;",
             "(Subnational areas will be grouped into bins)")
    ),
    value = 5,
    min = 2,
    step = 1,
    width = "100%"
  ) %>%
    tagList()
}


#' Get number of bins Server
#' 
#' @noRd 
#'
#' @importFrom shiny debounce reactive moduleServer isTruthy
mod_get_nbins_srv <- function(id, n_default = 5) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      reactive({
        if (isTruthy(input$nbins_number)) {
          return(input$nbins_number)
        } else {
          return(n_default)
        }
      }) %>% debounce(500)
    })
}


# Get admin levels ------------------------------------------------


#' Get admin levels to plot UI
#' 
#' @noRd 
#'
#' @importFrom shiny NS tagList numericInput
mod_get_admin_levels_ui <-
  function(id) {
    ns <- NS(id)
    shiny::uiOutput(ns("admin_levels")) %>%
      tagList()
  }


#' Get number of bins Server
#' 
#' @param preplot_dta reactive data object with PTI data to plot.
#' @param def_adm_opt,show_adm_opt,choose_adm_opt character (sinple values) for 
#'      of golem options to search for clues about defualt admin layers to print
#'      or choose from.
#'
#' @importFrom shiny debounce reactive moduleServer isTruthy
mod_get_admin_levels_srv <- function(id, preplot_dta,
                                     def_adm_opt = "default_adm_level",
                                     show_adm_opt = "show_adm_levels",
                                     choose_adm_opt = "choose_adm_levels",....) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      out_dta <- reactiveVal(NULL)
      chb_iu <- reactiveVal(NULL)
      dta_levels <- reactiveVal(NULL)
      
      default_adm_level <- get_golem_options(def_adm_opt)
      show_adm_levels <- get_golem_options(show_adm_opt)
      choose_adm_levels <- get_golem_options(choose_adm_opt)
      
      observeEvent(
        preplot_dta(),
        {
          req(preplot_dta())
          preplot_dta() %>% get_current_levels() %>% dta_levels()
          
          if (!is.null(choose_adm_levels) && choose_adm_levels) {
            if (!is.null(show_adm_levels) &&
                (any(show_adm_levels %in% names(dta_levels())) |
                 any(show_adm_levels %in% dta_levels()))
                ) {
              
              dta_levels() %>% 
                `[`(names(.) %in% show_adm_levels |
                      (.) %in% show_adm_levels) %>%  
                dta_levels()
              
            }
            
            plot_levels <- c(all = "All", dta_levels())  %>% unname()
            
            radioButtons(ns("adm_lvls_chb"), NULL, plot_levels, "All", inline = TRUE) %>%
              column(12, .) %>% 
              fluidRow(align="center") %>% 
              chb_iu()
            
            dta_levels() %>% out_dta()
            
          } else {
            if (isTruthy(default_adm_level) &&
                (any(default_adm_level %in% names(dta_levels())) |
                    any(default_adm_level %in% dta_levels()))) {
              
              dta_levels() %>% 
                `[`(names(.) %in% default_adm_level | 
                      (.) %in% default_adm_level) %>%  
                dta_levels()
              
            } else if (isTruthy(default_adm_level) && 
                       any(str_detect(default_adm_level, regex("all", ignore_case = T)))) {
              
              # Do nothing and return full data
              
            } else if (isTruthy(default_adm_level) &&
                       !(any(default_adm_level %in% names(dta_levels())) |
                         any(default_adm_level %in% dta_levels()))) {
              
              dta_levels() %>% 
                `[`(length(.)) %>%
                dta_levels()
              
            } else  if (!isTruthy(default_adm_level) && 
                       isTruthy(show_adm_levels) &&
                       any(show_adm_levels %in% names(dta_levels()))) {
              
              dta_levels() %>% 
                `[`(names(.) %in% show_adm_levels | 
                      (.) %in% show_adm_levels) %>%
                dta_levels()
              
            }
            
            dta_levels() %>% out_dta()
            
          }
        }, 
        ignoreNULL = FALSE, 
        ignoreInit = FALSE)
      
      # output[["admin_levels"]] <- renderUI({req(chb_iu())})
      # 
      # adm_lvl_debounce <- reactive(input$adm_lvls_chb) %>% debounce(700)
      # 
      # observeEvent(adm_lvl_debounce(), {
      #   req(chb_iu())
      #   req(adm_lvl_debounce())
      #   
      #   if (any(stringr::str_detect(adm_lvl_debounce(), 
      #                               stringr::regex("all", ignore_case = TRUE)))) {
      #     
      #     dta_levels() %>% out_dta()
      #     
      #   } else {
      #     
      #     adm_lvl_debounce() %>% out_dta()
      #     
      #   }
      # }, 
      # ignoreNULL = FALSE, 
      # ignoreInit = FALSE)
      
      reactive({out_dta()})
      
    })
}



# Map download as image ---------------------------------------------------

#' Get number of bins Server
#' 
#' @noRd 
#'
#' @importFrom shiny NS tags downloadLink tagList
mod_map_dwnld_ui <- function(id) {
  ns <- NS(id)
  tags$p(
    style = "font-size: 12px;",
    tags$i(
      "Download map as ",
      downloadLink(ns("map_png"), ".png"),
      " or ",
      downloadLink(ns("map_pdf"), ".pdf")
    ),
    style = "text-align: right; margin: 0 0 0px !important;"
  ) %>%
    tagList(
      tags$p(
        style = "font-size: 12px; text-align: right; margin: 0 0 0px !important;",
        tags$i(mod_dwnld_local_file_ui(ns("map_metadata"), "Download metadata"))
      )
    )
  
}




#' Get number of bins Server
#' 
#' @noRd 
#'
#' @importFrom shiny debounce reactive moduleServer isTruthy downloadHandler
#'
#' @importFrom curl curl_version
#' @import ggplot2
#' 
mod_map_dwnld_srv <- function(id, plotting_map, metadata_path = NULL) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$map_png <- 
        downloadHandler(
          filename = function() {
            paste("pti-map-", Sys.Date(), ".png", sep="")
          },
          content = function(file) {
            # file.copy(plotting_map(), file)
            
            # 
            withProgress({
              incProgress(4/10, detail = "Gathering all data and generating the plot")
            plot_dta <- plotting_map()
            
            png(
              filename = file,
              width = 29,
              height = 21,
              units = "cm",
              res = 300
            )
            
            if (plot_dta$poly) {
              print(
                do.call(make_spplot, args = plot_dta)
              )
            } else  {
              print(
                do.call(make_sp_line_map, args = plot_dta)
              )
            }
        
            dev.off()
            
            },
            min = 0,
            value = 0.1,
            message = "Rendering the map as an image.")
            
            # ggplot2::ggsave(
            #   file,
            #   plot = plotting_map(),
            #   device = "png", 
            #   scale = 1,
            #   width = 29,
            #   height = 21, 
            #   units = "cm"
            # )
            
            # message(curl::curl_version()) # check curl is installed
            # if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
            #   chromote::set_default_chromote_object(
            #     chromote::Chromote$new(chromote::Chrome$new(
            #       args = c("--disable-gpu", 
            #                "--no-sandbox", 
            #                "--disable-dev-shm-usage", # required bc the target easily crashes
            #                c("--force-color-profile", "srgb"))
            #     ))
            #   )
            # }
            # 
            # 
            # withProgress({
            #   incProgress(1/10, detail = "Gathering all data")
            #   map_example <- tempfile(fileext = ".html")
            #   out_map <- plotting_map()
            #   mapview::mapshot(x = out_map,
            #                    url = map_example, vwidth = 1400,
            #                    vheight = 1150, zoom = 2)
            #   
            #   incProgress(4/10, detail = "Exporting the map into a 'png' file")
            #   
            #   webshot2::webshot(
            #     url = map_example,
            #     file = file,
            #     vwidth = 1400,
            #     vheight = 1150,
            #     selector = NULL,
            #     cliprect = NULL,
            #     expand = NULL,
            #     delay = 1,
            #     zoom = 2,
            #     useragent = NULL,
            #     max_concurrent = getOption("webshot.concurrent", default = 6)
            #   )
            #   
            # },
            # min = 0,
            # value = 0.1,
            # message = "Rendering the map as an image.")
            
          }
        )
      
      output$map_pdf <- 
        downloadHandler(
          filename = function() {
            paste("pti-map-", Sys.Date(), ".pdf", sep="")
          },
          content = function(file) {
            
            
            withProgress({
              incProgress(4/10, detail = "Gathering all data and generating the plot")
              plot_dta <- plotting_map()
              
              pdf(
                file = file,
                width = 11,
                height = 8.25
              )
              
              if (plot_dta$poly) {
                print(
                  do.call(make_spplot, args = plot_dta)
                )
              } else {
                print(
                  do.call(make_sp_line_map, args = plot_dta)
                )
              }
              
              dev.off()
              
            },
            min = 0,
            value = 0.1,
            message = "Rendering the map as an image.")
            
            
          }
        )
      
      output$map_html <- 
        downloadHandler(
          filename = function() {
            paste("pti-map-", Sys.Date(), ".html", sep="")
          },
          content = function(file) {
            
            withProgress({
              incProgress(1/10, detail = "Gathering all the data and generating an 'html'.")
              
              # mapview::mapshot(x = plotting_map(),
              #                  url = file, vwidth = 1400,
              #                  vheight = 1150, zoom = 2)
            },
            min = 0,
            value = 0.1,
            message = "Rendering the map as a single webpage.")
            
          }
        )
      
      if (is.null(metadata_path) || !file.exists(metadata_path)) {
        mod_dwnld_local_file_server("map_metadata", "app-metadata/15-zam-pti-data-overview.pdf")
        mod_dwnld_local_file_server(ns("map_metadata"), "app-metadata/15-zam-pti-data-overview.pdf")
      } else {
        mod_dwnld_local_file_server("map_metadata", metadata_path)
        mod_dwnld_local_file_server(ns("map_metadata"), metadata_path)
      }
      
    })
  
}


