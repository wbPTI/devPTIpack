#' dwnld_dta UI Function
#'
#' @description A shiny Module.
#'
#' @param id module ID. If sued inside a parent module, NULL
#' @param outputId optional. If use multiple downloads in same NS, we allow for 
#'        outputId variation. Remember to specify same id for the server side fucntion.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList icon
mod_dwnld_dta_btn_ui <-
  function(id = NULL,
           outputId = "dta.dwld",
           label = "Download data",
           icon = shiny::icon("download"),
           class = "btn-info  btn-xs",
           style = "width: 49%") {
    ns <- NS(id)
    tagList(
      shinyjs::useShinyjs(),
      downloadButton(
        ns(outputId),
        label = label,
        icon = icon,
        class = class,
        style = style
      ) %>%
        shinyjs::disabled()
    )
  }


mod_dwnld_dta_link_ui <-
  function(id,
           outputId = "dta.dwld",
           label = "Download data",
           prefix = "",
           suffix = "",
           ...) {
    ns <- NS(id)
    tagList(
      shinyjs::useShinyjs(),
      prefix,
      downloadLink(
        ns(outputId),
        label = label,
        ...
      ),
      suffix
    )
  }

#' dwnld_dta Server Functions
#'
#' @noRd 

mod_dwnld_dta_xlsx_server <- function(id,
                                      outputId = "dta.dwld",
                                      file_name = reactive(NULL),
                                      dta_dwnld = reactive(NULL),
                                      ...) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(dta_dwnld(), {
        
        if (isTruthy(dta_dwnld()))
          shinyjs::enable(id = outputId)
        else
          shinyjs::disable(id = outputId)
          
      }, ignoreNULL = FALSE, ignoreInit = FALSE)
      
      # Write export data
      output[[outputId]] <- downloadHandler(
        filename = function() {
          if (isTruthy(file_name())) {
            flnm <- file_name()
          } else {
            flnm <- str_c("data-export-", Sys.date(), ".xlsx")
          }
          flnm
        },
        content = function(con) {
          writexl::write_xlsx(dta_dwnld(), con)
        }
      )
    })
}



#' dwnld_file Server Function for file download
#'
#' @noRd 
mod_dwnld_file_server <- function(id, outputId = "dta.dwld", filepath = NULL, ...) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      output[[outputId]] <- downloadHandler(
        filename = function() {basename(filepath)},
        content = function(con) {file.copy(filepath, con)}
      )
    })
}
## To be copied in the UI
# mod_dwnld_dta_ui("dwnld_dta_ui_1")
    
## To be copied in the server
# mod_dwnld_dta_server("dwnld_dta_ui_1")
