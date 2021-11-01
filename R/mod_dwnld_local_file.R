#' dwnld_local_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dwnld_local_file_ui <- function(id, label){
  ns <- NS(id)
  tagList(
    downloadLink(ns("dwnld_local_file"), label)
  )
}
    
#' dwnld_local_file Server Function
#'
#' @noRd 
mod_dwnld_local_file_server <- function(id, filepath) {
  moduleServer(
    id,
    function(input, output, session) {
      output$dwnld_local_file <- 
        downloadHandler(
          filename = function() {
            paste(as.character(golem::get_golem_options("pti.name")), "-metadata-", Sys.Date(), ".pdf", sep="")
          },
          content = function(file) {
            file.copy(filepath, file)
          }, 
          contentType = "application/pdf"
        )
      
    }
  )
}
    