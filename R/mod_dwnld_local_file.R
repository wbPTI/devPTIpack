#' dwnld_local_file Server Function
#'
#' @noRd 
#' @importFrom tools file_ext
mod_dwnld_local_file_server <- function(id, filepath, 
                                        name_glue = "metadata-{.pti_name}-{Sys.Date()}.{.ext}") {
  moduleServer(
    id,
    function(input, output, session) {
      output$dwnld_local_file <- 
        downloadHandler(
          filename = function() {
            .pti_name <- as.character(golem::get_golem_options("pti.name"))
            if (isTRUE(identical(character(0), .pti_name))) .pti_name <- ""
            .filepath <- filepath
            .ext <- tools::file_ext(filepath)
            glue::glue(name_glue)
          },
          content = function(file) {
            file.copy(filepath, file)
          }
        )
      
    }
  )
}
    