#' Serve a packaged file via a Shiny download handler
#'
#' Wraps `shiny::downloadHandler()` to ship a single file from disk under a
#' generated filename. Filename is built from a `glue::glue()` template that
#' has access to `.pti_name` (from `golem::get_golem_options("pti.name")`),
#' `.filepath`, and `.ext` (from [tools::file_ext()]).
#'
#' @param id Character. Shiny module namespace ID.
#' @param filepath Character. Absolute or relative path to the file to
#'   serve.
#' @param name_glue Glue template for the download filename. Defaults to
#'   `"metadata-{.pti_name}-{Sys.Date()}.{.ext}"`.
#'
#' @return No explicit return value. Called for side effects (registers
#'   `output$dwnld_local_file` within the Shiny session).
#'
#' @importFrom shiny moduleServer downloadHandler
#' @importFrom tools file_ext
#' @importFrom glue glue
#' @noRd
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
