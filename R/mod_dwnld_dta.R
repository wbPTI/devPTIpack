#' UI helper for a data-download link
#'
#' Returns a `tagList()` containing a Shiny `downloadLink()` plus optional
#' surrounding HTML, used inside larger UI compositions where the download
#' link sits next to other controls.
#'
#' @param id Character. Shiny module namespace ID.
#' @param outputId Character. Output slot name registered by the matching
#'   server module. Defaults to `"dta.dwld"`.
#' @param label Character. Visible link label.
#' @param prefix,suffix HTML or character. Optional content inserted before
#'   and after the download link.
#' @param ... Further arguments passed to `shiny::downloadLink()`.
#'
#' @return A `shiny::tagList()` containing the download link and any
#'   provided prefix / suffix content.
#'
#' @importFrom shiny NS tagList downloadLink
#' @noRd
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

#' Server module that writes a reactive named-list to xlsx on download
#'
#' Registers a `downloadHandler()` that writes `dta_dwnld()` (a named list of
#' tibbles) to a single multi-sheet xlsx file via `writexl::write_xlsx()`.
#' Enables/disables the matching link based on `dta_dwnld()` truthiness so
#' the user cannot trigger a download when there is nothing to write.
#'
#' @param id Character. Shiny module namespace ID.
#' @param outputId Character. Output slot name. Must match the `outputId`
#'   used in the paired `mod_dwnld_dta_link_ui()` call.
#' @param file_name Reactive yielding a character filename (with extension)
#'   or `NULL`. When `NULL`, falls back to `data-export-<date>.xlsx`.
#' @param dta_dwnld Reactive yielding the named list of tibbles to write.
#' @param ... Unused; retained for forward compatibility.
#'
#' @return No explicit return value. Called for side effects (registers
#'   `output[[outputId]]` and toggles the download link's enabled state).
#'
#' @importFrom shiny moduleServer downloadHandler observeEvent isTruthy reactive
#' @importFrom stringr str_c
#' @noRd
mod_dwnld_dta_xlsx_server <- function(id,
                                      outputId = "dta.dwld",
                                      file_name = reactive(NULL),
                                      dta_dwnld = reactive(NULL),
                                      ...) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observeEvent(dta_dwnld(), {

        if (isTruthy(dta_dwnld()))
          shinyjs::enable(id = outputId)
        else
          shinyjs::disable(id = outputId)

      }, ignoreNULL = FALSE, ignoreInit = FALSE)

      output[[outputId]] <- downloadHandler(
        filename = function() {
          if (isTruthy(file_name())) {
            flnm <- file_name()
          } else {
            flnm <- str_c("data-export-", Sys.Date(), ".xlsx")
          }
          flnm
        },
        content = function(con) {
          writexl::write_xlsx(dta_dwnld(), con)
        }
      )
    })
}

#' Server module that streams a single file via a download handler
#'
#' Companion to [mod_dwnld_local_file_server()] but parameterised by
#' output-slot name so it can be wired into download links other than
#' `output$dwnld_local_file`.
#'
#' @param id Character. Shiny module namespace ID.
#' @param outputId Character. Output slot name registered for the
#'   `downloadHandler()`. Defaults to `"dta.dwld"`.
#' @param filepath Character or NULL. Path to the file streamed to the
#'   client.
#' @param ... Unused; retained for forward compatibility.
#'
#' @return No explicit return value. Called for side effects (registers
#'   `output[[outputId]]` within the Shiny session).
#'
#' @importFrom shiny moduleServer downloadHandler
#' @noRd
mod_dwnld_file_server <- function(id, outputId = "dta.dwld", filepath = NULL, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output[[outputId]] <- downloadHandler(
        filename = function() {basename(filepath)},
        content = function(con) {file.copy(filepath, con)}
      )
    })
}
