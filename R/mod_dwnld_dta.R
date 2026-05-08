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
#' `output$dwnld_local_file`. Validates `filepath` at registration:
#' when `NULL`, missing on disk, or pointing at a directory, the
#' download link is visually disabled via `shinyjs::disable()`, and
#' if a click sneaks through anyway the content callback writes a
#' short placeholder text file (`unavailable-<date>.txt`) instead of
#' the silent broken `.html` the function used to ship. When
#' `filepath` is valid, the handler streams the file under its base
#' name.
#'
#' @param id Character. Shiny module namespace ID.
#' @param outputId Character. Output slot name registered for the
#'   `downloadHandler()`. Defaults to `"dta.dwld"`.
#' @param filepath Character or `NULL`. Path to the file streamed to
#'   the client. When `NULL`, an empty string, a directory path, or a
#'   path that does not exist on disk, the download link is disabled
#'   and the handler ships a text-file placeholder. Callers that want
#'   a working download must pass a valid file path.
#' @param ... Unused; retained for forward compatibility.
#'
#' @return No explicit return value. Called for side effects (registers
#'   `output[[outputId]]` within the Shiny session, and disables the
#'   download link via `shinyjs::disable()` when `filepath` is
#'   invalid).
#'
#' @importFrom shiny moduleServer downloadHandler isTruthy
#' @noRd
mod_dwnld_file_server <- function(id, outputId = "dta.dwld", filepath = NULL, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      valid_file <-
        shiny::isTruthy(filepath) &&
        is.character(filepath) &&
        length(filepath) == 1L &&
        file.exists(filepath) &&
        !dir.exists(filepath)

      if (!valid_file) {
        shinyjs::disable(id = outputId)
      }

      output[[outputId]] <- downloadHandler(
        filename = function() {
          if (!valid_file) {
            return(paste0("unavailable-", Sys.Date(), ".txt"))
          }
          basename(filepath)
        },
        content = function(con) {
          if (!valid_file) {
            writeLines(
              "This download is not available -- no file path was provided to launch_pti().",
              con
            )
            return(invisible(NULL))
          }
          file.copy(filepath, con)
        }
      )
    })
}
