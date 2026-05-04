#' Read PTI metadata at app startup
#'
#' Server-side module that wraps [`fct_template_reader()`] in a `reactive()`
#' so the metadata list is available to downstream modules without re-reading
#' the xlsx file on every reactive flush. Resolves the source in priority
#' order: `data_dta` (in-memory list) > `data_path` (explicit file) >
#' first xlsx in `data_fldr`.
#'
#' @param id Character. Shiny module namespace ID.
#' @param data_fldr Character. Directory scanned for an `.xlsx` file when
#'   neither `data_path` nor `data_dta` is supplied.
#' @param data_path Character or NULL. Explicit path to a single metadata
#'   xlsx file. Takes precedence over `data_fldr`.
#' @param data_dta List or NULL. Pre-read metadata list (same shape as
#'   `fct_template_reader()` output). If supplied, no file is read.
#'
#' @return A `reactive()` expression yielding the metadata list (admin
#'   sheets, `metadata`, `general`).
#'
#' @importFrom shiny moduleServer reactive
#' @noRd
mod_fetch_data_srv <- function(id, data_fldr, data_path = NULL, data_dta = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      reactive({
        if (!is.null(data_dta)) {
          data_dta
        } else if (is.null(data_path) || !file.exists(data_path)) {
          fct_template_reader(data_fldr, list.files(data_fldr, "*.xlsx")[[1]])
        } else {
          fct_template_reader(data_path)
        }
      })
    }
  )
}
