#' Load PTI shape data inside a Shiny module
#'
#' Thin server-side wrapper around [get_shape()] that exposes the named list
#' of admin-level `sf` tibbles as a `reactive()` so downstream modules can
#' depend on it without re-reading the `.rds` file on every reactive flush.
#'
#' @param id Character. Shiny module namespace ID.
#' @param shapes_fldr Character. Directory scanned for an `.rds` shape file
#'   matching `shape_country` when neither `shape_path` nor `shape_dta` is
#'   supplied.
#' @param shape_country Character. Pattern matched against filenames in
#'   `shapes_fldr` (typically the country name).
#' @param shape_path Character or NULL. Explicit path to a single `.rds`
#'   shape file. Takes precedence over `shapes_fldr`.
#' @param shape_dta List or NULL. Pre-loaded named list of `sf` tibbles. If
#'   supplied, no file is read.
#'
#' @return A `reactive()` yielding the named list of admin-level `sf`
#'   tibbles.
#'
#' @importFrom shiny moduleServer reactive
#' @noRd
mod_get_shape_srv <-
  function(id,
           shapes_fldr = "app-shapes/",
           shape_country = "Country name",
           shape_path = NULL,
           shape_dta = NULL) {
    moduleServer(
      id,
      function(input, output, session) {
        ns <- session$ns
        reactive({
          get_shape(shapes_fldr, shape_country, shape_path, shape_dta)
        })
      })
  }

#' Load admin-level shape data for a PTI app
#'
#' Reads a named list of `sf` tibbles describing administrative boundaries
#' at multiple levels. Resolves the source in priority order: `shape_dta`
#' (in-memory list) > `shape_path` (explicit `.rds` file) > first `.rds`
#' in `shapes_fldr` whose filename matches `shape_country`.
#'
#' @param shapes_fldr Character. Directory scanned for an `.rds` shape file
#'   matching `shape_country` when neither `shape_path` nor `shape_dta` is
#'   supplied.
#' @param shape_country Character. Pattern matched against `.rds` filenames
#'   in `shapes_fldr`.
#' @param shape_path Character or NULL. Explicit path to a single `.rds`
#'   shape file. Takes precedence over `shapes_fldr`.
#' @param shape_dta List or NULL. Pre-loaded named list of `sf` tibbles. If
#'   supplied, returned unchanged.
#'
#' @return Named list of `sf` tibbles, one per admin level (e.g.
#'   `admin0_Country`, `admin1_Oblast`, `admin2_Rayon`,
#'   `admin4_Hexagon`).
#'
#' @importFrom magrittr extract
#' @importFrom stringr str_detect str_replace_all str_c
#' @importFrom readr read_rds
#' @export
#'
#' @examples
#' data(rwa_shp)
#'
#' # In-memory short-circuit: returns shape_dta unchanged.
#' shp <- get_shape(shape_dta = rwa_shp)
#' names(shp)
get_shape <- function(shapes_fldr, shape_country, shape_path = NULL, shape_dta = NULL) {

  if (!is.null(shape_dta)) return(shape_dta)
  if (is.null(shape_path) || !file.exists(shape_path)) {
    list.files(shapes_fldr, "rds") %>%
      magrittr::extract(stringr::str_detect(., shape_country)) %>%
      stringr::str_replace_all("\\.rds", "") %>%
      stringr::str_c(shapes_fldr, ., ".rds") %>%
      readr::read_rds() %>%
      return()
  } else {
    shape_path %>%
      readr::read_rds() %>%
      return()
  }
}
