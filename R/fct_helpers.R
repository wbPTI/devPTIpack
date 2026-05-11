#' Build the navbar title HTML with the World Bank logo
#'
#' Internal helper used by `launch_pti()` to construct the navbar
#' brand area. Combines the bundled WBG horizontal logo image with the
#' supplied `pti_name` (or the `pti.name` golem option when `pti_name`
#' is `NULL`) into a single `tagList`.
#'
#' @param pti_name Optional character. App display name shown next to
#'   the logo. When `NULL`, falls back to
#'   `golem::get_golem_options("pti.name")`.
#'
#' @return A `shiny.tag.list` containing a div with the logo image and
#'   the app name -- suitable for use as a `navbarPage(title = ...)`
#'   value.
#'
#' @importFrom htmltools div img tagList
#' @importFrom golem get_golem_options
#' @noRd
add_logo <- function(pti_name = NULL) {
  div(
    div(
      id = "img-logo-navbar",
      style="right: 70px;",
      img(src = "www/WBG_Horizontal-black-web.png",
          style = "width: auto; height: 40px;")
    ),
    if (!is.null(pti_name))
      pti_name
    else
      as.character(golem::get_golem_options("pti.name"))
  ) %>%
    tagList()
}
