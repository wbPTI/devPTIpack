#' add html tags with te PTI app name and logo.
#' 
#' @noRd
#' @importFrom htmltools div img tagList
#' @importFrom golem get_golem_options
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
