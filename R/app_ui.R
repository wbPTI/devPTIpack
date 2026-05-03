#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom shiny tags
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom bsplus use_bs_tooltip
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path('www', app_sys('app/www'))

  tags$head(favicon(ext = 'png'),
            bundle_resources(path = app_sys('app/www'),
                             app_title = as.character(golem::get_golem_options("pti.name"))))
}
