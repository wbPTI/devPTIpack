

#' App UI function - KEY!
#' @import shiny
#' @importFrom golem get_golem_options
#' @noRd
app_ui <- function(request) {
  
  spinner <- tagList(
    waiter::spin_chasing_dots(),
    br(),
    span(
      str_c("Loading ", as.character(golem::get_golem_options("pti.name")), "..."), style="color:white;")
  )
  
  full_ui <- get_golem_options("full_ui")
  if (!is.null(full_ui) && !full_ui)
  {
    full_ui <- FALSE
  } else {
    full_ui <- TRUE
  }
  
  pages <- 
    tagList(
      navbarPage(
        title = div(div(
          id = "img-logo-navbar",
          img(src = "www/WBG_Horizontal-black-web.png",
              style = "width: auto; height: 40px;")
          ),
          as.character(golem::get_golem_options("pti.name"))
        ), 
        collapsible = TRUE,
        id = "main_sidebar", 
        selected = "Info",
        tabPanel("Info"),
        tabPanel("PTI", mod_weights_ui("new_pti_core", full_ui = full_ui)),
        tabPanel("PTI comparison", mod_map_pti_leaf_page_ui("compare_leaf_map")),
        tabPanel("Data explorer", mod_explorer_tab_ui(id = "data_explorer"))#,
        # tabPanel("How it works?")
      )
    )
  
  wrt_opt <- golem::get_golem_options("show_waiter")
  
  tagList(
    golem_add_external_resources(),
    use_cicerone(),
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    if (is.null(wrt_opt) | (!is.null(wrt_opt) && wrt_opt)) {waiter::waiter_show_on_load(spinner)},
    pages
  )
}




#' App UI function - for the PTI display only
#' @import shiny
#' @importFrom golem get_golem_options
#' @noRd
app_server_sample_pti_vis_ui <- function(request) {
  
  spinner <- tagList(#
    waiter::spin_chasing_dots(),
    br(),
    span(str_c(
      "Loading ",
      as.character(golem::get_golem_options("pti.name")), "..."
    ),
    style = "color: white;"))
  
  
  full_ui <- get_golem_options("full_ui")
  if (!is.null(full_ui) && !full_ui)
  {
    full_ui <- FALSE
  } else {
    full_ui <- TRUE
  }
  
  pages <- 
    tagList(
      navbarPage(
        title = div(div(
          id = "img-logo-navbar",
          img(src = "www/WBG_Horizontal-black-web.png",
              style = "width: auto; height: 40px;")
        ),
        as.character(golem::get_golem_options("pti.name"))
        ), 
        collapsible = TRUE,
        id = "main_sidebar",
        tabPanel("Info"),
        tabPanel("PTI", mod_weights_ui("new_pti_core", full_ui = full_ui)),
        tabPanel("PTI comparison", mod_map_pti_leaf_page_ui("compare_leaf_map")),
        tabPanel("Data explorer", mod_dta_explorer2_ui(id = "explorer_page_leaf")),
        tabPanel("How it works?")
      )
    )
  
  wrt_opt <- golem::get_golem_options("show_waiter")
  
  tagList(
    golem_add_external_resources(),
    use_cicerone(),
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    if (is.null(wrt_opt) | (!is.null(wrt_opt) && wrt_opt)) {waiter::waiter_show_on_load(spinner)},
    pages
  )
}




#' App UI function - for the PTI display only
#' @import shiny
#' @importFrom golem get_golem_options
#' @noRd
app_new_pti_ui <- function(request) {
  
  spinner <- 
    str_c("Loading ", as.character(golem::get_golem_options("pti.name")), "...") %>%
    span(., style = "color: white;") %>%
    tagList(., br(), waiter::spin_chasing_dots())
  
  full_ui <- get_golem_options("full_ui")
  if (!is.null(full_ui) && !full_ui)
  {
    full_ui <- FALSE
  } else {
    full_ui <- TRUE
  }
  
  pages <- 
    tagList(
      navbarPage(
        title = div(div(
          id = "img-logo-navbar",
          img(src = "www/WBG_Horizontal-black-web.png",
              style = "width: auto; height: 40px;")
        ),
        as.character(golem::get_golem_options("pti.name"))
        ), 
        collapsible = TRUE,
        id = "main_sidebar",
        tabPanel("Info"),
        tabPanel("PTI", mod_weights_ui("new_pti_core", full_ui = full_ui)),
        tabPanel("PTI comparison", mod_map_pti_leaf_page_ui("compare_leaf_map")),
        tabPanel("Data explorer", mod_dta_explorer2_ui(id = "explorer_page_leaf"))
        # ,
        # tabPanel("How it works?")
      )
    )
  
  wrt_opt <- golem::get_golem_options("show_waiter")
  
  tagList(
    golem_add_external_resources(),
    use_cicerone(),
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    if (is.null(wrt_opt) | (!is.null(wrt_opt) && wrt_opt)) {waiter::waiter_show_on_load(spinner)},
    pages
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom bsplus use_bs_tooltip
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path('www', app_sys('app/www'))
  
  tags$head(favicon(ext = 'png'),
            bundle_resources(path = app_sys('app/www'),
                             app_title = as.character(golem::get_golem_options("pti.name"))))
}
