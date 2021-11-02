#' Run PTI app as standalone Shiny
#'
#' @param ... A series of options to be used inside the app.
#' 
#' @param na_rm_pti TRUE/FALSE, (default FALSE). Specifies if the NAs for some polygons
#'     in the PTI calculations should be assumed zeros. 
#'     
#' @param show_adm_levels,explorer_show_adm specifies what administrative 
#'     boundaries should be shown.
#'     Its value depends on the on the underlining shapes data. If data contains 
#'     administrative boundaries names as admin1, admin2, ..., this parameter should 
#'     be a vector `c()` that contains relevant administrative levels. If kept `NULL`,
#'     as a default specification, all administrative levels are plotted.
#'
#' @param choose_adm_levels,explorer_choose_adm `TRUE`/`FALSE` - enables users 
#'     to select at what 
#'     administrative level to plot all data. If `FALSE` or `NULL` (default), 
#'     the use cannot choose what  
#'     
#' @param default_adm_level,explorer_default_adm sets a default administrative 
#'     level at which data is plotted on the PTI maps and explorer. If 
#'     `choose_adm_levels` is FALSE or not specified, this will be the only 
#'     admin level that the data is plotted at.
#'     
#'     
#' @param show_waiter If true, displays waiter UI when app launches
#' 
#' @param full_ui `TRUE`/`FALSE` - indicates weather complete UI for manipulating 
#'     weights should be added of a simplified one where the weights could only 
#'     be changed and saved.
#'     
#' @param explorer_multiple_var `TRUE`/`FALSE` - indicates weather multiple variables 
#'     selection is allowed in explorer
#'     
#' @param pti_landing_page path to the landing page file, which could be either 
#'     a markdown or an HTML file.  
#' 
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom rlang dots_list
run_new_pti <- function(
  shape_path = NULL,
  data_path = NULL,
  metadata_path = NULL,
  shape_dta = NULL,
  data_dta = NULL,
  ...
) {
  
  options( "golem.app.prod" = FALSE)
  options(shiny.fullstacktrace = FALSE)
  options(shiny.reactlog = FALSE)
  
  with_golem_options(
    app = shinyApp(
      ui = app_new_pti_ui, 
      server = function(input, output, session) {
        app_new_pti_server(
          input, output, session,
          shape_path, data_path, metadata_path, shape_dta, data_dta
        )
      }
    ), 
    golem_opts = rlang::dots_list(...)
  )
}



#' @describeIn run_new_pti runs a DEVELOPMENT version of the PTI with a simplified UI
#' 
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom rlang dots_list
run_dev_map_pti <- function(
  shape_path = NULL,
  data_path = NULL,
  metadata_path = NULL,
  shape_dta = NULL,
  data_dta = NULL,
  ...
) {
  
  options( "golem.app.prod" = FALSE)
  options(shiny.fullstacktrace = FALSE)
  options(shiny.reactlog = FALSE) 
  # devPTIpack::run_app() # add parameters here (if any)
  
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = function(input, output, session) {
        app_server_input_simple(
          input, output, session,
          shape_path = shape_path,
          data_path = data_path,
          metadata_path = metadata_path,
          shape_dta = shape_dta, 
          data_dta = data_dta
        )
      }
    ), 
    golem_opts = rlang::dots_list(...)
  )
}


#' @describeIn run_new_pti Runs a dev ver. of PTI with mainly plotting component.
#' 
#' @param rand_weights weather or not manual weights should be substituted.
#' 
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom rlang dots_list
run_dev_pti_plot <- function(
  shape_path = NULL,
  data_path = NULL,
  metadata_path = NULL,
  shape_dta = NULL,
  data_dta = NULL,
  ...
) {
  
  options( "golem.app.prod" = FALSE)
  options(shiny.fullstacktrace = FALSE)
  # options(shiny.reactlog = FALSE) 
  # devPTIpack::run_app() # add parameters here (if any)
  
  
  with_golem_options(
    app = shinyApp(
      ui = app_server_sample_pti_vis_ui, 
      server = function(input, output, session) {
        app_server_sample_pti_vis(
          input, output, session,
          shape_path = shape_path,
          data_path = data_path,
          metadata_path = metadata_path,
          shape_dta = shape_dta, 
          data_dta = data_dta
        )
      }
    ), 
    golem_opts = rlang::dots_list(...)
  )
}


#' @describeIn run_new_pti Runs a updated version of the PTI app
#' 
#' 
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom rlang dots_list
run_pti <- function(
  shape_path = NULL,
  data_path = NULL,
  metadata_path = NULL,
  ...
) {
  
  options( "golem.app.prod" = TRUE)
  options(shiny.fullstacktrace = FALSE)
  options(shiny.reactlog = FALSE) 
  # devPTIpack::run_app() # add parameters here (if any)
  
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = function(input, output, session) {
        app_server(
          input, output, session,
          shape_path = shape_path,
          data_path = data_path,
          metadata_path = metadata_path
        )
      }
    ), 
    golem_opts = rlang::dots_list(...)
  )
}




#' @describeIn run_new_pti run onepage pti
#' @export
run_onepage_pti <- function(shape_path = NULL,
                            data_path = NULL,
                            metadata_path = NULL,
                            shape_dta = NULL,
                            data_dta = NULL,
                            demo_weights = FALSE,
                            ...) {
  options("golem.app.prod" = FALSE)
  options(shiny.fullstacktrace = TRUE)
  options(shiny.reactlog = FALSE)
  with_golem_options(
    app = shinyApp(
      ui = mod_pti_onepage_ui(id = "onpagepti_1"),
      
      server = function(input, output, session) {
        mod_pti_onepage_server(
          "onpagepti_1",
          shape_path,
          data_path,
          metadata_path,
          shape_dta,
          data_dta,
          demo_weights = demo_weights
        )
        
      }
      
    ),
    
    golem_opts = rlang::dots_list(...)
  )
}

#' @describeIn run_new_pti run onepage pti with sample data
#' @export
run_onepage_pti_sample <- function(demo_weights = TRUE) {
  run_onepage_pti(
    shape_dta = devPTIpack::ukr_shp,
    data_dta = devPTIpack::ukr_mtdt_full,
    demo_weights = demo_weights
    )
}
