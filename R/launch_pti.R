
#' Start two-col PTI with some sample data
#' 
#' @importFrom rlang is_missing
#' 
#' @inheritParams mod_ptipage_twocol_ui
#' @inheritParams mod_ptipage_newsrv
#' @param ui_type character or "twocol" or "box" defines the type of layout used.
#' 
#' @description Launches a two-column PTI module stand-alone.
#' 
#' @examples
#'\dontrun{
#' launch_pti_onepage(shp_dta = devPTIpack::ukr_shp, 
#'                    imp_dta = devPTIpack::ukr_mtdt_full)
#' launch_pti_onepage(shp_dta = devPTIpack::ukr_shp, 
#'                    imp_dta = devPTIpack::ukr_mtdt_full, 
#'                    show_waiter = FALSE)
#'}
#' @export
launch_pti_onepage <- 
  function(shp_dta, 
           imp_dta, 
           ui_type = c("twocol", "box"),
           app_name = NULL, 
           show_waiter = TRUE, 
           show_adm_levels = NULL,
           wt_dwnld_options = c("data", "weights", "shapes", "metadata"),
           map_dwnld_options = c("shapes", "metadata"),
           shapes_path = ".",
           mtdtpdf_path = ".",
           map_height = "calc(100vh)", 
           dt_style = "zoom:0.9; height: calc(95vh - 250px);", 
           ...) {
    
    if (rlang::is_missing(shp_dta)) {
      stop("'shp_dta' is missing. Provide a valid list with geometries!")
    }
    
    if (rlang::is_missing(imp_dta)) {
      stop("'imp_dta' is missing. Provide a valid list with metadata!")
    }
    
    ui_fn <- 
      switch(ui_type[[1]],
             twocol = mod_ptipage_twocol_ui,
             box = mod_ptipage_box_ui
             )
    
    map_dwnld_options <- 
      switch(ui_type[[1]],
             twocol = map_dwnld_options,
             box = NULL
             )
    
    # ui
    ui_here <-
      bootstrapPage(
        title = app_name,
        ui_fn("pagepti",
              wt_dwnld_options = wt_dwnld_options,
              map_dwnld_options = map_dwnld_options,
              map_height = map_height,
              dt_style = dt_style , 
              ...
              ) 
      )
    
    # server
    server_here <- function(input, output, session) {
      mod_ptipage_newsrv("pagepti",
                         imp_dta = reactive(imp_dta),
                         shp_dta = reactive(shp_dta),
                         show_adm_levels =  show_adm_levels,
                         show_waiter = show_waiter,
                         shapes_path = normalizePath(shapes_path),
                         mtdtpdf_path = normalizePath(mtdtpdf_path),
                         ...
      )
    }
    
    with_golem_options(
      app = shinyApp(ui = ui_here, server = server_here), 
      golem_opts = rlang::dots_list(...)
    )
    
  }


