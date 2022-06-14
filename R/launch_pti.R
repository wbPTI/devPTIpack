#' Start two-col or box-based PTI
#' 
#' @importFrom rlang is_missing
#' 
#' @inheritParams mod_ptipage_twocol_ui
#' @inheritParams mod_ptipage_newsrv
#' @inheritParams mod_waiter_newsrv
#' @param ui_type character or "twocol" or "box" defines the type of layout used.
#' 
#' @description Launches a two-column PTI module stand-alone.
#' 
#' @examples
#'\dontrun{
#' launch_pti_onepage(shp_dta = devPTIpack::ukr_shp, 
#'                    inp_dta = devPTIpack::ukr_mtdt_full)
#' launch_pti_onepage(shp_dta = devPTIpack::ukr_shp, 
#'                    inp_dta = devPTIpack::ukr_mtdt_full, 
#'                    show_waiter = FALSE)
#'}
#' @export
launch_pti_onepage <- 
  function(shp_dta, 
           inp_dta, 
           ui_type = c("twocol", "box"),
           app_name = NULL, 
           show_waiter = TRUE, 
           show_adm_levels = NULL,
           wt_dwnld_options = c("data", "weights", "shapes", "metadata"),
           map_dwnld_options = c("shapes", "metadata"),
           shapes_path = ".",
           mtdtpdf_path = ".",
           map_height = "calc(100vh)", 
           dt_style = "zoom:1; height: calc(95vh - 250px);", 
           ...) {
    
    if (rlang::is_missing(shp_dta)) {
      stop("'shp_dta' is missing. Provide a valid list with geometries!")
    }
    
    if (rlang::is_missing(inp_dta)) {
      stop("'inp_dta' is missing. Provide a valid list with metadata!")
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
              show_waiter = show_waiter,
              dt_style = dt_style , 
              ...
        ) 
      ) %>% 
      tagList(golem_add_external_resources())
    
    # server
    server_here <- function(input, output, session) {
      mod_ptipage_newsrv("pagepti",
                         inp_dta = reactive(inp_dta),
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



#' @describeIn launch_pti_onepage start a PTI app with explorer and compare page
#' 
#' @param tabs character vector, where user can decide what PTI components to include.
#'   Options are: c("info", "compare", "explorer", "how").
#' @export
launch_pti <- 
  function(shp_dta, 
           inp_dta, 
           ui_type = c("twocol", "box"),
           app_name = "Some app", 
           tabs = c("info", "compare", "explorer"),
           show_waiter = TRUE, 
           show_adm_levels = NULL,
           wt_dwnld_options = c("data", "weights", "shapes", "metadata"),
           map_dwnld_options = c("shapes", "metadata"),
           shapes_path = ".",
           mtdtpdf_path = ".",
           map_height = "calc(100vh - 60px)", 
           dt_style = "zoom:0.9; height: calc(95vh - 250px);", 
           ...) {
    
    if (rlang::is_missing(shp_dta)) {
      stop("'shp_dta' is missing. Provide a valid list with geometries!")
    }
    
    if (rlang::is_missing(inp_dta)) {
      stop("'inp_dta' is missing. Provide a valid list with metadata!")
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
    
    if (!"info" %in% tabs) {selected_tab <- "PTI"} else {selected_tab <- "Info"}
    # ui
    ui_here <-
      navbarPage(
        title = add_logo(app_name),
        collapsible = TRUE,
        id = "tabpan",
        selected = selected_tab,
        if ("info" %in% tabs) tabPanel("Info", ""),
        tabPanel("PTI",
                 use_cicerone(),
                 ui_fn("pagepti",
                       wt_dwnld_options = wt_dwnld_options,
                       map_dwnld_options = map_dwnld_options,
                       map_height = map_height,
                       show_waiter = show_waiter,
                       dt_style = dt_style) %>% 
                   fluidRow()
        ),
        if ("compare" %in% tabs) tabPanel("PTI comparison", 
                 mod_pti_comparepage_ui("page_comparepti")
        ),
        if ("explorer" %in% tabs) tabPanel("Data explorer",
                 mod_dta_explorer2_ui("explorer_page", 
                                      multi_choice = FALSE, 
                                      height = "calc(100vh - 60px)")
        ),
        if ("how" %in% tabs) tabPanel("How it works?")
      ) %>% 
      tagList(golem_add_external_resources(), use_cicerone())
    
    
    # server
    server_here <- function(input, output, session) {
      
      # Checking what tab is open.
      active_tab <- reactive(input$tabpan)
      # observe(cat("tab: ", active_tab(), "\n"))
      
      # Info tab + guide logic
      mod_infotab_server(NULL, 
                         tabpan_id = "tabpan", 
                         infotab_id = "Info", 
                         firsttab_id = "PTI", 
                         ptitab_id = "PTI", 
                         comparetab_id = "PTI comparison", 
                         exploretab_id = "Data explorer")
      
      plt_dta <- 
        mod_ptipage_newsrv("pagepti",
                           inp_dta = reactive(inp_dta),
                           shp_dta = reactive(shp_dta),
                           show_adm_levels =  show_adm_levels,
                           show_waiter = show_waiter,
                           shapes_path = normalizePath(shapes_path),
                           mtdtpdf_path = normalizePath(mtdtpdf_path),
                           active_tab = active_tab,
                           target_tabs = "PTI",
                           ...
        )
      
      # Compare page visualization
      mod_pti_comparepage_newsrv("page_comparepti",
                                 shp_dta = reactive(shp_dta),
                                 map_dta = plt_dta$map_dta,
                                 wt_dta = plt_dta$wt_dta,
                                 show_adm_levels =  show_adm_levels,
                                 active_tab = active_tab,
                                 target_tabs = "PTI comparison",
                                 mtdtpdf_path = normalizePath(mtdtpdf_path),
                                 shapes_path = normalizePath(shapes_path))

      # Adding explorer
      mod_dta_explorer2_server("explorer_page",
                               shp_dta = reactive(shp_dta),
                               input_dta = reactive(inp_dta),
                               active_tab = active_tab,
                               target_tabs = "Data explorer",
                               mtdtpdf_path = normalizePath(mtdtpdf_path),
                               shapes_path = normalizePath(shapes_path))
    }
    
    with_golem_options(
      app = shinyApp(ui = ui_here, server = server_here), 
      golem_opts = rlang::dots_list(...) %>% append(list(pti.name = app_name))
    )
    
  }


