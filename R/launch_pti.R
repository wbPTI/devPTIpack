#' Launch a single-page PTI Shiny app
#'
#' Starts a stand-alone PTI app with one page -- the weights input, the
#' calculated index, and the leaflet map living together in either a
#' two-column or a box layout. Use this for a focused viewer when no
#' compare or explorer tabs are needed; for the multi-tab experience
#' use [launch_pti()].
#'
#' @param shp_dta Named list of `sf` tibbles, one per admin level
#'   (e.g. the bundled [ukr_shp]). Each element must carry the
#'   `adminNPcod` / `adminNName` / `geometry` columns.
#' @param inp_dta Named list of metadata tibbles in the package format
#'   (e.g. the bundled [ukr_mtdt_full]); typically built by
#'   [fct_template_reader()].
#' @param ui_type Character. Either `"twocol"` (default) for the
#'   side-by-side weights / map layout or `"box"` for the boxed layout.
#'   Only the first element is used.
#' @param app_name Character or `NULL`. Title shown in the browser tab
#'   and used as the waiter spinner label.
#' @param show_waiter Logical. If `TRUE` (default) a loading spinner
#'   covers the page until the first map render completes.
#' @param show_adm_levels Optional character vector restricting which
#'   admin levels appear in the level selector. `NULL` (default) shows
#'   every level present in `shp_dta`.
#' @param wt_dwnld_options Character vector of weights-tab download
#'   buttons to expose. Subset of `c("data", "weights", "shapes",
#'   "metadata")`.
#' @param map_dwnld_options Character vector of map-tab download
#'   buttons. Subset of `c("shapes", "metadata")`. Forced to `NULL`
#'   when `ui_type = "box"`.
#' @param shapes_path,mtdtpdf_path Character paths used by the download
#'   handlers to locate the source shapefiles and the metadata PDF.
#'   Default `"."` resolves to the working directory at launch.
#' @param map_height,dt_style CSS strings forwarded to the map
#'   container and the weights DataTable respectively.
#' @param ... Additional arguments forwarded to
#'   [mod_ptipage_newsrv()] and to [golem::with_golem_options()].
#'
#' @return A [shiny::shinyApp()] object wrapped in
#'   [golem::with_golem_options()]. Called primarily for its side
#'   effect of starting the Shiny app.
#'
#' @importFrom rlang is_missing dots_list
#' @importFrom shiny shinyApp bootstrapPage reactive
#' @importFrom htmltools tagList
#' @importFrom golem with_golem_options
#' @export
#'
#' @examples
#' \dontrun{
#' launch_pti_onepage(shp_dta = ukr_shp, inp_dta = ukr_mtdt_full)
#'
#' launch_pti_onepage(shp_dta = ukr_shp,
#'                    inp_dta = ukr_mtdt_full,
#'                    ui_type = "box",
#'                    show_waiter = FALSE)
#' }
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



#' Launch a multi-tab PTI Shiny app
#'
#' Starts a full PTI app with a top-level navbar that can include any
#' combination of an info landing tab, the PTI page itself, a PTI
#' compare tab, a data explorer tab, and a how-it-works tab. Use this
#' when stakeholders need the comparison and explorer surfaces; for a
#' focused single-page viewer use [launch_pti_onepage()] instead.
#'
#' @inheritParams launch_pti_onepage
#' @param app_name Character. Title rendered in the navbar via
#'   `add_logo()`; also stored as `pti.name` in the golem options so
#'   downstream modules can reach it through
#'   [golem::get_golem_options()].
#' @param tabs Character vector picking which top-level tabs to show.
#'   Subset of `c("info", "compare", "explorer", "how")`. The PTI tab
#'   is always rendered. Defaults to
#'   `c("info", "compare", "explorer")`.
#'
#' @return A [shiny::shinyApp()] object wrapped in
#'   [golem::with_golem_options()]. Called primarily for its side
#'   effect of starting the Shiny app.
#'
#' @importFrom rlang is_missing dots_list
#' @importFrom shiny shinyApp navbarPage tabPanel reactive fluidRow
#' @importFrom htmltools tagList
#' @importFrom golem with_golem_options
#' @importFrom cicerone use_cicerone
#' @export
#'
#' @examples
#' \dontrun{
#' launch_pti(shp_dta = ukr_shp,
#'            inp_dta = ukr_mtdt_full,
#'            app_name = "Ukraine PTI demo")
#'
#' launch_pti(shp_dta = ukr_shp,
#'            inp_dta = ukr_mtdt_full,
#'            tabs = c("info", "compare", "explorer", "how"))
#' }
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


