#' Materialize download-source paths for [launch_pti()] / [launch_pti_onepage()]
#'
#' For each path argument: when non-`NULL`, normalizes it and returns it
#' unchanged. When `NULL`, falls back to writing the in-memory object to
#' a session-scoped tempfile under a predictable, date-stamped name so
#' the download buttons have something to serve. The metadata PDF has
#' no in-memory equivalent: when `mtdtpdf_path` is `NULL` it stays
#' `NULL` and the corresponding download link is disabled downstream by
#' [mod_dwnld_file_server()].
#'
#' Behaviour rationale (Phase 2.5 §12 row #13): every default
#' `launch_pti()` call before this fix produced broken downloads
#' because path defaults were `"."` (a directory, not a file). Auto-
#' materialization preserves the demo path
#' (`launch_pti(shp_dta = ukr_shp, inp_dta = ukr_mtdt_full)`) while
#' giving "shapes" and "data" download buttons working content. The
#' filename stem is intentionally distinct from any source filename
#' (`pti-shapes-<date>.rds`, `pti-data-export-<date>.xlsx`) so that
#' callers who want users to download the *original* source file can
#' tell at a glance that they need to pass an explicit path.
#'
#' @param shp_dta The named list of `sf` tibbles passed to
#'   [launch_pti()]. Used as the source for `shapes_path` when that
#'   argument is `NULL`.
#' @param inp_dta The named list of metadata tibbles passed to
#'   [launch_pti()]. Used as the source for `data_path` when that
#'   argument is `NULL`.
#' @param shapes_path,mtdtpdf_path,data_path Character or `NULL`. The
#'   user-supplied paths from the launcher. Each one is normalized when
#'   provided; when `NULL`, `shapes_path` and `data_path` are
#'   materialized from `shp_dta` / `inp_dta`, and `mtdtpdf_path` stays
#'   `NULL`.
#'
#' @return Named list with elements `shapes_path`, `mtdtpdf_path`,
#'   `data_path`. `mtdtpdf_path` may be `NULL`; the other two are
#'   always character file paths.
#'
#' @importFrom writexl write_xlsx
#' @noRd
materialize_dwnld_paths <- function(shp_dta, inp_dta,
                                    shapes_path = NULL,
                                    mtdtpdf_path = NULL,
                                    data_path = NULL) {

  if (is.null(shapes_path)) {
    shapes_path <- file.path(
      tempdir(),
      paste0("pti-shapes-", Sys.Date(), ".rds")
    )
    saveRDS(shp_dta, shapes_path)
  } else {
    shapes_path <- normalizePath(shapes_path, mustWork = FALSE)
  }

  if (is.null(data_path)) {
    data_path <- file.path(
      tempdir(),
      paste0("pti-data-export-", Sys.Date(), ".xlsx")
    )
    writexl::write_xlsx(inp_dta, data_path)
  } else {
    data_path <- normalizePath(data_path, mustWork = FALSE)
  }

  if (!is.null(mtdtpdf_path)) {
    mtdtpdf_path <- normalizePath(mtdtpdf_path, mustWork = FALSE)
  }

  list(
    shapes_path  = shapes_path,
    mtdtpdf_path = mtdtpdf_path,
    data_path    = data_path
  )
}


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
#' @param shapes_path,mtdtpdf_path Character or `NULL`. Filesystem
#'   paths served by the download handlers. When `shapes_path` is
#'   `NULL` (the default), `shp_dta` is auto-written to a tempfile so
#'   the "Download shapes" button serves the in-memory data as an
#'   `.rds`. When `mtdtpdf_path` is `NULL` (the default), the metadata
#'   PDF link is disabled (no in-memory equivalent to materialize). To
#'   serve a specific source file, pass the path explicitly.
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
#' @family pti-launch
#' @export
#'
#' @examples
#' \dontrun{
#' launch_pti_onepage(shp_dta = rwa_shp, inp_dta = rwa_mtdt_full)
#'
#' launch_pti_onepage(shp_dta = rwa_shp,
#'                    inp_dta = rwa_mtdt_full,
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
           shapes_path = NULL,
           mtdtpdf_path = NULL,
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

    paths <- materialize_dwnld_paths(
      shp_dta      = shp_dta,
      inp_dta      = inp_dta,
      shapes_path  = shapes_path,
      mtdtpdf_path = mtdtpdf_path
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
                         shapes_path = paths$shapes_path,
                         mtdtpdf_path = paths$mtdtpdf_path,
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
#' @param data_path Character or `NULL`. Filesystem path served by the
#'   data-explorer's "Download data" button. When `NULL` (the default),
#'   `inp_dta` is auto-written to a tempfile so the button serves the
#'   in-memory data as an `.xlsx`. To serve the original source xlsx
#'   instead (preserving its formatting), pass the path explicitly.
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
#' @family pti-launch
#' @export
#'
#' @examples
#' \dontrun{
#' launch_pti(shp_dta = rwa_shp,
#'            inp_dta = rwa_mtdt_full,
#'            app_name = "Rwanda PTI demo")
#'
#' launch_pti(shp_dta = rwa_shp,
#'            inp_dta = rwa_mtdt_full,
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
           shapes_path = NULL,
           mtdtpdf_path = NULL,
           data_path = NULL,
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

    paths <- materialize_dwnld_paths(
      shp_dta      = shp_dta,
      inp_dta      = inp_dta,
      shapes_path  = shapes_path,
      mtdtpdf_path = mtdtpdf_path,
      data_path    = data_path
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
                           shapes_path = paths$shapes_path,
                           mtdtpdf_path = paths$mtdtpdf_path,
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
                                 mtdtpdf_path = paths$mtdtpdf_path,
                                 shapes_path = paths$shapes_path)

      # Adding explorer
      mod_dta_explorer2_server("explorer_page",
                               shp_dta = reactive(shp_dta),
                               input_dta = reactive(inp_dta),
                               active_tab = active_tab,
                               target_tabs = "Data explorer",
                               mtdtpdf_path = paths$mtdtpdf_path,
                               shapes_path = paths$shapes_path,
                               data_path = paths$data_path)
    }
    
    with_golem_options(
      app = shinyApp(ui = ui_here, server = server_here), 
      golem_opts = rlang::dots_list(...) %>% append(list(pti.name = app_name))
    )
    
  }


