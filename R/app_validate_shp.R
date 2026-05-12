#' Visual shapefile inspector for PTI deployers
#'
#' Launches a standalone Shiny app that renders the supplied shapefile
#' list on a leaflet map so the deployer can visually spot holes, wrong
#' boundaries, or geometry artefacts before running the calculation
#' pipeline. Internally calls [validate_geometries()] with
#' `error_on_fail = FALSE` and surfaces the structured diagnostic
#' summary alongside the map. The app is read-only -- it never mutates
#' the inputs.
#'
#' Use this in tutorial Step 1 (`build-pti-1-shapefiles.qmd`) or any
#' time you receive a new country's boundary data: a 30-second visual
#' pass catches misaligned admin levels, dropped polygons, and
#' projection mistakes that a structural validator alone misses.
#'
#' @param shp Named list of `sf` tibbles, one slot per admin level
#'   (same structure as the bundled [ukr_shp]). Slot names follow the
#'   `admin<N>_HumanName` convention (e.g. `admin1_Oblast`). Each layer
#'   must carry an `admin<N>Pcod` and `admin<N>Name` column. The app
#'   also renders shapes that fail [validate_geometries()] so the
#'   deployer can see what is wrong instead of getting a console error.
#' @param app_name Character. Title shown in the browser tab and the
#'   page header. Defaults to `"Validate shapefiles"`.
#'
#' @return A [shiny::shinyApp()] object. Called primarily for its side
#'   effect of starting an interactive Shiny app.
#'
#' @seealso [validate_geometries()] for the underlying structural
#'   validator; [launch_pti()] for the full PTI app.
#'
#' @importFrom shiny shinyApp fluidPage titlePanel sidebarLayout
#'   sidebarPanel mainPanel h4 verbatimTextOutput renderText tags NS
#'   moduleServer
#' @importFrom leaflet leaflet leafletOutput renderLeaflet
#'   addProviderTiles addPolygons addLayersControl layersControlOptions
#'   providers fitBounds highlightOptions
#' @importFrom htmltools HTML
#' @importFrom rlang is_missing
#' @importFrom sf st_bbox
#' @importFrom grDevices hcl.colors
#' @family validation
#' @export
#'
#' @examples
#' \dontrun{
#' # Inspect the bundled Rwanda shapefile.
#' app_validate_shp(rwa_shp)
#'
#' # The app still renders if a layer is structurally broken -- useful
#' # for spotting *what* is wrong rather than only *that* something is.
#' broken <- rwa_shp
#' broken$admin1_Province$admin1Pcod <- NULL
#' app_validate_shp(broken)
#' }
app_validate_shp <- function(shp, app_name = "Validate shapefiles") {

  if (rlang::is_missing(shp) || is.null(shp)) {
    stop(
      "'shp' is missing or NULL. Provide a named list of sf tibbles ",
      "(e.g. the bundled `ukr_shp`).",
      call. = FALSE
    )
  }
  if (!is.list(shp)) {
    stop("'shp' must be a list (a named list of sf tibbles).", call. = FALSE)
  }
  if (length(shp) == 0L) {
    stop(
      "'shp' is an empty list. Provide a non-empty named list of sf tibbles.",
      call. = FALSE
    )
  }
  if (is.null(names(shp)) || any(!nzchar(names(shp)))) {
    stop(
      "'shp' must be a named list -- slots should follow the ",
      "'admin<N>_HumanName' convention (e.g. 'admin1_Province').",
      call. = FALSE
    )
  }

  ui <- app_validate_shp_ui(id = "validator", app_name = app_name)
  server <- function(input, output, session) {
    app_validate_shp_server(id = "validator", shp = shp)
  }

  shiny::shinyApp(ui = ui, server = server)
}


#' UI for [app_validate_shp()]
#'
#' Sidebar carries the validation summary; main panel holds the
#' leaflet map. Wrapped in a module namespace so `testServer()` can
#' exercise the server.
#'
#' @param id Character. Shiny module namespace ID.
#' @param app_name Character. Page title.
#'
#' @return A `shiny.tag.list` UI definition.
#' @noRd
app_validate_shp_ui <- function(id, app_name = "Validate shapefiles") {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    title = app_name,
    shiny::titlePanel(app_name),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("Validation status"),
        shiny::verbatimTextOutput(ns("validation_status")),
        shiny::h4("Layer summary"),
        shiny::verbatimTextOutput(ns("validation_summary")),
        shiny::tags$p(
          shiny::tags$small(
            "This view is read-only. To fix problems, edit the source ",
            "GeoJSON / shapefile and rerun the Step 1 pipeline."
          )
        )
      ),
      shiny::mainPanel(
        width = 9,
        leaflet::leafletOutput(ns("map"), height = "calc(100vh - 100px)")
      )
    )
  )
}


#' Server for [app_validate_shp()]
#'
#' Renders the leaflet map (one toggleable layer per admin level) and
#' two summary outputs (`validation_status` + `validation_summary`).
#' Captures [validate_geometries()] with `error_on_fail = FALSE` so the
#' module remains usable even when the shapes structurally fail.
#'
#' @param id Character. Shiny module namespace ID.
#' @param shp Named list of `sf` tibbles, as passed to
#'   [app_validate_shp()].
#'
#' @return Nothing. Called for side effects (renders outputs).
#' @noRd
app_validate_shp_server <- function(id, shp) {
  shiny::moduleServer(id, function(input, output, session) {

    diag <- validate_geometries(shp, error_on_fail = FALSE)

    output$validation_status <- shiny::renderText(diag$status)

    output$validation_summary <- shiny::renderText({
      n_rows <- vapply(shp, NROW, integer(1))
      layer_lines <- paste0("  ", names(shp), " (", n_rows, " polygons)")
      header <- paste0(
        length(shp), " admin layer",
        if (length(shp) == 1L) "" else "s", ":"
      )
      paste(header, paste(layer_lines, collapse = "\n"), sep = "\n")
    })

    output$map <- leaflet::renderLeaflet({
      build_validation_leaflet(shp)
    })
  })
}


#' Build the leaflet map shown by [app_validate_shp()]
#'
#' One filled polygon overlay per admin layer plus a `addLayersControl`
#' so the deployer can toggle individual levels on and off. Skips
#' layers that don't have a `geometry` column or that error out during
#' projection / bbox extraction so a single broken layer doesn't take
#' down the whole map.
#'
#' @param shp Named list of `sf` tibbles.
#'
#' @return A `leaflet` htmlwidget.
#' @noRd
build_validation_leaflet <- function(shp) {
  palette <- grDevices::hcl.colors(max(length(shp), 3L), palette = "Dark 3")

  map <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)

  # Try to fit bounds to the first layer that has a usable bbox.
  bbox <- NULL
  for (lyr in shp) {
    bbox <- tryCatch(sf::st_bbox(lyr), error = function(e) NULL)
    if (!is.null(bbox) && all(is.finite(unlist(bbox)))) break
    bbox <- NULL
  }
  if (!is.null(bbox)) {
    map <- map |> leaflet::fitBounds(
      lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
      lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
    )
  }

  for (i in seq_along(shp)) {
    slot   <- names(shp)[i]
    layer  <- shp[[i]]

    if (!inherits(layer, "sf") || !"geometry" %in% names(layer)) next

    name_col <- grep("Name$", names(layer), value = TRUE)
    pcod_col <- grep("Pcod$", names(layer), value = TRUE)

    name_vec <- if (length(name_col) > 0L) {
      as.character(layer[[name_col[1]]])
    } else {
      as.character(seq_len(nrow(layer)))
    }
    pcod_vec <- if (length(pcod_col) > 0L) {
      as.character(layer[[pcod_col[1]]])
    } else {
      rep("", nrow(layer))
    }

    labels <- paste0(
      "<strong>", slot, "</strong><br/>",
      name_vec, " (", pcod_vec, ")"
    )

    map <- tryCatch(
      map |> leaflet::addPolygons(
        data         = layer,
        group        = slot,
        fillColor    = palette[i],
        fillOpacity  = 0.30,
        color        = palette[i],
        weight       = 1.5,
        opacity      = 0.9,
        label        = lapply(labels, htmltools::HTML),
        highlightOptions = leaflet::highlightOptions(
          weight = 3, color = "#222", fillOpacity = 0.5, bringToFront = TRUE
        )
      ),
      error = function(e) map
    )
  }

  map |> leaflet::addLayersControl(
    overlayGroups = names(shp),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )
}
