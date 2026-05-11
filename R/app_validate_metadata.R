#' Standalone validation app -- geometry + metadata + Data Explorer
#'
#' Launches a Shiny app that runs both [validate_geometries()] and
#' [validate_metadata()] on the supplied in-memory inputs, displays the
#' structured pass / warn / fail status in a sidebar, and embeds the
#' existing Data Explorer ([mod_dta_explorer2_ui()] /
#' [mod_dta_explorer2_server()]) so the deployer can spot-check
#' indicator values on a leaflet map next to the validation report.
#'
#' Use this in tutorial Step 3 (`build-pti-3-metadata.qmd`) after
#' editing the metadata Excel: a single launcher catches schema /
#' pipeline / cross-reference issues that `validate_metadata()` alone
#' surfaces only as console output, and lets the deployer visually
#' confirm indicator distributions before compiling the deployment
#' bundle in Step 5.
#'
#' The app renders even when the validators report `status = "fail"`
#' so the deployer can see *what* is wrong instead of getting an R
#' error and no UI. The Data Explorer is wrapped in `tryCatch()` so a
#' single broken indicator does not take the whole launcher down -- if
#' the explorer fails to construct, only its tab area shows an error
#' message and the validation summary is still readable.
#'
#' @param shp_dta Named list of `sf` tibbles, one slot per admin level
#'   (same structure as the bundled [ukr_shp]). Slot names follow the
#'   `admin<N>_HumanName` convention.
#' @param inp_dta Named list of metadata tibbles as returned by
#'   [fct_template_reader()] (same structure as the bundled
#'   [ukr_mtdt_full]). Must include `general` and `metadata` slots
#'   plus per-admin tibbles.
#' @param app_name Character. Title shown in the browser tab and the
#'   page header. Defaults to `"Validate metadata"`.
#'
#' @return A [shiny::shinyApp()] object wrapped in
#'   [golem::with_golem_options()] so the embedded Data Explorer can
#'   read its `explorer_*` golem options. Called primarily for its
#'   side effect of starting an interactive Shiny app.
#'
#' @seealso [validate_geometries()], [validate_metadata()],
#'   [app_validate_shp()] for the geometry-only sibling, and
#'   [launch_pti()] for the full PTI app.
#'
#' @importFrom shiny shinyApp fluidPage titlePanel sidebarLayout
#'   sidebarPanel mainPanel h4 hr verbatimTextOutput renderText tags
#'   NS moduleServer reactive showNotification
#' @importFrom rlang is_missing
#' @importFrom golem with_golem_options
#' @export
#'
#' @examples
#' \dontrun{
#' # Inspect the bundled Rwanda shapefile + synthetic metadata.
#' app_validate_metadata(rwa_shp, rwa_mtdt_full)
#'
#' # Renders even when the geometry validator fails -- useful for
#' # visualising *what* is broken rather than only *that* something is.
#' broken_shp <- rwa_shp
#' broken_shp$admin1_Province$admin1Pcod <- NULL
#' app_validate_metadata(broken_shp, rwa_mtdt_full)
#' }
app_validate_metadata <- function(shp_dta, inp_dta,
                                  app_name = "Validate metadata") {

  validate_validation_inputs(shp_dta, inp_dta)

  ui <- app_validate_metadata_ui(id = "validator", app_name = app_name)
  server <- function(input, output, session) {
    app_validate_metadata_server(
      id      = "validator",
      shp_dta = shp_dta,
      inp_dta = inp_dta
    )
  }

  golem::with_golem_options(
    app        = shiny::shinyApp(ui = ui, server = server),
    golem_opts = list(pti.name = app_name)
  )
}


#' Defensive input validation for [app_validate_metadata()]
#'
#' Errors with informative messages on missing / NULL / non-list /
#' empty / unnamed inputs. Centralised so the same checks apply to
#' both arguments without duplicated branches in the launcher body.
#'
#' @param shp_dta,inp_dta The launcher arguments, passed through.
#'
#' @return `NULL` invisibly. Side effect: throws on bad input.
#' @noRd
validate_validation_inputs <- function(shp_dta, inp_dta) {
  if (rlang::is_missing(shp_dta) || is.null(shp_dta)) {
    stop(
      "'shp_dta' is missing or NULL. Provide a named list of sf ",
      "tibbles (e.g. the bundled `ukr_shp`).",
      call. = FALSE
    )
  }
  if (rlang::is_missing(inp_dta) || is.null(inp_dta)) {
    stop(
      "'inp_dta' is missing or NULL. Provide a named list of metadata ",
      "tibbles as returned by `fct_template_reader()` (e.g. the bundled ",
      "`ukr_mtdt_full`).",
      call. = FALSE
    )
  }
  if (!is.list(shp_dta) || length(shp_dta) == 0L) {
    stop("'shp_dta' must be a non-empty list.", call. = FALSE)
  }
  if (!is.list(inp_dta) || length(inp_dta) == 0L) {
    stop("'inp_dta' must be a non-empty list.", call. = FALSE)
  }
  if (is.null(names(shp_dta)) || any(!nzchar(names(shp_dta)))) {
    stop(
      "'shp_dta' must be a named list -- slots should follow the ",
      "'admin<N>_HumanName' convention.",
      call. = FALSE
    )
  }
  if (is.null(names(inp_dta)) || any(!nzchar(names(inp_dta)))) {
    stop(
      "'inp_dta' must be a named list with `general`, `metadata`, ",
      "and one tibble per admin level.",
      call. = FALSE
    )
  }
  invisible(NULL)
}


#' UI for [app_validate_metadata()]
#'
#' Sidebar carries the two validation statuses and a free-text summary;
#' main panel holds the embedded Data Explorer ([mod_dta_explorer2_ui()]).
#'
#' @param id Character. Shiny module namespace ID for the validation
#'   outputs. The Data Explorer uses its own (`"explorer"`) namespace.
#' @param app_name Character. Page title.
#'
#' @return A `shiny.tag.list` UI definition.
#' @noRd
app_validate_metadata_ui <- function(id, app_name = "Validate metadata") {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    title = app_name,
    shiny::titlePanel(app_name),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("validate_geometries()"),
        shiny::verbatimTextOutput(ns("geometries_status")),
        shiny::h4("validate_metadata()"),
        shiny::verbatimTextOutput(ns("metadata_status")),
        shiny::hr(),
        shiny::h4("Summary"),
        shiny::verbatimTextOutput(ns("summary")),
        shiny::tags$p(
          shiny::tags$small(
            "Validation runs once at app start. Edit the source data ",
            "and relaunch to re-run."
          )
        )
      ),
      shiny::mainPanel(
        width = 9,
        shiny::h4("Data Explorer"),
        # The validator server is wrapped in `moduleServer(id, ...)`, so
        # the embedded explorer's server-side namespace ends up nested
        # as `<id>-explorer`. Mirror that here on the UI side via
        # `ns("explorer")` -- otherwise the output IDs don't bind and
        # the leaflet + picker render but never populate.
        mod_dta_explorer2_ui(
          ns("explorer"),
          multi_choice = FALSE,
          height = "calc(100vh - 250px)"
        )
      )
    )
  )
}


#' Server for [app_validate_metadata()]
#'
#' Materialises the in-memory `shp_dta` / `inp_dta` to tempfiles via
#' [materialize_dwnld_paths()] so [validate_metadata()] (which reads
#' from disk) can be invoked, runs both validators with
#' `error_on_fail = FALSE`, exposes their statuses + a free-text
#' summary as outputs, and delegates the map / variable-picker UI to
#' [mod_dta_explorer2_server()].
#'
#' Wrapped in a `tryCatch()` so a structurally broken metadata file
#' does not abort the launcher: if [validate_metadata()] errors hard
#' the corresponding output reports `"error"` and the explorer still
#' tries to render.
#'
#' @param id Character. Shiny module namespace ID.
#' @param shp_dta Named list of `sf` tibbles.
#' @param inp_dta Named list of metadata tibbles.
#'
#' @return Nothing. Called for side effects (renders outputs).
#' @noRd
app_validate_metadata_server <- function(id, shp_dta, inp_dta) {
  shiny::moduleServer(id, function(input, output, session) {

    paths <- materialize_dwnld_paths(shp_dta, inp_dta)

    geom_diag <- tryCatch(
      validate_geometries(shp_dta, error_on_fail = FALSE),
      error = function(e) list(status = "error", summary = conditionMessage(e))
    )

    mtdt_diag <- tryCatch(
      validate_metadata(paths$shapes_path, paths$data_path,
                        error_on_fail = FALSE),
      error = function(e) list(status = "error", summary = conditionMessage(e))
    )

    output$geometries_status <- shiny::renderText(geom_diag$status)
    output$metadata_status   <- shiny::renderText(mtdt_diag$status)

    output$summary <- shiny::renderText({
      n_layers <- length(shp_dta)
      n_polys  <- sum(vapply(shp_dta, NROW, integer(1)))
      n_indicators <- if ("metadata" %in% names(inp_dta)) {
        NROW(inp_dta$metadata)
      } else {
        NA_integer_
      }
      paste(
        paste0("Layers: ", n_layers),
        paste0("Polygons (total): ", n_polys),
        paste0("Indicators in metadata sheet: ", n_indicators),
        sep = "\n"
      )
    })

    # Embed the Data Explorer ONLY when both validators pass / warn.
    # The explorer's reactive chain runs the same calc pipeline as
    # validate_metadata; on a "fail" input that pipeline aborts inside
    # the explorer's reactive (where Shiny's default error handler
    # sometimes propagates the error all the way up and crashes the R
    # process -- we hit this on PR #92 visual review). Embedding only
    # when validators are healthy avoids the crash.
    #
    # The validation summary in the sidebar is the actionable output
    # in the fail case anyway: the deployer's first action is to fix
    # what the validators flagged. Surface that explicitly via
    # `showNotification` so the empty right pane isn't confusing.
    statuses_ok <-
      geom_diag$status %in% c("pass", "warn") &&
      mtdt_diag$status %in% c("pass", "warn")

    if (statuses_ok) {
      tryCatch(
        mod_dta_explorer2_server(
          "explorer",
          shp_dta      = shiny::reactive(shp_dta),
          input_dta    = shiny::reactive(inp_dta),
          active_tab   = shiny::reactive("explore"),
          target_tabs  = "explore",
          shapes_path  = paths$shapes_path,
          data_path    = paths$data_path,
          mtdtpdf_path = paths$mtdtpdf_path
        ),
        error = function(e) {
          msg <- paste0(
            "Data Explorer failed to construct: ", conditionMessage(e)
          )
          message(msg)
          shiny::showNotification(msg, type = "error", duration = NULL)
        }
      )
    } else {
      shiny::showNotification(
        paste0(
          "Validation failed (", geom_diag$status, " / ",
          mtdt_diag$status, ") -- Data Explorer disabled. Fix the ",
          "issues listed in the sidebar and relaunch."
        ),
        type = "error", duration = NULL
      )
    }
  })
}
