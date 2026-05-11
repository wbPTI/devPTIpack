#' Map side-panel UI for a PTI page
#'
#' Composes the side-panel of a PTI map: an optional caller-supplied
#' `side_ui` block (typically the weights-input box), the n-bins
#' selector (`mod_get_nbins_ui()`), the admin-level selector
#' (`mod_get_admin_levels_ui()`), and the per-format download links
#' (`mod_map_dwnld_ui()`). The whole panel is rendered inside a
#' floating `shiny::absolutePanel()` overlaid on the leaflet output.
#'
#' @param id Character. Shiny module namespace ID. Wired to the matching
#'   per-widget servers (`mod_get_nbins_srv()`,
#'   `mod_get_admin_levels_srv()`, `mod_map_dwnld_srv()`).
#' @param side_width Numeric. Side-panel width in pixels.
#' @param side_ui Tag list or `NULL`. Caller-supplied content placed at
#'   the top of the panel (e.g. the compact weights-input UI).
#' @param map_dwnld_options Character vector of download options to
#'   expose. Any subset of `c("data", "weights", "shapes", "metadata")`.
#'   `NULL` or empty means no download options.
#' @param ... Unused; retained for forward compatibility.
#'
#' @return A `shiny::absolutePanel()` containing the assembled side
#'   panel.
#'
#' @importFrom shiny NS tagList absolutePanel tags
#' @family shiny-modules
#' @export
#'
#' @examples
#' \dontrun{
#' mod_leaf_side_panel_ui("page_pti", side_width = 250)
#' }
mod_leaf_side_panel_ui <- function(id, side_width = 200, side_ui = NULL,
                                   map_dwnld_options = c("shapes", "metadata"),
                                   ...){
  ns <- NS(id)

  absolutePanel(
    id = "nbins_panel",
    style =
      "z-index:950; min-width: 250px; background-color: rgba(255,255,255,0.8);
    box-shadow: 0 0 15px rgba(0,0,0,0.2); border-radius: 5px;
    padding: 10px 10px 10px 10px;
    zoom: 0.85; transition: opacity 500ms 1s;",
    fixed = FALSE,
    draggable = FALSE,
    left = "auto", bottom = "auto",
    width = side_width,
    height = "auto",
    top = 10, right = 10,

    tagList(
      side_ui,
      if (!is.null(side_ui)) hr(style = "margin: 5px;"),
      mod_get_nbins_ui(id),
      mod_get_admin_levels_ui(id),
      mod_map_dwnld_ui(id, map_dwnld_options)
    )
  )
}



# Get n_bins ---------------------------------------------------------------

#' UI for the n-bins selector
#'
#' Renders a `shiny::numericInput()` for the number of bins used when
#' discretising PTI scores into legend categories. Default is 5; the
#' input is bounded below by 2.
#'
#' @param id Character. Shiny module namespace ID.
#' @param label Character. Visible input label.
#'
#' @return A `shiny::tagList()` containing the numeric input.
#'
#' @importFrom shiny NS tagList numericInput tags
#' @noRd
mod_get_nbins_ui <- function(id, label = "Number of bins") {
  ns <- NS(id)
  numericInput(
    ns("nbins_number"),
    label = tags$span(
      label,
      tags$br(),
      tags$i(style = "font-size:10px; font-weight: 100;",
             "(Subnational areas will be grouped into bins)")
    ),
    value = 5,
    min = 2,
    step = 1,
    width = "100%"
  ) %>%
    tagList()
}


#' Server for the n-bins selector
#'
#' Returns a `reactive()` exposing the user's chosen number of bins,
#' debounced by 500ms so the downstream legend recompute does not flush
#' on every keystroke. Falls back to `n_default` when the input is
#' empty or invalid.
#'
#' @param id Character. Shiny module namespace ID.
#' @param n_default Integer. Default number of bins when the user has
#'   not entered a valid value.
#'
#' @return A debounced `reactive()` yielding the integer bin count.
#'
#' @importFrom shiny debounce reactive moduleServer isTruthy
#' @noRd
mod_get_nbins_srv <- function(id, n_default = 5) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      reactive({
        if (isTruthy(input$nbins_number)) {
          return(input$nbins_number)
        } else {
          return(n_default)
        }
      }) %>% debounce(500)
    })
}


# Get admin levels ------------------------------------------------


#' UI for the admin-level selector
#'
#' Server-rendered `shiny::uiOutput()` slot populated by
#' [mod_get_admin_levels_srv()]. The actual control depends on the
#' admin levels available for the current PTI dataset.
#'
#' @param id Character. Shiny module namespace ID.
#'
#' @return A `shiny::tagList()` containing the `uiOutput()` slot.
#'
#' @importFrom shiny NS tagList uiOutput
#' @noRd
mod_get_admin_levels_ui <-
  function(id) {
    ns <- NS(id)
    shiny::uiOutput(ns("admin_levels")) %>%
      tagList()
  }


#' Server for the admin-level selector
#'
#' Resolves which admin levels should be passed downstream as the
#' "currently selected" set, based on (in priority order)
#' `default_adm_level`, `show_adm_levels`, and the levels actually
#' present in `cur_levels()`. Also reads the matching Golem options
#' (`show_adm_levels`) so deployment can override at startup. The
#' choose-from-list path (`choose_adm_levels`) is currently disabled
#' but its parameters are retained for forward compatibility.
#'
#' @param id Character. Shiny module namespace ID.
#' @param cur_levels Reactive yielding the named character vector of
#'   admin levels available for the current PTI data (e.g.
#'   `c(admin1 = "Oblast", admin2 = "Rayon")`).
#' @param show_adm_levels Character vector or `NULL`. Admin levels to
#'   show. When a single non-matching value is given the function falls
#'   back to the most disaggregated available level.
#' @param show_adm_opt Character. Golem option key consulted as a
#'   fallback for `show_adm_levels`.
#' @param default_adm_level Character or `NULL`. Default admin level.
#'   Takes precedence over `show_adm_levels` when supplied. The literal
#'   string `"all"` (case-insensitive) leaves all levels selected.
#' @param choose_adm_levels,def_adm_opt,choose_adm_opt Disabled. Retained
#'   for forward compatibility.
#' @param ... Unused; retained for forward compatibility.
#'
#' @return A `reactive()` yielding the resolved admin-level vector.
#'
#' @importFrom shiny moduleServer reactiveVal observeEvent reactive
#'   isTruthy req
#' @importFrom stringr str_detect regex
#' @noRd
mod_get_admin_levels_srv <- function(id,
                                     cur_levels,
                                     show_adm_levels = NULL,
                                     show_adm_opt = "show_adm_levels",
                                     default_adm_level = NULL,
                                     choose_adm_levels = NULL,
                                     def_adm_opt = "default_adm_level",
                                     choose_adm_opt = "choose_adm_levels",
                                     ...) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      out_dta <- reactiveVal(NULL)
      chb_iu <- reactiveVal(NULL)
      dta_levels <- reactiveVal(NULL)

      if(!is.null(get_golem_options(show_adm_opt))) show_adm_levels <- get_golem_options(show_adm_opt)
      # if(!is.null(get_golem_options(choose_adm_opt))) choose_adm_levels <- get_golem_options(choose_adm_opt)
      # if(!is.null(get_golem_options(def_adm_opt))) default_adm_level <- get_golem_options(def_adm_opt)

      observeEvent(
        cur_levels(),
        {
          req(cur_levels())
          cur_levels() %>% dta_levels()
          # if (!is.null(choose_adm_levels) && choose_adm_levels) {
          #   if (!is.null(show_adm_levels) &&
          #       (any(show_adm_levels %in% names(dta_levels())) |
          #        any(show_adm_levels %in% dta_levels()))
          #       ) {
          #     dta_levels() %>%
          #       `[`(names(.) %in% show_adm_levels |
          #             (.) %in% show_adm_levels) %>%
          #       dta_levels()
          #
          #   }
          #
          #   plot_levels <- c(all = "All", dta_levels())  %>% unname()
          #
          #   radioButtons(ns("adm_lvls_chb"), NULL, plot_levels, "All", inline = TRUE) %>%
          #     column(12, .) %>%
          #     fluidRow(align="center") %>%
          #     chb_iu()
          #
          #   dta_levels() %>% out_dta()
          #
          # } else
          {
            {
              if (isTruthy(default_adm_level)) {
                if (any(default_adm_level %in% names(dta_levels())) |
                    any(default_adm_level %in% dta_levels())) {
                  dta_levels() %>%
                    `[`(names(.) %in% default_adm_level |
                          (.) %in% default_adm_level) %>%
                    dta_levels()

                } else if (any(str_detect(default_adm_level, regex("all", ignore_case = T)))) {
                  # Do nothing and return full data

                } else if (!(any(default_adm_level %in% names(dta_levels())) |
                             any(default_adm_level %in% dta_levels()))) {
                  dta_levels() %>%
                    `[`(length(.)) %>%
                    dta_levels()
                }
              } else {
                if (isTruthy(show_adm_levels) &&
                    any(show_adm_levels %in% names(dta_levels()) |
                        show_adm_levels %in% dta_levels())) {

                  dta_levels() %>%
                    `[`(names(.) %in% show_adm_levels |
                          (.) %in% show_adm_levels) %>%
                    dta_levels()

                } else if (isTruthy(show_adm_levels) &&
                           !any(show_adm_levels %in% names(dta_levels()) |
                                show_adm_levels %in% dta_levels()) &&
                           length(show_adm_levels) == 1) {

                  dta_levels() %>% `[`(length(.)) %>% dta_levels()

                }
              }
            }

            dta_levels() %>% out_dta()

          }
        },
        ignoreNULL = FALSE,
        ignoreInit = FALSE)

      # output[["admin_levels"]] <- renderUI({req(chb_iu())})
      #
      # adm_lvl_debounce <- reactive(input$adm_lvls_chb) %>% debounce(700)
      #
      # observeEvent(adm_lvl_debounce(), {
      #   req(chb_iu())
      #   req(adm_lvl_debounce())
      #
      #   if (any(stringr::str_detect(adm_lvl_debounce(),
      #                               stringr::regex("all", ignore_case = TRUE)))) {
      #
      #     dta_levels() %>% out_dta()
      #
      #   } else {
      #
      #     adm_lvl_debounce() %>% out_dta()
      #
      #   }
      # },
      # ignoreNULL = FALSE,
      # ignoreInit = FALSE)

      reactive({out_dta()})

    })
}



# Map download as image ---------------------------------------------------

#' UI for the map-download links block
#'
#' Renders the "Export map as .png or .pdf" line plus, when
#' `map_dwnld_options` is non-empty, a comma-separated "Download data,
#' weights, shapes and metadata" sentence with one link per requested
#' option. Each link's `outputId` is wired to a paired
#' [mod_dwnld_dta_link_ui()] / [mod_map_dwnld_srv()].
#'
#' @param id Character. Shiny module namespace ID.
#' @param map_dwnld_options Character vector. Any subset of
#'   `c("data", "weights", "shapes", "metadata")`. Empty means just the
#'   image-export links are shown.
#'
#' @return A `shiny::tagList()` containing the export sentence and any
#'   per-option download links.
#'
#' @importFrom shiny NS tags downloadLink tagList
#' @importFrom purrr pmap keep
#' @noRd
mod_map_dwnld_ui <- function(id, map_dwnld_options = c("shapes", "metadata")) {
  ns <- NS(id)

  if (length(map_dwnld_options) > 0) {
    dwnld_text <-
      list(
        c("data", "weights", "shapes", "metadata"),
        c("dta.download.side", "weights.download.side", "shp.files.side", "mtdt.files.side"),
        c("data", "scores", "shapes", "metadata")
      ) %>%
      pmap( ~ {
        if (..1 %in% map_dwnld_options)
          mod_dwnld_dta_link_ui(NULL, ns(..2), ..3, prefix = NULL, suffix = NULL)
        else
          NULL
      }) %>%
      purrr::keep(function(x) !is.null(x))

    if (length(dwnld_text) >= 2) {
      dwnld_text <-
        c(rep(", ", max(length(dwnld_text) - 2, 0)), " and ", "") %>%
        map2(dwnld_text, ~ tagList(.y, .x))
    }

    dwnld_text <-
      dwnld_text %>%
      tagList("Download ", ., ".") %>%
      tags$i() %>%
      tags$p(style = "font-size: 12px;",
             style = "text-align: right; margin: 0 0 0px !important;")
  } else {
    dwnld_text <- tagList()
  }

  tags$p(
    style = "font-size: 12px; ; text-align: right; margin: 0 0 0px !important;",
    tags$i(
      "Export map as ",
      downloadLink(ns("map_png"), ".png"),
      " or ",
      downloadLink(ns("map_pdf"), ".pdf"),
      "."
    ),
    style = "text-align: right; margin: 0 0 0px !important;"
  ) %>%
    tagList(dwnld_text)
  }




#' Server for the map-download module
#'
#' Wires the per-format download handlers (`map_png`, `map_pdf`,
#' `map_html`) and registers the metadata / shapes file streamers via
#' [mod_dwnld_local_file_server()]. The PNG and PDF handlers gather the
#' current `plotting_map()` payload, dispatch through [make_ggmap()] or
#' [make_gg_line_map()] depending on `plot_dta$poly`, and write to the
#' user-chosen file inside a `shiny::withProgress()` block.
#'
#' @param id Character. Shiny module namespace ID.
#' @param plotting_map Reactive yielding the export payload (a list with
#'   `poly`, `shp_dta`, optionally `preplot_dta`, `selected_layer`, and
#'   `show_interval`). Produced by [mod_plot_leaf_export()].
#' @param metadata_path,shapes_path Character or `NULL`. Filesystem
#'   paths to the metadata PDF and shapes archive served via the
#'   per-file download handlers.
#'
#' @return No explicit return value. Called for side effects (registers
#'   the per-format download handlers within the Shiny session).
#'
#' @importFrom shiny moduleServer downloadHandler withProgress incProgress
#' @import ggplot2
#' @noRd
mod_map_dwnld_srv <- function(id, plotting_map, metadata_path = NULL, shapes_path = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$map_png <-
        downloadHandler(
          filename = function() {
            paste("pti-map-", Sys.Date(), ".png", sep="")
          },
          content = function(file) {
            # file.copy(plotting_map(), file)

            #
            withProgress({
              incProgress(4/10, detail = "Gathering all data and generating the plot")
            plot_dta <- plotting_map()
            png(
              filename = file,
              width = 29,
              height = 21,
              units = "cm",
              res = 300
            )

            if (plot_dta$poly) {
              print(do.call(make_ggmap, args = plot_dta))
            } else  {
              print(do.call(make_gg_line_map, args = plot_dta))
            }

            dev.off()

            },
            min = 0,
            value = 0.1,
            message = "Rendering the map as an image.")

            # ggplot2::ggsave(
            #   file,
            #   plot = plotting_map(),
            #   device = "png",
            #   scale = 1,
            #   width = 29,
            #   height = 21,
            #   units = "cm"
            # )

            # message(curl::curl_version()) # check curl is installed
            # if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
            #   chromote::set_default_chromote_object(
            #     chromote::Chromote$new(chromote::Chrome$new(
            #       args = c("--disable-gpu",
            #                "--no-sandbox",
            #                "--disable-dev-shm-usage", # required bc the target easily crashes
            #                c("--force-color-profile", "srgb"))
            #     ))
            #   )
            # }
            #
            #
            # withProgress({
            #   incProgress(1/10, detail = "Gathering all data")
            #   map_example <- tempfile(fileext = ".html")
            #   out_map <- plotting_map()
            #   mapview::mapshot(x = out_map,
            #                    url = map_example, vwidth = 1400,
            #                    vheight = 1150, zoom = 2)
            #
            #   incProgress(4/10, detail = "Exporting the map into a 'png' file")
            #
            #   webshot2::webshot(
            #     url = map_example,
            #     file = file,
            #     vwidth = 1400,
            #     vheight = 1150,
            #     selector = NULL,
            #     cliprect = NULL,
            #     expand = NULL,
            #     delay = 1,
            #     zoom = 2,
            #     useragent = NULL,
            #     max_concurrent = getOption("webshot.concurrent", default = 6)
            #   )
            #
            # },
            # min = 0,
            # value = 0.1,
            # message = "Rendering the map as an image.")

          }
        )

      output$map_pdf <-
        downloadHandler(
          filename = function() {
            paste("pti-map-", Sys.Date(), ".pdf", sep="")
          },
          content = function(file) {


            withProgress({
              incProgress(4/10, detail = "Gathering all data and generating the plot")
              plot_dta <- plotting_map()

              pdf(
                file = file,
                width = 11,
                height = 8.25
              )

              if (plot_dta$poly) {
                print(do.call(make_ggmap, args = plot_dta))
              } else {
                print(do.call(make_gg_line_map, args = plot_dta))
              }

              dev.off()

            },
            min = 0,
            value = 0.1,
            message = "Rendering the map as an image.")


          }
        )

      output$map_html <-
        downloadHandler(
          filename = function() {
            paste("pti-map-", Sys.Date(), ".html", sep="")
          },
          content = function(file) {

            withProgress({
              incProgress(1/10, detail = "Gathering all the data and generating an 'html'.")

              # mapview::mapshot(x = plotting_map(),
              #                  url = file, vwidth = 1400,
              #                  vheight = 1150, zoom = 2)
            },
            min = 0,
            value = 0.1,
            message = "Rendering the map as a single webpage.")

          }
        )

      # if (is.null(metadata_path) || !file.exists(metadata_path)) {
        mod_dwnld_local_file_server("map_metadata", metadata_path)
        mod_dwnld_local_file_server(ns("map_metadata"), metadata_path)
      # }


      # if (is.null(shapes_path) || !file.exists(shapes_path)) {
        mod_dwnld_local_file_server("map_shapes", shapes_path, basename(shapes_path))
        mod_dwnld_local_file_server(ns("map_shapes"), shapes_path, basename(shapes_path))
      # }

    })

}
