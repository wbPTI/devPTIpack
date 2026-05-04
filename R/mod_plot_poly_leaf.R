#' Render and update PTI polygons on a Leaflet map
#'
#' Server module that diffs the incoming `preplot_dta` against the
#' previously rendered map, removes polygons whose data or legend
#' changed, adds new polygons, refreshes the layer-control, and
#' delegates legend rendering to [mod_plot_poly_legend_server()]. Tracks
#' the currently selected layer (driven by Leaflet's
#' `*_groups` input) so other modules can read which layer the user is
#' looking at.
#'
#' @param id Character. Shiny module namespace ID.
#' @param preplot_dta Reactive yielding the post-filter / post-legend
#'   structured PTI list (one element per admin level; each element a
#'   list with `pti_dta`, `pti_codes`, `admin_level`, and a `leg`
#'   palette / labels block).
#' @param shp_dta Reactive yielding a named list of `sf` tibbles, one
#'   per admin level. Forwarded to [mod_plot_leaf_export()].
#' @param leg_type One of `"value"` (numeric labels) or `"priority"`
#'   (categorical priority labels). Forwarded to
#'   [mod_plot_poly_legend_server()].
#' @param ... Unused; retained for forward compatibility.
#'
#' @return The reactive returned by [mod_plot_leaf_export()] -- a
#'   `reactiveVal()` carrying the structured payload other modules use
#'   to reproduce the current map state (e.g. for ggplot export).
#'
#' @importFrom shiny moduleServer observeEvent reactiveVal isTruthy
#' @importFrom leaflet leafletProxy
#' @importFrom purrr keep map_lgl
#' @noRd
mod_plot_poly_leaf_server <- function(id, preplot_dta, shp_dta, leg_type = "value", ...){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    previous_plot <- reactiveVal(NULL)
    remove_old_poly <- reactiveVal(NULL)
    add_new_poly <- reactiveVal(NULL)
    selected_layer <- reactiveVal(NULL)

    observeEvent(
      preplot_dta(), {
        if (isTruthy(previous_plot()) && isTruthy(preplot_dta())) {
          keep_vals <-
            intersect(names(preplot_dta()), names(previous_plot())) %>%
            keep(function(.x) {
              out <- FALSE
              new_dta <- preplot_dta()[[.x]]$pti_dta$pti_score
              old_dta <- previous_plot()[[.x]]$pti_dta$pti_score
              if (isTRUE(all.equal(new_dta, old_dta)))
                out <- TRUE
              out
            }) %>%
            unlist()

          leg_change <-
            keep_vals %>%
            map_lgl(~{
              all.equal(
                preplot_dta()[[.x]]$leg$our_labels,
                previous_plot()[[.x]]$leg$our_labels
                ) %>%
                isTRUE()
            })

          keep_vals <- keep_vals[leg_change]

          previous_plot()[setdiff(names(previous_plot()), keep_vals)] %>% remove_old_poly()
          preplot_dta()[setdiff(names(preplot_dta()), keep_vals)] %>% add_new_poly()

        } else if (isTruthy(previous_plot()) && !isTruthy(preplot_dta())) {
          previous_plot() %>% remove_old_poly()
          add_new_poly(NULL)

        } else if (!isTruthy(previous_plot()) && isTruthy(preplot_dta())) {
          remove_old_poly(NULL)
          preplot_dta() %>% add_new_poly()

        } else {
          remove_old_poly(NULL)
          add_new_poly(NULL)
        }

      },
      ignoreInit = FALSE,
      ignoreNULL = FALSE
    )

    observeEvent(
      input[["leaf_id_groups"]], {
        selected_layer(input[["leaf_id_groups"]])
      }, ignoreNULL = FALSE, ignoreInit = TRUE)

    observeEvent(
      preplot_dta(), {
        if (!isTruthy(preplot_dta())) {
          leaflet::leafletProxy("leaf_id", deferUntilFlush = TRUE) %>%
            clean_pti_polygons(remove_old_poly()) %>%
            clean_pti_poly_controls(remove_old_poly())
          remove_old_poly(NULL)
          add_new_poly(NULL)
          previous_plot(NULL)
        } else {
          leaflet::leafletProxy("leaf_id", deferUntilFlush = TRUE) %>%
            clean_pti_polygons(remove_old_poly()) %>%
            plot_pti_polygons(add_new_poly()) %>%
            clean_pti_poly_controls(remove_old_poly()) %>%
            add_pti_poly_controls(preplot_dta(), selected_layer())
          remove_old_poly(NULL)
          add_new_poly(NULL)
          previous_plot(preplot_dta())
        }

      }, ignoreInit = FALSE, ignoreNULL = FALSE)

    mod_plot_poly_legend_server(NULL, preplot_dta, selected_layer, leg_type = leg_type)

    out <- mod_plot_leaf_export(NULL, shp_dta, preplot_dta, selected_layer)

    out
  })
}


#' Capture the current Leaflet map state for non-Shiny export
#'
#' Server module that mirrors the polygon module's state into a plain
#' `reactiveVal()` so downloaders / report-generators can reproduce the
#' map outside the reactive Leaflet output. Re-emits whenever `shp_dta`,
#' `preplot_dta`, or `selected_layer` changes.
#'
#' @param id Character. Shiny module namespace ID.
#' @param shp_dta Reactive yielding a named list of `sf` tibbles.
#' @param preplot_dta Reactive yielding the post-filter structured PTI
#'   list.
#' @param selected_layer Reactive character giving the currently selected
#'   layer label.
#' @param ... Unused; retained for forward compatibility.
#'
#' @return A `reactiveVal()`. Either `list(poly = FALSE, shp_dta)` (when
#'   only the boundary lines should be reproduced) or
#'   `list(poly = TRUE, preplot_dta, selected_layer, show_interval,
#'   shp_dta)` (when a fully rendered polygon snapshot is available).
#'
#' @importFrom shiny moduleServer observeEvent reactiveVal req
#' @importFrom stringr str_detect
#' @noRd
mod_plot_leaf_export <-
  function(id, shp_dta, preplot_dta, selected_layer, ...) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      leaf_out <- reactiveVal(NULL)

      observeEvent(
        shp_dta(), {
          req(shp_dta())

          list(
            poly = FALSE,
            shp_dta = shp_dta()
          ) %>%
            leaf_out()


        }, ignoreNULL = FALSE, ignoreInit = FALSE)

      observeEvent(
        list(preplot_dta(), selected_layer()), {
          req(preplot_dta())
          req(selected_layer())

          list(
            poly = TRUE,
            preplot_dta = preplot_dta(),
            selected_layer = selected_layer(),
            show_interval = str_detect(ns(""), "explor"),
            shp_dta = shp_dta()
           ) %>%
            leaf_out()


        }, ignoreNULL = FALSE, ignoreInit = FALSE)

      leaf_out
    })
  }




#' Render a single PTI layer as a static ggplot map
#'
#' Picks the layer in `preplot_dta` whose `<pti_codes> (<admin_level>)`
#' label matches `selected_layer[[1]]` and draws its polygons via
#' `ggplot2::geom_sf()`, using the layer's recoded categorical labels
#' and palette. Used when downloading the map as a static image.
#'
#' @param preplot_dta The post-filter / post-legend structured PTI list
#'   (one element per admin level).
#' @param selected_layer Character vector. The first element is the
#'   layer label to render.
#' @param show_interval Logical. If `TRUE`, uses the interval-style
#'   recoded labels (`recode_function_intervals` / `our_labels`); else
#'   uses the categorical recoded labels (`recode_function` /
#'   `our_labels_category`).
#' @param shp_dta Named list of `sf` tibbles or `NULL`. When supplied,
#'   the country name from the coarsest admin level is used as the plot
#'   title.
#' @param ... Unused; retained for forward compatibility.
#'
#' @return A `ggplot2::ggplot` object.
#'
#' @import ggplot2 sf
#' @importFrom dplyr mutate select contains pull
#' @importFrom purrr keep set_names
#' @importFrom stringr str_c
#' @importFrom shiny isTruthy
#' @noRd
make_ggmap <- function(preplot_dta, selected_layer, show_interval = FALSE, shp_dta = NULL, ...) {

  map_to_plot <-
    preplot_dta %>%
    purrr::keep(function(.x) {
      str_c(.x$pti_codes, " (", .x$admin_level, ")") %in% selected_layer[[1]]
    }) %>%
    `[[`(1)

  layer_id <-
    str_c(map_to_plot$pti_codes, " (", map_to_plot$admin_level, ")")

  if (show_interval) {
    plt_dta <-
      map_to_plot$pti_dta %>%
      mutate(
        pti_score_category = map_to_plot$leg$recode_function_intervals(pti_score),
        pti_score_category = factor(pti_score_category,
                                    levels = map_to_plot$leg$recode_function_intervals(map_to_plot$leg$our_values))
      )

    col_list <- map_to_plot$leg$pal(map_to_plot$leg$our_values)
  } else {
    plt_dta <-
      map_to_plot$pti_dta %>%
      mutate(
        pti_score_category = map_to_plot$leg$recode_function(pti_score),
        pti_score_category = factor(pti_score_category,
                                    levels = map_to_plot$leg$our_labels_category)
      )
    col_list <- set_names(
      map_to_plot$leg$pal(map_to_plot$leg$our_values),
      map_to_plot$leg$our_labels_category
    )
  }

  if (isTruthy(shp_dta)) {
    main_lable <-
      shp_dta[[1]] %>%
      select(contains("Name")) %>%
      pull(1) %>%
      unique() %>%
      `[[`(1)
  } else {
    main_lable = NULL
  }
  plt_dta %>%
    ggplot2::ggplot() +
    ggplot2::aes(fill = pti_score_category) +
    ggplot2::geom_sf() +
    ggplot2::scale_fill_manual(values = col_list) +
    ggplot2::labs(fill = layer_id) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = main_lable,
                  subtitle = layer_id)

}




#' Render admin-level boundary lines as a static ggplot map
#'
#' Companion to [make_ggmap()] for downloads where only the outline is
#' wanted (e.g. when no PTI layer is selected). Stacks all admin levels
#' except the finest, using line type / colour / weight to distinguish
#' levels.
#'
#' @param shp_dta Named list of `sf` tibbles, one per admin level.
#' @param ... Unused; retained for forward compatibility.
#'
#' @return A `ggplot2::ggplot` object.
#'
#' @import ggplot2 sf
#' @importFrom dplyr mutate select contains pull
#' @importFrom purrr pmap_dfr
#' @noRd
make_gg_line_map <- function(shp_dta, ...) {
  main_lable <-
    shp_dta[[1]] %>%
    select(contains("Name")) %>%
    pull(1) %>%
    unique() %>%
    `[[`(1)

  dta <-
    shp_dta %>%
    `[`(-length(.)) %>%
    list(.x = ., .y = names(.), .z = rev(seq_along(.)) / max(seq_along(.))) %>%
    pmap_dfr(function(...) {..1 %>% mutate(line = ..2, width = ..3)})
  dta %>%
    ggplot2::ggplot() +
    ggplot2::aes(group = line, linetype = line, colour = line, linewidth  = width) +
    ggplot2::geom_sf(fill = NA) +
    ggplot2::scale_colour_brewer(palette = "Dark2") +
    ggplot2::scale_linewidth_continuous(range = c(0.55, 2)) +
    ggplot2::theme_bw()  +
    ggplot2::theme(legend.position="none") +
    ggplot2::labs(title = main_lable)

}
