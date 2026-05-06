
#' Reshape weighted data into a per-admin x per-scheme plotting list
#'
#' Flattens the nested output of [get_weighted_data()] into a flat list
#' with one entry per (admin level, weighting scheme) pair. Each entry
#' bundles the scheme-specific score columns (renamed to drop the
#' `..<scheme>` suffix) together with the admin geometry, the
#' admin-level identifier, and the scheme code -- the shape consumed by
#' [add_legend_paras()] and [complete_pti_labels()] downstream.
#'
#' @param wghtd_dta Named list keyed by admin level, each element a
#'   list with `pti_data` (tibble of scores in wide form), `pti_codes`
#'   (named character of weighting schemes), and `admin_level` (named
#'   character identifying the admin level).
#'
#' @return Flat named list. Each entry has slots `pti_dta` (tibble with
#'   admin / area / geometry / spatial_name columns plus the
#'   scheme-specific score columns), `pti_codes` (named character of
#'   length 1), and `admin_level` (named character of length 1). Names
#'   are `<admin_level>.<scheme>`.
#'
#' @importFrom purrr map imap set_names
#' @importFrom dplyr contains select rename_with matches
#' @importFrom stringr str_replace str_c
#' @noRd
preplot_reshape_wghtd_dta <- function(wghtd_dta) {
  wghtd_dta %>%
    purrr::map(~ {
      dta <- .x
      dta$pti_codes %>%
        purrr::imap(~ {
          from_name <- str_c("..", .y, "$")
          from_match <- str_c(.y, "$")
          list(
            pti_dta =
              dta$pti_data %>%
              dplyr::select(
                dplyr::contains("admin"),
                dplyr::contains("area"),
                dplyr::contains("geometry"),
                dplyr::contains("spatial_name"),
                dplyr::matches(from_match)
              ) %>%
              dplyr::rename_with(
                ~ stringr::str_replace(., from_name, ""), .cols = matches(from_match)
                ),
            pti_codes = purrr::set_names(.x, .y),
            admin_level = dta$admin_level
          )
        })
    }) %>%
    unlist(recursive = FALSE)
}


#' Extract unique admin levels from a plotting list
#'
#' Pulls the `admin_level` slot from every entry of a flattened
#' plotting list (as returned by [preplot_reshape_wghtd_dta()]) and
#' returns a deduplicated named character vector preserving the order
#' of first appearance.
#'
#' @param dta Flat plotting list as returned by
#'   [preplot_reshape_wghtd_dta()].
#'
#' @return Named character vector of unique admin level codes. Names
#'   are the admin keys (e.g. `admin1`, `admin2`); values are the
#'   display names (e.g. `Oblast`, `Rayon`).
#'
#' @importFrom purrr map as_vector
#' @noRd
get_current_levels <- function(dta) {
  dta %>%
    purrr::map("admin_level") %>%
    unname() %>%
    purrr::as_vector() %>%
    `[`(!duplicated(.))
}


#' Filter a plotting list by admin level
#'
#' Subsets a flattened plotting list to entries matching `to_fltr`.
#' Accepts the literal token `"all"` (case-insensitive, no filter), an
#' admin display value (e.g. `"Oblast"`), or an admin key
#' (e.g. `"admin1"`).
#'
#' @param dta Flat plotting list as returned by
#'   [preplot_reshape_wghtd_dta()].
#' @param to_fltr Character. Admin level(s) to retain. Defaults to
#'   `"all"`.
#'
#' @return Filtered plotting list, or `NULL` when `to_fltr` is empty
#'   / not truthy.
#'
#' @note Pinned bug (PR #25): when `to_fltr` matches admin keys (e.g.
#'   `"admin1"`) the gating branch enters, but the inner `keep()`
#'   predicate compares against admin display values
#'   (e.g. `"Oblast"`), so a key-only filter returns 0 entries. The
#'   value-only path works as expected. Tier-1 test pin:
#'   tests/testthat/test-plot-helpers.R.
#'
#' @importFrom purrr keep
#' @importFrom shiny isTruthy
#' @importFrom stringr str_detect regex
#' @noRd
filter_admin_levels <- function(dta, to_fltr = "all") {

  out <- NULL
  if (shiny::isTruthy(to_fltr)) {

    if (any(str_detect(to_fltr, regex("all", ignore_case = T)))) {
      out <- dta
    }

    if (any(to_fltr %in% get_current_levels(dta)) |
        any(to_fltr %in% names(get_current_levels(dta)))) {
      out <- dta %>% keep(function(x) {x$admin_level %in% to_fltr})
    }

  }

  return(out)

}


#' Attach legend parameters to each plotting-list entry
#'
#' Calls [legend_map_satelite()] on every entry's `pti_score` column
#' and stores the returned legend object (palette function, recode
#' closures, label vectors) under `$leg`. Used to build the leaflet
#' legend / popup payload before polygons are drawn.
#'
#' @param dta Flat plotting list as returned by
#'   [preplot_reshape_wghtd_dta()].
#' @param nbins Integer. Number of legend bins to request. Defaults to
#'   `5`.
#'
#' @return The input list with each entry augmented by a `$leg` slot
#'   (output of [legend_map_satelite()]). When `dta` is not truthy the
#'   input is returned unchanged.
#'
#' @importFrom purrr map
#' @importFrom shiny isTruthy
#' @noRd
add_legend_paras <- function(dta, nbins = 5) {

  if (isTruthy(dta)) {
    dta <-
      dta %>%
      map( ~ {
        .x$leg <-
          legend_map_satelite(
            .x$pti_dta$pti_score,
            n_groups = nbins,
            legend_paras = .x$legend_paras
          )
        .x
      })
  }

  dta
}



#' Append the priority-rank suffix to PTI popup labels
#'
#' Walks each plotting-list entry and appends a `<strong>...</strong>`
#' suffix to the `pti_label` column built from the entry's legend
#' `recode_function(pti_score)`. Run once after [add_legend_paras()] so
#' every polygon's hover-popup ends with its priority-rank category.
#'
#' @param dta Flat plotting list with `$leg` populated, as returned by
#'   [add_legend_paras()].
#'
#' @return The input list with `pti_label` augmented per entry. When
#'   `dta` is not truthy the input is returned unchanged.
#'
#' @importFrom purrr map
#' @importFrom stringr str_c
#' @importFrom dplyr mutate
#' @importFrom shiny isTruthy
#' @noRd
complete_pti_labels <- function(dta) {

  if (isTruthy(dta)) {
    dta <- dta %>%
      purrr::map( ~ {
        .x$pti_dta <-
          .x$pti_dta %>%
          dplyr::mutate(
            pti_label = stringr::str_c(
              pti_label,
              "<strong>",
              .x$leg$recode_function(pti_score),
              "</strong>"
            )
          )
        .x
      })
  }

  dta
}



#' Draw PTI choropleth polygons onto a leaflet map
#'
#' Iterates over a plotting list and adds one
#' [leaflet::addPolygons()] call per (admin level, scheme) entry,
#' colouring fills by `pti_score` via the entry's legend palette and
#' attaching pre-built HTML popup labels. Layers are grouped by
#' `<scheme> (<admin level>)` so [add_pti_poly_controls()] can wire
#' up base-group toggles.
#'
#' @param leaf_map A `leaflet` map or `leaflet_proxy`.
#' @param poly_dta Flat plotting list with `$leg` and completed
#'   `pti_label`s, as returned by [complete_pti_labels()].
#'
#' @return The input map with polygon layers added (or the original
#'   map when `poly_dta` is not truthy).
#'
#' @importFrom purrr reduce map
#' @importFrom stringr str_c
#' @importFrom leaflet addPolygons highlightOptions pathOptions
#' @importFrom shiny HTML isTruthy
#' @noRd
plot_pti_polygons <- function(leaf_map, poly_dta) {
  if(isTruthy(poly_dta)) {
  leaf_map %>%
    list() %>%
    append(poly_dta) %>%
    purrr::reduce(function(x, y) {
      id_var <- stringr::str_c(names(y$admin_level), "Pcod")
      x %>%
        leaflet::addPolygons(
          data = y$pti_dta,
          fillColor = ~ y$leg$pal(y$pti_dta$pti_score),
          label =  map(y$pti_dta$pti_label, ~ shiny::HTML(.)),
          options = pathOptions(pane = "polygons"),
          group = str_c(y$pti_codes, " (", y$admin_level, ")"),
          color = "white",
          weight = 1,
          opacity = 1,
          dashArray = "2",
          fillOpacity = 1,
          highlight = leaflet::highlightOptions(
            weight = 2,
            color = "#666",
            dashArray = "",
            bringToFront = TRUE
          )
        )
    })
  } else {
    leaf_map
  }
}


#' Clear PTI polygon layers from a leaflet map
#'
#' Inverse of [plot_pti_polygons()]: walks the same plotting list and
#' calls [leaflet::clearGroup()] on each `<scheme> (<admin level>)`
#' group, leaving the underlying tiles intact. Used when the
#' weighting scheme or admin-level selection changes.
#'
#' @param leaf_map A `leaflet` map or `leaflet_proxy`.
#' @param poly_dta Flat plotting list as returned by
#'   [complete_pti_labels()].
#'
#' @return The input map with polygon groups cleared (or the original
#'   map when `poly_dta` is not truthy).
#'
#' @importFrom purrr reduce
#' @importFrom stringr str_c
#' @importFrom leaflet clearGroup
#' @importFrom shiny isTruthy
#' @noRd
clean_pti_polygons <- function(leaf_map, poly_dta) {

  if (isTruthy(poly_dta)) {
    leaf_map <-
      leaf_map %>%
      list() %>%
      append(poly_dta) %>%
      purrr::reduce(function(x, y) {
        id_var <- stringr::str_c(names(y$admin_level), "Pcod")
        x %>%
          leaflet::clearGroup(group = str_c(y$pti_codes, " (", y$admin_level, ")"))
      })
  }

  leaf_map
}


#' Add a base-group layer-control panel for PTI polygons
#'
#' Replaces the leaflet map's layer control with one whose base groups
#' enumerate every `<scheme> (<admin level>)` combination in
#' `poly_dta`. Honours the `default_adm_level` golem option to
#' pre-select a specific admin level, and falls back to
#' [check_existing_groups()] when `old_grps` carries a previously
#' selected group across re-renders.
#'
#' @param leaf_map A `leaflet` map or `leaflet_proxy`.
#' @param poly_dta Flat plotting list as returned by
#'   [complete_pti_labels()].
#' @param old_grps Character vector of previously selected group
#'   labels, or `NULL` for the initial render.
#'
#' @return The input map with layer controls (re)added.
#'
#' @importFrom leaflet addLayersControl hideGroup removeLayersControl
#'   showGroup layersControlOptions
#' @importFrom purrr map_chr map as_vector
#' @importFrom stringr str_c str_detect regex
#' @importFrom golem get_golem_options
#' @importFrom shiny isTruthy
#' @noRd
add_pti_poly_controls <- function(leaf_map, poly_dta, old_grps = NULL) {

  grps <- poly_dta %>%
    purrr::map_chr(~{ stringr::str_c(.x$pti_codes, " (", .x$admin_level, ")")}) %>%
    unname()

  adm_level <- get_golem_options("default_adm_level")

  leaf_map <-
    leaf_map %>%
    leaflet::removeLayersControl() %>%
    leaflet::addLayersControl(
      baseGroups = grps,
      position = "bottomright",
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  if (!isTruthy(old_grps) && isTruthy(adm_level) ) {
    grps_in <- poly_dta %>% purrr::map("admin_level") %>%  unname() %>% as_vector()
    out_show <-
      grps %>%
      `[`(str_detect(grps_in, regex(adm_level, ignore_case = T))|
            str_detect(names(grps_in), regex(adm_level, ignore_case = T)))
    if (isTruthy(out_show)) {
      leaf_map <-
        leaf_map %>%
        leaflet::hideGroup(grps) %>%
        leaflet::showGroup(out_show[[1]])
    }
  }

  if (isTruthy(old_grps)) {
    grps2 <- check_existing_groups(grps, old_grps[[length(old_grps)]], adm_level)
    leaf_map <-
      leaf_map %>%
      leaflet::hideGroup(grps2$out_hide) %>%
      leaflet::showGroup(grps2$out_show[[1]])
  }

  leaf_map

}


#' Remove the PTI polygon layer-control panel
#'
#' Hides every `<scheme> (<admin level>)` group present in `poly_dta`
#' and removes the layer-control widget. Inverse of
#' [add_pti_poly_controls()].
#'
#' @param leaf_map A `leaflet` map or `leaflet_proxy`.
#' @param poly_dta Flat plotting list as returned by
#'   [complete_pti_labels()].
#'
#' @return The input map with groups hidden and layer control removed.
#'
#' @importFrom leaflet hideGroup removeLayersControl
#' @importFrom purrr map_chr
#' @importFrom stringr str_c
#' @noRd
clean_pti_poly_controls <- function(leaf_map, poly_dta) {

  grps <-
    poly_dta %>%
    purrr::map_chr(~{ str_c(.x$pti_codes, " (", .x$admin_level, ")")}) %>%
    unname()

  leaf_map <-
    leaf_map %>%
    leaflet::hideGroup(grps) %>%
    leaflet::removeLayersControl()

  leaf_map
}


#' Pick the surviving group when re-rendering after data change
#'
#' Given a fresh set of group labels (`cur_grps`) and a previously
#' selected single label (`old_grps`), returns lists of which groups
#' to show vs. hide on the layer control. Tries an exact match first,
#' falls back to a scheme-name match (stripping the admin-level
#' suffix), and finally to the first remaining group when nothing
#' matches. Called by [add_pti_poly_controls()].
#'
#' @param cur_grps Character vector of currently rendered group
#'   labels (`<scheme> (<admin level>)`).
#' @param old_grps Single character group label previously selected,
#'   or `character(0)` on the initial render.
#' @param priority_group Character. The `default_adm_level` golem
#'   option, currently unused inside the function (retained for API
#'   stability with [add_pti_poly_controls()]).
#'
#' @return List with two elements: `out_show` (groups to show) and
#'   `out_hide` (groups to hide).
#'
#' @importFrom stringr str_replace str_trim str_c str_detect
#' @noRd
check_existing_groups <- function(cur_grps, old_grps, priority_group) {

  if (length(old_grps) > 0) {
    check_this_too <-
      old_grps %>%
      stringr::str_replace(" \\s*\\([^\\)]+\\)", "") %>%
      stringr::str_trim() %>%
      stringr::str_c(., " \\(")

    grps_in <- cur_grps %>% `[`((.) %in% old_grps)

    if (length(grps_in) == 0) {
      grps_in <- cur_grps %>% `[`(str_detect(., check_this_too))
    }
  } else {
    grps_in <- character(0)
  }

  if (length(grps_in) >= 2) {
    grps_in <- grps_in[[1]]
  }

  grps_out <- cur_grps %>% `[`(! ((.) %in% grps_in))

  out_hide <- NULL
  out_show <- NULL

  if (length(grps_in) > 0) {
    out_show <- grps_in
    out_hide <- grps_out
  }

  if (length(grps_in) == 0 & length(grps_out) > 0) {
    out_show <- grps_out[[1]]
    out_hide <- grps_out[!grps_out%in%out_show]
  }

  list(out_show = out_show, out_hide = out_hide)
}
