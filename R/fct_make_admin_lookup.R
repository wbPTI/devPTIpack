#' Build the parent-child P-code cascade for a shapefile list
#'
#' Given a named list of `sf` admin layers (any number, any levels
#' including `admin9_Hexagon`), populates every parent `admin<k>Pcod`
#' column on every layer via centroid-in-polygon joins, so the
#' resulting list satisfies the cascade rule used by the calculation
#' pipeline and [validate_geometries()].
#'
#' This is the **second** of the two-step Step-1 hex workflow:
#' [make_hex_grid()] (arch-10 §2, issue
#' \href{https://github.com/worldbank/devPTIpack/issues/108}{#108})
#' produces the partial `admin9_Hexagon` layer; `make_admin_lookup()`
#' wires it (and every other sub-admin layer) into the parent chain so
#' the list can be saved to `app-data/shapes.rds` and consumed by the
#' app.
#'
#' Algorithm (arch-10 §3.3):
#'
#' 1. **Parse order** -- extract the `<N>` digit from each
#'    `admin<N>_<HumanName>` slot name and sort layers numerically
#'    ascending (coarsest -> finest). Level numbers need not be
#'    contiguous (e.g. `admin0`, `admin1`, `admin2`, `admin9` is a
#'    valid sequence). The order of the input list does not matter.
#' 2. **Pre-flight validation** per layer -- checks the layer is an
#'    `sf` object, that `admin<N>Pcod` and `admin<N>Name` exist + are
#'    unique + have no `NA`, and that an `area` column is present. If
#'    `area` is missing, the function warns and computes it in place
#'    as `as.numeric(units::set_units(sf::st_area(geometry), "km^2"))`
#'    in the layer's existing CRS (assumed EPSG:4326 -- no
#'    reprojection is performed).
#' 3. **Centroid-in-polygon join** per adjacent level pair (parent ->
#'    child). The child's centroids are spatially joined to the
#'    parent's polygons via `sf::st_within()`. Both the centroid step
#'    and the join step are wrapped in an `s2` fallback (see
#'    [make_hex_grid()] for the rationale). When a centroid lies
#'    exactly on a boundary and matches two or more parents, one
#'    parent is picked at random and a warning lists the number of
#'    affected polygons.
#' 4. **Many-to-one validation** -- every child Pcod must match exactly
#'    one parent. Zero matches (orphan child) is an error.
#' 5. **Cascade propagation** -- after the immediate parent Pcod is
#'    populated, every ancestor Pcod (already present on the parent
#'    layer from previous iterations) is also copied onto the child
#'    by joining on the immediate-parent Pcod.
#'
#' Hexagons (`admin9_Hexagon`) are processed identically to any other
#' admin layer; there are no exceptions to the cascade rule.
#'
#' @param shp_list Named list of `sf` objects. Slot names must follow
#'   the `admin<N>_<HumanName>` convention (e.g. `admin1_Province`).
#'   The list may contain any subset of levels 0-9 and need not be in
#'   any particular order. Each layer must already be in **EPSG:4326**;
#'   no reprojection is performed.
#'
#' @return A named list with the same slot names as the input but
#'   every sub-admin layer enriched with all ancestor `admin<k>Pcod`
#'   columns required by the cascade rule. The coarsest layer is
#'   unchanged. The internal lookup table is **not** returned -- it is
#'   an implementation detail.
#'
#' @seealso [make_hex_grid()] for the upstream hex grid builder;
#'   [validate_geometries()] for the structural validator that the
#'   `make_admin_lookup()`-enriched list satisfies.
#'
#' @importFrom sf st_geometry st_crs st_centroid st_drop_geometry
#'   st_join st_within st_area sf_use_s2
#' @importFrom dplyr left_join select all_of
#' @importFrom tibble tibble
#' @importFrom rlang is_missing
#' @importFrom units set_units
#' @family data-input
#' @export
#'
#' @examples
#' data(rwa_shp)
#'
#' # Strip the cascade columns to simulate raw admin layers.
#' raw <- list(
#'   admin0_Country  = rwa_shp$admin0_Country,
#'   admin1_Province = rwa_shp$admin1_Province[, c(
#'     "admin1Pcod", "admin1Name", "area", "geometry"
#'   )],
#'   admin2_District = rwa_shp$admin2_District[, c(
#'     "admin2Pcod", "admin2Name", "area", "geometry"
#'   )]
#' )
#'
#' enriched <- make_admin_lookup(raw)
#' names(enriched$admin2_District)
#' # admin0Pcod, admin1Pcod, admin2Pcod, admin2Name, area, geometry
make_admin_lookup <- function(shp_list) {

  # ----- 1. Input validation ------------------------------------------------

  if (rlang::is_missing(shp_list) || is.null(shp_list)) {
    stop(
      "'shp_list' is missing or NULL. Provide a named list of sf tibbles ",
      "(e.g. the bundled `rwa_shp`).",
      call. = FALSE
    )
  }
  if (!is.list(shp_list)) {
    stop("'shp_list' must be a list.", call. = FALSE)
  }
  if (length(shp_list) == 0L) {
    stop(
      "'shp_list' is empty. Provide at least one admin layer.",
      call. = FALSE
    )
  }
  if (is.null(names(shp_list)) || any(!nzchar(names(shp_list)))) {
    stop(
      "'shp_list' must be a named list -- slots should follow the ",
      "'admin<N>_<HumanName>' convention (e.g. 'admin1_Province').",
      call. = FALSE
    )
  }

  # ----- 2. Parse level digit from each slot name ---------------------------

  parsed <- regmatches(names(shp_list), regexpr("^admin(\\d)_", names(shp_list)))
  if (length(parsed) != length(shp_list)) {
    bad <- names(shp_list)[!grepl("^admin\\d_", names(shp_list))]
    stop(
      "Slot names must match 'admin<N>_<HumanName>' (N in 0-9). Offending: ",
      paste(shQuote(bad), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  levels_int <- as.integer(sub("^admin(\\d)_", "\\1", parsed))

  # Sort coarsest -> finest. Level numbers need not be contiguous.
  ord <- order(levels_int)
  shp_list <- shp_list[ord]
  levels_int <- levels_int[ord]

  # ----- 3. Force s2 enabled within this call; restore on exit --------------

  s2_state <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(s2_state), add = TRUE)
  sf::sf_use_s2(TRUE)

  # ----- 4. Pre-flight validation per layer ---------------------------------

  for (i in seq_along(shp_list)) {
    slot <- names(shp_list)[i]
    lvl <- levels_int[i]
    layer <- shp_list[[slot]]
    pcod_col <- paste0("admin", lvl, "Pcod")
    name_col <- paste0("admin", lvl, "Name")

    if (!inherits(layer, "sf")) {
      stop(
        "Layer '", slot, "' must be an sf object; got '",
        paste(class(layer), collapse = "/"), "'.",
        call. = FALSE
      )
    }
    if (!pcod_col %in% names(layer)) {
      stop(
        "Layer '", slot, "' is missing required column '", pcod_col, "'.",
        call. = FALSE
      )
    }
    if (!name_col %in% names(layer)) {
      stop(
        "Layer '", slot, "' is missing required column '", name_col, "'.",
        call. = FALSE
      )
    }
    if (any(is.na(layer[[pcod_col]]))) {
      stop(
        "Layer '", slot, "': column '", pcod_col, "' contains NA values.",
        call. = FALSE
      )
    }
    if (any(is.na(layer[[name_col]]))) {
      stop(
        "Layer '", slot, "': column '", name_col, "' contains NA values.",
        call. = FALSE
      )
    }
    if (anyDuplicated(layer[[pcod_col]])) {
      stop(
        "Layer '", slot, "': column '", pcod_col,
        "' has duplicate values.",
        call. = FALSE
      )
    }
    if (anyDuplicated(layer[[name_col]])) {
      stop(
        "Layer '", slot, "': column '", name_col,
        "' has duplicate values.",
        call. = FALSE
      )
    }

    if (!"area" %in% names(layer) || !is.numeric(layer[["area"]])) {
      warning(
        "Layer '", slot, "' is missing a numeric 'area' column; ",
        "computing it in km^2 in the layer's existing CRS ",
        "(assumed EPSG:4326).",
        call. = FALSE
      )
      layer[["area"]] <- as.numeric(
        units::set_units(sf::st_area(sf::st_geometry(layer)), "km^2")
      )
      shp_list[[slot]] <- layer
    }
  }

  # ----- 5. Iterate parent -> child pairs in coarse-to-fine order -----------

  if (length(shp_list) < 2L) {
    return(shp_list)
  }

  for (i in 2:length(shp_list)) {
    child_slot <- names(shp_list)[i]
    parent_slot <- names(shp_list)[i - 1]
    parent_lvl <- levels_int[i - 1]
    child_lvl <- levels_int[i]

    parent_pcod <- paste0("admin", parent_lvl, "Pcod")
    child_pcod <- paste0("admin", child_lvl, "Pcod")

    child_layer <- shp_list[[child_slot]]
    parent_layer <- shp_list[[parent_slot]]

    # Centroid-in-polygon join (both spatial steps wrapped in s2 fallback).
    centroids <- with_s2_fallback(function() {
      sf::st_centroid(sf::st_geometry(child_layer))
    })
    centroids_sf <- sf::st_as_sf(
      tibble::tibble(`__child_pcod__` = child_layer[[child_pcod]]),
      geometry = centroids,
      crs = sf::st_crs(child_layer)
    )

    parent_for_join <- parent_layer[, parent_pcod, drop = FALSE]
    joined <- with_s2_fallback(function() {
      sf::st_join(centroids_sf, parent_for_join, join = sf::st_within)
    })
    joined_tbl <- sf::st_drop_geometry(joined)
    names(joined_tbl) <- c(child_pcod, parent_pcod)

    # Tie-break: child centroids landing on a boundary may match multiple
    # parents and produce duplicate rows. Pick a random parent per child;
    # warn with the count of affected children.
    dup_children <- joined_tbl[[child_pcod]][
      duplicated(joined_tbl[[child_pcod]])
    ]
    dup_children <- unique(dup_children)
    if (length(dup_children) > 0L) {
      warning(
        "Centroid-in-polygon tie-break: ", length(dup_children),
        " '", child_slot, "' centroid", if (length(dup_children) == 1L) "" else "s",
        " landed on a boundary between multiple '", parent_slot,
        "' polygons; one parent picked at random per ambiguous child.",
        call. = FALSE
      )
      # Randomly permute then keep the first occurrence per child.
      perm <- sample.int(nrow(joined_tbl))
      joined_tbl <- joined_tbl[perm, , drop = FALSE]
      joined_tbl <- joined_tbl[!duplicated(joined_tbl[[child_pcod]]), ,
                               drop = FALSE]
    }

    # Many-to-one validation: every child must match exactly one parent.
    orphans <- joined_tbl[[child_pcod]][is.na(joined_tbl[[parent_pcod]])]
    if (length(orphans) > 0L) {
      stop(
        "Orphan children: ", length(orphans), " '", child_slot,
        "' centroid", if (length(orphans) == 1L) "" else "s",
        " fell outside every '", parent_slot, "' polygon. Offending Pcod",
        if (length(orphans) == 1L) "" else "s", ": ",
        paste(shQuote(utils::head(orphans, 5)), collapse = ", "),
        if (length(orphans) > 5L) ", ..." else "",
        ".",
        call. = FALSE
      )
    }

    # Cascade propagation: carry every ancestor Pcod from the parent layer
    # by joining on the immediate-parent Pcod we just resolved.
    parent_ancestor_cols <- grep(
      "^admin\\d+Pcod$",
      names(parent_layer),
      value = TRUE
    )
    parent_ancestor_cols <- setdiff(parent_ancestor_cols, parent_pcod)
    if (length(parent_ancestor_cols) > 0L) {
      parent_pcod_table <- sf::st_drop_geometry(parent_layer)[,
        c(parent_pcod, parent_ancestor_cols), drop = FALSE
      ]
      joined_tbl <- dplyr::left_join(
        joined_tbl,
        parent_pcod_table,
        by = parent_pcod
      )
    }

    # Drop any pre-existing parent-Pcod columns on the child to avoid
    # join-suffix collisions; the cascade builder is authoritative.
    existing_parent_cols <- intersect(
      names(child_layer),
      c(parent_pcod, parent_ancestor_cols)
    )
    if (length(existing_parent_cols) > 0L) {
      child_layer <- child_layer[, setdiff(
        names(child_layer), existing_parent_cols
      ), drop = FALSE]
    }

    # Join the lookup back onto the child layer.
    child_layer <- dplyr::left_join(child_layer, joined_tbl, by = child_pcod)
    shp_list[[child_slot]] <- child_layer
  }

  shp_list
}
