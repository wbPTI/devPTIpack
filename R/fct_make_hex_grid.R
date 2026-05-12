#' Build an H3 hexagonal grid covering a country boundary
#'
#' Constructs a uniform H3 hexagonal grid as a standard `sf` tibble
#' conforming to the basic `admin9_Hexagon` column contract
#' (`admin0Pcod`, `admin9Pcod`, `admin9Name`, `area`, `geometry`).
#'
#' This is the **first** of the two-step Step-1 hex workflow: this
#' function produces the partial layer; `make_admin_lookup()` (arch-10
#' §3, issue
#' \href{https://github.com/worldbank/devPTIpack/issues/109}{#109})
#' then populates the parent Pcod columns (`admin1Pcod`, `admin2Pcod`,
#' ...) via centroid-in-polygon joins so the layer satisfies the full
#' cascade rule. Calling [validate_geometries()] on a `my_shp` list
#' that contains `make_hex_grid()`'s raw output but no parent Pcods
#' on the hex layer will fail the cascade check -- this is expected.
#' Use `make_admin_lookup()` to complete the cascade before validation.
#'
#' The algorithm has three steps, only two of which involve spatial
#' operations:
#'
#' 1. **Coarse spatial filter** (spatial): find every H3 cell at
#'    `resolution - 2` that intersects the (unioned) country polygon.
#'    Operates on a handful of large cells.
#' 2. **H3 child expansion** (pure H3 math): expand each coarse cell to
#'    all of its `resolution` children deterministically via the H3
#'    hierarchy. No spatial computation.
#' 3. **Centroid filter** (spatial): compute centroids of every fine H3
#'    cell from step 2; retain only those whose centroid lies within
#'    the country polygon. Point-in-polygon is fast even at tens of
#'    thousands of points.
#'
#' Both spatial steps (1 and 3) are wrapped in an `s2` fallback: on
#' error with `s2` enabled, the call is retried with `s2` disabled.
#' `on.exit()` restores the caller's `sf::sf_use_s2()` state regardless
#' of outcome.
#'
#' All hex cells at a given resolution have near-identical area
#' (H3-5 ~252 km², H3-6 ~36 km², H3-7 ~5.16 km²) -- this is expected,
#' not a data error.
#'
#' @param country_polygon `sf` object containing the country boundary.
#'   If multi-row, all rows are unioned via [sf::st_union()] before
#'   proceeding -- the user may pass any admin layer (e.g.
#'   `admin1_Province`) and the function will derive the outer
#'   envelope. If the object carries an `admin0Pcod` column, the first
#'   row's value is inherited onto every hex cell; otherwise
#'   `admin0Pcod` is set to `NA` and a warning is emitted. The input
#'   is automatically reprojected to EPSG:4326 if not already so.
#' @param resolution Integer scalar. H3 resolution for the returned
#'   grid. Acceptable values: `5`, `6`, `7`. Defaults to `6`. The
#'   internal coarse-filter resolution is derived as
#'   `resolution - 2`.
#'
#' @return An `sf` tibble in EPSG:4326 with one row per retained hex
#'   cell. Columns: `admin0Pcod` (character, inherited from input or
#'   `NA`), `admin9Pcod` (character, H3 index at `resolution`,
#'   globally unique), `admin9Name` (character, same as `admin9Pcod`
#'   -- hexagons have no human name), `area` (numeric, km²),
#'   `geometry` (`sfc_POLYGON`).
#'
#' @seealso [make_admin_lookup()] (forthcoming, arch-10 §3) for the
#'   cascade builder that wires hex cells into the parent admin layers;
#'   [validate_geometries()] for the structural validator the
#'   `make_admin_lookup()`-enriched layer satisfies.
#'
#' @importFrom sf st_union st_geometry st_crs st_transform st_within
#'   st_area sf_use_s2 st_as_sf
#' @importFrom dplyr rename mutate select
#' @importFrom rlang is_missing
#' @importFrom units set_units
#' @importFrom h3jsr polygon_to_cells get_children cell_to_point
#'   cell_to_polygon
#' @family data-input
#' @export
#'
#' @examples
#' data(rwa_shp)
#'
#' # Build a coarse H3-5 hex grid over Rwanda (~100 cells; fast for
#' # examples). The default is resolution = 6 (~500 cells, ~40 km^2);
#' # use 7 for ~5 km^2 cells on small countries or high-detail apps.
#' hex <- make_hex_grid(rwa_shp$admin0_Country, resolution = 5)
#' nrow(hex)
#' head(hex)
#'
#' # The function tolerates multi-row input: provinces are unioned to
#' # the country envelope before tiling.
#' hex_from_provinces <- make_hex_grid(
#'   rwa_shp$admin1_Province,
#'   resolution = 5
#' )
make_hex_grid <- function(country_polygon, resolution = 6) {

  # ----- 1. Input validation ------------------------------------------------

  if (rlang::is_missing(country_polygon) || is.null(country_polygon)) {
    stop(
      "'country_polygon' is missing or NULL. Provide an sf object ",
      "(e.g. `rwa_shp$admin0_Country`).",
      call. = FALSE
    )
  }
  if (!inherits(country_polygon, "sf")) {
    stop(
      "'country_polygon' must be an sf object; got '",
      paste(class(country_polygon), collapse = "/"),
      "'.",
      call. = FALSE
    )
  }
  if (nrow(country_polygon) == 0L) {
    stop(
      "'country_polygon' is empty (0 rows). Provide at least one polygon.",
      call. = FALSE
    )
  }
  if (!is.numeric(resolution) || length(resolution) != 1L ||
        is.na(resolution)) {
    stop("'resolution' must be a single integer (5, 6, or 7).", call. = FALSE)
  }
  resolution <- as.integer(resolution)
  if (!resolution %in% c(5L, 6L, 7L)) {
    stop(
      "'resolution' must be one of 5, 6, or 7; got ",
      resolution,
      ".",
      call. = FALSE
    )
  }

  # ----- 2. Force s2 enabled within this call; restore on exit --------------

  s2_state <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(s2_state), add = TRUE)
  sf::sf_use_s2(TRUE)

  # ----- 3. Inherit admin0Pcod (or NA + warn) -------------------------------

  if ("admin0Pcod" %in% names(country_polygon)) {
    admin0Pcod_val <- as.character(country_polygon$admin0Pcod[[1]])
  } else {
    warning(
      "'country_polygon' has no 'admin0Pcod' column; setting admin0Pcod ",
      "to NA on every hex cell. Add the column upstream if cascade ",
      "joins are needed.",
      call. = FALSE
    )
    admin0Pcod_val <- NA_character_
  }

  # ----- 4. Union to a single sfc; reproject to EPSG:4326 -------------------

  poly_sfc <- sf::st_union(sf::st_geometry(country_polygon))
  if (is.na(sf::st_crs(poly_sfc))) {
    stop(
      "'country_polygon' has no CRS set. Use sf::st_set_crs() to declare ",
      "its native CRS first (typically EPSG:4326).",
      call. = FALSE
    )
  }
  if (sf::st_crs(poly_sfc) != sf::st_crs(4326)) {
    poly_sfc <- sf::st_transform(poly_sfc, 4326)
  }

  # ----- 5. Coarse spatial filter (resolution - 2) --------------------------

  coarse_res <- resolution - 2L
  coarse_cells <- with_s2_fallback(function() {
    cells_list <- h3jsr::polygon_to_cells(
      poly_sfc, res = coarse_res, simple = TRUE
    )
    unique(unlist(cells_list, use.names = FALSE))
  })
  if (length(coarse_cells) == 0L) {
    stop(
      "Coarse H3 filter returned 0 cells for the supplied polygon at ",
      "resolution ", coarse_res,
      ". The input polygon may be empty or have an invalid geometry.",
      call. = FALSE
    )
  }

  # ----- 6. H3 child expansion (pure math, no spatial ops) ------------------

  fine_cells <- unique(unlist(
    h3jsr::get_children(coarse_cells, res = resolution, simple = TRUE),
    use.names = FALSE
  ))

  # ----- 7. Centroid filter -------------------------------------------------

  centroids <- h3jsr::cell_to_point(fine_cells, simple = FALSE)
  inside <- with_s2_fallback(function() {
    idx_matrix <- sf::st_within(centroids, poly_sfc, sparse = FALSE)
    as.logical(idx_matrix[, 1L, drop = TRUE])
  })
  retained_cells <- fine_cells[inside]
  if (length(retained_cells) == 0L) {
    stop(
      "Centroid filter retained 0 cells. The polygon may be too small ",
      "for resolution ", resolution,
      " or the centroid-in-polygon test misbehaved.",
      call. = FALSE
    )
  }

  # ----- 8. Build hex polygons + assemble output ----------------------------

  hex_sf <- h3jsr::cell_to_polygon(retained_cells, simple = FALSE)
  hex_sf <- sf::st_as_sf(hex_sf)
  hex_sf <- dplyr::rename(hex_sf, admin9Pcod = "h3_address")
  hex_sf <- dplyr::mutate(
    hex_sf,
    admin0Pcod = admin0Pcod_val,
    admin9Name = admin9Pcod,
    area = as.numeric(
      units::set_units(sf::st_area(geometry), "km^2")
    )
  )
  hex_sf <- dplyr::select(
    hex_sf,
    admin0Pcod, admin9Pcod, admin9Name, area, geometry
  )

  hex_sf
}


#' Run a spatial operation with an s2-disabled fallback
#'
#' Wraps `op` so that an error raised under `sf::sf_use_s2(TRUE)` is
#' retried under `sf::sf_use_s2(FALSE)`. After the fallback path
#' returns (success or failure), `sf::sf_use_s2(TRUE)` is restored
#' so that subsequent calls inside `make_hex_grid()` (e.g.
#' `sf::st_area()`) see s2 enabled -- without it, EPSG:4326 area
#' computation requires the `lwgeom` package, which may not be
#' installed on every deployment.
#'
#' The outer caller is still responsible for the surrounding
#' `on.exit(sf::sf_use_s2(<caller-initial>))` guard that restores
#' the caller's chosen global state when `make_hex_grid()` returns.
#'
#' @param op A zero-argument function performing the spatial
#'   computation. Should be deterministic enough to be safely retried.
#'
#' @return The result of `op()`, possibly under `s2 = FALSE`.
#' @noRd
with_s2_fallback <- function(op) {
  tryCatch(
    op(),
    error = function(e) {
      sf::sf_use_s2(FALSE)
      on.exit(sf::sf_use_s2(TRUE), add = TRUE)
      op()
    }
  )
}
