#' Aggregate hex-level indicator data to administrative shapes
#'
#' Takes the tibble from [fetch_hex_data()] and aggregates each indicator
#' column to every admin level present in `shp_dta`, using the per-variable
#' weight × function strategy supplied by the deployer. Aggregation is
#' always **hex → admin level directly** (never chained through intermediate
#' levels).
#'
#' Population always aggregates as an unweighted sum regardless of any
#' `population` entry in `strategy` (a warning is emitted and the entry
#' is ignored). The population column is preserved in every output tibble
#' for downstream use by [build_hex_metadata()].
#'
#' @param hex_data Tibble returned by [fetch_hex_data()]: must contain
#'   a `hex_id` column, a `population` column (unless no indicator
#'   requests `weight = "pop"`), and one column per indicator.
#' @param hex_layer The `admin9_Hexagon` sf layer (or plain tibble) from
#'   `shapes.rds`, as built by [make_hex_grid()] and enriched by
#'   [make_admin_lookup()]. Must contain `admin<N>Pcod` (own level) and
#'   parent Pcod columns for every admin level in `shp_dta`.
#' @param shp_dta Named list of sf tibbles (or plain tibbles), one per
#'   admin level including the hex level. Names must follow the
#'   `admin<N>_<HumanName>` convention.
#' @param strategy Named list specifying the aggregation weight and
#'   function for each indicator. **Must** contain a `.default` entry.
#'   Each entry is a named character vector with elements:
#'   - `weight`: `"pop"` (population-weighted), `"area"` (area-weighted),
#'     or `"none"` (unweighted).
#'   - `fun`: `"mean"`, `"sum"`, `"median"`, `"min"`, or `"max"`.
#'     Ignored when `weight` is `"pop"` or `"area"` (always uses
#'     weighted mean).
#'   Temporal variable columns (e.g. `nightlights_2020`) are matched by
#'   stripping the `_<year>` suffix to get the stem, then looking up the
#'   stem in `strategy` before falling back to `.default`.
#'
#' @return Named list of tibbles, one per slot in `shp_dta`. Each tibble
#'   contains `admin<N>Pcod`, `admin<N>Name`, parent Pcod columns,
#'   `population` (aggregated), and one column per indicator.
#'
#' @importFrom dplyr left_join group_by summarise across all_of
#' @importFrom sf st_drop_geometry
#' @importFrom cli cli_warn cli_abort
#' @importFrom tibble as_tibble
#' @importFrom stats weighted.mean median setNames
#' @family data-input
#' @export
#'
#' @examples
#' \dontrun{
#' my_shp    <- readRDS("app-data/shapes.rds")
#' hex_layer <- my_shp$admin9_Hexagon
#' hex_ids   <- hex_layer$admin9Pcod
#' vars      <- use_hex_vars("flood_exposure_15cm_1in100")
#' hex_data  <- fetch_hex_data(hex_ids, vars)
#' aggregated <- aggregate_hex_to_shapes(
#'   hex_data  = hex_data,
#'   hex_layer = hex_layer,
#'   shp_dta   = my_shp,
#'   strategy  = list(
#'     .default                   = c(weight = "pop",  fun = "mean"),
#'     flood_exposure_15cm_1in100 = c(weight = "none", fun = "sum")
#'   )
#' )
#' names(aggregated)
#' }
aggregate_hex_to_shapes <- function(hex_data, hex_layer, shp_dta, strategy) {

  # ── 1. Input validation ──────────────────────────────────────────────
  if (!is.data.frame(hex_data) || !"hex_id" %in% names(hex_data)) {
    cli::cli_abort(c(
      "{.arg hex_data} must be a data frame with a {.col hex_id} column.",
      "i" = "Use {.fn fetch_hex_data} to produce {.arg hex_data}."
    ))
  }
  if (!is.list(shp_dta) || length(shp_dta) == 0L ||
      is.null(names(shp_dta)) || any(!nzchar(names(shp_dta)))) {
    cli::cli_abort("{.arg shp_dta} must be a non-empty named list of sf layers.")
  }
  if (!is.list(strategy) || is.null(strategy$.default)) {
    cli::cli_abort(c(
      "{.arg strategy} must be a list with a {.code .default} entry.",
      "i" = 'Example: {.code list(.default = c(weight = "pop", fun = "mean"))}'
    ))
  }

  # ── 2. Warn if population is in strategy ────────────────────────────
  if ("population" %in% names(strategy)) {
    cli::cli_warn(c(
      "!" = "{.arg strategy} entry for {.val population} is ignored.",
      "i" = "Population always aggregates as {.code weight = 'none', fun = 'sum'}."
    ))
    strategy[["population"]] <- NULL
  }

  # ── 3. .x/.y collision warning ──────────────────────────────────────
  if (any(grepl("\\.(x|y)$", names(hex_data)))) {
    cli::cli_warn(c(
      "!" = "{.arg hex_data} contains {.code .x}/{.code .y} suffixed columns.",
      "i" = "This usually means a canonical name collision in a {.fn full_join}.",
      "i" = "Rename conflicting columns before calling {.fn aggregate_hex_to_shapes}."
    ))
  }

  # ── 4. Detect hex level from hex_layer ──────────────────────────────
  hex_pcod_cols <- grep("^admin[0-9]+Pcod$", names(hex_layer), value = TRUE)
  if (length(hex_pcod_cols) == 0L) {
    cli::cli_abort(c(
      "{.arg hex_layer} has no {.code admin<N>Pcod} column.",
      "i" = "Ensure {.arg hex_layer} comes from {.fn make_hex_grid} + {.fn make_admin_lookup}."
    ))
  }
  lvls_in_hex  <- as.integer(sub("admin([0-9]+)Pcod", "\\1", hex_pcod_cols))
  hex_own_lvl  <- max(lvls_in_hex)
  hex_pcod_col <- paste0("admin", hex_own_lvl, "Pcod")

  # ── 5. Build hex lookup: drop geometry, enrich with Name columns ─────
  hex_lookup <- sf::st_drop_geometry(hex_layer)

  for (slot in names(shp_dta)) {
    m <- regmatches(slot, regexpr("^admin([0-9]+)_", slot))
    if (length(m) == 0L) next
    lvl <- as.integer(sub("admin([0-9]+)_.*", "\\1", m))
    if (lvl == hex_own_lvl) next
    pcod_col <- paste0("admin", lvl, "Pcod")
    name_col <- paste0("admin", lvl, "Name")
    if (!pcod_col %in% names(hex_lookup)) next
    if (name_col %in% names(hex_lookup)) next
    layer_flat <- sf::st_drop_geometry(shp_dta[[slot]])
    avail <- intersect(c(pcod_col, name_col), names(layer_flat))
    if (length(avail) < 2L) next
    name_tbl   <- unique(layer_flat[, avail, drop = FALSE])
    hex_lookup <- dplyr::left_join(hex_lookup, name_tbl, by = pcod_col)
  }

  # ── 6. Validate weight requirements ─────────────────────────────────
  data_cols    <- setdiff(names(hex_data), "hex_id")
  pop_present  <- "population" %in% data_cols
  area_present <- "area" %in% names(hex_lookup)

  all_strats   <- Filter(Negate(is.null),
                         c(list(strategy$.default),
                           strategy[names(strategy) != ".default"]))
  weights_used <- unique(unname(vapply(all_strats, `[[`, character(1L), "weight")))

  if ("pop" %in% weights_used && !pop_present) {
    cli::cli_abort(c(
      "{.arg strategy} requests {.code weight = 'pop'} but {.col population} \\
is absent from {.arg hex_data}.",
      "i" = "Ensure {.fn fetch_hex_data} was called with the population variable."
    ))
  }
  if ("area" %in% weights_used && !area_present) {
    cli::cli_abort(c(
      "{.arg strategy} requests {.code weight = 'area'} but {.col area} \\
is absent from {.arg hex_layer}.",
      "i" = "Ensure {.fn make_hex_grid} output is passed as {.arg hex_layer}."
    ))
  }

  # ── 7. Join hex_data onto the lookup table ───────────────────────────
  hex_joined <- dplyr::left_join(
    hex_lookup,
    hex_data,
    by = stats::setNames("hex_id", hex_pcod_col)
  )

  # ── 8. All admin-level Pcod columns available for grouping ──────────
  all_pcod_cols <- grep("^admin[0-9]+Pcod$", names(hex_lookup), value = TRUE)
  all_lvls      <- as.integer(sub("admin([0-9]+)Pcod", "\\1", all_pcod_cols))

  # ── 9. Aggregate per admin level ─────────────────────────────────────
  indicator_cols <- setdiff(data_cols, "population")
  result <- list()

  for (slot in names(shp_dta)) {
    m_slot <- regmatches(slot, regexpr("^admin([0-9]+)", slot))
    if (length(m_slot) == 0L) next
    lvl      <- as.integer(sub("admin([0-9]+)", "\\1", m_slot))
    pcod_col <- paste0("admin", lvl, "Pcod")
    name_col <- paste0("admin", lvl, "Name")
    if (!pcod_col %in% names(hex_joined)) next

    coarser_pcods <- all_pcod_cols[all_lvls < lvl]
    group_cols    <- intersect(
      unique(c(pcod_col, name_col, coarser_pcods)),
      names(hex_joined)
    )

    grouped   <- dplyr::group_by(hex_joined, dplyr::across(dplyr::all_of(group_cols)))
    # Population must be last so its original vector is available for weighting
    # in earlier expressions (dplyr summarise sees each result in sequence).
    ordered_cols <- c(setdiff(data_cols, "population"),
                      if ("population" %in% data_cols) "population")
    agg_exprs <- hex_agg_build_exprs(ordered_cols, strategy, area_present)
    agg_tbl   <- dplyr::summarise(grouped, !!!agg_exprs, .groups = "drop")

    # Warn about all-NA indicator results (per polygon)
    for (col in indicator_cols) {
      if (!col %in% names(agg_tbl)) next
      na_rows <- is.na(agg_tbl[[col]])
      if (any(na_rows)) {
        na_pcods <- agg_tbl[[pcod_col]][na_rows]
        cli::cli_warn(c(
          "!" = "All hexes are NA for {.col {col}} in \\
{length(na_pcods)} {.field {slot}} polygon{?s}.",
          "i" = "Affected: {.val {na_pcods}}."
        ))
      }
    }

    result[[slot]] <- tibble::as_tibble(agg_tbl)
  }

  result
}


# ── Internal helpers ─────────────────────────────────────────────────────────

# Build named list of bquote aggregation expressions for dplyr::summarise.
# Uses only base/stats functions so no environment capture is needed.
hex_agg_build_exprs <- function(data_cols, strategy, area_present) {
  lapply(setNames(data_cols, data_cols), function(col) {
    if (col == "population") {
      return(bquote(sum(.data[[.(col)]], na.rm = TRUE)))
    }

    stem        <- sub("_[0-9]{4}$", "", col)
    strat       <- strategy[[stem]] %||% strategy[[col]] %||%
                   strategy$.default %||% c(weight = "none", fun = "mean")
    weight_type <- unname(strat["weight"])
    fun_type    <- unname(strat["fun"])

    if (weight_type %in% c("pop", "area") &&
        (weight_type != "area" || area_present)) {
      w_col <- if (weight_type == "pop") "population" else "area"
      bquote({
        .agg_v <- .data[[.(col)]]; .agg_w <- .data[[.(w_col)]]
        if (all(is.na(.agg_v))) NA_real_
        else {
          .agg_v[is.na(.agg_v)] <- 0; .agg_w[is.na(.agg_w)] <- 0
          stats::weighted.mean(.agg_v, .agg_w)
        }
      })
    } else {
      inner <- switch(fun_type,
        "sum"    = quote(sum(.agg_v)),
        "min"    = quote(min(.agg_v)),
        "max"    = quote(max(.agg_v)),
        "median" = quote(stats::median(.agg_v)),
        quote(mean(.agg_v))
      )
      bquote({
        .agg_v <- .data[[.(col)]]
        if (all(is.na(.agg_v))) NA_real_
        else { .agg_v[is.na(.agg_v)] <- 0; .(inner) }
      })
    }
  })
}
