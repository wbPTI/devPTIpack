#' Validate one geometry layer of a shapes list
#'
#' Walks a single named element of a shapes list (e.g.
#' `existing_shapes["admin1_Oblast"]`) and records issues for layer
#' naming, `sf` class membership, geometry-column presence and naming,
#' geometry types (`POLYGON` / `MULTIPOLYGON` only), uniqueness of
#' `admin<N>Pcod` and `admin<N>Name`, and parent-child mapping coverage to
#' less-aggregated layers. Per-issue findings are emitted as cli alerts
#' as they are discovered; on completion, [finalize_validation()] prints
#' a one-line summary and returns the structured `list(status, summary,
#' issues)` result. Returns invisibly. If `error_on_fail = TRUE` (the
#' default) and a `fail`-level check is recorded, throws an error.
#'
#' @param focus_geom Length-1 named list. The single layer being
#'   validated, named like `admin1_Oblast` (level prefix + underscore +
#'   human name) -- typically the result of subsetting a shapes list with
#'   `existing_shapes[i]`.
#' @param full_geom The full shapes list `focus_geom` was sliced from.
#'   Used to cross-check parent-level `admin<N>Pcod` columns.
#' @param error_on_fail Logical. If `TRUE` (default), throws on any
#'   `fail`-level issue. Pass `FALSE` to inspect the structured result
#'   instead.
#'
#' @return Invisibly, a list with components `status` (`"pass"`,
#'   `"warn"`, or `"fail"`), `summary` (one-line string), and `issues`
#'   (list of issue records).
#'
#' @importFrom purrr walk map_lgl
#' @importFrom dplyr select distinct pull one_of
#' @importFrom glue glue
#' @importFrom stringr str_extract str_replace str_c str_detect
#' @importFrom sf st_geometry_type st_drop_geometry
#' @noRd
validate_single_geom <- function(focus_geom, full_geom, error_on_fail = TRUE) {

  issues <- new_issues()

  focus_df <- focus_geom[[1]]
  focus_cols_names <- focus_df %>% names()

  adm_level <- names(focus_geom)
  adm_name  <- str_extract(adm_level, "(?<=\\_)\\D+")
  adm_level <- str_replace(adm_level, str_c("_", adm_name), "")
  adm_order <- str_extract(adm_level, "\\d") %>% as.numeric()

  if (is.na(adm_order) || is.nan(adm_order) || is.null(adm_order)) {
    issues <- emit_issue(
      issues, "warn", "layer-name-format",
      glue("Layer '{adm_level}_{adm_name}' name does not follow `admin<N>_<HumanName>` (e.g. `admin0_Country`).")
    )
  }

  if (!"sf" %in% class(focus_df)) {
    issues <- emit_issue(
      issues, "warn", "is-sf-class",
      glue("Layer '{adm_level}_{adm_name}' is not an `sf` object -- wrap it with `sf::st_as_sf()`.")
    )
  }

  cols_types <-
    focus_df %>% map_lgl(~ class(.x) %in% c("sfc_GEOMETRY", "sfc") %>% any())

  if (!any(cols_types)) {
    issues <- emit_issue(
      issues, "warn", "geometry-column-present",
      glue("Layer '{adm_level}_{adm_name}' has no geometry column. Expected a column called `geometry` of class `sfc`.")
    )
  } else if (!"geometry" %in% names(cols_types[cols_types])) {
    issues <- emit_issue(
      issues, "warn", "geometry-column-name",
      glue("Layer '{adm_level}_{adm_name}' has its geometry stored in column '{names(cols_types[cols_types])}' rather than 'geometry'. Rename it.")
    )
  }

  if (any(cols_types)) {
    geo_types <- st_geometry_type(focus_df)
    unsupported_geo <-
      geo_types[!geo_types %in% c("POLYGON", "MULTIPOLYGON")] %>%
      unique() %>%
      as.character()

    if (length(unsupported_geo) > 0) {
      issues <- emit_issue(
        issues, "warn", "geometry-type",
        glue("Layer '{adm_level}_{adm_name}' contains unsupported geometry types: {str_c(unsupported_geo, collapse = ', ')}. Only POLYGON / MULTIPOLYGON are accepted.")
      )
    }
  }

  id_col <- str_c(adm_level, "Pcod")
  nm_col <- str_c(adm_level, "Name")

  if (!all(c(nm_col, id_col) %in% names(focus_df))) {
    msg <- glue(
      "Layer '{adm_level}_{adm_name}' is missing one of '{id_col}' / '{nm_col}'. Every spatial dataframe must have these two columns; no further checks can run for this layer."
    )
    issues <- emit_issue(issues, "fail", "pcod-name-present", msg)
    return(finalize_validation(issues, "validate_single_geom", error_on_fail))
  }

  ids <-
    focus_df %>%
    st_drop_geometry() %>%
    dplyr::select(one_of(id_col)) %>%
    dplyr::distinct() %>%
    dplyr::pull()

  if (any(is.na(ids))) {
    issues <- emit_issue(
      issues, "warn", "pcod-na",
      glue("Layer '{adm_level}_{adm_name}' has NA values in '{id_col}'.")
    )
  }

  n_unique <- length(unique(ids[!is.na(ids)]))
  if (n_unique + sum(is.na(ids)) < nrow(focus_df)) {
    issues <- emit_issue(
      issues, "warn", "pcod-unique",
      glue("Layer '{adm_level}_{adm_name}' has {nrow(focus_df)} rows but only {n_unique} unique '{id_col}' values. Every polygon must have a unique identifier.")
    )
  }

  names_vec <-
    focus_df %>%
    st_drop_geometry() %>%
    dplyr::select(one_of(nm_col)) %>%
    dplyr::distinct() %>%
    dplyr::pull()

  if (any(is.na(names_vec))) {
    issues <- emit_issue(
      issues, "warn", "name-na",
      glue("Layer '{adm_level}_{adm_name}' has NA values in '{nm_col}'.")
    )
  }

  n_unique_names <- length(unique(names_vec[!is.na(names_vec)]))
  if (n_unique_names + sum(is.na(names_vec)) < nrow(focus_df)) {
    issues <- emit_issue(
      issues, "warn", "name-unique",
      glue("Layer '{adm_level}_{adm_name}' has {nrow(focus_df)} rows but only {n_unique_names} unique '{nm_col}' values. Every polygon must have a unique name.")
    )
  }

  if (!is.na(adm_order) && adm_order - 1 > 0) {

    previous_admins <-
      names(full_geom) %>%
      `[`(. != names(focus_geom)) %>%
      `[`(as.numeric(str_extract(., "\\d")) < adm_order) %>%
      str_extract("admin\\d")

    previous_levels <- str_c(previous_admins, "Pcod")
    missing_parents <- setdiff(previous_levels, names(focus_df))

    if (length(missing_parents) > 0) {
      issues <- emit_issue(
        issues, "warn", "parent-pcod-present",
        glue("Layer '{adm_level}_{adm_name}' is missing parent P-code column(s): {str_c(missing_parents, collapse = ', ')}.")
      )
    }

    purrr::walk(previous_admins, function(parent_level) {
      check_admin_code <- str_c(parent_level, "Pcod")
      compare_to_data <- full_geom[str_detect(names(full_geom), parent_level)]
      if (length(compare_to_data) == 0) return()
      compare_to_data_df <- compare_to_data[[1]]

      if (!check_admin_code %in% names(compare_to_data_df)) {
        issues <<- emit_issue(
          issues, "warn", "parent-pcod-in-parent",
          glue("Column '{check_admin_code}' is missing in the parent layer '{names(compare_to_data)}'.")
        )
      } else if (check_admin_code %in% names(focus_df)) {
        in_main <-
          focus_df %>%
          st_drop_geometry() %>%
          dplyr::select(one_of(check_admin_code)) %>%
          dplyr::distinct() %>%
          dplyr::pull()

        in_check <-
          compare_to_data_df %>%
          st_drop_geometry() %>%
          dplyr::select(one_of(check_admin_code)) %>%
          dplyr::distinct() %>%
          dplyr::pull()

        orphans <- setdiff(in_main, in_check)
        if (length(orphans) > 0) {
          issues <<- emit_issue(
            issues, "warn", "parent-pcod-cascade",
            glue("Some values in '{check_admin_code}' of '{adm_level}_{adm_name}' are not present in '{names(compare_to_data)}': {str_c(utils::head(orphans, 5), collapse = ', ')}{if (length(orphans) > 5) sprintf(' (and %d more)', length(orphans) - 5) else ''}."),
            details = list(orphans = orphans)
          )
        }
      }
    })
  }

  finalize_validation(issues, "validate_single_geom", error_on_fail)
}


#' Validate every geometry layer in a shapes list
#'
#' Iterates over `existing_shapes` and runs the per-layer checks on
#' each, then performs a top-level mapping-table consistency check:
#' the row count of [get_mt()] applied to the full shapes list must
#' equal the row count of the most-disaggregated layer (admin levels
#' are required to form a strict hierarchy). Per-layer findings are
#' emitted as cli alerts as they are discovered; on completion, prints
#' a single summary and returns the structured `list(status, summary,
#' issues)` result.
#'
#' @param existing_shapes Named list of `sf` tibbles, one per admin
#'   level. Element names must follow `admin<N>_<Name>` (e.g.
#'   `admin1_Oblast`); see [ukr_shp] for the canonical shape.
#' @param error_on_fail Logical. If `TRUE` (default), throws on any
#'   `fail`-level issue at the end of the run. Pass `FALSE` to inspect
#'   the structured result instead.
#'
#' @return Invisibly, a list with components `status` (`"pass"`,
#'   `"warn"`, or `"fail"`), `summary` (one-line string), and `issues`
#'   (list of issue records). Each issue carries `level`, `check`,
#'   `message`, and optional `details`. The `check` field is a stable
#'   short identifier (e.g. `"pcod-unique"`, `"parent-pcod-cascade"`,
#'   `"hierarchy-row-count"`); programmatic callers should branch on it
#'   rather than on `message` text.
#'
#' @importFrom purrr walk map_dbl
#' @importFrom cli cli_h2 cli_alert_success cli_alert_danger
#' @export
#'
#' @examples
#' result <- validate_geometries(ukr_shp)
#' result$status
#' length(result$issues)
validate_geometries <- function(existing_shapes, error_on_fail = TRUE) {

  issues <- new_issues()

  purrr::walk(seq_along(existing_shapes), function(i) {
    cli::cli_h2(sprintf("Layer: %s", names(existing_shapes)[i]))
    layer_res <- validate_single_geom(
      focus_geom    = existing_shapes[i],
      full_geom     = existing_shapes,
      error_on_fail = FALSE
    )
    issues <<- c(issues, layer_res$issues)
  })

  cli::cli_h2("Cross-layer hierarchy")
  max_nrow <- max(map_dbl(existing_shapes, nrow))
  mt_nrow  <- nrow(get_mt(existing_shapes))

  if (mt_nrow != max_nrow) {
    issues <- emit_issue(
      issues, "fail", "hierarchy-row-count",
      sprintf(
        "get_mt() produced %d rows but the most-disaggregated layer has %d rows. Admin levels must form a strict hierarchy where each child polygon maps to exactly one parent.",
        mt_nrow, max_nrow
      )
    )
  } else {
    cli::cli_alert_success("Hierarchy row count matches most-disaggregated layer.")
  }

  finalize_validation(issues, "validate_geometries", error_on_fail)
}
