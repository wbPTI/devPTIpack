#' Validate one geometry layer of a shapes list
#'
#' Walks a single named element of a shapes list (e.g.
#' `existing_shapes["admin1_Oblast"]`) and emits diagnostics for layer
#' naming, `sf` class membership, geometry-column presence and naming,
#' geometry types (`POLYGON` / `MULTIPOLYGON` only), uniqueness of
#' `admin<N>Pcod` and `admin<N>Name`, and parent-child mapping coverage to
#' less-aggregated layers. Reports problems via `rlang::inform()` /
#' `rlang::warn()` / `rlang::abort()` and a final `testthat::test_that`
#' summary block.
#'
#' @param focus_geom Length-1 named list. The single layer being
#'   validated, named like `admin1_Oblast` (level prefix + underscore +
#'   human name) -- typically the result of subsetting a shapes list with
#'   `existing_shapes[i]`.
#' @param full_geom The full shapes list `focus_geom` was sliced from.
#'   Used to cross-check parent-level `admin<N>Pcod` columns.
#'
#' @return Invisibly `NULL`. Called for side effects (validator messages
#'   and a `testthat` summary).
#'
#' @importFrom purrr walk map_lgl
#' @importFrom testthat test_that expect_false expect_equal
#' @importFrom rlang format_error_bullets inform abort warn
#' @importFrom dplyr select distinct pull one_of
#' @importFrom glue glue
#' @importFrom stringr str_extract str_replace str_c str_detect
#' @importFrom sf st_geometry_type st_drop_geometry
#' @noRd
validate_single_geom <- function(focus_geom, full_geom){

  focus_df <- focus_geom[[1]]
  focus_cols_names <- focus_df %>% names()

  adm_level <- names(focus_geom)

  adm_name <- str_extract(adm_level, "(?<=\\_)\\D+")
  adm_level <- str_replace(adm_level, str_c("_", adm_name), "")
  adm_order <- str_extract(adm_level, "\\d") %>% as.numeric()

  if (is.na(adm_order) || is.nan(adm_order) || is.null(adm_order)) {
    msg <-
      format_error_bullets(c(
        x = glue("{adm_level}_{adm_name} has some problems with layer order specificaiton"),
        I = glue("It has to start from `admin`, followed by a digit 0, 1, 2, 3, 4 or 5 depending on the order."),
        i = glue("Contain underscore `_` and be followed by a name. For example: `admin0_Country`.")
      ))
    tot_warnings <- tot_warnings + 1
    inform(message = msg)
  }


  tot_warnings <- 0
  tot_errors <- 0


  if (!"sf" %in% class(focus_df)) {
    msg <-
      format_error_bullets(c(
        x = glue("{adm_level}_{adm_name} layer does not contain class \"sf\"."),
        x = glue("It is not a geometry containing data frame."),
        i = glue("Use 'sf' package to create a gis data frame")
      ))
    tot_warnings <- tot_warnings + 1
    inform(message = msg)
  }


  cols_types <-
    focus_df %>% map_lgl( ~ class(.x) %in% c("sfc_GEOMETRY", "sfc") %>% any())

  if (!any(cols_types)) {
    msg <-
      format_error_bullets(c(
        x = glue("Geometry column is not present in {adm_level}_{adm_name}"),
        i = glue("Such column should be named 'geometry' and contain classes 'c(\"sfc_GEOMETRY\", \"sfc\")'")
      ))
    tot_warnings <- tot_warnings + 1
    inform(message = msg)
  }

  if (!c("geometry") %in% names(cols_types[cols_types])) {
    msg <-
      format_error_bullets(c(
        x = glue("Geometry column in {adm_level}_{adm_name} is not called \"geometry\""),
        i = glue("It is instead calles \"{names(cols_types[cols_types])}\". Consider renaming.")
      ))
    tot_warnings <- tot_warnings + 1
    inform(message = msg)
  }


  geo_types <- st_geometry_type(focus_df)
  unsupported_geo <-
    geo_types[!geo_types %in% c("POLYGON", "MULTIPOLYGON")] %>%
    unique() %>%
    as.character()

  if (!all(geo_types %in% c("POLYGON", "MULTIPOLYGON"))) {
    msg <-
      format_error_bullets(c(
        x = glue("Some polygons in '{names(cols_types[cols_types])}' columns have unsupported type(s): {unsupported_geo}"),
        x = glue("Make sure that all polygins in {adm_level}_{adm_name} are \"POLYGON\" or \"MULTIPOLYGON\""),
        i = "Use `st_geometry_type()` to check the type of geomerty."
      ))
    tot_warnings <- tot_warnings + 1
    inform(message = msg)
  }


  id_col <- str_c(adm_level, "Pcod")
  nm_col <- str_c(adm_level, "Name")

  if (!all(c(nm_col, id_col) %in% names(focus_df))) {
    msg <-
      format_error_bullets(c(
        x = glue("Column {id_col} or {nm_col} are not in the {adm_level}_{adm_name}."),
        x = glue("There are {str_c(names(focus_df), collapse = ", ")} columns in {adm_level}_{adm_name}."),
        i = "Every spatial dataframe must contain a unique identifier ending with \"Pcod\".",
        i = "No further checks could be made. Fix this problem and re-run the analysis again."
      ))
    tot_errors <- tot_errors + 1
    abort(message = msg)
  }


  n_unique <-
    focus_df %>%
    st_drop_geometry() %>%
    dplyr::select(one_of(id_col)) %>%
    dplyr::distinct() %>%
    nrow()


  if (
    focus_df %>%
    st_drop_geometry() %>%
    dplyr::select(one_of(id_col)) %>%
    dplyr::distinct() %>%
    dplyr::pull() %>%
    is.na() %>%
    any()
  ) {
    msg <-
      format_error_bullets(c(
        x = glue("Some rows are 'NA' in {id_col} in the {adm_level}_{adm_name}")
      ))
    tot_warnings <- tot_warnings + 1
    warn(message = msg)
  }


  if (n_unique < nrow(focus_df)) {
    msg <-
      format_error_bullets(c(
        x = glue("In {adm_level}_{adm_name}, there are {nrow(focus_df)} polygons, but only {n_unique} unique identifiers."),
        i = glue("Every polygon must have a unique identifier.")
      ))
    tot_warnings <- tot_warnings + 1
    warn(message = msg)
  }




  n_unique2 <-
    focus_df %>%
    st_drop_geometry() %>%
    dplyr::select(one_of(nm_col)) %>%
    dplyr::distinct() %>%
    nrow()

  if (n_unique2 < nrow(focus_df)) {
    msg <-
      format_error_bullets(c(
        x = glue("In {adm_level}_{adm_name}, there are {nrow(focus_df)} polygons, but only {n_unique2} unique polygons names."),
        i = glue("Every polygon must have a unique name! (not only a unique identifier).")
      ))
    tot_warnings <- tot_warnings + 1
    warn(message = msg)
  }


  if (
    focus_df %>%
    st_drop_geometry() %>%
    dplyr::select(one_of(nm_col)) %>%
    dplyr::distinct() %>%
    dplyr::pull() %>%
    is.na() %>%
    any()
  ) {
    msg <-
      format_error_bullets(c(
        x = glue("Some rows are 'NA' in {nm_col} in the {adm_level}_{adm_name}")
      ))
    tot_warnings <- tot_warnings + 1
    warn(message = msg)
  }


  if (adm_order-1 > 0) {

    previous_admins <-
      names(full_geom) %>%
      `[`(. != names(focus_geom)) %>%
      `[`(as.numeric(str_extract(., "\\d")) < adm_order) %>%
        str_extract("admin\\d")

    previous_levels <-
      previous_admins %>%
      str_c(., "Pcod")

    if (!all(previous_levels %in% names(focus_df))) {
      msg <-
        format_error_bullets(c(
          x = glue("{adm_level}_{adm_name} must contain columns { str_c(previous_levels, collapse = ', ')}."),
          x = glue("Include columns {str_c(previous_levels [previous_levels %in% names(focus_df)], collapse = ', ')} to the {adm_level}_{adm_name}."),
          i = glue("These columns are used to map more aggregated layers to the less aggregated.")
        ))
      tot_warnings <- tot_warnings + 1
      warn(message = msg)
    }


    previous_admins %>%
      walk(~{
        check_admin <- .x
        check_admin_code <- str_c(check_admin, "Pcod")
        compare_to_data <- full_geom[str_detect(names(full_geom), check_admin)]
        if(length(compare_to_data) == 0) return()
        compare_to_data_df <- compare_to_data[[1]]

        if (!check_admin_code %in% names(compare_to_data_df)) {
          tot_warnings <- tot_warnings + 1
          warn(message =
                 format_error_bullets(c(
                   x = glue(
                     "Column {check_admin_code} is not present in the {names(compare_to_data)}."
                   )
                 )))
        } else {
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

          if (any(!in_main %in% in_check)) {
            tot_warnings <- tot_warnings + 1
            warn(
              message =
                format_error_bullets(c(
                  x = glue(
                    "Some values in column `{check_admin_code}` of `{adm_level}_{adm_name}` are not present in same column of `{names(compare_to_data)}`."
                  )
                ))
            )
          }
        }

      })
  }

  testthat::test_that("number of varnings is not zero in the validation results", {
    testthat::expect_equal(tot_warnings, 0, info = "There are some warning to resolve")
    testthat::expect_equal(tot_errors, 0, info = "There are some errors to resolve")
  })


}


#' Validate every geometry layer in a shapes list
#'
#' Iterates over `existing_shapes` and runs [validate_single_geom()] on
#' each layer, then performs a top-level mapping-table consistency
#' check: the row count of [get_mt()] applied to the full shapes list
#' must equal the row count of the most-disaggregated layer (admin
#' levels are required to form a strict hierarchy). Side effects only --
#' validator messages and a final `testthat` summary block.
#'
#' @param existing_shapes Named list of `sf` tibbles, one per admin
#'   level. Element names must follow `admin<N>_<Name>` (e.g.
#'   `admin1_Oblast`); see [ukr_shp] for the canonical shape.
#'
#' @return Invisibly `NULL`. Called for side effects.
#'
#' @importFrom purrr walk map_dbl
#' @importFrom rlang inform format_error_bullets
#' @importFrom glue glue
#' @importFrom testthat test_that expect_success expect_equal
#' @export
#'
#' @examples
#' validate_geometries(ukr_shp)
validate_geometries <- function(existing_shapes) {

  seq_along(existing_shapes) %>%
    walk(~{
      inform(format_error_bullets(c(i = glue("Checking {names(existing_shapes[.x])}"))))
      validate_single_geom(focus_geom = existing_shapes[.x], existing_shapes)
    })

  max_nrow <- max(map_dbl(existing_shapes, nrow))

  cat('test_that("`get_mt()` works", {...}) - ')
  testthat::test_that("`get_mt()` works", {
    testthat::expect_success(
      testthat::expect_equal({
        nrow(get_mt(existing_shapes))
      }, max_nrow,
      info =
        "Administrative levels should have a hierarchical structure.
    Only several IDs at lower aggregation level (i.e. admin4) could map to one ID at higher level (i.e. admin3).
    Mapping table created with `get_mt()` should produce as many rows as there are in the most disaggregated geometry.")
    )
  })


}
