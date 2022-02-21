#' Validate single geometries data frame
#' @noRd
#' @export
#' @importFrom purrr walk
#' @importFrom testthat test_that expect_false expect_equal
#' @importFrom rlang format_error_bullets inform abort warn
#' @importFrom dplyr select distinct pull
#' @importFrom glue glue
#' @importFrom sf st_geometry_type st_drop_geometry
#' 
validate_single_geom <- function(focus_geom, full_geom){
  
  # Geometry to check --------------------------------------------------------
  # full_geom <- existing_shapes
  # focus_geom <- existing_shapes[3]
  focus_df <- focus_geom[[1]]
  focus_cols_names <- focus_df %>% names()
  
  
  
  # Identifying key fields --------------------------------------------------
  adm_level <- names(focus_geom)
  
  adm_name <- str_extract(adm_level, "(?<=\\_)\\D+")
  adm_level <- str_replace(adm_level, str_c("_", adm_name), "")
  adm_order <- str_extract(adm_level, "\\d") %>% as.numeric()
  
  
  # test_that(glue("{adm_level} level named '{adm_name}' has a numerical order"),
  #           {
  #             expect_false(is.na(adm_order))
  #             expect_false(is.nan(adm_order))
  #             expect_false(is.null(adm_order))
  #           })
  
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
  
  
  # All geometries are polygons or multy-polygon ------------------------------
  
  
  # Checking if the data frame has SF feature  --- --- ---
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
  
  
  # Checking is a geometry field is present --- --- ---
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
  
  # Geometry name  --- --- ---
  if (!c("geometry") %in% names(cols_types[cols_types])) {
    msg <-
      format_error_bullets(c(
        x = glue("Geometry column in {adm_level}_{adm_name} is not called \"geometry\""),
        i = glue("It is instead calles \"{names(cols_types[cols_types])}\". Consider renaming.")
      ))
    tot_warnings <- tot_warnings + 1
    inform(message = msg)
  }
  
  
  
  # Geometry types in the geometry column  --- --- ---
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
  
  
  # Check unique names in key field --- --- --- --- --- --- --- --- ---
  
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
  
  
  
  # Checking the mapping tables between admin levels ---------------------------------
  
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
    
    
    # Checking if appropriate mapping tables are there --- --- --- --- --- --- ---
    previous_admins %>% 
      walk(~{
        check_admin <- .x
        check_admin_code <- str_c(check_admin, "Pcod")
        # browser()
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


#' @describeIn validate_single_geom Validate all geometries in a data frame
#' @noRd
#' @export
#' @importFrom purrr walk
#' 
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
