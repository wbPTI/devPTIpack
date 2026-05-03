#' Verify that a shapes file and metadata file together produce valid PTI scores
#'
#' End-to-end pre-flight validator for a PTI deployment. Calls
#' [validate_read_shp()] and [validate_read_metadata()] on the inputs,
#' then runs the full calculation pipeline ([pivot_pti_dta()] ->
#' [get_weighted_data()] -> [get_scores_data()] -> [expand_adm_levels()] ->
#' [agg_pti_scores()]) under an all-equal weighting and asserts that the
#' number of scored pillars matches `nrow(indicators_list)`. Side effects
#' only: emits `testthat` reporter output for each check; does not
#' return a structured result.
#'
#' @param shp_path Character. Path to an `.rds` file containing the
#'   shapes list (the on-disk form of objects shaped like [ukr_shp]).
#' @param mtdt_path Character. Path to the metadata `.xlsx` template
#'   (the on-disk form of [ukr_mtdt_full]).
#'
#' @return Invisibly `NULL`. Called for side effects -- emits validation
#'   results via `testthat`.
#'
#' @importFrom testthat test_that expect_true expect_success
#' @importFrom readr read_rds
#' @importFrom purrr map_dfr
#' @importFrom dplyr count
#' @export
#'
#' @examples
#' \dontrun{
#' validate_metadata(
#'   shp_path = "path/to/shapes.rds",
#'   mtdt_path = "path/to/metadata.xlsx"
#' )
#' }
validate_metadata <- function(shp_path, mtdt_path) {

  validate_read_shp(shp_path)
  validate_read_metadata(mtdt_path)

  test_that("provided metadata and shape files can produce valid PTI scores", {

    testthat::expect_success({

      testthat::expect_true({
        shp_dta <- read_rds(shp_path)
        imp_dta <- fct_template_reader(mtdt_path)

        imp_dta$indicators_list <- get_indicators_list(imp_dta)
        long_vars <-
          imp_dta %>% pivot_pti_dta(imp_dta$indicators_list)
        existing_shapes <- shp_dta %>% clean_geoms()
        mt <- shp_dta %>% get_mt()
        adm_lvls <- mt %>% get_adm_levels()

        imp_dta$weights_clean <-
          imp_dta$indicators_list$var_code %>%
          get_all_weights_combs(1)

        calc_pti_at_once <-
          get_weighted_data(imp_dta$weights_clean, long_vars, imp_dta$indicators_list) %>%
          get_scores_data() %>%
          imap( ~ expand_adm_levels(.x, mt) %>% merge_expandedn_adm_levels())

        na_agg <-
          calc_pti_at_once %>%
          agg_pti_scores(existing_shapes)

        nrow_pti <-
          na_agg %>%
          map_dfr(~ .x %>% count(pti_name)) %>%
          count(pti_name) %>%
          nrow()

        nrow(imp_dta$indicators_list) ==  nrow_pti
      })

    })


  })


}


#' Validate a shapes `.rds` file in isolation
#'
#' Reads the `.rds` at `shp_path` and checks two invariants: (1) the
#' object is a non-empty named list, and (2) every `admin<N>Pcod` column
#' referenced inside any layer has a corresponding top-level `admin<N>_*`
#' element. Side effects only -- emits `testthat` reporter output.
#'
#' @param shp_path Character. Path to an `.rds` file containing the
#'   shapes list (the on-disk form of objects shaped like [ukr_shp]).
#'
#' @return Invisibly `NULL`. Called for side effects.
#'
#' @note Issue [#7](https://github.com/worldbank/devPTIpack/issues/7) is
#'   pinned: when the shapes file is "perfect" (no extra admin codes),
#'   the internal `str_c(extra_level, collapse = "|")` produces an
#'   empty-pattern `str_detect` call that errors. Fix is part of the
#'   broader runtime-`test_that` refactor.
#'
#' @importFrom readr read_rds
#' @importFrom testthat test_that expect_gt expect_true
#' @importFrom purrr map keep
#' @importFrom stringr str_extract str_detect str_c
#' @importFrom glue glue
#' @export
#'
#' @examples
#' \dontrun{
#' validate_read_shp("path/to/shapes.rds")
#' }
validate_read_shp <- function(shp_path) {

  testthat::test_that("read shapefiles works", {
    shp_dta <- readr::read_rds(shp_path)

    testthat::expect_gt(
      length(shp_dta),
      expected = 0,
      label = "R object with boundaries should be a list with names that is greater than zero.")
  })


  testthat::test_that("In the admin boundaries, all 'admin{X}Pcod' fields have a corresponding dataframe with boundaries", {
    shp_dta <- readr::read_rds(shp_path)

    all_admin_codes <-
      shp_dta %>%
      map(names) %>%
      unlist() %>%
      unique() %>%
      `[`(str_detect(., "admin\\dPcod")) %>%
      str_extract( "admin\\d")

    all_admin_levels <-
      shp_dta %>%
      names %>%
      str_extract( "admin\\d")

    extra_level <-
      all_admin_codes[!all_admin_codes %in% all_admin_levels] %>%
      str_c(collapse = "|")

    probl_ms <-
      shp_dta %>%
      keep(function(x) any(str_detect(names(x), extra_level))) %>%
      names() %>%
      str_c(collapse = ", ")

    testthat::expect_true(
      all(all_admin_codes %in% all_admin_levels),
      label =
        glue::glue(
          "In admin bounds {probl_ms} there are variables with admin levels ({extra_level}), which are not present as separea admin bounds"
        ))
  })




}


#' Validate a metadata `.xlsx` file in isolation
#'
#' Reads the metadata template at `mtdt_path` via [fct_template_reader()]
#' and checks two invariants: (1) the `metadata` sheet has at least one
#' row, and (2) every `fltr_*` column is read as logical (the template
#' rules require `TRUE`/`FALSE` rather than `1`/`0` strings). Side
#' effects only -- emits `testthat` reporter output.
#'
#' @param mtdt_path Character. Path to the metadata `.xlsx` template
#'   (the on-disk form of [ukr_mtdt_full]).
#'
#' @return Invisibly `NULL`. Called for side effects.
#'
#' @importFrom testthat test_that expect_gt expect_true
#' @importFrom dplyr select contains any_of
#' @importFrom purrr map_lgl is_logical
#' @export
#'
#' @examples
#' \dontrun{
#' validate_read_metadata("path/to/metadata.xlsx")
#' }
validate_read_metadata <- function(mtdt_path) {

  test_that("read metadata works", {
    imp_dta <- fct_template_reader(mtdt_path)

    testthat::expect_gt(
      length(imp_dta$metadata),
      expected = 0,
      label = "metadata sheet in the metadata file has no valid rows, check it."
    )


    testthat::expect_true(
      {
        imp_dta <- fct_template_reader(mtdt_path)

        imp_dta$metadata %>%
          dplyr::select(contains("fltr_"), any_of("legend_revert_colours ")) %>%
          purrr::map_lgl(~purrr::is_logical(.x)) %>%
          all
      },
      label = "filter columns in the metadata are not read as logical columns"
    )


  })

}
