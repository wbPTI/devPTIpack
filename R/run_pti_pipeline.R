#' Run the full PTI calculation pipeline
#'
#' Runs the seven-step PTI calculation pipeline (pivot → weight →
#' standardise → expand → merge → aggregate → label → structure) as a single
#' callable, mirroring the reactive chain in
#' [`mod_calc_pti2_server()`][mod_calc_pti2_server] but free of any Shiny
#' dependency. Designed for testing and headless reproduction of the
#' calculation a deployed app would perform for a given weight scheme.
#'
#' @param weights_clean Named list of weight tibbles, one per weight
#'   scheme. Each element must be a tibble with character column
#'   `var_code` and numeric column `weight`. Compatible with the output of
#'   [get_rand_weights()] and [get_all_weights_combs()].
#' @param inp_dta The metadata list as returned by [fct_template_reader()] —
#'   contains admin-level sheets (`admin1_*`, `admin2_*`, …), `metadata`,
#'   and `general`.
#' @param shp_dta Named list of `sf` tibbles, one per admin level
#'   (e.g. `admin1_Oblast`, `admin2_Rayon`).
#' @param indicators_list Indicator definitions as a tibble, defaulting to
#'   [get_indicators_list()] applied to `inp_dta`.
#' @param na_rm Logical. Forwarded to [agg_pti_scores()] as `na_rm_pti2` —
#'   if `TRUE`, missing indicator values are ignored when row-summing
#'   standardised contributions to the PTI score.
#'
#' @return A named list with one element per admin level; each element is a
#'   list with `pti_data` (an `sf` tibble), `pti_codes` (named character),
#'   and `admin_level` (named character). This is the same structure that
#'   `mod_calc_pti2_server()` returns to its callers via reactive value.
#'
#' @section Determinism:
#' This function is deterministic given fixed inputs. It does not consult
#' Golem options. Pass `na_rm` explicitly rather than relying on
#' `golem::get_golem_options("na_rm_pti")` to keep tests hermetic.
#'
#' @importFrom purrr imap
#' @export
#'
#' @examples
#' data(ukr_shp)
#' data(ukr_mtdt_full)
#'
#' indicators <- get_indicators_list(ukr_mtdt_full)
#' weights <- get_rand_weights(indicators)
#'
#' result <- run_pti_pipeline(
#'   weights_clean   = weights,
#'   inp_dta         = ukr_mtdt_full,
#'   shp_dta         = ukr_shp,
#'   indicators_list = indicators
#' )
#' names(result)
#' names(result[[1]])
run_pti_pipeline <- function(weights_clean,
                             inp_dta,
                             shp_dta,
                             indicators_list = get_indicators_list(inp_dta),
                             na_rm = FALSE) {

  long_vars       <- pivot_pti_dta(inp_dta, indicators_list)
  existing_shapes <- clean_geoms(shp_dta)
  mt              <- get_mt(shp_dta)

  weights_clean |>
    get_weighted_data(long_vars, indicators_list = indicators_list) |>
    get_scores_data() |>
    purrr::imap(~ expand_adm_levels(.x, mt) |>
                  merge_expandedn_adm_levels()) |>
    agg_pti_scores(existing_shapes, na_rm_pti2 = na_rm) |>
    label_generic_pti() |>
    structure_pti_data(shp_dta)
}
