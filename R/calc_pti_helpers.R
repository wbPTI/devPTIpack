#' Build admin-code mapping table from a country shapes list
#'
#' Strips geometries from each `sf` element of `country_shapes`, then
#' iteratively `full_join`s the admin P-code columns across levels to
#' produce a single tibble where each row uniquely maps an
#' admin-N polygon to its parent admin-(N-1)..admin0 codes. Used as
#' the canonical level-to-level lookup downstream (e.g. by
#' [expand_adm_levels()] and [agg_pti_scores()]).
#'
#' @param country_shapes Named list of `sf` tibbles, one per admin
#'   level, where each element carries `admin{N}Pcod` / `admin{N}Name`
#'   columns. Slot names follow the `adminN_HumanName` convention
#'   (see [ukr_shp]).
#'
#' @return A `tbl_df` with one column per admin level's `Pcod`
#'   (e.g. `admin0Pcod`, `admin1Pcod`, ...) and one row per leaf-level
#'   polygon. Rows where any admin code is `NA` are dropped.
#'
#' @importFrom purrr map reduce
#' @importFrom dplyr select contains full_join all_vars filter_all
#' @importFrom sf st_drop_geometry
#' @importFrom stringr str_detect
#'
#' @noRd
get_mt <- function(country_shapes) {

  country_shapes %>%
    purrr::map(~{.x %>% sf::st_drop_geometry()}) %>%
    purrr::reduce(function(x, y) {
      x %>%
        dplyr::select(dplyr::contains("admin")) %>%
        dplyr::select(-dplyr::contains("Name", ignore.case = F)) %>%
        dplyr::full_join(y %>% dplyr::select(dplyr::contains("admin")),
                         by = names(x) %>% `[`(., stringr::str_detect(., "Pcod")))
    }) %>%
    dplyr::select(dplyr::contains("Pcod")) %>%
    dplyr::filter_all(dplyr::all_vars(!is.na(.)))
}

#' Extract sorted admin-level identifiers from a names vector
#'
#' Pulls all `admin\\d{1,2}` substrings from `names(dta)`, sorts them
#' lexicographically, and returns them as a self-named character
#' vector ready for [purrr::imap()] iteration.
#'
#' @param dta Any object whose `names()` contain `admin{N}*` columns
#'   (typically a `country_shapes` element or the mapping table from
#'   [get_mt()]).
#'
#' @return A self-named character vector of admin level identifiers
#'   (e.g. `c(admin0 = "admin0", admin1 = "admin1", ...)`).
#'
#' @note Sort is **lexicographic**, so `admin1 < admin10 < admin2`.
#'   Internally consistent with downstream comparisons (which extract
#'   only the first digit) but wrong for any deployment with ≥10 admin
#'   levels. Pinned in `test-calc-pipeline.R` ("get_adm_levels: sort is
#'   lexicographic, not numeric (PINNED)") — see PLAN.md §12.
#'
#' @importFrom stringr str_extract
#' @importFrom rlang set_names
#'
#' @noRd
get_adm_levels <-
  function(dta) {
    dta %>%
      names() %>%
      str_extract("admin\\d{1,2}") %>%
      sort() %>%
      set_names(.)
  }


#' Pivot per-admin indicator tibbles into long form for PTI calc
#'
#' Selects the admin-keying columns plus any indicator (`var_code`)
#' columns and pivots indicators from wide to long, dropping `NA`
#' values. Drops `area` and `year` retention if present. Output is
#' the canonical input shape for [get_weighted_data()].
#'
#' @param input_dta Named list of per-admin tibbles (as in
#'   [ukr_mtdt_full]'s `admin*` slots).
#' @param indicators_list A `tbl_df` with at least a `var_code` column
#'   listing which indicator columns to pivot.
#'
#' @return A named list with the same admin slots as `input_dta`,
#'   each a `tbl_df` with admin keys plus `var_code` and `value`
#'   columns; rows with `NA` `value` are removed.
#'
#' @importFrom purrr imap
#' @importFrom dplyr select contains any_of matches distinct
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer
#'
#' @noRd
pivot_pti_dta <- function(input_dta, indicators_list) {
  var_codes <- indicators_list$var_code %>% unique()
  input_dta %>%
    `[`(names(.) %>% str_detect("admin")) %>%
    purrr::imap( ~ {
      .x %>%
        dplyr::select(
          dplyr::contains("agg_"),
          dplyr::matches("admin\\d"),
          dplyr::any_of(c("area", "year", var_codes))) %>%
        tidyr::pivot_longer(
          cols = any_of(var_codes),
          names_to = "var_code",
          values_to = "value",
          values_drop_na = TRUE
        ) %>%
        dplyr::distinct()
    })
}


#' Drop geometries and normalise admin-slot names
#'
#' Strips geometry columns from each element of `country_shapes` and
#' renames the slots to bare admin-level identifiers (e.g.
#' `admin1_Oblast` → `admin1`). Used to align shape-derived metadata
#' with downstream join keys.
#'
#' @param country_shapes Named list of `sf` tibbles per admin level
#'   (e.g. [ukr_shp]).
#'
#' @return A named list of `tbl_df` (no geometries) renamed to
#'   `admin0`, `admin1`, ...
#'
#' @importFrom purrr map
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr as_tibble
#' @importFrom stringr str_extract str_replace
#'
#' @noRd
clean_geoms <- function(country_shapes) {
  existing_shapes <-
    country_shapes %>%
    purrr::map(~{sf::st_drop_geometry(.x) %>% dplyr::as_tibble()})
  names(existing_shapes) <-
    names(existing_shapes) %>%
    stringr::str_extract("^(.*?)_") %>%
    stringr::str_replace("_", "")
  existing_shapes
}



#' Apply weighting schemes to long-form PTI data
#'
#' For each weighting scheme in `wt_list`, replaces missing weights
#' with 0; if any non-zero weight survives, drops the zero-weight
#' rows. Inner-joins the surviving weights onto every per-admin slot
#' of `vars_dta_list` by `var_code` and multiplies `value` by `weight`,
#' returning the weighted long-form tibble for each (scheme × admin)
#' combination.
#'
#' @param wt_list Named list of weight tibbles (one per weighting
#'   scheme), each with columns `var_code` and `weight`.
#' @param vars_dta_list Named list of per-admin long-form tibbles
#'   (output of [pivot_pti_dta()]).
#' @param indicators_list Unused in the current implementation
#'   (retained for signature stability with upstream callers).
#'
#' @return A nested named list: outer names are weighting schemes,
#'   inner names are admin levels; each leaf is a weighted long-form
#'   tibble with `value = value * weight` and weight columns dropped.
#'
#' @importFrom purrr map
#' @importFrom dplyr inner_join mutate select filter contains
#' @importFrom tidyr replace_na
#'
#' @noRd
get_weighted_data <- function(wt_list, vars_dta_list, indicators_list) {
  wt_list %>%
    purrr::map(~{
      ws <- .x %>%
        dplyr::mutate(weight = tidyr::replace_na(weight, 0))
      if (!all(ws$weight == 0)) {
        ws <-
          ws %>%
          filter(weight != 0, !is.na(weight))
      }
      vars_dta_list %>%
        purrr::map(~{
          .x %>%
            dplyr::inner_join(ws, by = "var_code") %>%
            dplyr::mutate(value = value * weight) %>%
            dplyr::select(-contains("weight"))
        })
    })

}




#' Z-score standardise weighted PTI data within (year, var_code)
#'
#' For every `(year, var_code)` group in each leaf tibble, replaces
#' `value` with `(value - mean) / sd`, treating `NA` as missing. When
#' the resulting score is `NaN` (zero variance), it is replaced with
#' `0`. Empty leaf tibbles are passed through unchanged.
#'
#' @param wt_dta_list Nested named list as returned by
#'   [get_weighted_data()] (outer = scheme, inner = admin level).
#'
#' @return A list of the same shape as `wt_dta_list` with `value`
#'   replaced by per-`(year, var_code)` z-scores (zero-variance groups
#'   become 0).
#'
#' @note 1-row `(year, var_code)` groups produce `NA` (not `0`)
#'   because `sd()` of length-1 returns `NA` rather than `NaN`, so
#'   the `is.nan` filter misses. Pinned in `test-calc-pipeline.R`
#'   ("get_scores_data: 1-row groups produce NA, not 0 (PINNED)") —
#'   see PLAN.md §12.
#'
#' @importFrom purrr map
#' @importFrom dplyr group_by_at mutate ungroup vars any_of
#'
#' @noRd
get_scores_data <- function(wt_dta_list) {
  wt_dta_list %>%
    purrr::map( ~ {
      .x %>%
        purrr::map( ~ {
          if (nrow(.x) > 0) {
            .x <-
              .x %>%
              dplyr::group_by_at(vars(any_of(c("year", "var_code")))) %>%
              dplyr::mutate(value = (value - mean(value, na.rm = T)) / sd(value, na.rm = T)) %>%
              dplyr::mutate(value = ifelse(is.nan(value), 0, value)) %>%
              dplyr::ungroup()
          }
          .x
        })
    })

}
