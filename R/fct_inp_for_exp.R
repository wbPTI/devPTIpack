#' Reshape PTI inputs into export-ready slots
#'
#' Internal helper used by `mod_wt_dwnload_newsrv` (via
#' `prepare_export_data`) to convert the raw `input_dta` list into the
#' slot layout expected by the xlsx download writer. Keeps only the
#' `general`, `adminN_*`, `point_*`, and `metadata` slots; renames
#' indicator columns inside admin sheets from machine-readable
#' `var_code` to human-readable `var_name`; drops the `adminNPcod`
#' identifier columns; trims the metadata sheet down to user-facing
#' fields and only those rows that survive the explorer/PTI filters;
#' drops slots that end up with no rows at all.
#'
#' @param dta Named list of input tibbles as produced by
#'   `fct_template_reader()` (or the bundled `ukr_mtdt_full`).
#'
#' @return Named list of tibbles -- structurally similar to `dta`
#'   minus the dropped slots and with renamed indicator columns.
#'   Empty slots are removed.
#'
#' @importFrom dplyr select rename_at vars any_of distinct filter
#'   arrange across if_any everything matches
#' @importFrom purrr map_dfr imap map keep
#' @importFrom stringr str_detect
#' @noRd
fct_inp_for_exp <- function(dta) {
  new_names <-
    list(
      dta %>% get_indicators_list("fltr_exclude_pti"),
      dta %>% get_indicators_list("fltr_exclude_explorer")
    ) %>%
    map_dfr(~ {
      .x %>% select(var_code, var_name)
    }) %>%
    distinct()

  new_names <- setNames(object = new_names$var_name, new_names$var_code)

  dta %>%
    `[`(str_detect(names(.), "general|admin\\d_|point_|metad")) %>%
    imap( ~ {
      if (str_detect(.y, "admin\\d_")) {
        .x <- .x %>%
          rename_at(vars(any_of(names(new_names))), function(x) {new_names[x]}) %>%
          select(-matches("admin\\dPcod"))
      }

      if (str_detect(.y, "metadata")) {
        .x <-
          .x %>%
          filter(!fltr_exclude_pti | !fltr_exclude_explorer) %>%
          arrange(across(any_of(c('pillar_group', "var_order")))) %>%
          select(any_of(c("var_name", "var_description", "var_units", "pillar_name", "pillar_description"))) %>%
          distinct()
      }
      .x
    }) %>%
    map(~{.x %>% filter(if_any(everything(), ~ !is.na(.x)))}) %>%
    keep(.p = function(x) nrow(x) > 0)
}



#' Reshape the internal weights store into the export wide format
#'
#' Internal helper used by `prepare_export_data` to convert the
#' reactive `weights_clean` store (a named list, one tibble per saved
#' weighting scheme) into a single long tibble keyed by scheme, then
#' enriched with human-readable `var_name` from `indicators_list`.
#'
#' @param weights_clean Named list of two-column tibbles (`var_code`,
#'   `weight`) produced by the weights-input UI -- one element per
#'   saved weighting scheme.
#' @param indicators_list Tibble with at least `var_code` and
#'   `var_name` columns, typically the output of `get_indicators_list()`.
#'
#' @return A tibble with columns `var_code`, `var_name`, `weight`,
#'   `weight_scheme`. One row per (variable, scheme) combination.
#'
#' @importFrom dplyr left_join select contains
#' @importFrom purrr imap_dfr
#' @importFrom tibble tibble
#' @noRd
fct_internal_wt_to_exp <- function(weights_clean, indicators_list) {
  if (length(weights_clean) == 0) {
    return(tibble::tibble(
      var_code      = character(),
      var_name      = character(),
      weight        = numeric(),
      weight_scheme = character()
    ))
  }
  weights_clean %>%
    imap_dfr(~{
      .x %>% mutate(weight_scheme = .y)
    }) %>%
    left_join(indicators_list %>% select(var_code, var_name), by = "var_code") %>%
    select(contains("var_"), contains("weight"))
}
