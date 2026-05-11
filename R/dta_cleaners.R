#' Build the indicators-list tibble from a metadata template
#'
#' Distils a metadata-template list (the output of
#' [fct_template_reader()]) down to a one-row-per-indicator tibble that
#' downstream calculation and weighting code expects. Filters out
#' indicators excluded by the named `fltr_*` column, drops admin levels
#' that contain no indicator data, attaches per-indicator availability
#' (`admin_levels_years`: a nested tibble of admin levels and the years
#' the indicator is observed at each), and joins pillar metadata.
#'
#' @param dta A list-of-tibbles metadata template as returned by
#'   [fct_template_reader()] (or the bundled [ukr_mtdt_full]).
#' @param fltr_var Character. Name of the boolean filter column in
#'   `dta$metadata` whose `TRUE` rows are excluded. Defaults to
#'   `"fltr_exclude_pti"`. Use `"fltr_exclude_explorer"` to derive the
#'   explorer-only indicators list.
#'
#' @return A tibble with one row per surviving indicator, joined to
#'   pillar metadata, with a nested-list column `admin_levels_years`
#'   describing data availability per admin level.
#'
#' @importFrom magrittr not extract extract2
#' @importFrom dplyr filter select arrange mutate across contains distinct_at
#'   group_by ungroup row_number group_by_at vars semi_join left_join rename any_of
#' @importFrom purrr map_lgl map walk transpose set_names
#' @importFrom stringr str_detect str_split regex
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer nest
#' @importFrom rlang sym
#' @noRd
get_indicators_list <- function(dta, fltr_var = "fltr_exclude_pti") {
  meta_dta <-
    dta$metadata  %>%
    filter(magrittr::not(!!sym(fltr_var))) %>%
    select(-contains("fltr")) %>%
    arrange(pillar_group, var_order) %>%
    mutate(
      across(contains("pillar_group"), ~ifelse(is.na(.), 9999, .)),
      across(contains("pillar"), ~ifelse(is.na(.), "", .))
    )

  element_names <- dta %>% names()

  pillars <-
    meta_dta %>%
    distinct_at(vars(contains("pillar"))) %>%
    arrange(pillar_group, pillar_name, pillar_description) %>%
    group_by(pillar_name) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    arrange(pillar_group)

  spatial_levels_names <-
    element_names %>%
    magrittr::extract(str_detect(., "\\D{1,}\\d{1}_")) %>%
    str_split("_") %>%
    transpose() %>%
    set_names(c("admin_level", "admin_level_name")) %>%
    map(~ unlist(.x)) %>%
    as_tibble()

  used_vars <- meta_dta$var_code %>% unique()

  levels_with_data <-
    spatial_levels_names$admin_level %>%
    map_lgl( ~ {
      rows <-
        dta %>%
        magrittr::extract(names(.) %>% str_detect(regex(.x, ignore_case = T))) %>%
        magrittr::extract2(1) %>%
        select(any_of(c("year", used_vars))) %>%
        length()
      rows != 0
    })

  if (any(!levels_with_data)) {
    spatial_levels_names[!levels_with_data, ]$admin_level %>%
      walk(~ {
        remove <-
          dta %>%
          names() %>%
          magrittr::extract(str_detect(., .x))
        dta[[remove]] <- NULL
      })

    spatial_levels_names <-
      spatial_levels_names[levels_with_data,]

  }
  vars_admins <-
    spatial_levels_names$admin_level %>%
    map( ~ {
      dta %>%
        magrittr::extract(names(.) %>% str_detect(regex(.x, ignore_case = T))) %>%
        magrittr::extract2(1) %>%
        select(any_of(c("year", used_vars))) %>%
        tidyr::pivot_longer(
          cols = any_of(used_vars),
          names_to = "var_code",
          values_to = "val"
        ) %>%
        filter(!is.na(val)) %>%
        distinct_at(vars(any_of(c(
          "year", "var_code"
        )))) %>%
        mutate(admin_level = .x)

    }) %>%
    bind_rows() %>%
    left_join(spatial_levels_names, by = "admin_level") %>%
    group_by_at(vars(any_of(
      c("var_code", "admin_level", "admin_level_name")
    ))) %>%
    tidyr::nest() %>%
    mutate(years = map(data, ~ {
      as.vector(.x[[1]]) %>% sort() %>% unlist()
    })) %>%
    select(-data) %>%
    group_by(var_code) %>%
    tidyr::nest() %>%
    rename(admin_levels_years = data)

  meta_dta %>%
    select(-contains("pillar_group"),
           -contains("pillar_descr"),
           -contains("spatial")) %>%
    semi_join(vars_admins, by = "var_code") %>%
    left_join(vars_admins, by = "var_code") %>%
    left_join(pillars, by = "pillar_name")

}
