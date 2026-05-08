#' Build a list of ggplot maps from PTI admin data
#'
#' For each indicator column in `dta`, joins the matching admin
#' geometries from `mt` and produces a `ggplot` choropleth. Used by
#' the metadata-PDF rmarkdown templates
#' (`inst/metadata.Rmd`, `inst/sample_pti/app-data/pti-metadata-pdf.Rmd`)
#' to render one map per indicator alongside its description.
#'
#' Variable codes are relabelled to human-readable `var_name` values
#' when `metadata` supplies them; otherwise the raw code is kept.
#' Rows whose value is `NA` are dropped.
#'
#' @param dta A tibble with one or more `adminN` identifier columns
#'   and additional indicator columns to plot. Long format produced
#'   by `pivot_pti_dta()` is not required -- the function pivots
#'   internally.
#' @param multiply Numeric. Currently unused; preserved for backward
#'   compatibility with caller scripts.
#' @param mt Named list of admin geometry tibbles (e.g. `ukr_shp`).
#'   The element whose name matches the `adminN` column in `dta` is
#'   joined onto the long-pivoted data via `dplyr::right_join`.
#'   Required (the function errors if `NULL`).
#' @param metadata Optional tibble with `var_code` and `var_name`
#'   columns used to relabel indicators in the plot title. When
#'   `NULL`, indicator codes are used verbatim.
#'
#' @return A list of `ggplot` objects, one per indicator, with
#'   `geom_sf` polygons filled by the indicator value.
#'
#' @importFrom dplyr right_join left_join select group_by mutate filter
#' @importFrom magrittr extract
#' @importFrom tidyr pivot_longer nest
#' @importFrom stringr str_detect str_extract
#' @importFrom tibble tibble
#' @importFrom rlang dots_list
#' @importFrom purrr pmap
#' @importFrom ggplot2 ggplot aes geom_sf theme_minimal theme labs
#' @export
#'
#' @examples
#' data(ukr_shp)
#' data(ukr_mtdt_full)
#' maps <- gg_admin_list(
#'   dta      = ukr_mtdt_full$admin1_Oblast,
#'   mt       = ukr_shp,
#'   metadata = ukr_mtdt_full$metadata
#' )
#' length(maps)
#' inherits(maps[[1]], "ggplot")
gg_admin_list <- function(dta,
                          multiply = 1,
                          mt = NULL,
                          metadata = NULL) {
  if (is.null(mt)) {
    stop(
      "`mt` is required: provide a named list of sf tibbles ",
      "(e.g. the bundled `ukr_shp`).",
      call. = FALSE
    )
  }
  spat_agg <-
    dta %>%
    names() %>%
    magrittr::extract(str_detect(., "admin\\d")) %>%
    str_extract("admin\\d") %>%
    `[[`(1)

  mtt <- mt[str_detect(names(mt), spat_agg)][[1]]
  if (is.null(metadata)) {
    metadata <-
      tibble(var_code = NA_character_, var_name = NA_character_)
  }
  plot_list <-
    dta %>%
    pivot_longer(any_of(names(.)[!str_detect(names(.), "admin\\d")]),
                 names_to = "var", values_to = "val") %>%
    dplyr::right_join(mtt) %>%
    left_join(metadata %>% select(var_code, var_name),
              by = c("var" = "var_code")) %>%
    mutate(var = ifelse(!is.na(var_name), var_name, var),
           val = val) %>%
    filter(!is.na(var)) %>%
    select(-var_name) %>%
    group_by(var) %>%
    nest() %>%
    rowwise() %>%
    pmap( ~ {
      ddta <- rlang::dots_list(...)
      ddta$data %>%
        ggplot() +
        aes(fill = val,  geometry = geometry) +
        geom_sf(colour = NA) +
        theme_minimal() +
        theme(legend.position = c(0.9, 0.2)) +
        labs(title = ddta$var)
    })
  plot_list

}
