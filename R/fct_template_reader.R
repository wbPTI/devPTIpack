#' Read a PTI metadata Excel template into the package's list-of-tibbles format
#'
#' Reads every sheet of the metadata `.xlsx` template and returns a named
#' list of tibbles in the shape downstream calculation and visualisation
#' code expects. Per-admin sheets are reduced to the columns referenced
#' by `metadata$var_code` (plus the `agg_*`, `admin<N>*`, `area`, `year`
#' helpers); admin sheets that have no remaining indicator columns after
#' that reduction are dropped. If a `weights_table` sheet is present and
#' non-empty, it is converted to the package-internal
#' `weights_clean` shape via [fct_convert_weight_to_clean()]. Logical
#' filter columns in `metadata` (`fltr_exclude_pti`,
#' `fltr_exclude_explorer`, `fltr_overlay_pti`, `fltr_overlay_explorer`,
#' `legend_revert_colours`) are coerced to logical and `NA`s defaulted
#' to `FALSE`.
#'
#' @param ... Character path components forming the path to the metadata
#'   `.xlsx` file. Passed verbatim to [file.path()], so either a single
#'   path string or several path segments are accepted.
#'
#' @return A named list of tibbles. The shape mirrors [ukr_mtdt_full]:
#'   one tibble per metadata sheet (`general`, per-admin tibbles named
#'   `admin<N>_*`, and `metadata`), plus a derived `weights_clean` slot
#'   when the input contains a `weights_table` sheet.
#'
#' @import dplyr purrr stringr readxl
#' @family data-input
#' @export
#'
#' @examples
#' template_path <- system.file(
#'   "sample_pti/app-data/sample-metadata.xlsx",
#'   package = "devPTIpack"
#' )
#' if (nzchar(template_path)) {
#'   tmplt <- fct_template_reader(template_path)
#'   names(tmplt)
#' }
fct_template_reader <- function(...) {
  flpth <- file.path(...)
  tmplt <-
    flpth %>%
    readxl::excel_sheets() %>%
    purrr::set_names(., .) %>%
    purrr::map( ~ {
      readxl::read_xlsx(path = flpth,
                        sheet = .x,
                        guess_max = 10000)
    })
  allowed_vars <- tmplt$metadata %>% distinct(var_code, spatial_level)
  tmplt <-
    tmplt %>%
    imap( ~ {
      if (str_detect(.y, "admin\\d")) {
        allowed_here <-
          allowed_vars %>%
          pull(var_code)
        .x <-
          .x %>%
          dplyr::select(contains("agg_"), matches("admin\\d"),
                 any_of(c("area", "year", allowed_here)))
      }
      .x
    }) %>%
    imap(~{
      if (str_detect(.y, "admin\\d")) {
        short_df <-
          .x %>%
          dplyr::select(-contains("agg_"), -matches("admin\\d"), -any_of(c("area", "year")))
        if (length(short_df) == 0)
          return(NULL)
        else {
          .x
        }
      }  else {
        .x
      }
    })  %>%
    keep(~!is.null(.))

  if (!is.null(tmplt$weights_table ) &&
      nrow(tmplt$weights_table) > 0) {
    tmplt$weights_clean <-
      tmplt$weights_table %>%
      fct_convert_weight_to_clean()
  } else {
    tmplt$weights_clean <- NULL
  }


  if (!is.null(tmplt$metadata ) &&
      nrow(tmplt$metadata) > 0) {

    tmplt$metadata <-
      tmplt$metadata %>%
      dplyr::mutate(across(c(fltr_exclude_pti, fltr_exclude_explorer, fltr_overlay_pti,
                   fltr_overlay_explorer, legend_revert_colours), ~ as.logical(.)
        )) %>%

      dplyr::mutate(across(c(fltr_exclude_pti, fltr_exclude_explorer, fltr_overlay_pti,
                             fltr_overlay_explorer, legend_revert_colours), ~ ifelse(is.na(.), FALSE, .)
        )
      )

  }

  tmplt
}

#' Convert a `weights_table` sheet into the package-internal `weights_clean` shape
#'
#' Picks the `ws\d..` (weight-set) column groups out of the wide
#' `weights_table` tibble and reshapes them into a flat named list where
#' each element is a tibble of `var_code` plus the weight column for one
#' weighting scheme.
#'
#' @param dta A tibble matching the `weights_table` sheet of a metadata
#'   template -- at minimum a `var_code` column plus weight-set columns
#'   following the `ws\d..<name>` naming convention.
#'
#' @return A flat named list of one-column-per-scheme tibbles. List
#'   element names come from each scheme's `name` row in the source
#'   tibble.
#'
#' @importFrom dplyr select rename_all contains
#' @importFrom purrr map set_names
#' @importFrom stringr str_detect str_extract str_replace str_replace_all
#' @importFrom magrittr extract
#' @noRd
fct_convert_weight_to_clean <- function(dta) {
  dta %>%
    names() %>%
    magrittr::extract(str_detect(., "ws\\d\\.\\.")) %>%
    str_extract("ws\\d\\.\\.") %>%
    str_replace("\\.\\.", "") %>%
    unique()  %>%
    map(~ {
      wtbl <-
        dta %>%
        dplyr::select(var_code, contains(.x)) %>%
        rename_all(list(~ str_replace_all(., "ws\\d\\.\\.", "")))
      set_names(list(wtbl %>% dplyr::select(-name)), wtbl$name %>% unique())
    }) %>%
    unlist(recursive = F)
}
