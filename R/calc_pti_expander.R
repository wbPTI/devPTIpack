#' Expand weighted/scored PTI data across admin levels
#'
#' For each `(adm_from, adm_to)` admin-level pair, reshapes the
#' source-level scored data and joins it to the mapping table at the
#' target level. Behaviour by direction:
#' \itemize{
#'   \item Upward (`adm_from < adm_to`): each `adm_to` polygon
#'     inherits the source-level value via `full_join` on the
#'     `adm_from` P-code.
#'   \item Same level (`adm_from == adm_to`): pass-through.
#'   \item Downward (`adm_from > adm_to`): join then group on the
#'     `adm_to` polygon and `mean()` the source-level values.
#' }
#' Source-level columns are renamed `<var_code>._._.<adm_from>` to
#' keep level provenance through the join.
#'
#' @param wtd_scrd_dta Nested named list (scheme × admin) of scored
#'   long-form tibbles, as returned by [get_scores_data()].
#' @param mt Admin-mapping table from [get_mt()] (one column per
#'   `admin{N}Pcod`).
#'
#' @return A nested named list: outer names are admin source levels,
#'   inner names are admin target levels; each leaf is a wide tibble
#'   with `<var_code>._._.<adm_from>` columns at the target level.
#'   Leaves are `NULL` when no `wtd_scrd_dta` slot matches the source
#'   level, or when the matched slot is empty (zero rows).
#'
#'   Errors when more than one `wtd_scrd_dta` slot matches a single
#'   admin level — the input contract is one slot per level
#'   (`adminN_HumanName`).
#'
#' @importFrom purrr imap
#' @importFrom dplyr select contains all_of distinct pull rename_at vars any_of everything full_join filter_at group_by_at summarise_all
#' @importFrom stringr str_detect str_extract str_c
#' @importFrom tidyr pivot_wider
#' @importFrom rlang set_names
#'
#' @noRd
expand_adm_levels <- function(wtd_scrd_dta, mt) {

  adm_lvls <- mt %>% get_adm_levels() %>% set_names(.)

  purrr::imap(adm_lvls, ~ {
    adm_from <- .x
    # Boundary-anchored: adm_from='admin1' must not match 'admin10_...'.
    adm_pattern <- paste0("^", adm_from, "(_|$)")
    adm_from_dta_full <-
      wtd_scrd_dta[stringr::str_detect(names(wtd_scrd_dta), adm_pattern)]

    if (length(adm_from_dta_full) > 1) {
      stop(
        "more than one slot in `wtd_scrd_dta` matches admin level '",
        adm_from, "': ",
        paste(names(adm_from_dta_full), collapse = ", "),
        call. = FALSE
      )
    }

    if (length(adm_from_dta_full) == 1 &&
        nrow(adm_from_dta_full[[1]]) > 0) {
      adm_from_dta_full <-
        adm_from_dta_full[[1]] %>%
        dplyr::select(dplyr::contains(adm_from), dplyr::all_of(c("year", "var_code", "value"))) %>%
        dplyr::select(- dplyr::contains("Name"), -contains("year"))

      to_rename <-
        adm_from_dta_full %>% dplyr::distinct(var_code) %>% dplyr::pull()

      adm_to_dta <-
        adm_from_dta_full %>%
        tidyr::pivot_wider(names_from = "var_code", values_from = "value")

      adm_from_dta <-
        adm_to_dta %>%
        dplyr::rename_at(dplyr::vars(dplyr::any_of(to_rename)),
                         list(~ str_c(., "._._.", adm_from)))
      adm_lvls %>%
        purrr::imap( ~ {
          adm_to <-  .x

          local_mt <-
            mt %>%
            dplyr::select(dplyr::contains(adm_from), dplyr::contains(adm_to)) %>%
            dplyr::distinct()
          col_to <- mt %>% names() %>% `[`(stringr::str_detect(., adm_to))
          col_from <- mt %>% names() %>% `[`(stringr::str_detect(., adm_from))
          sl_to <- stringr::str_extract(col_to, "\\d") %>% as.integer()
          sl_from <- stringr::str_extract(col_from, "\\d") %>% as.integer()

          # s2.1 Upwards scaling
          if (sl_from < sl_to) {
            out <-
              local_mt %>%
              dplyr::distinct() %>%
              dplyr::full_join(adm_from_dta, by = col_from) %>%
              dplyr::select(dplyr::contains(adm_to), dplyr::everything()) %>%
              dplyr::select(-dplyr::contains(stringr::str_c(adm_from, "Pcod"))) %>%
              dplyr::filter_at(dplyr::vars(dplyr::contains(adm_to)), dplyr::all_vars(!is.na(.)))
          }

          # s2.2 Same level — pass through
          if (sl_from == sl_to) {
            out <- adm_to_dta
          }

          # s2.3 Downward scaling — group + mean
          if (sl_from > sl_to) {
            out <-
              local_mt %>%
              dplyr::distinct() %>%
              dplyr::full_join(adm_from_dta, by = col_from) %>%
              dplyr::select(dplyr::contains(adm_to), dplyr::everything()) %>%
              dplyr::select(-dplyr::contains(str_c(adm_from, "Pcod"))) %>%
              dplyr::filter_at(dplyr::vars(dplyr::contains(adm_to)), dplyr::all_vars(!is.na(.))) %>%
              dplyr::group_by_at(dplyr::vars(dplyr::contains(adm_to))) %>%
              dplyr::summarise_all(list(~ mean(., na.rm = TRUE)))
          }
          out
        })
    } else {
      adm_lvls %>% imap( ~ {NULL})
    }

  })

}


#' Merge expanded admin-level results back into per-target-level tibbles
#'
#' Transposes the nested list so the outer key is the target admin
#' level, then `left_join`s each source-level contribution into a
#' single tibble keyed on `<adm_to>Pcod`. Drops all-`NA` columns at
#' the end.
#'
#' @param dta Nested list as returned by [expand_adm_levels()]
#'   (outer = source level, inner = target level).
#'
#' @return A named list of `tbl_df` (one per admin target level), each
#'   keyed on `<adm_to>Pcod` with one column per `<var_code>._._.<adm_from>`
#'   contribution.
#'
#' @importFrom purrr transpose imap map_lgl reduce
#' @importFrom dplyr left_join select any_of
#' @importFrom magrittr extract
#' @importFrom stringr str_c str_detect
#'
#' @noRd
merge_expandedn_adm_levels <- function(dta) {
  dta %>%
    transpose() %>%
    imap( ~ {
      by_join <- str_c(.y, "Pcod")

      .x %>%
        `[`(map_lgl(., ~ !is.null(.x))) %>%
        reduce(function(x, y) {
          remove <-
            names(x) %>%
            magrittr::extract(!str_detect(., by_join))
          x %>%
            left_join(y %>% select(-any_of(remove)), by_join)
        }) %>%
        select_if(function(x) !all(is.na(x)))
    })
}

#' Aggregate per-(scheme × admin) PTI scores into final tibbles
#'
#' For each weighting scheme and target admin level, sums the native
#' and most-disaggregated foreign indicator columns row-wise to
#' produce `pti_score`, then attaches the spatial name via the
#' admin-mapping table. Foreign columns named
#' `<var_code>._._.<adm_from>` are filtered to keep only the
#' highest-numbered source level per `var_code` to avoid double
#' counting.
#'
#' @param extrap_dta Nested list of expanded data as returned by
#'   [merge_expandedn_adm_levels()].
#' @param adm_ids Admin metadata list (typically `country_shapes` or
#'   the `clean_geoms()` output) used to map polygon codes to
#'   `spatial_name`.
#' @param na_rm_pti2 Logical or `NULL`. If non-`NULL`, used as the
#'   `na.rm` argument to `rowSums()` for `pti_score`. When `NULL`
#'   (the default), the value is read from
#'   `get_golem_options("na_rm_pti")`, falling back to `FALSE` if
#'   that option is unset.
#'
#' @return A named list of `tbl_df` (one per admin target level)
#'   bound across schemes; columns include the admin-keying P-codes,
#'   `pti_score`, `pti_name` (the scheme), and `spatial_name`.
#'
#' @importFrom purrr imap transpose
#' @importFrom tidyr separate
#' @importFrom dplyr select rename_at vars matches mutate left_join as_tibble filter_at all_vars contains any_of group_by ungroup pull bind_rows
#' @importFrom golem get_golem_options
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#'
#' @noRd
agg_pti_scores <- function(extrap_dta, adm_ids, na_rm_pti2 = NULL) {

  na_rm_pti <- get_golem_options("na_rm_pti")
  if (is.null(na_rm_pti)) { na_rm_pti <- FALSE }
  if (!is.null(na_rm_pti2)) { na_rm_pti <- na_rm_pti2}

  extrap_dta  %>%
    purrr::imap( ~ {
      pti_name <- .y
      .x %>%
        purrr::imap( ~ {

          dta <- .x

          map_name <-
            adm_ids[[.y]] %>%
            dplyr::select(dplyr::contains(.y)) %>%
            dplyr::rename_at(dplyr::vars(dplyr::matches("Name")), list( ~ "spatial_name"))

          nonsum_cols <-
            dplyr::select(dta, dplyr::matches("^admin\\d"), dplyr::matches("^year$")) %>%
            names()

          naitive_col <- names(dta) %>%
            `[`(!(.) %in% nonsum_cols) %>%
            `[`(!str_detect(., "\\.\\_\\.\\_\\.admin\\d"))

          foreign_coll_1 <-
            names(dta) %>%
            `[`(!(.) %in% nonsum_cols) %>%
            `[`(str_detect(., "\\.\\_\\.\\_\\.admin\\d")) %>%
            tibble(for_col = .) %>%
            tidyr::separate(
              for_col,
              into = c("col", "level"),
              sep = "\\.\\_\\.\\_\\.",
              remove = F
            ) %>%
            filter(!col %in% naitive_col)

          if (nrow(foreign_coll_1) > 0) {
            foreign_coll <-
              foreign_coll_1 %>%
              group_by(col) %>%
              filter(level == max(level, na.rm = T)) %>%
              ungroup() %>%
              pull(for_col)
          } else {
            foreign_coll <- character(0)
          }

          dta %>%
            dplyr::select(any_of(nonsum_cols)) %>%
            dplyr::mutate(
              pti_score = dta %>%
                dplyr::select(!any_of(nonsum_cols)) %>%
                dplyr::select(any_of(foreign_coll), all_of(naitive_col)) %>%
                rowSums(na.rm = na_rm_pti),
              pti_name = pti_name
            ) %>%
            dplyr::left_join(map_name, by = nonsum_cols) %>%
            dplyr::as_tibble() %>%
            dplyr::filter_at(dplyr::vars(dplyr::contains(nonsum_cols)), dplyr::all_vars(!is.na(.)))
        })
    }) %>%
    purrr::transpose() %>%
    purrr::imap(~ dplyr::bind_rows(.x))

}

#' Apply a glue-template `pti_label` column to per-admin PTI tibbles
#'
#' Iterates over each admin-level tibble in `dta` and creates a
#' `pti_label` column by interpolating `glue_expr` against the row's
#' columns (typically `spatial_name`, `pti_name`, `pti_score`).
#' Override `glue_expr` to customise hover popups in the leaflet map.
#'
#' @param dta Named list of per-admin PTI tibbles, as returned by
#'   [agg_pti_scores()] (must contain the columns referenced by
#'   `glue_expr`).
#' @param glue_expr Character. A glue-compatible template; defaults
#'   to [generic_pti_glue()]. Each row's columns are available as
#'   replacement variables.
#'
#' @return A named list of the same shape as `dta`, each tibble with
#'   an added `pti_label` character column.
#'
#' @importFrom purrr map
#' @importFrom dplyr mutate
#' @importFrom glue glue
#' @family pti-pipeline
#' @export
#'
#' @examples
#' scores <- list(
#'   admin1 = tibble::tibble(
#'     spatial_name = c("Cherkasy", "Kyiv"),
#'     pti_name = "Equal weights",
#'     pti_score = c(0.42, -0.31)
#'   )
#' )
#'
#' labelled <- label_generic_pti(scores)
#' labelled$admin1$pti_label[1]
#'
#' custom <- "{spatial_name}: {round(pti_score, 2)}"
#' label_generic_pti(scores, glue_expr = custom)$admin1$pti_label
label_generic_pti <- function(dta, glue_expr = generic_pti_glue()) {
  dta  %>%
    map(~ {
      .x %>%
        mutate(pti_label =
                 glue::glue(glue_expr))
    })

}

#' Default glue template for PTI map popups
#'
#' Returns the package's default hover-popup template string used by
#' [label_generic_pti()]. Three lines, combining `spatial_name`,
#' `pti_name` (weighting scheme), and a 5-decimal `pti_score`. Users
#' can copy/modify this as a starting point for custom popups.
#'
#' @return A length-1 character string with glue placeholders for
#'   `spatial_name`, `pti_name`, and `pti_score`.
#'
#' @importFrom stringr str_c
#' @family pti-pipeline
#' @export
#'
#' @examples
#' generic_pti_glue()
generic_pti_glue <- function() {
  c(
    "<strong>{spatial_name}</strong>",
    "<br/>Weighting scheme: <strong>{ifelse(is.na(pti_name), 'No data', pti_name)}</strong>",
    "<br/>PTI score: <strong>{ifelse(is.na(pti_score), 'No data', scales::label_number(accuracy = 0.00001)(pti_score))}</strong>",
    "<br/>"
  ) %>%
    str_c(., collapse = "")
}



#' Restructure aggregated PTI data for the mapping module
#'
#' For each admin-level tibble in `dta`, encodes the weighting-scheme
#' name into a short `pti_code` (`pti_ind_1`, ...), pivots scores and
#' labels wide so each `(score, label)` pair lives in its own
#' `pti_score..pti_ind_N` / `pti_label..pti_ind_N` column, fills in
#' "No data" labels for admin polygons that have no score, and joins
#' the result back onto the geometries from `shp_dta`. The output is
#' the canonical input shape for [mod_plot_pti2_srv()].
#'
#' @param dta Named list of per-admin PTI tibbles as returned by
#'   [label_generic_pti()] (must include `pti_name`, `pti_score`,
#'   `pti_label`, `spatial_name`, and the admin P-code columns).
#' @param shp_dta Named list of `sf` tibbles per admin level (e.g.
#'   [ukr_shp]).
#'
#' @return A named list (one element per admin level) where each
#'   element is itself a list with three slots: `pti_data` (an `sf`
#'   tibble with the wide score/label columns), `pti_codes` (the
#'   short-code → scheme-name map), and `admin_level` (a self-named
#'   character mapping `adminN` → `HumanName`).
#'
#' @importFrom purrr imap
#' @importFrom dplyr left_join select distinct mutate all_of bind_rows
#' @importFrom tidyr expand pivot_wider
#' @importFrom stringr str_c str_extract str_replace str_detect
#' @importFrom magrittr extract extract2
#' @importFrom rlang sym set_names
#'
#' @noRd
structure_pti_data <- function(dta, shp_dta) {

  dta %>%
    purrr::imap(~ {
      name_var <-  str_c(.y, "Name")
      name_var <- sym(name_var)
      pti_codes <-
        .x$pti_name %>%
        unique() %>%
        set_names(nm = str_c("pti_ind_", seq_along(.)))
      pti_codes2 <- tibble(pti_code = names(pti_codes), pti_name = pti_codes)

      .x  %>% left_join(pti_codes2, by = "pti_name")

      pti_data1 <-
        .x  %>%
        left_join(pti_codes2, by = "pti_name") %>%
        select(-pti_name)

      var_match <- pti_data1 %>% select(matches("admin\\dPcod")) %>% names()
      var_match2 <- sym(var_match)
      var_to_expand <- c(var_match, "pti_code")
      var_to_distinct <- c(var_match, "spatial_name")

      pti_data2 <-
        pti_data1 %>%
        tidyr::expand(!!var_match2, pti_code) %>%
        left_join(distinct(select(pti_data1, all_of(var_to_distinct))), by = var_match) %>%
        left_join(pti_data1, by = c(var_match, "spatial_name", "pti_code")) %>%
        mutate(
          pti_label =
            ifelse(
              is.na(pti_label),
              str_c("<strong>",spatial_name, "</strong><br/>PTI score: <strong>No data</strong><br/>"),
              pti_label
              )
        ) %>%
        tidyr::pivot_wider(
          names_from = "pti_code",
          values_from = c("pti_score", "pti_label"),
          names_sep = ".."
        )

      pti_data0 <-
        shp_dta %>%
        magrittr::extract(str_detect(names(.), .y)) %>%
        magrittr::extract2(1) %>%
        left_join(pti_data2, by = names(.x) %>% magrittr::extract(str_detect(., "admin\\d")))

      admin_level <- shp_dta %>% names() %>% `[`(str_detect(., .y))
      admin_level <-
        set_names(
          str_extract(admin_level, "_(.*?)$") %>% str_replace("_", ""),
          str_extract(admin_level, "^(.*?)_") %>% str_replace("_", "")
        )

      list(pti_data = pti_data0,
           pti_codes = pti_codes,
           admin_level =  admin_level)

    })

}
