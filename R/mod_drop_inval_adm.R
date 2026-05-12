#' Drop-invalid-admin module server
#'
#' Filters administrative levels that cannot be plotted because the
#' currently-weighted indicators are unavailable there. Watches
#' `wt_dta()$indicators_list` to derive the unavailable admin levels per
#' weighting scheme via [get_vars_un_avbil()] / [get_min_admin_wght()];
#' fires a `shiny::showNotification()` listing the dropped levels when
#' any are dropped; returns a reactive that yields `dta()` filtered via
#' [drop_inval_adm()].
#'
#' @param id Character. Shiny module namespace ID.
#' @param dta Reactive. The pre-plot data structure that downstream
#'   modules consume -- typically the output of
#'   [preplot_reshape_wghtd_dta()].
#' @param wt_dta Reactive. A list-like value carrying at least
#'   `indicators_list` (a tibble with availability metadata) and
#'   `weights_clean` (the named list of per-scheme weights tibbles).
#'
#' @return A reactive expression that yields `dta()` with admin levels
#'   excluded where the active weighting cannot produce scores.
#'
#' @importFrom shiny moduleServer eventReactive observeEvent showNotification isTruthy
#' @importFrom purrr map_lgl imap keep
#' @importFrom stringr str_c
#' @family validation
#' @export
#'
#' @examples
#' \dontrun{
#' mod_drop_inval_adm("drop-inval", dta = preplot_dta, wt_dta = weights_dta)
#' }
mod_drop_inval_adm <- function(id, dta, wt_dta){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    adm_to_filter <-
      eventReactive(
        wt_dta()$indicators_list,
        {
          wt_dta()$indicators_list %>%
            get_vars_un_avbil(names(get_current_levels(dta()))) %>%
            get_min_admin_wght(wt_dta()$weights_clean)
        })

    observeEvent(
      adm_to_filter(),
      {

        do_notify <-
          adm_to_filter() %>%
          map_lgl(~length(.x) > 0) %>%
          any()

        if (do_notify) {
          cur_lvls <- get_current_levels(dta())
          ws_dropped <-
            adm_to_filter() %>%
            keep(function(x) length(x) > 0) %>%
            imap( ~ {
              mss <-
                cur_lvls[names(cur_lvls) %in% .x] %>%
                unname() %>%
                str_c(collapse = ", ")
              str_c(.y, " - ", mss, "; ")
            })

          shiny::showNotification(
            str_c(
              "Some admin levels are ommited because there are no data. These are: ",
              str_c(ws_dropped, collapse = "")
            ),
            id = "dropped-admin-levels",
            type = "default",
            duration = 10
          )
        }

      }, ignoreInit = F
    )

    eventReactive(
      dta(),
      {
        if (isTruthy(adm_to_filter()) && isTruthy(dta()) && length(adm_to_filter()) > 0) {
          dta() %>% drop_inval_adm(adm_to_filter())
        } else {
          dta()
        }
      }, ignoreNULL = FALSE)
  })
}


#' Identify (variable, admin-level) pairs with no native data
#'
#' Returns the `(var_code, admin_level)` pairs for which the indicator
#' has no native data at that admin level. Pairs are emitted
#' symmetrically: an indicator with data only at admin1 surfaces as
#' unavailable at admin2 / admin4 (and vice versa). Used as the first
#' step in [mod_drop_inval_adm()] to decide which admin levels must be
#' hidden for a given weighting.
#'
#' @param ind_list A tibble shaped like the output of the internal
#'   indicators-list pipeline -- at minimum a `var_code` column and a
#'   list-column `admin_levels_years` whose elements describe per-admin
#'   data presence.
#' @param admin_levels Optional character vector restricting the admin
#'   levels considered (e.g. `c("admin1", "admin2")`). Defaults to the
#'   sorted unique levels seen in `ind_list`.
#'
#' @return A tibble with one row per unavailable
#'   `(var_code, admin_level)` pair, with columns `var_code` and
#'   `admin_level`.
#'
#' @importFrom tidyr unnest
#' @importFrom tibble as_tibble
#' @importFrom dplyr select distinct anti_join
#' @family data-export
#' @export
#'
#' @examples
#' # `ind_b` only has data at admin2 -- the function flags it as
#' # unavailable at admin1.
#' ind_list <- tibble::tibble(
#'   var_code = c("ind_a", "ind_b"),
#'   admin_levels_years = list(
#'     tibble::tibble(
#'       admin_level = c("admin1", "admin2"),
#'       admin_level_name = c("Oblast", "Rayon"),
#'       years = list(c(2020), c(2020, 2021))
#'     ),
#'     tibble::tibble(
#'       admin_level = "admin2",
#'       admin_level_name = "Rayon",
#'       years = list(c(2020))
#'     )
#'   )
#' )
#' get_vars_un_avbil(ind_list, admin_levels = c("admin1", "admin2"))
get_vars_un_avbil <- function(ind_list, admin_levels = NULL) {

  ind_extend <-
    ind_list %>%
    dplyr::select(var_code, admin_levels_years) %>%
    tidyr::unnest(cols = c(admin_levels_years)) %>%
    dplyr::distinct(var_code, admin_level)

  if (is.null(admin_levels)) {
    admin_levels <- unique(ind_extend$admin_level) %>% sort()
  }

  expand.grid(
    var_code = unique(ind_extend$var_code),
    admin_level = admin_levels,
    stringsAsFactors = FALSE
  ) %>%
    tibble::as_tibble() %>%
    dplyr::anti_join(ind_extend, by = c("var_code", "admin_level"))
}


#' Reduce per-scheme weights to the admin levels that must be hidden
#'
#' For each weighting scheme in `wght_list`, returns the unique admin
#' levels that carry a non-zero-weighted indicator missing data (per
#' `un_available_vars`). Schemes where no weighted indicator is missing
#' return `NULL`.
#'
#' @param un_available_vars A tibble of unavailable
#'   `(var_code, admin_level)` pairs as returned by
#'   [get_vars_un_avbil()].
#' @param wght_list Named list of per-scheme weight tibbles. Each
#'   element must contain `var_code` and `weight` columns.
#'
#' @return A named list mirroring `wght_list`'s names. Each element is
#'   either a character vector of admin levels to hide, or `NULL` if
#'   the scheme is unaffected.
#'
#' @importFrom purrr map
#' @importFrom dplyr pull inner_join filter
#' @family weights
#' @export
#'
#' @examples
#' unavail <- tibble::tibble(
#'   var_code   = "ind_b",
#'   admin_level = "admin1"
#' )
#' wghts <- list(
#'   scheme_x = tibble::tibble(
#'     var_code = c("ind_a", "ind_b"),
#'     weight   = c(0.5, 0.5)
#'   ),
#'   scheme_y = tibble::tibble(
#'     var_code = c("ind_a", "ind_b"),
#'     weight   = c(1, 0)
#'   )
#' )
#' get_min_admin_wght(unavail, wghts)
get_min_admin_wght <- function(un_available_vars, wght_list) {
  wght_list %>%
    map(~{
      inab_dta <-
        .x %>%
        dplyr::filter(weight != 0) %>%
        dplyr::inner_join(un_available_vars, by = "var_code")

      if (nrow(inab_dta) > 0) {
        inab_dta <-
          inab_dta %>%
          dplyr::pull(admin_level) %>%
          unique()
      } else {
        inab_dta <- NULL
      }
      inab_dta
    })
}


#' Strip unplottable admin levels out of a pre-plot data structure
#'
#' Walks the pre-plot list and drops any element whose `admin_level`
#' name is listed in `adm_to_drom` for that element's `pti_codes`.
#'
#' @param dta A pre-plot list as produced upstream of the plotting
#'   modules -- each element has at least a `pti_codes` slot and a named
#'   `admin_level`.
#' @param adm_to_drom Named list keyed by `pti_codes` whose values are
#'   the admin-level names to remove for that PTI -- typically the output
#'   of [get_min_admin_wght()].
#'
#' @return The input list with offending elements removed.
#'
#' @importFrom purrr imap keep
#' @family validation
#' @export
#'
#' @examples
#' \dontrun{
#' drop_inval_adm(preplot_dta, get_min_admin_wght(unavail, weights_clean))
#' }
drop_inval_adm <- function(dta, adm_to_drom) {
  dta %>%
    imap(~{
      to_drop <- adm_to_drom[[.x$pti_codes]]
      if (!is.null(to_drop) && names(.x$admin_level) %in% to_drop) {
        return(NULL)
      } else {
        return(.x)
      }
    }) %>%
    keep(function(x) !is.null(x))
}
