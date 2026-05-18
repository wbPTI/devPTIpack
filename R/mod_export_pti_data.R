#' Reactive PTI export data assembler
#'
#' Server module that assembles the named list returned to download
#' handlers when the user exports the current PTI session. Combines the
#' country tibble (`weights_dta()$general`), the per-scheme weights
#' tibble ([get_pti_weights_export()]), and the per-admin-level scores
#' tibbles ([get_pti_scores_export()]) into a single named list ready
#' for `writexl::write_xlsx()`.
#'
#' @param id Character. Shiny module namespace ID.
#' @param plotted_dta Reactive yielding the post-filter / post-legend
#'   structured PTI list (output of [mod_plot_pti2_srv()]'s internal
#'   `pre_map_dta` reactive).
#' @param weights_dta Reactive yielding the weights list (output of the
#'   weights-input module). Must contain `weights_clean`,
#'   `indicators_list`, and `general`.
#'
#' @return A `reactive()` yielding the named list:
#'   `Country`, `Weighting schemes`, then one per-admin-level slot
#'   (finer admin first).
#'
#' @importFrom shiny moduleServer reactive req
#' @noRd
mod_export_pti_data_server <- function(id, plotted_dta, weights_dta){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    reactive({
      req(plotted_dta())
      req(weights_dta())
      get_pti_scores_export(plotted_dta()) %>%
        rev() %>%
        prepend(
          list(`Weighting schemes` =
                 get_pti_weights_export(weights_dta()$weights_clean,
                                        weights_dta()$indicators_list))
        ) %>%
        prepend(list(Country = weights_dta()$general))
    })

  })
}


#' Reshape PTI scores into per-admin-level export tibbles
#'
#' Walks the structured PTI list and, for each admin level, joins every
#' scheme's score tibble into a single wide tibble with one
#' `<scheme> - PTI Score` and `<scheme> - PTI Priority` column pair per
#' weighting scheme. Drops geometry, label, and area columns. The
#' priority column is computed via the per-scheme `recode_function()`
#' attached by `add_legend_paras()`.
#'
#' @param plotted_dta The post-filter / post-legend structured PTI list
#'   (one element per admin level / scheme; each element a list with
#'   `pti_dta`, `pti_codes`, `admin_level`, and a `leg` block).
#'
#' @return A named list of tibbles -- one per admin level. Each name is
#'   `"<admin_level> PTI Scores"` (e.g. `"admin1 PTI Scores"`).
#'
#' @importFrom dplyr select rename_with mutate full_join contains
#' @importFrom purrr map2 imap reduce set_names
#' @importFrom sf st_drop_geometry
#' @importFrom stringr str_detect str_replace str_c
#' @family data-export
#' @export
#'
#' @examples
#' \dontrun{
#' # plotted_dta is the post-legend reactive returned by mod_plot_pti2_srv()
#' get_pti_scores_export(plotted_dta())
#' }
get_pti_scores_export <- function(plotted_dta) {
  plotted_dta %>%
    get_current_levels() %>%
    map2(names(.), ~ {
      by_joint <-
        plotted_dta %>%
        `[`(str_detect(names(.), .y)) %>%
        map(~{.x$pti_dta %>% names()}) %>%
        unlist() %>% unname() %>% unique() %>%
        `[`(str_detect(., "admin\\d|spatial_name"))
      plotted_dta %>%
        `[`(str_detect(names(.), .y)) %>%
        imap( ~ {
          repl <- .x$pti_codes %>% unname()
          .x$pti_dta %>%
            sf::st_drop_geometry() %>%
            select(-contains("pti_label"),-contains("area")) %>%
            mutate(
              `pti_score - PTI Score` = pti_score,
              `pti_score - PTI Priority` = .x$leg$recode_function(pti_score)
            ) %>%
            select(-pti_score) %>%
            rename_with( ~ str_replace(., "pti_score", repl))
        }) %>%
        reduce(full_join, by = by_joint) %>%
        list() %>%
        set_names(str_c(.x, " PTI Scores"))
    }) %>%
    unname() %>%
    unlist(recursive = FALSE)
}



#' Reshape weight schemes into a single export tibble
#'
#' Joins each weight scheme's `(var_code, weight)` tibble onto the
#' indicator dictionary (`var_code`, `var_name`, `pillar_name`),
#' relabelling the weight column to `"Weights - <scheme>"`. Reduces all
#' schemes to a single wide tibble keyed by indicator, then drops
#' `var_code` and renames the dictionary columns to user-facing
#' headers.
#'
#' @param wghts_dta Named list of weight tibbles, one per scheme. Each
#'   element a tibble with `var_code` and `weight`.
#' @param indic_dta The indicator dictionary tibble (with `var_code`,
#'   `var_name`, `pillar_name`).
#'
#' @return A tibble with `Variable name`, `Pillar`, and one
#'   `Weights - <scheme>` column per scheme.
#'
#' @importFrom dplyr select left_join rename_with
#' @importFrom purrr map2 reduce
#' @importFrom stringr str_c str_replace
#' @family data-export
#' @export
#'
#' @examples
#' data(rwa_mtdt_full)
#' weights <- get_rand_weights(rwa_mtdt_full$metadata)
#' get_pti_weights_export(weights, rwa_mtdt_full$metadata)
get_pti_weights_export <- function(wghts_dta, indic_dta) {
  indic_dta <-
    indic_dta %>%
    select(var_code , var_name, pillar_name)

  wghts_dta %>%
    map2(names(.), ~ {
      new_name <- str_c("Weights - ", .y)
      indic_dta %>%
        left_join(.x, "var_code") %>%
        rename_with(~ str_replace(., "weight", new_name))
    }) %>%
    reduce(left_join, by = c("var_code", "var_name", "pillar_name")) %>%
    select(-var_code) %>%
    rename_with(~ str_replace(., "var_name", "Variable name")) %>%
    rename_with(~ str_replace(., "pillar_name", "Pillar"))
}
