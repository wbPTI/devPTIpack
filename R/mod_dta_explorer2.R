#' Data-explorer page server module
#'
#' Server module powering the data-explorer page. Computes the
#' indicator list (excluding indicators tagged
#' `fltr_exclude_explorer`), reshapes the metadata into per-admin
#' explorer data, wires the variable selector (`mod_fltr_sel_var2_srv()`),
#' the admin-level selector (`mod_get_admin_levels_srv()`), and the
#' n-bins selector (`mod_get_nbins_srv()`). Defers map rendering until
#' the host tab is opened (via `mod_first_open_count_server()`).
#'
#' @param id Character. Shiny module namespace ID. Must match the `id`
#'   passed to [mod_dta_explorer2_ui()].
#' @param shp_dta Reactive yielding a named list of `sf` tibbles, one
#'   per admin level.
#' @param input_dta Reactive yielding the metadata list (output of
#'   [fct_template_reader()]).
#' @param active_tab Reactive character indicating the currently
#'   selected tab.
#' @param target_tabs Character vector of tab names this module should
#'   render for.
#' @param mtdtpdf_path,shapes_path,data_path Character or `NULL`.
#'   Filesystem paths to the metadata PDF, shapes archive, and metadata
#'   xlsx served via the side-panel download links.
#' @param expl_show_adm_levels,expl_default_adm_level Forwarded to
#'   `mod_get_admin_levels_srv()` (`show_adm_levels` and
#'   `default_adm_level`). Take precedence over the `explorer_*`
#'   Golem options.
#' @param add_selected Reactive yielding character vector of indicator
#'   names to programmatically add to the variable picker (e.g.
#'   triggered by another tab). Defaults to `reactive(NULL)`.
#' @param ... Unused; retained for forward compatibility.
#'
#' @return A `reactive()` yielding `list(pre_map_dta, init_leaf)`. Other
#'   modules subscribing to the explorer's state read this reactive.
#'
#' @importFrom shiny reactive req
#' @export
#'
#' @examples
#' \dontrun{
#' mod_dta_explorer2_server(
#'   "explorer",
#'   shp_dta     = shp_dta,
#'   input_dta   = input_dta,
#'   active_tab  = active_tab,
#'   target_tabs = "Explore data"
#' )
#' }
mod_dta_explorer2_server <-
  function(id,
           shp_dta,
           input_dta,
           active_tab,
           target_tabs,
           mtdtpdf_path = NULL,
           shapes_path = NULL,
           data_path = NULL,
           expl_show_adm_levels = NULL,
           expl_default_adm_level = NULL,
           add_selected = reactive(NULL),
           ...) {

    first_open <- mod_first_open_count_server(id, active_tab, target_tabs)

    indicators_list <- reactive({get_indicators_list(input_dta(), "fltr_exclude_explorer")})

    pre_map_dta_1 <- reactive({
      req(indicators_list())
      input_dta() %>%
        pivot_pti_dta(indicators_list()) %>%
        reshaped_explorer_dta(indicators_list()) %>%
        structure_pti_data(shp_dta()) %>%
        preplot_reshape_wghtd_dta()
    })

    var_choices <- reactive({req(indicators_list()) %>% get_var_choices()})

    pre_map_dta_2 <- mod_fltr_sel_var2_srv(id, pre_map_dta_1, var_choices, first_open,
                                           add_selected = add_selected)

    sel_adm_levels <-
      mod_get_admin_levels_srv(
        id,
        reactive(get_current_levels(pre_map_dta_2())),
        show_adm_levels = expl_show_adm_levels,
        default_adm_level = expl_default_adm_level,
        def_adm_opt = "explorer_default_adm",
        show_adm_opt = "explorer_show_adm",
        choose_adm_opt = "explorer_choose_adm"
        )

    n_bins <- mod_get_nbins_srv(id)

    pre_map_dta_3 <- reactive({
      req(sel_adm_levels())
      req(n_bins())
      req(first_open())
      pre_map_dta_2() %>%
        filter_admin_levels(sel_adm_levels()) %>%
        add_legend_paras(nbins = n_bins()) %>%
        rev()
    })

    init_leaf <- mod_plot_init_leaf_server(id, shp_dta)

    out_leaf <- mod_plot_poly_leaf_server(id, pre_map_dta_3, shp_dta, leg_type = "value")

    mod_map_dwnld_srv(id, out_leaf, metadata_path = mtdtpdf_path, shapes_path = shapes_path)
    mod_dwnld_file_server(id, "mtdt.files.side", filepath = mtdtpdf_path)
    mod_dwnld_file_server(id, "shp.files.side", filepath = shapes_path)
    mod_dwnld_file_server(id, "dta.download.side", filepath = data_path)

    reactive({
      list(pre_map_dta = pre_map_dta_3, init_leaf = init_leaf)
    })

  }

#' Data-explorer page UI
#'
#' Builds the explorer page layout: a full-width Leaflet output overlaid
#' by the explorer side panel (`mod_dta_explorer2_side_ui()`), wrapped
#' in a [shiny::fluidRow()] and a Cicerone-tour-friendly stack of
#' nested `div`s.
#'
#' @param id Character. Shiny module namespace ID. Must match the `id`
#'   passed to [mod_dta_explorer2_server()].
#' @param multi_choice Logical or `NULL`. Whether the indicator picker
#'   accepts multiple selections. Forwarded to `mod_select_var_ui()`;
#'   may be overridden by the `explorer_multiple_var` Golem option.
#' @param max_choice Integer. Max number of indicators a user may pick
#'   when `multi_choice = TRUE`.
#' @param map_dwnld_options Character vector of download options
#'   exposed in the side panel. Any subset of
#'   `c("data", "weights", "shapes", "metadata")`.
#' @param ... Forwarded to `leaflet::leafletOutput()`.
#'
#' @return A `shiny::tagList()` containing the explorer layout.
#'
#' @importFrom shiny NS tagList fluidRow tags
#' @importFrom leaflet leafletOutput
#' @export
#'
#' @examples
#' \dontrun{
#' mod_dta_explorer2_ui("explorer", multi_choice = FALSE)
#' }
mod_dta_explorer2_ui <- function(
    id,
    multi_choice,
    max_choice = 3,
    map_dwnld_options = c("shapes", "metadata"),
    ...){
  ns <- NS(id)
  tagList(
    leafletOutput(ns("leaf_id"), width = "100%", ...),
    mod_dta_explorer2_side_ui(
      id,
      multi_choice = multi_choice,
      max_choice = max_choice,
      map_dwnld_options = map_dwnld_options
    )
  ) %>%
    tags$div(style = "position:relative;") %>%
    tags$div(id = "explorer_1") %>%
    tags$div(id = "explorer_2") %>%
    tags$div(id = "explorer_3") %>%
    fluidRow() %>%
    tagList(
      golem_add_external_resources(),
      shinyjs::useShinyjs()
      )
}


#' Side panel UI for the explorer page
#'
#' Floating `shiny::absolutePanel()` housing the explorer's variable
#' selector ([mod_select_var_ui()]), admin-level selector
#' ([mod_get_admin_levels_ui()]), n-bins selector
#' ([mod_get_nbins_ui()]), and per-format download links
#' ([mod_map_dwnld_ui()]).
#'
#' @param id Character. Shiny module namespace ID.
#' @param multi_choice Logical or `NULL`. Whether the indicator picker
#'   accepts multiple selections.
#' @param max_choice Integer. Max number of indicators when
#'   `multi_choice = TRUE`.
#' @param map_dwnld_options Character vector of download options exposed
#'   in the side panel.
#' @param ... Unused; retained for forward compatibility.
#'
#' @return A `shiny::absolutePanel()` containing the explorer side panel.
#'
#' @importFrom shiny NS absolutePanel div
#' @noRd
mod_dta_explorer2_side_ui <-
  function(id,
           multi_choice,
           max_choice = 3,
           map_dwnld_options = c("shapes", "metadata"),
           ...) {

  ns <- NS(id)

  absolutePanel(
    id = ("nbins_panel"),
    fixed = FALSE,
    draggable = FALSE,
    left = "auto", bottom = "auto",
    width = 400,
    height = "auto",
    top = 10, right = 10,

    div(mod_select_var_ui(id, multi_choice, max_choice), id = ns("var_choice")),
    mod_get_admin_levels_ui(id),
    div(mod_get_nbins_ui(id, "Number of bins"), id = ns("bins_choice")),
    div(mod_map_dwnld_ui(id, map_dwnld_options), id = ns("map_dwnload"))
  )
}


#' UI for the explorer's indicator selector
#'
#' `shinyWidgets::pickerInput()` with searchable dropdown for selecting
#' one or more indicators to visualise on the explorer map. Multi-select
#' behaviour is controlled by the `multi_choice` argument or the
#' `explorer_multiple_var` Golem option (option wins when set).
#'
#' @param id Character. Shiny module namespace ID.
#' @param multi_choice Logical or `NULL`. Default multi-select state.
#' @param max_choice Integer. Max simultaneous selections.
#' @param ... Unused; retained for forward compatibility.
#'
#' @return A `shiny::tagList()` containing the picker input.
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput pickerOptions
#' @noRd
mod_select_var_ui <- function(id, multi_choice = NULL, max_choice = 3, ...) {
  ns <- NS(id)

  explorer_multiple_var <- golem::get_golem_options("explorer_multiple_var")
  if (is.null(explorer_multiple_var)) explorer_multiple_var <- multi_choice
  if (is.null(explorer_multiple_var)) explorer_multiple_var <- FALSE

  tagList(
    shinyWidgets::pickerInput(
      ns("indicators"),
      "Select an indicator",
      NULL, NULL,
      multiple = explorer_multiple_var,
      width = "100%",
      options = shinyWidgets::pickerOptions(dropdownAlignRight  = TRUE,
                                            liveSearch = TRUE,
                                            maxOptions = max_choice)
    )
  )

}


#' Server for the explorer's indicator selector
#'
#' Drives the indicator picker: refreshes the choices when `choices()`
#' changes, auto-selects the first indicator on first open of the host
#' tab, optionally adds programmatic selections from `add_selected()`,
#' and returns the filtered explorer data via [filter_var_explorer()],
#' debounced by 500ms.
#'
#' @param id Character. Shiny module namespace ID.
#' @param preplot_dta Reactive yielding the explorer data
#'   (output of [reshaped_explorer_dta()] threaded through the
#'   structure / preplot helpers).
#' @param choices Reactive yielding the named-list choice structure
#'   (output of [get_var_choices()]).
#' @param first_open Reactive yielding `TRUE` on first open of the
#'   explorer tab (output of [mod_first_open_count_server()]).
#' @param add_selected Reactive yielding character vector of indicator
#'   names to programmatically add to the picker selection. Defaults
#'   to `reactive(NULL)`.
#' @param ... Unused; retained for forward compatibility.
#'
#' @return A `reactive()` yielding the filtered explorer data (or
#'   `NULL` when no indicator is selected).
#'
#' @note **Pinned bug (PLAN.md §12, PR #37):** the `add_selected()`
#'   observer's predicate (a `purrr::map_lgl()` over `choices()` that
#'   tests `.x \%in\% c(selected_add, selected_now) | .x \%in\% names(...)`)
#'   errors with "Result must be length 1, not N" whenever any pillar
#'   holds >1 variable, because `.x` is the length-N character vector
#'   for that pillar and `\%in\%` returns length-N. Should be wrapped
#'   in `any()`. Tier-2 test
#'   `tests/testthat/test-mod-var-selector.R` exercises the happy path
#'   with single-var pillars and pins the broken predicate separately.
#'
#' @importFrom shiny moduleServer observeEvent reactive eventReactive
#'   debounce isTruthy req
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom purrr map map_lgl
#' @noRd
mod_fltr_sel_var2_srv <- function(id, preplot_dta, choices, first_open,
                                  add_selected = reactive(NULL), ...) {

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observeEvent(
        choices(),
        {
          shinyWidgets::updatePickerInput(
            session,
            inputId = "indicators",
            choices = choices() %>% map(~.x %>% names()),
            selected = NULL
          )
        },
        ignoreNULL = TRUE,
        ignoreInit = FALSE)

      observeEvent(
        first_open(),
        {
          req(first_open())
          shinyWidgets::updatePickerInput(
            session,
            inputId = "indicators",
            selected = choices() %>% map(~.x %>% names()) %>% unlist() %>% `[[`(1)
          )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE)

      observeEvent(
        add_selected(),
        {
          req(add_selected())
          selected_now <- selected_var()
          selected_add <- add_selected()
          shinyWidgets::updatePickerInput(
            session,
            inputId = "indicators",
            selected =
              choices() %>%
              `[`(purrr::map_lgl(.,  ~ {
                .x %in% c(selected_add, selected_now) |
                  .x %in% names(c(selected_add, selected_now))
              }))
          )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE)

      selected_var <- reactive({input$indicators}) %>% debounce(500)

      eventReactive(
        selected_var(), {
          out <- NULL
          if (isTruthy(selected_var())) {
            out <-
              preplot_dta() %>%
              filter_var_explorer(selected_var())
          }
          out
        })

    }
  )

}


#' Reshape long-format metadata for the explorer map
#'
#' Walks the long-format pivoted metadata (one tibble per admin level)
#' and shapes each admin-level tibble for the explorer renderer:
#' renames `value -> pti_score`, joins indicator names from `ind_list`,
#' picks the admin-level Pcod / Name columns, and applies
#' [label_generic_pti()] with an explorer-specific glue template that
#' shows the variable name and value rather than a PTI rank.
#'
#' @param long_dta Long-format pivoted metadata (one tibble per admin
#'   level, output of [pivot_pti_dta()]).
#' @param ind_list Indicator dictionary tibble (must contain `var_code`
#'   and `var_name`).
#'
#' @return A named list of tibbles -- one per admin level. Names are
#'   the bare admin level (`admin1`, `admin2`, ...).
#'
#' @importFrom dplyr left_join select one_of
#' @importFrom purrr map
#' @importFrom rlang sym
#' @importFrom stringr str_c str_detect str_extract
#' @noRd
reshaped_explorer_dta <- function(long_dta, ind_list) {
  out <-
    long_dta %>%
    map(~ {
      adm_level <-
        names(.x) %>%
        `[`(str_detect(., 'admin\\d')) %>%
        max() %>%
        str_extract(., 'admin\\d')

      id_var <- str_c(adm_level, "Pcod")
      nm_var <- str_c(adm_level, "Name")

      .x %>%
        left_join(ind_list %>% select(var_code, var_name), by = "var_code") %>%
        select(
          one_of(id_var),
          pti_score = value,
          pti_name = var_name,
          spatial_name = !!sym(nm_var)
        )
    }) %>%
    label_generic_pti(
      str_c(
        "<strong>{spatial_name}</strong>",
        "<br/>Variable: <strong>{ifelse(is.na(pti_name), 'No data', pti_name)}</strong>",
        "<br/>Value: <strong>{ifelse(is.na(pti_score), 'No data', round(pti_score, 6))}</strong>",
        "<br/>",
        collapse = ""
      )
    )

  new_name <- str_extract(names(out), "admin\\d")
  names(out) <- new_name
  out
}

#' Build the indicator-picker choice structure
#'
#' Groups indicators by `pillar_name`, sorted by `pillar_group` then
#' `var_order`, into a named list-of-lists where each top-level name
#' is a pillar and each inner element is the named character vector of
#' `var_name -> var_code` pairs for that pillar. When pillar names are
#' missing, falls back to `"Indicators"` (single pillar) or
#' `"Indicators 1"`, `"Indicators 2"`, ... (multiple).
#'
#' @param indicators_list Indicator dictionary tibble (with `var_code`,
#'   `var_name`, `pillar_name`, `pillar_group`, `var_order`).
#'
#' @return A named list-of-lists ready for
#'   `shinyWidgets::updatePickerInput()`.
#'
#' @note **Pinned bug (PLAN.md §12, PR #27):** when `indicators_list` is
#'   empty, the function errors with "attempt to set an attribute on
#'   NULL" because the fallback branch `names(out) <- "Indicators"`
#'   runs even when `out` is `NULL`. A length-0 list output would be
#'   more useful. Pinned in
#'   [test-explorer-helpers.R](tests/testthat/test-explorer-helpers.R).
#'
#' @importFrom dplyr arrange group_by
#' @importFrom tidyr nest
#' @importFrom purrr pmap set_names
#' @importFrom stringr str_c
#' @noRd
get_var_choices <- function(indicators_list) {
  out <-
    indicators_list %>%
    dplyr::arrange(pillar_group, var_order) %>%
    dplyr::group_by(pillar_name) %>%
    tidyr::nest() %>%
    pmap(
      .f = function(pillar_name, data) {
        purrr::set_names(list(
          purrr::set_names(data$var_code, data$var_name)), pillar_name)
      }
    ) %>%
    unlist(recursive = F)
  if (all(is.na(names(out))) || all(names(out) == "")) {

    if (length(names(out)) > 1) {
      names(out) <- str_c("Indicators ", seq_along(names(out)))
    } else {
      names(out) <- "Indicators"
    }
  }

  if (any(is.na(names(out))) || any(names(out) == "")) {
    names(out)[is.na(names(out))] <- seq_along(names(out))[is.na(names(out))]
  }
  out
}


#' Filter explorer pre-plot data to the selected indicators
#'
#' Subsets the pre-plot list (one element per `<indicator> (<admin>)`
#' combination) to only those whose `pti_codes` value or name appears
#' in `vars`. Used inside [mod_fltr_sel_var2_srv()] to translate the
#' picker's selection into a filtered map dataset.
#'
#' @param preplot_dta The pre-plot structured PTI list.
#' @param vars Character vector or named character vector of indicator
#'   codes / names to keep.
#'
#' @return The filtered subset of `preplot_dta`.
#'
#' @importFrom purrr map_lgl
#' @noRd
filter_var_explorer <- function(preplot_dta, vars) {
  preplot_dta %>%
    `[`(purrr::map_lgl(.,  ~ {
      .x$pti_codes %in% vars | .x$pti_codes %in% names(vars)
    }))
}
