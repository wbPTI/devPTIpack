#' UI for the per-indicator DT-based weight-input widget
#'
#' Internal sub-module UI. Renders a `DT::dataTableOutput` slot
#' (sized via `height` / `dt_style`) that the paired server populates
#' with one row per indicator (and one styled row per pillar group).
#' Also injects the dtcenter / dtcustom CSS classes used by
#' `make_input_DT` for tight cell padding.
#'
#' @param id Character. Shiny module namespace ID.
#' @param height Character or `NULL`. CSS height passed to
#'   `DT::dataTableOutput`. Defaults to `"550px"` when `NULL`.
#' @param dt_style Character or `NULL`. Additional CSS applied to the
#'   outer wrapper div (e.g. to override `max-height`).
#' @param ... Additional attributes forwarded to the wrapper div.
#'
#' @return A `shiny.tag.list` with the `DT` slot and a `<style>` block.
#'
#' @importFrom shiny NS tagList tags HTML div
#' @importFrom DT dataTableOutput
#' @noRd
mod_DT_inputs_ui <- function(id, height = NULL, dt_style = NULL, ...){
  ns <- NS(id)
  if(is.null(height)) height <- "550px"

  DT::dataTableOutput(ns('wghts_dt'), height = height)  %>%
    shiny::tagList(shiny::tags$style(
      shiny::HTML(
        ".dtcenter .form-group {margin-bottom: 0px !important};
        .dtcenter {text-align: -webkit-center;};
        .dtcustom {padding: 2px 3px !important;};
        .td {padding: 2px'};
        "
      )
    )) %>%
    shiny::tagList(
      ) %>%
    div(style = dt_style, ...)
}

#' Server for the per-indicator DT-based weight-input widget
#'
#' Internal sub-module server invoked from `mod_wt_inp_server`.
#' Renders the indicator/pillar table, wires "All 0" / "All 1" pillar
#' buttons via `mod_wt_btns_srv`, collects current weight values via
#' `mod_collect_wt_srv`, and applies pre-loaded weight updates pushed
#' through `update_dta`. The collected current values are throttled
#' (500 ms) before being returned so rapid keyboard input does not
#' force a recompute on every keystroke.
#'
#' @param id Character. Shiny module namespace ID.
#' @param ind_list Reactive returning the indicators tibble produced
#'   by `get_indicators_list()` (must include `pillar_group`,
#'   `var_code`, `var_name`).
#' @param update_dta Reactive returning either `NULL` or a tibble
#'   with two columns matched positionally as `(input_id, value)`;
#'   when truthy, each row triggers `updateNumericInput()`.
#'
#' @return A throttled reactive returning the current values as a
#'   tibble with columns `var_code` and `weight`.
#'
#' @importFrom shiny moduleServer reactive observe req throttle
#'   renderPrint updateNumericInput
#' @importFrom DT renderDataTable
#' @importFrom purrr pwalk
#' @noRd
mod_DT_inputs_server <- function(id, ind_list, update_dta = reactive(NULL)){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ind_DT <- reactive({ind_list() %>% make_input_DT(ns = ns)})

    output$wghts_dt <- DT::renderDataTable(
      ind_DT()$dt_out,
      server = FALSE
    )

    mod_throw_tooltip(NULL, ind_DT)

    mod_wt_btns_srv(NULL, ind_list,  dtn_id = "_set_zero", to_value = 0)
    mod_wt_btns_srv(NULL, ind_list,  dtn_id = "_set_one", to_value = 1)

    current_values <-
      mod_collect_wt_srv(NULL, ind_list) %>%
      throttle(500)

    observe({
      req(update_dta())
      update_dta() %>%
        pwalk(~ {
          updateNumericInput(session = session, inputId = ..1, value = ..2)
        })
    })

    output$wghts_dt_values = renderPrint({
      current_values() %>% glimpse()
    })

    current_values

  })
}

#' Pillar-level "All 0" / "All 1" action button pair
#'
#' Internal helper used by `prep_input_data` to render two compact
#' action buttons in each pillar header row of the DT widget. Button
#' input IDs follow the pattern `pillar_<id>_set_one` and
#' `pillar_<id>_set_zero`, matching what `mod_wt_btns_srv` listens
#' for.
#'
#' @param id Character. The `pillar_group` value identifying which
#'   pillar this button pair targets.
#' @param ns Namespace function (typically `session$ns` or the UI's
#'   `NS(id)`).
#'
#' @return A `shiny.tag.list` with two `actionButton`s ("All 1" and
#'   "All 0").
#'
#' @importFrom shiny actionButton tagList
#' @importFrom stringr str_c
#' @noRd
add_two_action_btn <- function(id, ns) {

  btn_one <-
    str_c("pillar_", id, "_set_one") %>%
    ns() %>%
    actionButton(.,
                 "All 1",
                 class = "btn-primary btn-xs",
                 width = "40%",
                 style = "margin-top: 0px;")

  btn_two <-
    str_c("pillar_", id, "_set_zero") %>%
    ns() %>%
    actionButton(.,
                 "All 0",
                 class = "btn-primary btn-xs",
                 width = "40%",
                 style = "margin-top: 0px; padding: 1px;")

  tagList(btn_one, btn_two)

}


#' Reshape the indicators list into a DT-ready interleaved tibble
#'
#' Internal helper used by `make_input_DT`. Takes an indicators tibble
#' and returns one row per pillar (carrying the pillar buttons) and
#' one row per variable (carrying a `numericInput`), interleaved in
#' display order (`pillar_group`, then `var_order`). Also constructs
#' the tooltip text from var/pillar descriptions and emits a per-row
#' `ttip_id` used by `mod_throw_tooltip` to surface info modals.
#'
#' @param ind_list Tibble produced by `get_indicators_list()` (must
#'   include `pillar_group`, `pillar_name`, `pillar_description`,
#'   `var_code`, `var_name`, `var_description`, `var_order`,
#'   `admin_levels_years`).
#' @param ns Namespace function used to namespace the action button
#'   and `numericInput` IDs.
#'
#' @return A tibble with columns `var_name`, `var_code`,
#'   `var_description`, `type` (`"pillar"` or `"variable"`), `pillar`,
#'   `ui` (HTML for the input cell), `ttip_id`, `ttip_var_name`.
#'
#' @importFrom dplyr mutate select arrange group_by across everything
#'   case_when bind_rows
#' @importFrom tidyr nest
#' @importFrom purrr map_chr map pmap_chr pmap_dfr
#' @importFrom shiny markdown numericInput tags icon actionLink
#' @importFrom stringr str_c str_replace_all str_replace_na
#' @importFrom glue glue
#' @importFrom rlang dots_list
#' @importFrom tibble tibble
#' @noRd
prep_input_data <- function(ind_list, ns) {
  ind_list %>%
    mutate(
      var_adm_levels = map_chr(admin_levels_years, ~ {str_c(.x$admin_level_name, collapse = ", ")}),
      var_years = map_chr(admin_levels_years, ~ {
        if (length(.x$years) > 0 && !(identical(logical(0), unlist(.x$years)) |
                                      identical(numeric(0), unlist(.x$years)))) {
          str_c(.x$years, collapse = ", ")
        } else {
          NA_character_
        }
      }) %>%
        ifelse(is.na(.), ., str_c("<p>Available year(s): ", ., "</p>")),
      var_description =
        map_chr(var_description, ~shiny::markdown(.x)) %>%
        str_replace_all("\n", ""),
      var_description =
        ifelse(
          length(var_description) == 0 | is.na(var_description),
          "", var_description),
      tooltip_text =
        glue("<strong>{var_name}</strong>",
             "{var_description}",
             "Admin level(s): {var_adm_levels}",
             "{var_years}",
             .na = ""),
      var_description = tooltip_text,
      pillar_description = as.character(pillar_description),
      pillar_description = map(pillar_description, ~shiny::markdown(.x)),
      pillar_description = glue("<strong>{pillar_name}</strong><p>{pillar_description}</p>")
    ) %>%
    select(-tooltip_text, -var_adm_levels, -var_years) %>%
    arrange(pillar_group, var_order) %>%
    group_by(pillar_group, pillar_name, pillar_description) %>%
    nest() %>%
    pmap_dfr(.f = function(...){
      dts <- rlang::dots_list(...)
      tibble(var_name = dts$pillar_name,
             var_code = dts$pillar_group,
             var_description = dts$pillar_description,
             type = "pillar") %>%
        mutate(across(everything(), ~as.character(.))) %>%
        bind_rows(
          dts$data %>%
            mutate(type = "variable") %>%
            mutate(across(everything(), ~as.character(.)))
          ) %>%
        select(var_name, var_code, var_description, type) %>%
        mutate(pillar =
                 c(dts$pillar_name, dts$pillar_description) %>%
                 str_replace_na("") %>%
                 str_c(sep = " ", collapse = " "))
    }) %>%
    mutate(
      ui = case_when(
        type == "pillar" ~
          map_chr(var_code, ~{
            add_two_action_btn(.x, ns = ns) %>% as.character()
          } ),
        type == "variable" ~
          map_chr(var_code, ~{
            numericInput(ns(.x), label = NULL, value = 0, step = 1, width = "100%") %>%
              as.character()
          }),
        FALSE ~ ""
      )
    ) %>%
    mutate(
      ttip_id =  str_c("inp-inf-", row_number()),
      ttip_var_name = var_name,
      ttip_id = ifelse(var_name == "" | is.na(var_name), NA_character_, ttip_id),
      var_name =
        pmap_chr(
          list(var_name, var_description, row_number(), ttip_id),
          ~ {
            ttip <-
              actionLink(inputId = ns(..4),
                         label = "",
                         icon = shiny::icon("info-sign", lib = "glyphicon", style = "color:blue;")
                         )

            ttip <-
              ttip %>%
              shiny::tags$sup() %>%
              as.character() %>%
              str_replace_all("[\n|\r]", "")

            if (!is.na(..1) && ..1 != "") {
              str_c(..1, " ", ttip)
            } else{
              str_c(..1, " <span> </span>")
            }

          }))
}


#' Compute visible / invisible column targets for the input DT
#'
#' Internal helper used by `make_input_DT` to translate the column
#' names of the interleaved tibble produced by `prep_input_data` into
#' DT `columnDefs` entries: only `var_name` and `ui` are visible; all
#' other columns are hidden but searchable. Also assigns the tight
#' `dtcustom dtcenter` CSS classes to the input column and `dtcustom`
#' to the variable-name column.
#'
#' @param nested_dta Tibble produced by `prep_input_data`.
#'
#' @return A list with two slots: `columnDefs` (list of DT column
#'   definitions, hidden columns first) and `colnames` (character
#'   vector with all names blanked so the DT renders no header).
#'
#' @importFrom rlang set_names
#' @noRd
make_vis_targets_for_dt <- function(nested_dta) {
  visible_vars <-
    names(nested_dta) %>%
    set_names(seq_along(.)-1, .) %>%
    `[`(names(.) %in% c("var_name", "ui"))

  invisible_vars <-
    names(nested_dta) %>%
    set_names(seq_along(.)-1, .) %>%
    `[`(!names(.) %in% c("var_name", "ui"))

  visible_targets <-
    visible_vars %>%
    unname() %>%
    list(
      c("55%", rep("45%", length(.)-1)),
      c("150px", rep("100px", length(.)-1))
    ) %>%
    pmap(.,~{list(targets=c(..1), visible=TRUE, width = ..2
                  )})

  visible_targets[[length(visible_targets)]]$className <- c("dtcustom dtcenter")
  visible_targets[[1]]$className <- c("dtcustom")

  invisible_targets <-
    invisible_vars%>%
    unname() %>%
    c()

  invisible_targets <-
    list(targets=c(invisible_targets), visible=FALSE, searchable = TRUE, width="0px")

  colnames <-
    nested_dta %>%
    names() %>%
    setNames(., rep("", length(.)))

  list(
    columnDefs = append(list(invisible_targets), visible_targets),
    colnames = colnames
  )

}


#' Construct the per-indicator DT widget
#'
#' Internal helper used by `mod_DT_inputs_server`. Combines
#' `prep_input_data` (interleaved pillar/variable rows) and
#' `make_vis_targets_for_dt` (column visibility) into a configured
#' `DT::datatable`. Disables paging in favour of the `scrollResize`
#' DataTables plugin (https://datatables.net/blog/2017-12-31), forces
#' header rows hidden via a `headerCallback` JS hook, and enables
#' Shiny input rebinding inside the table so the embedded
#' `numericInput`s remain reactive.
#'
#' @param ind_list Indicators tibble produced by
#'   `get_indicators_list()`.
#' @param ns Namespace function used inside `prep_input_data` to
#'   namespace `numericInput` IDs.
#' @param width,height,scrollY DT widget dimensions; defaults match
#'   the production layout.
#'
#' @return A list with two slots: `dt_out` (the `DT::datatable`
#'   widget) and `nested_dta` (the prepped tibble, returned so callers
#'   like `mod_throw_tooltip` can read the `ttip_id` / `var_description`
#'   columns without rebuilding it).
#'
#' @importFrom DT datatable formatStyle styleEqual
#' @importFrom htmlwidgets JS
#' @noRd
make_input_DT <- function(ind_list, ns = function(x) x, width = "100%", height = "100%", scrollY="450px") {

  nested_dta <- prep_input_data(ind_list, ns = ns)
  targets_dta <- make_vis_targets_for_dt(nested_dta)
  dt_out <-
    nested_dta %>%
    datatable(
      width = width,
      height = height,
      escape = FALSE,
      selection = 'none',
      fillContainer = F,
      rownames = NULL,
      colnames = NULL,
      plugins = c('scrollResize'),
      options = list(
        dom = 'ft',
        bPaginate = FALSE,
        columnDefs = targets_dta$columnDefs,
        ordering = FALSE,
        autoWidth = F,

        paging = FALSE,
        scrollResize = TRUE,
        scrollY =  100,
        scrollCollapse = TRUE,

        headerCallback = JS(
          "function(thead, data, start, end, display){
          $('th', thead).css('display', 'none');
          }"
        )
      ),
      callback = JS("table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());")
    ) %>%
    formatStyle(
      'type',
      target = 'row',
      backgroundColor = styleEqual("pillar", c('lightgray')),
      fontWeight = styleEqual("pillar", c('bold')),
    )
  list(dt_out = dt_out, nested_dta = nested_dta)
}


#' Surface info-modal popups when an indicator's tooltip link is clicked
#'
#' Internal server module invoked from `mod_DT_inputs_server`. Polls
#' each indicator row's per-row `actionLink` (named after `ttip_id`)
#' and shows a `modalDialog` with the indicator's
#' `var_description` whenever its click count increments. The
#' `last_info()` baseline avoids firing a modal for the first
#' (initial) read of every link.
#'
#' @param id Character. Shiny module namespace ID.
#' @param ind_DT Reactive returning the list output of `make_input_DT`
#'   (specifically the `nested_dta` slot, used to look up `ttip_id`,
#'   `var_description`, and `ttip_var_name`).
#'
#' @return Called for side effects (registers `observeEvent`s that
#'   call `shiny::showModal`).
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent
#'   isTruthy showModal modalDialog HTML
#' @importFrom dplyr filter
#' @importFrom purrr pmap pwalk
#' @importFrom rlang dots_list
#' @noRd
mod_throw_tooltip <-
  function(id, ind_DT = reactive(NULL)){
    moduleServer(id, function(input, output, session){

      last_info <- reactiveVal()

      tippy_DT <- reactive({ind_DT()$nested_dta %>% filter(!is.na(ttip_id))})
      curr_info <- reactive({
        tippy_DT() %>%
          pmap( ~ {
            input[[rlang::dots_list(...)$ttip_id]]
          })
      })

      observeEvent(
        curr_info(), {
          if (!isTruthy(last_info())) {
            curr_info() %>% last_info()
          }
        }, ignoreInit = TRUE, ignoreNULL = TRUE)

      observeEvent(
        curr_info(),
        {
          req(last_info())
          req(all(isTruthy(unlist(last_info()))))
          pwalk(
            list(
              last_info(),
              curr_info(),
              id = tippy_DT()$ttip_id,
              descr = tippy_DT()$var_description,
              var_name = tippy_DT()$ttip_var_name
            ),
            ~ {
              if (..1 != ..2) {
                shiny::showModal(shiny::modalDialog(
                  HTML(..4),
                  title = HTML(..5),
                  size = "m",
                  easyClose = TRUE,
                  fade = TRUE
                ))
              }
            }
          )
          curr_info() %>% last_info()
        })
    })
}
