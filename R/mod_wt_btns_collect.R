#' Set-zero / set-one button observers for weight inputs
#'
#' Internal helper used by `mod_DT_inputs_server`. For each pillar group
#' in `pti_indicators()`, watches a per-pillar trigger button and writes
#' `to_value` into every `numericInput` in that pillar via
#' `updateNumericInput`.
#'
#' @param id Shiny module namespace ID.
#' @param pti_indicators Reactive returning a tibble with `pillar_group`
#'   and `var_code` columns.
#' @param dtn_id Suffix appended to `"pillar_<group>"` to form the
#'   trigger button input ID (e.g. `"_set_zero"` or `"_set_one"`).
#' @param to_value Numeric value to write into each input when the
#'   trigger fires.
#'
#' @return Called for side effects (registers a Shiny observer).
#'
#' @importFrom shiny moduleServer reactive req reactiveVal observeEvent
#'   isTruthy updateNumericInput
#' @importFrom dplyr distinct group_by ungroup pull
#' @importFrom purrr transpose map map_lgl map2 walk
#' @importFrom stringr str_c
#' @noRd
mod_wt_btns_srv <- function(id, pti_indicators, dtn_id = "", to_value = 0) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns

      btn_groups <-
        reactive({
          req(pti_indicators())
          pti_indicators()%>%
            distinct(pillar_group,  var_code) %>%
            group_by(pillar_group) %>%
            tidyr::nest() %>%
            ungroup() %>%
            as.list() %>%
            transpose() %>%
            map(~{
              .x$input_codes <- pull(.x$data, var_code)
              .x$data <- NULL
              .x
            })
        })

      change_trigger <- reactive({
        req(btn_groups())
        out_trigers <-
          btn_groups() %>%
          map( ~ {
            .x$triger_id <- .x$pillar_group %>%
              str_c("pillar_", ., dtn_id)
            .x$triger_value <- input[[.x$triger_id]]
            .x
          })
        if (all(map_lgl(out_trigers, ~ !is.null(.x$triger_value))))
          return(out_trigers)
        else
          return(NULL)
      })

      pillars_zero_values <- reactiveVal(NULL)

      observeEvent(#
        change_trigger(), #
        {
          if (!isTruthy(pillars_zero_values())) {
            change_trigger() %>% pillars_zero_values()
          }

          if (isTruthy(pillars_zero_values())) {
            change_trigger() %>%
              map2(pillars_zero_values(), ~ {
                if (.x$triger_value > .y$triger_value) {
                  .y$triger_value <- .x$triger_value
                  .y$input_codes %>%
                    walk( ~ updateNumericInput(session, .x, value = to_value))
                }
                .y
              }) %>%
              pillars_zero_values()
          }
        }, ignoreInit = FALSE)

    })

}


#' Collect current weight values into a tibble
#'
#' Internal helper used by `mod_DT_inputs_server`. Polls each
#' `numericInput` named after a `var_code` in `pti_indicators()` and
#' returns a one-row-per-indicator tibble of the current values
#' (`NA_real_` if the input has not yet rendered).
#'
#' @param id Shiny module namespace ID.
#' @param pti_indicators Reactive returning a tibble with a `var_code`
#'   column.
#'
#' @return A reactive returning a tibble with columns `var_code` and
#'   `weight`, or `NULL` if `pti_indicators()` is not truthy.
#'
#' @importFrom shiny moduleServer reactive isTruthy
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @noRd
mod_collect_wt_srv <- function(id, pti_indicators) {
  moduleServer(#
    id,
    function(input, output, session) {
      ns <- session$ns
      reactive({
        if (isTruthy(pti_indicators())) {
          pti_indicators()$var_code %>%
            map_dfr(~ {
              tibble(var_code = .x,
                     weight =  ifelse(!isTruthy(input[[.x]]), NA_real_, input[[.x]]))
            }) %>%
            return()
        } else {
          return(NULL)
        }
      })
    })
}
