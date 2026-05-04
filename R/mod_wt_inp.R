#' Weights-input page UI
#'
#' Internal page-level UI for the weights editor. Composes the chosen
#' header layout (full vs short controls -- `full_wt_inp_ui` /
#' `short_wt_inp_ui`) on top of the per-indicator DT widget produced
#' by `mod_DT_inputs_ui`. Wraps everything in `step_5_modify_weights`
#' so the cicerone tour can target the table area.
#'
#' @param id Character. Shiny module namespace ID.
#' @param full_ui Logical. When `TRUE`, renders the full header
#'   (textInput + selectInput + save/delete + download/upload buttons
#'   stack); when `FALSE`, renders the short header (text input + save
#'   button + reset button + download links).
#' @param height Character. CSS height passed through to
#'   `mod_DT_inputs_ui`'s `dataTableOutput`.
#' @param dt_style Character. Additional CSS applied to the DT
#'   wrapper (commonly used to cap `max-height`).
#' @param wt_style Character or `NULL`. CSS applied to the outer
#'   weights-input wrapper div.
#' @param wt_dwnld_options Character vector. Subset of
#'   `c("data", "weights", "shapes", "metadata")` controlling which
#'   download links the short header emits. `NULL` or empty hides
#'   the download text.
#' @param ... Additional attributes forwarded to the outer wrapper
#'   div.
#'
#' @return A `shiny.tag` containing the assembled weights-input UI.
#'
#' @importFrom shiny NS tagList div
#' @noRd
#'
#' @examples
#' \dontrun{
#' column(
#'   5,
#'   mod_wt_inp_ui("input_tbl_1", dt_style = "max-height: calc(70vh);"),
#'   style = "padding-right: 0px; padding-left: 5px;"
#' )
#' }
mod_wt_inp_ui <- function(id,
                          full_ui = FALSE,
                          height = "inherit",
                          dt_style = "height: 450px;",
                          wt_style = NULL,
                          wt_dwnld_options = c("data", "weights", "shapes", "metadata"),
                          ...) {

  ns <- NS(id)

  if (full_ui) {
    controls_col <- full_wt_inp_ui(ns)
  } else {
    controls_col <- short_wt_inp_ui(ns, dwnld_options = wt_dwnld_options)
  }

  controls_col %>%
    div(style = "margin-bottom: 10px; width: 100%;") %>%
    tagList(
      div(id = "step_5_modify_weights",
          mod_DT_inputs_ui(ns(NULL), height, dt_style)
          )
      ) %>%
    div(., wt_style = wt_style, ...)
}

#' Diagnostic UI block for weight-input internals
#'
#' Internal helper that renders a `verbatimTextOutput` slot only when
#' the app is not running in golem production mode. Used to surface
#' the live state of `edited_ws`, `curr_wt`, and `selected_ws` to
#' developers debugging the weights editor.
#'
#' @param id Character. Shiny module namespace ID -- must match the
#'   ID passed to `mod_wt_inp_server`.
#'
#' @return A `shiny.tag` (or `NULL` in production mode) containing a
#'   `verbatimTextOutput` slot.
#'
#' @importFrom shiny NS verbatimTextOutput
#' @noRd
mod_wt_inp_test_ui <- function(id){
  ns <- NS(id)

  if (is.null(options("golem.app.prod")) || !isTRUE(options("golem.app.prod")[[1]]))
    verbatimTextOutput(ns("wt_tests_out"))
}


#' Full-controls header for the weights-input page
#'
#' Internal helper used by `mod_wt_inp_ui` when `full_ui = TRUE`.
#' Emits the wide header layout: PTI name input, existing-PTI
#' selector, save UI slot, delete button, download/upload buttons.
#' Each control is wrapped in a `step_*` div so the cicerone tour
#' can target it.
#'
#' @param ns Namespace function (typically the result of
#'   `NS(id)` inside the calling UI).
#'
#' @return A `shiny.tag.list` with the full-header controls.
#'
#' @importFrom shiny tagList textInput selectInput uiOutput
#'   actionButton downloadButton fileInput icon div
#' @noRd
full_wt_inp_ui <- function(ns) {

  tagList(
    textInput(
      ns("existing.weights.name"),
      label = "Name your PTI",
      placeholder = "Specify name for a weighting scheme",
      value = NULL,
      width = "100%"
    ) %>%
      div(id = "step_1_name"),

    tagList(
      selectInput(
        ns("existing.weights"),
        label = "Select existing PTI to modify",
        choices = NULL,
        selected = NULL,
        width = "100%"
      ) %>%
        shinyjs::hidden() %>%
        div(id = "step_2_select_existing")
      ,
      shiny::uiOutput(ns("save_btn")) %>%
        div(id = "step_3_save")
      ,
      {
        actionButton(
          ns("weights.delete"),
          "Delete PTI",
          icon = shiny::icon("remove", lib = "glyphicon"),
          class = "btn-danger",
          width = "100%"
        )
      } %>%
        div(id = "step_4_delete")
    ) %>%
      div(id = "step_234_controls1") %>%
      div(id = "step_234_controls2") %>%
      div(id = "step_234_controls3")
    ,
    tagList(
      downloadButton(
        ns("weights.download"),
        "Download PTIs",
        icon = shiny::icon("download"),
        class = "btn-primary",
        style = "width: 100%"
      ) %>%
        shinyjs::hidden(),

      downloadButton(
        ns("dwnld_data"),
        "Download PTI data",
        icon = shiny::icon("download"),
        class = "btn-primary",
        style = "width: 100%"
      ),

      fileInput(
        ns("weights_upload"),
        "Upload PTI",
        multiple = FALSE,
        accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        width = "100%",
        buttonLabel = "Browse...",
        placeholder = "No file selected"
      )
    ) %>%
      div(id = "step_5_downalod_upload1") %>%
      div(id = "step_5_downalod_upload2")
  )

}



#' Short-controls header for the weights-input page
#'
#' Internal helper used by `mod_wt_inp_ui` when `full_ui = FALSE`.
#' Emits the compact header: PTI name input, save UI slot, reset
#' button, and an inline "Download PTI <data>, <weights> ..." link
#' line composed from `mod_dwnld_dta_link_ui` calls. The download
#' line is suppressed entirely when `dwnld_options` is empty.
#'
#' @param ns Namespace function (typically the result of `NS(id)`
#'   inside the calling UI).
#' @param dwnld_options Character vector. Subset of
#'   `c("data", "weights", "shapes", "metadata")` controlling which
#'   download links are inlined. `NULL` or empty hides the line.
#'
#' @return A `shiny.tag.list` with the short-header controls.
#'
#' @importFrom shiny tagList textInput uiOutput tags div
#' @importFrom purrr pmap keep map2
#' @noRd
short_wt_inp_ui <- function(ns, dwnld_options = c("data", "weights", "shapes", "metadata")) {

  if (length(dwnld_options) > 0) {
    dwnld_text <-
      list(
        c("data", "weights", "shapes", "metadata"),
        c("dta.download", "weights.download", "shp.files", "mtdt.files"),
        c("data", "scores", "shapes", "metadata")
      ) %>%
      pmap( ~ {
        if (..1 %in% dwnld_options)
        {
          mod_dwnld_dta_link_ui(NULL, ns(..2), ..3, prefix = NULL, suffix = NULL)
        }
        else
          NULL
      }) %>%
      purrr::keep(function(x) !is.null(x))

    if (length(dwnld_text) >= 2) {
      dwnld_text <-
        c(rep(", ", max(length(dwnld_text) - 2, 0)), " and ", "") %>%
        map2(dwnld_text, ~ tagList(.y, .x))
    }

    dwnld_text <-
      dwnld_text %>%
      tagList("Download PTI ", ., ".") %>%
      tags$i() %>%
      tags$p(style = "font-size: 12px;",
             style = "text-align: right; margin: 0 0 0px !important;")
  } else {
    dwnld_text <- tagList()
  }

  tagList(

    textInput(
      ns("existing.weights.name"),
      label = "Name your PTI",
      placeholder = "Specify name for a weighting scheme",
      value = NULL,
      width = "100%"
    ) %>%
      div(id = "step_1_name"),

    tagList(
      shiny::uiOutput(ns("save_btn"), inline = TRUE) %>% tags$span(id = "step_3_save"),
      mod_wt_delete_ui(NULL, ns("weights.reset")) %>% tags$span(id = "step_4_delete")
    ) %>%
      div(id = "step_234_controls1") %>%
      div(id = "step_234_controls2") %>%
      div(id = "step_234_controls3"),

    tagList(
      dwnld_text
    ) %>%
      div(id = "step_5_downalod_upload1") %>%
      div(id = "step_5_downalod_upload2")
  )

}


#' Server orchestrating the weights-input page
#'
#' Internal page-level server that wires together the eight reactive
#' steps of the weights editor: extract indicators from `input_dta`,
#' render the DT widget (`mod_DT_inputs_server`), capture the current
#' weight-set name (`mod_wt_name_newsrv`), maintain a reactive
#' edited-store, save (`mod_wt_save_newsrv`), reset/delete
#' (`mod_wt_delete_newsrv`), select (`mod_wt_select_newsrv`), update
#' the inputs from a saved scheme (`mod_wt_upd_newsrv`), upload
#' (`mod_wt_uplod_newsrv`), and download (`mod_wt_dwnload_newsrv`).
#'
#' @param id Character. Shiny module namespace ID.
#' @param input_dta Reactive returning the input-data list as
#'   produced by `fct_template_reader()`.
#' @param plotted_dta Reactive returning the currently-plotted PTI
#'   reactive list (consumed by the download module to write the
#'   "scores" sheet). Defaults to `reactive(NULL)`.
#' @param shapes_path Character. Filesystem path to the shapes ZIP
#'   bundled with the deployment, served by the "shapes" download
#'   link.
#' @param mtdtpdf_path Character. Filesystem path to the metadata
#'   PDF, served by the "metadata" download link.
#'
#' @return A reactive returning the augmented `input_dta()` list with
#'   `weights_clean`, `indicators_list`, and `curr_wt` slots
#'   appended.
#'
#' @importFrom shiny moduleServer reactive reactiveValues observeEvent
#'   renderPrint
#' @noRd
mod_wt_inp_server <- function(id, input_dta, plotted_dta = reactive(NULL), shapes_path = "", mtdtpdf_path = "") {
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ind_list <- reactive({req(input_dta()) %>% get_indicators_list()})

    curr_wt <- mod_DT_inputs_server(NULL, ind_list, upd_wt)
    curr_wt_name <- mod_wt_name_newsrv(NULL, selected_ws)

    edited_ws <- reactiveValues(
      indicators_list = ind_list,
      weights_clean = reactive(NULL)
    )

    save_ws <- mod_wt_save_newsrv(NULL, edited_ws, curr_wt, curr_wt_name)
    observeEvent(save_ws(), {edited_ws$weights_clean <- save_ws})

    reset_ws <- mod_wt_delete_newsrv(NULL, edited_ws, curr_wt_name)
    observeEvent(reset_ws$invalidator(), {
      edited_ws$weights_clean <- reset_ws$value
    }, ignoreInit = TRUE)

    selected_ws <- mod_wt_select_newsrv(NULL, edited_ws, curr_wt_name)

    upd_wt <- mod_wt_upd_newsrv(NULL, edited_ws, curr_wt, selected_ws, reset_ws$invalidator)

    uploaded_ws <- mod_wt_uplod_newsrv(NULL, input_dta, ind_list)

    mod_wt_dwnload_newsrv(NULL, input_dta = input_dta, edited_ws = edited_ws,
                          plotted_dta = plotted_dta, filename_glue = "{.country}",
                          shapes_path = shapes_path, mtdtpdf_path = mtdtpdf_path)

    output$wt_tests_out <- renderPrint({
      list(
        edited_ws_weights_clean = edited_ws$weights_clean(),
        curr_wt_name = curr_wt_name(),
        curr_wt = curr_wt(),
        selected_ws = selected_ws()
      ) %>%
        str(max.level = 3)
    })

    reactive({
      input_dta() %>%
        append(
          list(
            weights_clean = edited_ws$weights_clean(),
            indicators_list = edited_ws$indicators_list(),
            curr_wt = curr_wt()
          )
        )
    })

  })
}


#' Sync the weight-set name input with the active selection
#'
#' Internal sub-module server. Mirrors `selected_ws()` into the
#' `existing.weights.name` text input (so picking a saved scheme
#' updates the visible name), and returns the user-edited name as a
#' 500ms-throttled reactive.
#'
#' @param id Character. Shiny module namespace ID.
#' @param selected_ws Reactive returning the currently-selected weight
#'   scheme name (or `NULL`).
#'
#' @return A throttled reactive returning the current value of
#'   `input$existing.weights.name`.
#'
#' @importFrom shiny moduleServer reactive observe isTruthy
#'   updateTextInput throttle
#' @noRd
mod_wt_name_newsrv <- function(id, selected_ws) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observe({
        if (isTruthy(selected_ws())) {
          updateTextInput(session, "existing.weights.name", value = selected_ws())
        } else {
          updateTextInput(session,
                          "existing.weights.name",
                          value = "",
                          placeholder = "Specify name for a weighting scheme")
        }
      })

      reactive(input$existing.weights.name) %>%
        shiny::throttle(millis = 500)
    })

}


#' Server for the save-weights button
#'
#' Internal sub-module server. Renders the save button into the
#' `save_btn` `uiOutput` slot, choosing one of five labels based on
#' the current state ("Modify weights" when all weights are zero;
#' "Provide a name" when the name is empty; "No changes to save"
#' when the current weights match an existing scheme; "Save changes
#' and plot PTI" when the current name overwrites a different
#' scheme; "Save and plot new PTI" when the name is novel). On click,
#' upserts the current weight tibble into `edited_ws$weights_clean`
#' under the current name and emits the resulting list.
#'
#' @param id Character. Shiny module namespace ID.
#' @param edited_ws `reactiveValues` carrying `weights_clean` (the
#'   live store of saved schemes) and `indicators_list`.
#' @param curr_wt Reactive returning the current (unsaved) weight
#'   tibble (`var_code`, `weight`).
#' @param curr_wt_name Reactive returning the user-entered scheme
#'   name.
#'
#' @return A `reactiveVal` that emits the upserted `weights_clean`
#'   list each time the save button is clicked.
#'
#' @importFrom shiny moduleServer reactiveVal renderUI observe
#'   observeEvent isTruthy actionButton icon
#' @noRd
mod_wt_save_newsrv <- function(id, edited_ws, curr_wt, curr_wt_name) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      return_ws <- reactiveVal()
      current_btn_ui <- reactiveVal()
      output$save_btn <- renderUI({current_btn_ui()})

      observeEvent(
        list(curr_wt_name(), curr_wt()),
        {
          cur_val <- curr_wt()$weight
          cur_val[is.na(cur_val)] <- 0
          if (all(cur_val == 0)) {
            actionButton(
              ns("weights.save"),
              "Modify weights",
              icon = shiny::icon("warning", lib = "glyphicon"),
              class = "btn-warning btn-xs",
              width = "63%"
            ) %>%
              shinyjs::disabled() %>%
              current_btn_ui()
          } else {

            if (!isTruthy(curr_wt_name())) {
              actionButton(
                ns("weights.save"),
                label = "Provide a name",
                icon = shiny::icon("warning", lib = "glyphicon"),
                class = "btn-warning btn-xs",
                width = "63%"
              ) %>%
                shinyjs::disabled() %>%
                current_btn_ui()
            } else {
              actionButton(
                ns("weights.save"),
                "Save and plot PTI",
                icon = shiny::icon("save"),
                class = "btn-success btn-xs",
                width = "63%"
              ) %>%
                current_btn_ui()
            }
          }
        },
        ignoreInit = FALSE,
        ignoreNULL = FALSE)


      observe({
        req(edited_ws$weights_clean())
        req(curr_wt())
        req(curr_wt_name())

        cur_val <- curr_wt()$weight
        cur_val[is.na(cur_val)] <- 0
        if (all(cur_val == 0)) {
          actionButton(
            ns("weights.save"),
            "Modify weights",
            icon = shiny::icon("warning", lib = "glyphicon"),
            class = "btn-warning btn-xs",
            width = "63%"
          ) %>%
            shinyjs::disabled() %>%
            current_btn_ui()
        } else {

          if (curr_wt_name() %in% names(edited_ws$weights_clean())) {
            existing_values <- edited_ws$weights_clean()[[curr_wt_name()]]

            if (isTRUE(all.equal(existing_values, curr_wt(), convert = TRUE))) {
              actionButton(
                ns("weights.save"),
                "No changes to save",
                icon = shiny::icon("warning", lib = "glyphicon"),
                class = "btn-info btn-xs",
                width = "63%"
              ) %>%
                shinyjs::disabled() %>%
                current_btn_ui()
            } else {
              actionButton(
                ns("weights.save"),
                "Save changes and plot PTI",
                icon = shiny::icon("save"),
                class = "btn-success btn-xs",
                width = "63%"
              ) %>%
                current_btn_ui()
            }
          } else {
            actionButton(
              ns("weights.save"),
              "Save and plot new PTI",
              icon = shiny::icon("save"),
              class = "btn-success btn-xs",
              width = "63%"
            ) %>%
              current_btn_ui()
          }
        }
      })


      observeEvent(
        input$weights.save,
        {
          out_wt <- edited_ws$weights_clean()
          out_wt[[curr_wt_name()]] <- curr_wt()
          out_wt %>% return_ws()
        })

      return_ws
    })
}


#' Reset/Delete weight-set button UI
#'
#' Internal helper that emits a single `actionButton` for the
#' weights-input page's reset/delete control. Called twice from
#' `full_wt_inp_ui` / `short_wt_inp_ui` (once with `inputId =
#' "weights.delete"` for the destructive-delete variant, once with
#' `"weights.reset"` for the soft-clear variant).
#'
#' @param id Character or `NULL`. Shiny module namespace ID; usually
#'   `NULL` since this helper is called from already-namespaced
#'   parents.
#' @param inputId Character. Input ID under which to register the
#'   button ("weights.reset" or "weights.delete").
#' @param label Character. Visible button label.
#'
#' @return A `shiny.tag` `actionButton`.
#'
#' @importFrom shiny NS actionButton icon
#' @noRd
mod_wt_delete_ui <- function(id = NULL,inputId = "weights.reset", label = "Reset PTI" ) {
  ns <- NS(id)
  actionButton(
    inputId = ns(inputId),
    label = label,
    icon = shiny::icon("remove", lib = "glyphicon"),
    class = "btn-danger btn-xs",
    width = "35%"
  )
}


#' Server for the reset/delete weight-set buttons
#'
#' Internal sub-module server that handles both `weights.delete`
#' (remove the named scheme from `weights_clean`) and `weights.reset`
#' (clear the entire store). Each click increments an
#' `invalidator` reactive (so listeners can trigger on either action
#' even when the resulting `value` is identical) and writes the new
#' store -- or `NULL` when empty -- to the `value` reactive. Also
#' enables/disables both buttons depending on whether the current
#' name matches an existing scheme.
#'
#' @param id Character. Shiny module namespace ID.
#' @param edited_ws `reactiveValues` with a `weights_clean` reactive.
#' @param curr_wt_name Reactive returning the current scheme name.
#'
#' @return A `reactiveValues` list with two reactives: `invalidator`
#'   (increments on each delete/reset) and `value` (the post-action
#'   `weights_clean` list, or `NULL`).
#'
#' @importFrom shiny moduleServer reactiveValues reactiveVal observe
#'   observeEvent isTruthy
#' @noRd
mod_wt_delete_newsrv <- function(id, edited_ws, curr_wt_name) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      return_ws <- reactiveValues(
        invalidator = reactiveVal(0),
        value = reactiveVal(NULL)
      )

      observe({
        if (!isTruthy(edited_ws$weights_clean())) {
          shinyjs::disable(id = "weights.delete")
          shinyjs::disable(id = "weights.reset")
        } else {
          shinyjs::enable(id = "weights.delete")
          shinyjs::enable(id = "weights.reset")
        }
      })

      observe({
        if (curr_wt_name() %in% names(edited_ws$weights_clean())) {
          shinyjs::enable(id = "weights.delete")
          shinyjs::enable(id = "weights.reset")
        } else {
          shinyjs::disable(id = "weights.delete")
          shinyjs::disable(id = "weights.reset")
        }
      })

      observeEvent(
        input$weights.delete,
        {
          out_wt <- edited_ws$weights_clean()
          out_wt[[curr_wt_name()]] <- NULL
          (return_ws$invalidator() + 1) %>% return_ws$invalidator()
          if (length(out_wt) == 0) {
            return_ws$value(NULL)
          } else {
            return_ws$value(out_wt)
          }
        })

      observeEvent(
        input$weights.reset,
        {
          (return_ws$invalidator() + 1) %>% return_ws$invalidator()
          return_ws$value(NULL)
        })

      return_ws
    })
}


#' Server for the existing-weights-set selector
#'
#' Internal sub-module server that drives the `existing.weights`
#' `selectInput`: hides it when the store is empty, shows it (with
#' updated choices) when the store has entries, and recovers a sane
#' fallback selection after a deletion that removed the active
#' scheme. Returns the current selection as a reactive.
#'
#' @param id Character. Shiny module namespace ID.
#' @param edited_ws `reactiveValues` with a `weights_clean` reactive.
#' @param curr_wt_name Reactive returning the current scheme name.
#'
#' @return A `reactiveVal` returning the currently-selected scheme
#'   name (or `NULL`).
#'
#' @importFrom shiny moduleServer reactiveVal observe isolate
#'   observeEvent isTruthy updateSelectInput
#' @noRd
mod_wt_select_newsrv <- function(id, edited_ws, curr_wt_name) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      past_sel <- reactiveVal(list(NULL))
      curr_sel <- reactiveVal(NULL)
      curr_choices <- reactiveVal(NULL)

      observe({
        input$existing.weights
        isolate({
          curr_sel(input$existing.weights)
        })
      })

      observe({
        curr_sel()
        isolate({
          past_sel() %>% prepend(list(curr_sel())) %>% past_sel()
        })
      })

      observeEvent(
        edited_ws$weights_clean(),
        {
          if (!isTruthy(edited_ws$weights_clean())) {
            shinyjs::hide(id = "existing.weights", anim = TRUE)
            curr_sel(NULL)
            curr_choices(NULL)
          } else {
            names(edited_ws$weights_clean()) %>% curr_choices()

            if (length(curr_choices()) >= 1) {
              shinyjs::show(id = "existing.weights", anim = TRUE)

              if (isTruthy(curr_wt_name()) &&
                  curr_wt_name() %in% curr_choices()) {
                curr_wt_name() %>% curr_sel()
                updateSelectInput(session, "existing.weights", choices = curr_choices(), selected = curr_sel())
              } else if (
                isTruthy(curr_wt_name()) &&
                !curr_wt_name() %in% curr_choices()
              ) {
                past_sel() %>%
                  unlist() %>%
                  `[`(. %in% names(edited_ws$weights_clean())) %>%
                  `[[`(1) %>%
                  curr_sel()
                updateSelectInput(session, "existing.weights", choices = curr_choices(), selected = curr_sel())

              }

            } else {
              curr_sel(NULL)
              shinyjs::hide(id = "existing.weights", anim = TRUE)
            }
          }
        }, ignoreNULL = FALSE)

      curr_sel
    })

}



#' Push the selected weight set's values into the input table
#'
#' Internal sub-module server. Watches the selected-scheme name and
#' the reset invalidator: when either fires, computes the target
#' weight tibble (the saved scheme's tibble, or an all-zero
#' var_code-only tibble after a reset) and emits it through the
#' returned reactive. `mod_DT_inputs_server` consumes this and calls
#' `updateNumericInput` per row.
#'
#' @param id Character. Shiny module namespace ID.
#' @param edited_ws `reactiveValues` with `weights_clean` and
#'   `indicators_list` reactives.
#' @param curr_wt Reactive returning the current weight tibble (used
#'   to skip a redundant push when the target equals the live state).
#' @param selected_wt_name Reactive returning the currently-selected
#'   scheme name.
#' @param reset_invalidator Reactive returned by
#'   `mod_wt_delete_newsrv` whose increment triggers an all-zero
#'   push.
#'
#' @return A `reactiveVal` returning the target weight tibble.
#'
#' @importFrom shiny moduleServer reactiveVal observeEvent isTruthy req
#' @importFrom dplyr select mutate
#' @noRd
mod_wt_upd_newsrv <- function(id, edited_ws, curr_wt, selected_wt_name, reset_invalidator) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      upd_to_reactive <- reactiveVal()

      observeEvent(
        selected_wt_name(),
        {
          if (!isTruthy(edited_ws$weights_clean())) {
            upd_to <-
              edited_ws$indicators_list() %>%
              select(var_code) %>%
              mutate(weight = 0L)
          } else {
            req(selected_wt_name() %in% names(edited_ws$weights_clean()))
            upd_to <- edited_ws$weights_clean()[[selected_wt_name()]]
          }
          req(!identical(upd_to, curr_wt()))
          upd_to %>% upd_to_reactive()
        }, ignoreInit = FALSE)

      observeEvent(
        reset_invalidator(),
        {
          upd_to <-
            edited_ws$indicators_list() %>%
            select(var_code) %>%
            mutate(weight = 0L)
          upd_to %>% upd_to_reactive()
        }, ignoreInit = TRUE, ignoreNULL = TRUE)

      upd_to_reactive

    })

}



#' Wire the four downloads on the weights-input page
#'
#' Internal sub-module server that mounts four downloads onto the
#' weights page: the input data xlsx (`dta.download`), the weights
#' xlsx (`weights.download`, includes inputs + weights + scores), the
#' shapes ZIP (`shp.files`), and the metadata PDF (`mtdt.files`).
#' All four pull their content from `prepare_export_data()`.
#'
#' @param id Character. Shiny module namespace ID.
#' @param input_dta Reactive returning the input-data list.
#' @param edited_ws `reactiveValues` with `weights_clean` and
#'   `indicators_list` reactives.
#' @param plotted_dta Reactive returning the currently-plotted PTI.
#' @param filename_glue Glue template for the filename stem; the
#'   `{.country}` placeholder is replaced with the first element of
#'   `input_dta()$general`.
#' @param shapes_path Character. Path served by the shapes download.
#' @param mtdtpdf_path Character. Path served by the metadata download.
#'
#' @return Called for side effects (registers download handlers).
#'
#' @importFrom shiny moduleServer reactive reactiveValues
#' @importFrom stringr str_c
#' @importFrom purrr flatten
#' @noRd
mod_wt_dwnload_newsrv <- function(id,
                                  input_dta = reactive(NULL),
                                  edited_ws = reactiveValues(weights_clean = reactive(NULL),
                                                             indicators_list = reactive(NULL)),
                                  plotted_dta = reactive(NULL),
                                  filename_glue = "{.country} weights.xlsx",
                                  shapes_path = NULL,
                                  mtdtpdf_path = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      exp_dta <-
        prepare_export_data(input_dta, edited_ws$weights_clean, edited_ws$indicators_list,
                            plotted_dta, filename_glue = filename_glue)

      mod_dwnld_dta_xlsx_server(NULL, "dta.download",
                                file_name = reactive(str_c("PTI data for ", exp_dta$file_name(), ".xlsx")),
                                dta_dwnld = reactive(list(exp_dta$exp_inputs()) %>% purrr::flatten()))

      mod_dwnld_dta_xlsx_server(NULL, "weights.download",
                                file_name = reactive(str_c("PTI weights for ", exp_dta$file_name(), ".xlsx")),
                                dta_dwnld = reactive(
                                  list(exp_dta$exp_inputs(), exp_dta$exp_wght(), exp_dta$exp_pltdta()) %>%
                                    purrr::flatten()
                                ))

      mod_dwnld_file_server(NULL, "mtdt.files", mtdtpdf_path)

      mod_dwnld_file_server(NULL, "shp.files", shapes_path)

    })
}


#' Build the reactive payload consumed by the weights-page downloads
#'
#' Internal helper called once from `mod_wt_dwnload_newsrv`. Wires
#' four `observeEvent` blocks that keep `exp_inputs` (input list
#' reshaped by `fct_inp_for_exp`), `exp_wght` (weights reshaped by
#' `fct_internal_wt_to_exp`), `exp_pltdta` (plotted data reshaped by
#' `get_pti_scores_export`), and `file_name` (glue-composed filename
#' stem) up to date with the corresponding upstream reactives.
#'
#' @param input_dta Reactive returning the input-data list.
#' @param weights_clean Reactive returning the saved-schemes list.
#' @param indicators_list Reactive returning the indicators tibble.
#' @param plotted_dta Reactive returning the currently-plotted PTI.
#' @param filename_glue Glue template for the filename stem (uses
#'   `{.country}`).
#'
#' @return A `reactiveValues` list with reactives `file_name`,
#'   `exp_inputs`, `exp_wght`, `exp_pltdta`, and `dta`.
#'
#' @importFrom shiny reactive reactiveValues reactiveVal observeEvent
#'   isTruthy
#' @importFrom glue glue
#' @importFrom stringr str_c
#' @noRd
prepare_export_data <-
  function(input_dta = reactive(NULL),
           weights_clean = reactive(NULL),
           indicators_list = reactive(NULL),
           plotted_dta = reactive(NULL),
           filename_glue = "{.country} weights.xlsx") {

    out_list <- reactiveValues(
      file_name = reactiveVal(NULL),
      exp_inputs = reactiveVal(NULL),
      exp_wght = reactiveVal(NULL),
      exp_pltdta = reactiveVal(NULL),
      dta = reactive(NULL)
    )

    observeEvent(
      input_dta(), {
        if (isTruthy(input_dta())) {
          input_dta() %>% fct_inp_for_exp() %>% out_list$exp_inputs()
        } else {
          out_list$exp_inputs(NULL)
        }
      })

    observeEvent(
      list(weights_clean(), indicators_list()),
      {
        if (isTruthy(weights_clean()) && isTruthy(indicators_list())) {
          weights_clean() %>% fct_internal_wt_to_exp(indicators_list()) %>% out_list$exp_wght() %>%
            str_c(., " ")
        } else {
          out_list$exp_wght(NULL)
        }
      })

    observeEvent(
      plotted_dta(),
      {
        if (isTruthy(plotted_dta())) {
          plotted_dta() %>%
            get_pti_scores_export() %>%
            out_list$exp_pltdta()
        } else {
          out_list$exp_pltdta(NULL)
        }
      })

    observeEvent(
      input_dta(),
      {
        if (isTruthy(input_dta()$general %>% unlist() %>% `[[`(1))) {
          .country <-
            input_dta()$general %>% unlist() %>% `[[`(1) %>% as.character()
        } else {
          .country <- ""
        }
        glue(filename_glue) %>% out_list$file_name()
      })

    out_list

  }



#' Stub server for uploading a weights workbook (TODO)
#'
#' Internal sub-module server reserved for the "Upload PTI" flow.
#' Currently a placeholder: the click handler body is fully
#' commented out pending the upload-validation rewrite, so the
#' module currently has no observable side effects beyond
#' registering the (no-op) `observeEvent` on `weights_upload`.
#'
#' @param id Character. Shiny module namespace ID.
#' @param imported_data Reactive returning the input-data list.
#' @param pti_indicators Reactive returning the indicators tibble.
#'
#' @return A reactive returning a `reactiveVal` (currently always
#'   empty); kept for signature compatibility with future
#'   re-enablement.
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent
#' @noRd
mod_wt_uplod_newsrv <- function(id, imported_data, pti_indicators) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      return_wt <- reactiveVal()
      observeEvent(
        input$weights_upload,
        {
        },
        ignoreInit = TRUE,
        ignoreNULL = FALSE)
      reactive({return_wt})
    })
}
