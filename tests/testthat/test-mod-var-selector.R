# Tier-2 module-server tests for mod_fltr_sel_var2_srv (arch-03 §2.5).
# Source: R/mod_dta_explorer2.R. Permanent function per arch-01.
#
# This module wires the data-explorer's variable-selector picker:
# - on `choices()` change -> repopulate the picker (`updatePickerInput`),
# - on `first_open()` truthy edge -> auto-select the first available
#   variable,
# - on `add_selected()` -> override the current selection, and
# - returns an eventReactive that filters `preplot_dta()` via
#   `filter_var_explorer()` whenever `selected_var()` (the
#   500ms-debounced `input$indicators`) emits.
#
# Mock-session caveats:
#   1. `shinyWidgets::updatePickerInput()` does not round-trip into
#      `input$indicators` under `MockShinySession`. The picker-update
#      tests intercept it via `testthat::local_mocked_bindings(.package
#      = "shinyWidgets")` and inspect the captured args; the
#      "selection produces filtered data" test drives `input$indicators`
#      directly via `session$setInputs()`.
#   2. The returned eventReactive has not fired until a selection is
#      set, so calling `session$getReturned()()` before that errors
#      (eventReactive default `ignoreNULL = TRUE`). Tests skip the
#      pre-selection read.

suppressPackageStartupMessages(library(shiny))

# Synthetic minimal inputs. `choices` mirrors `get_var_choices()`'s
# output shape: outer = pillar, inner = named character (display name
# -> var_code). `preplot_dta` mirrors `reshaped_explorer_dta()`'s
# output shape: list of admin-keyed slots, each with `$pti_codes`.
.build_var_inputs <- function() {
  list(
    choices = list(
      "Pillar A" = c("Var Name 1" = "var_1", "Var Name 2" = "var_2"),
      "Pillar B" = c("Var Name 3" = "var_3")
    ),
    preplot = list(
      admin1 = list(pti_codes = c(pti_score = "var_1"), pti_data = "stub_a1"),
      admin2 = list(pti_codes = c(pti_score = "var_2"), pti_data = "stub_a2"),
      admin3 = list(pti_codes = c(pti_score = "var_3"), pti_data = "stub_a3")
    )
  )
}

# ---------------------------------------------------------------------------
# Initial choices populated
# ---------------------------------------------------------------------------

test_that("mod_fltr_sel_var2_srv: choices() change -> updatePickerInput with display-name choices", {
  inp <- .build_var_inputs()
  upd_calls <- list()
  local_mocked_bindings(
    updatePickerInput = function(...) {
      upd_calls[[length(upd_calls) + 1L]] <<- list(...)
      invisible(NULL)
    },
    .package = "shinyWidgets"
  )

  testServer(
    mod_fltr_sel_var2_srv,
    args = list(
      preplot_dta = reactive(inp$preplot),
      choices     = reactive(inp$choices),
      first_open  = reactive(NULL)
    ),
    expr = { session$flushReact() }
  )

  # The choices observer fires once on initial flushReact.
  expect_gte(length(upd_calls), 1L)
  init <- upd_calls[[1]]
  expect_equal(init$inputId, "indicators")
  # Choices arg = list keyed by pillar, values = display names of vars.
  expect_named(init$choices, c("Pillar A", "Pillar B"))
  expect_equal(init$choices[["Pillar A"]], c("Var Name 1", "Var Name 2"))
  expect_equal(init$choices[["Pillar B"]], "Var Name 3")
  # On initial fire, no selection.
  expect_null(init$selected)
})

# ---------------------------------------------------------------------------
# Selection -> filtered data
# ---------------------------------------------------------------------------

test_that("mod_fltr_sel_var2_srv: setting input$indicators filters preplot_dta", {
  inp <- .build_var_inputs()
  local_mocked_bindings(
    updatePickerInput = function(...) invisible(NULL),
    .package = "shinyWidgets"
  )

  testServer(
    mod_fltr_sel_var2_srv,
    args = list(
      preplot_dta = reactive(inp$preplot),
      choices     = reactive(inp$choices),
      first_open  = reactive(NULL)
    ),
    expr = {
      session$flushReact()
      session$setInputs(indicators = "var_1")
      # `selected_var` is `input$indicators` debounced 500ms.
      session$elapse(500); session$flushReact()
      out <- session$getReturned()()
      expect_named(out, "admin1")
      expect_equal(out$admin1$pti_data, "stub_a1")
    }
  )
})

test_that("mod_fltr_sel_var2_srv: changing the selection swaps which slots survive", {
  inp <- .build_var_inputs()
  local_mocked_bindings(
    updatePickerInput = function(...) invisible(NULL),
    .package = "shinyWidgets"
  )

  testServer(
    mod_fltr_sel_var2_srv,
    args = list(
      preplot_dta = reactive(inp$preplot),
      choices     = reactive(inp$choices),
      first_open  = reactive(NULL)
    ),
    expr = {
      session$flushReact()
      session$setInputs(indicators = "var_1")
      session$elapse(500); session$flushReact()
      first <- session$getReturned()()
      expect_named(first, "admin1")

      session$setInputs(indicators = "var_3")
      session$elapse(500); session$flushReact()
      second <- session$getReturned()()
      expect_named(second, "admin3")
    }
  )
})

# ---------------------------------------------------------------------------
# first_open auto-select
# ---------------------------------------------------------------------------

test_that("mod_fltr_sel_var2_srv: first_open(TRUE) -> updatePickerInput selects first display name", {
  inp <- .build_var_inputs()
  upd_calls <- list()
  local_mocked_bindings(
    updatePickerInput = function(...) {
      upd_calls[[length(upd_calls) + 1L]] <<- list(...)
      invisible(NULL)
    },
    .package = "shinyWidgets"
  )

  fo <- reactiveVal(NULL)
  testServer(
    mod_fltr_sel_var2_srv,
    args = list(
      preplot_dta = reactive(inp$preplot),
      choices     = reactive(inp$choices),
      first_open  = reactive(fo())
    ),
    expr = {
      session$flushReact()
      n_init <- length(upd_calls)
      fo(TRUE)
      session$flushReact()
      # An additional updatePickerInput call must have happened.
      expect_gt(length(upd_calls), n_init)
      latest <- upd_calls[[length(upd_calls)]]
      expect_equal(latest$inputId, "indicators")
      expect_equal(latest$selected, "Var Name 1")
    }
  )
})

# ---------------------------------------------------------------------------
# add_selected override
# ---------------------------------------------------------------------------
#
# The active code path uses `purrr::map_lgl(., ~ { .x %in% c(...) | .x
# %in% names(c(...)) })`. When any pillar has >1 variable, `.x` is a
# length-N character vector and `%in%` returns a length-N logical —
# violating `map_lgl`'s length-1 contract. So the happy-path test
# below uses single-var-per-pillar choices; the multi-var failure is
# pinned in a follow-up test as a discovered bug (PLAN.md §12).

test_that("mod_fltr_sel_var2_srv: add_selected() change triggers updatePickerInput (single-var pillars)", {
  # Each pillar holds exactly one variable, so the buggy map_lgl
  # below stays length-1 and the observer succeeds.
  choices_single <- list(
    "Pillar A" = c("Var Name 1" = "var_1"),
    "Pillar B" = c("Var Name 2" = "var_2")
  )
  preplot <- .build_var_inputs()$preplot

  upd_calls <- list()
  local_mocked_bindings(
    updatePickerInput = function(...) {
      upd_calls[[length(upd_calls) + 1L]] <<- list(...)
      invisible(NULL)
    },
    .package = "shinyWidgets"
  )

  add_sel <- reactiveVal(NULL)
  testServer(
    mod_fltr_sel_var2_srv,
    args = list(
      preplot_dta  = reactive(preplot),
      choices      = reactive(choices_single),
      first_open   = reactive(NULL),
      add_selected = reactive(add_sel())
    ),
    expr = {
      session$flushReact()
      n_init <- length(upd_calls)
      add_sel("var_2")
      session$flushReact()
      expect_gt(length(upd_calls), n_init)
      latest <- upd_calls[[length(upd_calls)]]
      expect_equal(latest$inputId, "indicators")
      # `selected` is `choices()` filtered to pillars whose vars match
      # the override — Pillar B contains var_2.
      expect_named(latest$selected, "Pillar B")
      expect_equal(unname(latest$selected[["Pillar B"]]), "var_2")
    }
  )
})

test_that("mod_fltr_sel_var2_srv: add_selected() with multi-var pillar fails the inner predicate (PINNED BUG)", {
  # Multi-var-per-pillar choices break the `purrr::map_lgl(., ~ {
  # .x %in% c(...) | .x %in% names(c(...)) })` predicate — `.x` is a
  # length-N character vector so each `%in%` returns a length-N
  # logical, violating map_lgl's length-1 contract. Pinned per arch-03
  # §"Known issues to pin"; PLAN.md §12.
  #
  # Shiny's observeEvent captures the error via its stack-trace
  # machinery and routes it through reactive log handlers — neither
  # `expect_error` nor `expect_warning` reliably catches it under
  # MockShinySession across shiny versions. We pin the underlying
  # predicate directly: it is the *exact* expression the broken
  # observer body evaluates.
  multi_pillar <- list(
    "Pillar A" = c("Var Name 1" = "var_1", "Var Name 2" = "var_2")
  )
  expect_error(
    purrr::map_lgl(multi_pillar, ~ {
      .x %in% c("var_2") | .x %in% names(c("var_2"))
    }),
    "length 1, not 2"
  )
})
