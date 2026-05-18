# Tier-2 module-server tests for mod_wt_save_newsrv (arch-03 §2.6).
# Source: R/mod_wt_inp.R. Permanent function per arch-01.
#
# This module owns the "save weighting scheme" button and the data
# pipeline that merges the current weights into the in-memory scheme
# storage. It exposes:
#   - the returned reactiveVal `return_ws` (the merged scheme list,
#     emitted on `input$weights.save`),
#   - an `output$save_btn` UI whose enabled/disabled state and label
#     reflect the curr_wt / curr_wt_name combo.
#
# arch-03 §2.6 lists "Delete scheme" alongside the save cases, but
# delete logic actually lives in `mod_wt_delete_newsrv` (a separate
# permanent function not on the 1g checklist). This file covers
# save-only; delete is out of scope for #10's seven-module Tier-2
# scope.
#
# UI-state assertions read the internal `current_btn_ui()`
# reactiveVal directly from inside testServer's `expr` (it shares
# scope with the module body). `session$getOutput("save_btn")`
# would also work but only after a renderUI tick, which is awkward
# to drive in MockShinySession.

suppressPackageStartupMessages(library(shiny))

# Fixture builder. `existing` is the scheme storage that lives in
# `edited_ws$weights_clean`; pass `list()` for an empty store.
.build_save_inputs <- function(existing = list(),
                               curr_wt = tibble::tibble(
                                 var_code = c("v1", "v2"),
                                 weight   = c(0.3, 0.7)
                               ),
                               curr_wt_name = "scheme_a") {
  ind <- tibble::tibble(var_code = c("v1", "v2"))
  ws_rv <- reactiveVal(existing)
  list(
    edited_ws = reactiveValues(
      weights_clean   = ws_rv,
      indicators_list = reactive(ind)
    ),
    curr_wt      = reactiveVal(curr_wt),
    curr_wt_name = reactiveVal(curr_wt_name)
  )
}

# ---------------------------------------------------------------------------
# Save action — returned reactiveVal
# ---------------------------------------------------------------------------

test_that("mod_wt_save_newsrv: clicking save with empty storage adds the new scheme", {
  inp <- .build_save_inputs(existing = list())
  testServer(
    mod_wt_save_newsrv,
    args = list(
      edited_ws    = inp$edited_ws,
      curr_wt      = inp$curr_wt,
      curr_wt_name = inp$curr_wt_name
    ),
    expr = {
      session$flushReact()
      expect_null(session$getReturned()())

      session$setInputs(weights.save = 1L)
      session$flushReact()
      out <- session$getReturned()()
      expect_named(out, "scheme_a")
      expect_equal(out$scheme_a, inp$curr_wt())
    }
  )
})

test_that("mod_wt_save_newsrv: save with same name overwrites existing scheme", {
  old_wt <- tibble::tibble(var_code = c("v1", "v2"), weight = c(1, 1))
  new_wt <- tibble::tibble(var_code = c("v1", "v2"), weight = c(0.3, 0.7))
  inp <- .build_save_inputs(
    existing     = list(scheme_a = old_wt),
    curr_wt      = new_wt,
    curr_wt_name = "scheme_a"
  )
  testServer(
    mod_wt_save_newsrv,
    args = list(
      edited_ws    = inp$edited_ws,
      curr_wt      = inp$curr_wt,
      curr_wt_name = inp$curr_wt_name
    ),
    expr = {
      session$flushReact()
      session$setInputs(weights.save = 1L)
      session$flushReact()
      out <- session$getReturned()()
      expect_equal(out$scheme_a, new_wt)
    }
  )
})

test_that("mod_wt_save_newsrv: save under new name preserves existing schemes", {
  scheme_a <- tibble::tibble(var_code = c("v1", "v2"), weight = c(1, 1))
  scheme_b <- tibble::tibble(var_code = c("v1", "v2"), weight = c(0.5, 0.5))
  inp <- .build_save_inputs(
    existing     = list(scheme_a = scheme_a),
    curr_wt      = scheme_b,
    curr_wt_name = "scheme_b"
  )
  testServer(
    mod_wt_save_newsrv,
    args = list(
      edited_ws    = inp$edited_ws,
      curr_wt      = inp$curr_wt,
      curr_wt_name = inp$curr_wt_name
    ),
    expr = {
      session$flushReact()
      session$setInputs(weights.save = 1L)
      session$flushReact()
      out <- session$getReturned()()
      expect_named(out, c("scheme_a", "scheme_b"))
      expect_equal(out$scheme_a, scheme_a)
      expect_equal(out$scheme_b, scheme_b)
    }
  )
})

# ---------------------------------------------------------------------------
# Button UI state — labels and enabled/disabled
# ---------------------------------------------------------------------------

test_that("mod_wt_save_newsrv: name + non-zero weights, new scheme -> 'Save and plot new PTI' (enabled)", {
  # Both observers fire. The second one has the final word: empty
  # storage means `curr_wt_name() %in% names(weights_clean())` is
  # FALSE, so the "Save and plot new PTI" branch wins. (The first
  # observer momentarily sets "Save and plot PTI" — the no-suffix
  # form — but the second overwrites it. `isTruthy(list())` is TRUE
  # in shiny, so `req(weights_clean())` doesn't halt on empty list.)
  inp <- .build_save_inputs(existing = list())
  testServer(
    mod_wt_save_newsrv,
    args = list(
      edited_ws    = inp$edited_ws,
      curr_wt      = inp$curr_wt,
      curr_wt_name = inp$curr_wt_name
    ),
    expr = {
      session$flushReact()
      btn <- as.character(current_btn_ui())
      expect_match(btn, "Save and plot new PTI")
      expect_false(grepl("disabled", btn))
    }
  )
})

test_that("mod_wt_save_newsrv: empty name -> 'Provide a name' (disabled)", {
  inp <- .build_save_inputs(
    existing     = list(),
    curr_wt_name = ""
  )
  testServer(
    mod_wt_save_newsrv,
    args = list(
      edited_ws    = inp$edited_ws,
      curr_wt      = inp$curr_wt,
      curr_wt_name = inp$curr_wt_name
    ),
    expr = {
      session$flushReact()
      btn <- as.character(current_btn_ui())
      expect_match(btn, "Provide a name")
      expect_true(grepl("disabled", btn))
    }
  )
})

test_that("mod_wt_save_newsrv: all-zero weights -> 'Modify weights' (disabled)", {
  zero_wt <- tibble::tibble(var_code = c("v1", "v2"), weight = c(0, 0))
  inp <- .build_save_inputs(
    existing = list(),
    curr_wt  = zero_wt
  )
  testServer(
    mod_wt_save_newsrv,
    args = list(
      edited_ws    = inp$edited_ws,
      curr_wt      = inp$curr_wt,
      curr_wt_name = inp$curr_wt_name
    ),
    expr = {
      session$flushReact()
      btn <- as.character(current_btn_ui())
      expect_match(btn, "Modify weights")
      expect_true(grepl("disabled", btn))
    }
  )
})

test_that("mod_wt_save_newsrv: name matches existing + identical weights -> 'No changes to save' (disabled)", {
  existing_wt <- tibble::tibble(var_code = c("v1", "v2"), weight = c(0.3, 0.7))
  inp <- .build_save_inputs(
    existing     = list(scheme_a = existing_wt),
    curr_wt      = existing_wt,
    curr_wt_name = "scheme_a"
  )
  testServer(
    mod_wt_save_newsrv,
    args = list(
      edited_ws    = inp$edited_ws,
      curr_wt      = inp$curr_wt,
      curr_wt_name = inp$curr_wt_name
    ),
    expr = {
      session$flushReact()
      btn <- as.character(current_btn_ui())
      expect_match(btn, "No changes to save")
      expect_true(grepl("disabled", btn))
    }
  )
})

test_that("mod_wt_save_newsrv: name matches existing + changed weights -> 'Save changes and plot PTI' (enabled)", {
  existing_wt <- tibble::tibble(var_code = c("v1", "v2"), weight = c(1, 1))
  new_wt      <- tibble::tibble(var_code = c("v1", "v2"), weight = c(0.5, 0.5))
  inp <- .build_save_inputs(
    existing     = list(scheme_a = existing_wt),
    curr_wt      = new_wt,
    curr_wt_name = "scheme_a"
  )
  testServer(
    mod_wt_save_newsrv,
    args = list(
      edited_ws    = inp$edited_ws,
      curr_wt      = inp$curr_wt,
      curr_wt_name = inp$curr_wt_name
    ),
    expr = {
      session$flushReact()
      btn <- as.character(current_btn_ui())
      expect_match(btn, "Save changes and plot PTI")
      expect_false(grepl("disabled", btn))
    }
  )
})
