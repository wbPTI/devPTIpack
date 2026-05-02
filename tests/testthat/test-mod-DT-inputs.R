# Tier-2 module-server tests for mod_DT_inputs_server (arch-03 §2.2).
# Source: R/mod_DT_inputs.R. Permanent function per arch-01.
#
# This module wires the DT-rendered numericInput widgets that capture
# the user's per-indicator weights, plus the per-pillar "All 0" / "All 1"
# action buttons that mass-update those numericInputs. The returned
# reactive `current_values()` is throttled at 500 ms.
#
# Mock-session caveat: `updateNumericInput()` calls inside the server
# (used by the all-0 / all-1 buttons and the `update_dta()` observer)
# send a client message but do NOT round-trip back into
# `input[[id]]` under `shiny::testServer()`. To verify the downstream
# effect on `current_values()`, the relevant tests mirror the update
# with `session$setInputs()` — the same effect a live client would
# produce after applying the update message.

suppressPackageStartupMessages(library(shiny))

# Helper — splice a named list of input values into setInputs without
# requiring rlang `!!!` from inside testServer's expr block.
.set_inputs <- function(session, named_list) {
  do.call(session$setInputs, named_list)
}

# ---------------------------------------------------------------------------
# Initial render
# ---------------------------------------------------------------------------

test_that("mod_DT_inputs_server: initial current_values has all expected var_code rows", {
  testServer(
    mod_DT_inputs_server,
    args = list(ind_list = reactive(test_indicators)),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      expect_s3_class(out, "tbl_df")
      expect_named(out, c("var_code", "weight"))
      expect_setequal(out$var_code, test_indicators$var_code)
      # No inputs set yet -> weights are all NA_real_.
      expect_true(all(is.na(out$weight)))
    }
  )
})

# ---------------------------------------------------------------------------
# User input -> current_values
# ---------------------------------------------------------------------------

test_that("mod_DT_inputs_server: direct numericInput changes flow to current_values", {
  testServer(
    mod_DT_inputs_server,
    args = list(ind_list = reactive(test_indicators)),
    expr = {
      session$flushReact()
      vc <- test_indicators$var_code
      .set_inputs(session, setNames(list(0.5, 0.25), vc[1:2]))
      session$elapse(500)
      session$flushReact()
      out <- session$getReturned()()
      expect_equal(out$weight[match(vc[1], out$var_code)], 0.5)
      expect_equal(out$weight[match(vc[2], out$var_code)], 0.25)
      expect_true(all(is.na(out$weight[!out$var_code %in% vc[1:2]])))
    }
  )
})

# ---------------------------------------------------------------------------
# All-zero / all-one button paths (round-trip simulated via setInputs)
# ---------------------------------------------------------------------------

test_that("mod_DT_inputs_server: all-zero state -> all weights == 0", {
  testServer(
    mod_DT_inputs_server,
    args = list(ind_list = reactive(test_indicators)),
    expr = {
      session$flushReact()
      vc <- test_indicators$var_code
      # The all-zero button (input id `pillar_<group>_set_zero`) drives
      # `mod_wt_btns_srv`, which calls updateNumericInput on each
      # var_code in the pillar with value=0. Trigger the button AND
      # mirror via setInputs to simulate the client roundtrip
      # (see top-of-file caveat).
      session$setInputs(pillar_1_set_zero = 1L)
      .set_inputs(session, as.list(setNames(rep(0, length(vc)), vc)))
      session$elapse(500)
      session$flushReact()
      out <- session$getReturned()()
      expect_setequal(out$var_code, vc)
      expect_true(all(out$weight == 0))
    }
  )
})

test_that("mod_DT_inputs_server: all-one state -> all weights == 1", {
  testServer(
    mod_DT_inputs_server,
    args = list(ind_list = reactive(test_indicators)),
    expr = {
      session$flushReact()
      vc <- test_indicators$var_code
      session$setInputs(pillar_1_set_one = 1L)
      .set_inputs(session, as.list(setNames(rep(1, length(vc)), vc)))
      session$elapse(500)
      session$flushReact()
      out <- session$getReturned()()
      expect_true(all(out$weight == 1))
    }
  )
})

# ---------------------------------------------------------------------------
# Update from outside (`update_dta()`)
# ---------------------------------------------------------------------------

test_that("mod_DT_inputs_server: update_dta() pushes new weights into current_values", {
  upd_rv <- reactiveVal(NULL)
  testServer(
    mod_DT_inputs_server,
    args = list(
      ind_list   = reactive(test_indicators),
      update_dta = reactive(upd_rv())
    ),
    expr = {
      session$flushReact()

      # Push a new weights tibble through update_dta. The internal
      # observer dispatches updateNumericInput() per row; we mirror via
      # setInputs() to simulate the client roundtrip.
      upd <- tibble::tibble(
        var_code = test_indicators$var_code,
        weight   = seq_len(nrow(test_indicators)) / 10
      )
      upd_rv(upd)
      session$flushReact()
      .set_inputs(session, as.list(setNames(upd$weight, upd$var_code)))
      session$elapse(500)
      session$flushReact()

      out <- session$getReturned()()
      expect_equal(
        out$weight[match(upd$var_code, out$var_code)],
        upd$weight
      )
    }
  )
})

# ---------------------------------------------------------------------------
# Throttle (500 ms)
# ---------------------------------------------------------------------------

test_that("mod_DT_inputs_server: rapid input changes throttle to a single update", {
  testServer(
    mod_DT_inputs_server,
    args = list(ind_list = reactive(test_indicators)),
    expr = {
      session$flushReact()
      vc1 <- test_indicators$var_code[[1]]

      # Three rapid changes within the 500 ms throttle window.
      # The leading-edge throttle fire already happened on initial
      # ind_list setting (returning all-NA), so the next emission is
      # gated by the trailing edge.
      .set_inputs(session, setNames(list(0.1), vc1))
      session$elapse(50); session$flushReact()
      .set_inputs(session, setNames(list(0.2), vc1))
      session$elapse(50); session$flushReact()
      .set_inputs(session, setNames(list(0.3), vc1))
      session$elapse(50); session$flushReact()

      # 150 ms total elapsed — within the throttle window, current_values
      # has not yet seen any of the new values.
      out_mid <- session$getReturned()()
      expect_true(is.na(out_mid$weight[match(vc1, out_mid$var_code)]))

      # Past the window, only the final value emerges.
      session$elapse(500); session$flushReact()
      out_after <- session$getReturned()()
      expect_equal(out_after$weight[match(vc1, out_after$var_code)], 0.3)
    }
  )
})
