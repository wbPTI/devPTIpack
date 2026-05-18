# Tier-1 tests for the DT-input construction helpers (arch-03 §1.11).
# Source: R/mod_DT_inputs.R. Permanent functions per arch-01.
#
# Functions under test (all internal — accessed via :::):
#   - prep_input_data(ind_list, ns)
#   - make_vis_targets_for_dt(nested_dta)
#   - make_input_DT(ind_list, ns, ...)
#
# These functions assemble a Shiny-ready editable DT widget from an
# indicators_list. They use unnamespaced shiny / DT calls
# (e.g. `actionLink`, `numericInput`, `datatable`, `formatStyle`),
# so the test file attaches `shiny` for the duration.

suppressPackageStartupMessages(library(shiny))

# Convenience identity ns for tests (Shiny modules pass a real ns()).
.ns <- function(x) paste0("test-", x)

# ---------------------------------------------------------------------------
# prep_input_data
# ---------------------------------------------------------------------------

test_that("prep_input_data: returns a tibble with the expected columns", {
  out <- devPTIpack:::prep_input_data(test_indicators, .ns)
  expect_s3_class(out, "tbl_df")
  expect_true(all(c(
    "var_name", "var_code", "var_description", "type", "ui"
  ) %in% names(out)))
})

test_that("prep_input_data: emits 1 pillar row + 1 row per indicator", {
  # arch-03 §1.11 case "Row count = indicator count" is incorrect for
  # the current implementation: pillar headers are interleaved with
  # variable rows. Pin the actual: nrow = #pillars + #indicators.
  out <- devPTIpack:::prep_input_data(test_indicators, .ns)
  n_pillars    <- length(unique(test_indicators$pillar_name))
  n_indicators <- nrow(test_indicators)
  expect_equal(nrow(out), n_pillars + n_indicators)
  expect_setequal(unique(out$type), c("pillar", "variable"))
  expect_equal(sum(out$type == "variable"), n_indicators)
})

test_that("prep_input_data: variable rows carry numericInput HTML in `ui`", {
  out <- devPTIpack:::prep_input_data(test_indicators, .ns)
  vars <- out[out$type == "variable", ]
  # The numericInput renders an <input ... type="number" ...> tag.
  expect_true(all(grepl('type="number"', vars$ui, fixed = TRUE)))
  expect_true(all(grepl("shiny-input-number", vars$ui, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# make_vis_targets_for_dt
# ---------------------------------------------------------------------------

test_that("make_vis_targets_for_dt: returns columnDefs and colnames", {
  nested <- devPTIpack:::prep_input_data(test_indicators, .ns)
  vis <- devPTIpack:::make_vis_targets_for_dt(nested)
  expect_named(vis, c("columnDefs", "colnames"))
  expect_type(vis$columnDefs, "list")
  expect_equal(length(vis$colnames), ncol(nested))
})

test_that("make_vis_targets_for_dt: marks var_name + ui as visible", {
  # Every visible target def must reference a column position whose
  # source name is var_name or ui. Invisible targets cover the rest.
  nested <- devPTIpack:::prep_input_data(test_indicators, .ns)
  vis <- devPTIpack:::make_vis_targets_for_dt(nested)

  visible_positions <- unlist(lapply(
    vis$columnDefs[-1], function(cd) cd$targets
  ))
  visible_cols <- names(nested)[visible_positions + 1]
  expect_setequal(visible_cols, c("var_name", "ui"))

  invisible_positions <- vis$columnDefs[[1]]$targets
  invisible_cols <- names(nested)[invisible_positions + 1]
  expect_false(any(c("var_name", "ui") %in% invisible_cols))
})

# ---------------------------------------------------------------------------
# make_input_DT
# ---------------------------------------------------------------------------

test_that("make_input_DT: returns a list with dt_out + nested_dta", {
  out <- devPTIpack:::make_input_DT(test_indicators, .ns)
  expect_named(out, c("dt_out", "nested_dta"))
})

test_that("make_input_DT: dt_out is a DT htmlwidget", {
  out <- devPTIpack:::make_input_DT(test_indicators, .ns)
  expect_s3_class(out$dt_out, "datatables")
  expect_s3_class(out$dt_out, "htmlwidget")
})

test_that("make_input_DT: nested_dta is the prep_input_data tibble", {
  out <- devPTIpack:::make_input_DT(test_indicators, .ns)
  expect_s3_class(out$nested_dta, "tbl_df")
  expect_equal(
    out$nested_dta,
    devPTIpack:::prep_input_data(test_indicators, .ns)
  )
})
