# Tier-1 tests for explorer helpers (arch-03 §1.9).
# Source: R/mod_dta_explorer2.R. Permanent functions per arch-01.
#
# Functions under test:
#   - reshaped_explorer_dta(long_dta, ind_list)
#   - get_var_choices(indicators_list)
#   - filter_var_explorer(preplot_dta, vars)

# ---------------------------------------------------------------------------
# reshaped_explorer_dta
# ---------------------------------------------------------------------------

test_that("reshaped_explorer_dta: returns one tibble per admin level", {
  out <- reshaped_explorer_dta(test_pivoted, test_indicators)
  expect_type(out, "list")
  for (x in out) expect_s3_class(x, "tbl_df")
  # ukr_mtdt_full has admin1/2/4 sheets (no admin0 sheet to pivot).
  expect_setequal(names(out), c("admin1", "admin2", "admin4"))
})

test_that("reshaped_explorer_dta: tibbles use generic pti_* column names", {
  out <- reshaped_explorer_dta(test_pivoted, test_indicators)
  for (x in out) {
    expect_true(all(
      c("pti_score", "pti_name", "spatial_name", "pti_label") %in% names(x)
    ))
  }
})

test_that("reshaped_explorer_dta: pti_label is glue-class HTML", {
  out <- reshaped_explorer_dta(test_pivoted, test_indicators)
  expect_s3_class(out[[1]]$pti_label, "glue")
  expect_true(all(grepl("<strong>", out[[1]]$pti_label, fixed = TRUE)))
})

test_that("reshaped_explorer_dta: pti_name uses var_name (not var_code)", {
  out <- reshaped_explorer_dta(test_pivoted, test_indicators)
  declared <- test_indicators$var_name
  for (x in out) {
    expect_true(all(unique(x$pti_name) %in% declared))
  }
})

# ---------------------------------------------------------------------------
# get_var_choices
# ---------------------------------------------------------------------------

test_that("get_var_choices: groups choices by pillar (PINNED structure)", {
  # arch-03 §1.9 documents this as "Named vector (names = display,
  # values = var_code)" but the actual output is a *nested list* keyed
  # by pillar_name, with each element a named character vector
  # (var_name -> var_code). Pin the actual shape.
  out <- get_var_choices(test_indicators)
  expect_type(out, "list")
  expect_setequal(names(out), unique(test_indicators$pillar_name))
})

test_that("get_var_choices: each pillar entry maps var_name -> var_code", {
  out <- get_var_choices(test_indicators)
  for (group in out) {
    expect_type(group, "character")
    expect_setequal(names(group), test_indicators$var_name)
    expect_setequal(unname(group), test_indicators$var_code)
  }
})

test_that("get_var_choices: empty indicators tibble errors (PINNED)", {
  # PINNED BUG: when `out` ends up NULL (no pillars to group by), the
  # rescue block does `names(out) <- "Indicators"`, which errors with
  # 'attempt to set an attribute on NULL'. A length-0 list-typed
  # output would be more useful.
  empty <- test_indicators[0, ]
  expect_error(
    get_var_choices(empty),
    regexp = "attribute on NULL"
  )
})

# ---------------------------------------------------------------------------
# filter_var_explorer
# ---------------------------------------------------------------------------

test_that("filter_var_explorer: filters by pti_codes value", {
  preplot <- preplot_reshape_wghtd_dta(test_pipeline_out)
  out <- filter_var_explorer(preplot, "all_ones")
  expect_equal(length(out), 4L)  # 4 admin levels for the matching scheme
  for (x in out) expect_equal(unname(x$pti_codes), "all_ones")
})

test_that("filter_var_explorer: also matches via names(vars)", {
  # Predicate: `.x$pti_codes %in% vars | .x$pti_codes %in% names(vars)`.
  # So a vector named by the desired value (e.g. c(all_ones = "x"))
  # also matches via the names() branch.
  preplot <- preplot_reshape_wghtd_dta(test_pipeline_out)
  out <- filter_var_explorer(preplot, c(all_ones = "anything"))
  expect_equal(length(out), 4L)
})

test_that("filter_var_explorer: no matches -> empty list", {
  preplot <- preplot_reshape_wghtd_dta(test_pipeline_out)
  out <- filter_var_explorer(preplot, "bogus_scheme")
  expect_equal(length(out), 0L)
})

test_that("filter_var_explorer: empty input returns empty", {
  out <- filter_var_explorer(list(), "all_ones")
  expect_equal(length(out), 0L)
})
