# Tier-1 tests for `get_indicators_list` (arch-03 §1.2).
# Source: R/dta_cleaners.R. Permanent function per arch-01.

test_that("get_indicators_list: normal extraction returns a tbl_df", {
  out <- get_indicators_list(ukr_mtdt_full)
  expect_s3_class(out, "tbl_df")
  expect_gt(nrow(out), 0)
})

test_that("get_indicators_list: column set matches the documented schema", {
  out <- get_indicators_list(ukr_mtdt_full)
  expect_true(all(c(
    "var_code", "var_name", "var_description", "var_order",
    "admin_levels_years", "pillar_name"
  ) %in% names(out)))
})

test_that("get_indicators_list: var_code values are unique characters", {
  out <- get_indicators_list(ukr_mtdt_full)
  expect_type(out$var_code, "character")
  expect_equal(length(unique(out$var_code)), nrow(out))
})

test_that("get_indicators_list: admin_levels_years is a list of tibbles", {
  out <- get_indicators_list(ukr_mtdt_full)
  expect_type(out$admin_levels_years, "list")
  for (x in out$admin_levels_years) {
    expect_s3_class(x, "tbl_df")
    expect_true(all(
      c("admin_level", "admin_level_name", "years") %in% names(x)
    ))
  }
})

test_that("get_indicators_list: drops rows whose fltr_exclude_pti is TRUE", {
  modified <- ukr_mtdt_full
  modified$metadata$fltr_exclude_pti[1:3] <- TRUE
  out_filtered <- get_indicators_list(modified)
  out_full     <- get_indicators_list(ukr_mtdt_full)
  expect_lt(nrow(out_filtered), nrow(out_full))
  expect_false(modified$metadata$var_code[1] %in% out_filtered$var_code)
})

test_that("get_indicators_list: respects an alternative fltr_var argument", {
  out_default <- get_indicators_list(ukr_mtdt_full)
  out_alt <- get_indicators_list(
    ukr_mtdt_full, fltr_var = "fltr_exclude_explorer"
  )
  expect_s3_class(out_alt, "tbl_df")
  expect_equal(nrow(out_alt), nrow(out_default))  # both flags FALSE in fixture
})

test_that("get_indicators_list: errors on a non-existent fltr_var (PINNED)", {
  # arch-03 §1.2 case "Non-existent filter column": the rlang/dplyr stack
  # raises an "object not found" error inside the filter() call. Pin it.
  expect_error(
    get_indicators_list(ukr_mtdt_full, fltr_var = "bogus"),
    regexp = "bogus"
  )
})
