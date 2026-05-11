# Tier-1 tests for gg_admin_list (arch-03 §1.X).
# Source: R/supporting-goe-prep.R. Permanent function per arch-01.
#
# Pre PR #70 the default `mt = zam_bounds_simple` referenced an object
# that did not exist anywhere in the package -- calling
# `gg_admin_list(dta, metadata)` without `mt` errored at runtime with
# `object 'zam_bounds_simple' not found`. The fix changes the default
# to `mt = NULL` and emits a helpful `stop()` instead.

test_that("gg_admin_list: omitting `mt` errors with a helpful message", {
  expect_error(
    gg_admin_list(dta = ukr_mtdt_full$admin1_Oblast),
    regexp = "mt.*required"
  )
})

test_that("gg_admin_list: returns one ggplot per indicator with bundled data", {
  maps <- gg_admin_list(
    dta      = ukr_mtdt_full$admin1_Oblast,
    mt       = ukr_shp,
    metadata = ukr_mtdt_full$metadata
  )
  expect_type(maps, "list")
  expect_gt(length(maps), 0L)
  for (g in maps) expect_s3_class(g, "ggplot")
})
