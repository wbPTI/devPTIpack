# Tier-1 smoke tests for app_validate_shp() — issue #80.
#
# `app_validate_shp()` is a thin standalone-launcher that wraps an
# internal UI/server pair and calls `shiny::shinyApp()`. The bulk of the
# behaviour is leaflet rendering inside the server, which lives in Tier 3
# (manual / shinytest2 — arch-03). The Tier-1 surface here:
#
#   1) Construction smoke test — `app_validate_shp(ukr_shp)` returns a
#      `shiny.appobj` without error.
#   2) Defensive input validation — sensible errors for missing / NULL /
#      empty / unnamed / non-list inputs.
#   3) Render-on-fail contract — when `validate_geometries()` reports
#      `status = "fail"` for the supplied shapes, the launcher still
#      returns a `shiny.appobj` so the deployer can inspect the broken
#      shapes visually instead of getting a console error.
#
# The internal `app_validate_shp_server()` is exercised end-to-end via
# `shiny::testServer()` in two short Tier-2 blocks to confirm the
# leaflet map + the validation summary text are rendered.

suppressPackageStartupMessages(library(shiny))

# ---------------------------------------------------------------------------
# 1) Construction smoke

test_that("app_validate_shp(ukr_shp) returns a shiny.appobj", {
  app <- app_validate_shp(ukr_shp)
  expect_s3_class(app, "shiny.appobj")
})

# ---------------------------------------------------------------------------
# 2) Defensive input validation

test_that("app_validate_shp errors when 'shp' is missing", {
  expect_error(app_validate_shp(), regexp = "shp")
})

test_that("app_validate_shp errors when 'shp' is NULL", {
  expect_error(app_validate_shp(NULL), regexp = "shp")
})

test_that("app_validate_shp errors when 'shp' is not a list", {
  expect_error(app_validate_shp("not a list"), regexp = "list")
})

test_that("app_validate_shp errors when 'shp' is an empty list", {
  expect_error(app_validate_shp(list()), regexp = "empty|named")
})

test_that("app_validate_shp errors when 'shp' is unnamed", {
  expect_error(app_validate_shp(unname(ukr_shp)), regexp = "named")
})

# ---------------------------------------------------------------------------
# 3) Render-on-fail — broken input still returns a shiny.appobj

test_that("app_validate_shp renders even when validate_geometries() fails", {
  # Strip a required column from one layer to force a validation failure.
  broken <- ukr_shp
  broken$admin1_Oblast$admin1Pcod <- NULL

  # Sanity check: validate_geometries does report fail on this input.
  diag <- validate_geometries(broken, error_on_fail = FALSE)
  expect_equal(diag$status, "fail")

  # The launcher must still produce a usable app object.
  app <- app_validate_shp(broken)
  expect_s3_class(app, "shiny.appobj")
})

# ---------------------------------------------------------------------------
# 4) Tier-2: server renders a leaflet map and the validation summary

test_that("app_validate_shp_server: renders leaflet map and summary text", {
  testServer(app_validate_shp_server, args = list(shp = ukr_shp), expr = {
    session$flushReact()
    expect_false(is.null(output$map))             # cheap non-null guard
    expect_s3_class(output$map, "json")           # leaflet output is JSON-encoded
    expect_true(output$validation_status %in% c("pass", "warn", "fail"))
    expect_match(output$validation_summary, "layer|admin")
  })
})

test_that("app_validate_shp_server: surfaces fail status for broken input", {
  broken <- ukr_shp
  broken$admin1_Oblast$admin1Pcod <- NULL
  testServer(app_validate_shp_server, args = list(shp = broken), expr = {
    session$flushReact()
    expect_equal(output$validation_status, "fail")
  })
})
