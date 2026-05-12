# Tier-1 smoke + Tier-2 server tests for app_validate_metadata() — issue
# #81. The launcher mirrors `app_validate_shp()` but exercises both
# validators (geometries + metadata) and embeds the existing
# `mod_dta_explorer2_*` module so the deployer can visually inspect
# indicator values on a map alongside the structured validation report.

suppressPackageStartupMessages(library(shiny))

# ---------------------------------------------------------------------------
# 1) Construction smoke

test_that("app_validate_metadata(ukr_shp, ukr_mtdt_full) returns a shiny.appobj", {
  app <- app_validate_metadata(ukr_shp, ukr_mtdt_full)
  expect_s3_class(app, "shiny.appobj")
})

# ---------------------------------------------------------------------------
# 2) Defensive input validation

test_that("app_validate_metadata errors when 'shp_dta' is missing", {
  expect_error(app_validate_metadata(inp_dta = ukr_mtdt_full), regexp = "shp_dta")
})

test_that("app_validate_metadata errors when 'inp_dta' is missing", {
  expect_error(app_validate_metadata(shp_dta = ukr_shp), regexp = "inp_dta")
})

test_that("app_validate_metadata errors when 'shp_dta' is NULL", {
  expect_error(app_validate_metadata(NULL, ukr_mtdt_full), regexp = "shp_dta")
})

test_that("app_validate_metadata errors when 'inp_dta' is NULL", {
  expect_error(app_validate_metadata(ukr_shp, NULL), regexp = "inp_dta")
})

test_that("app_validate_metadata errors when inputs are unnamed lists", {
  expect_error(app_validate_metadata(unname(ukr_shp), ukr_mtdt_full), regexp = "named")
  expect_error(app_validate_metadata(ukr_shp, unname(ukr_mtdt_full)), regexp = "named")
})

# ---------------------------------------------------------------------------
# 3) Render-on-fail — broken input still returns a shiny.appobj

test_that("app_validate_metadata renders even when validators fail", {
  # Strip a key to break the geometry validator; the metadata validator
  # then also fails (cross-references the missing P-code).
  broken_shp <- ukr_shp
  broken_shp$admin1_Oblast$admin1Pcod <- NULL

  diag <- validate_geometries(broken_shp, error_on_fail = FALSE)
  expect_equal(diag$status, "fail")

  app <- app_validate_metadata(broken_shp, ukr_mtdt_full)
  expect_s3_class(app, "shiny.appobj")
})

# ---------------------------------------------------------------------------
# 4) Tier-2: server surfaces both validation statuses

test_that("app_validate_metadata_server: renders both validation statuses", {
  # On the bundled fixtures both validators are deterministic; assert
  # the exact value rather than just set membership so a regression in
  # the Ukrainian sample data is caught sharply.
  testServer(
    app_validate_metadata_server,
    args = list(shp_dta = ukr_shp, inp_dta = ukr_mtdt_full),
    expr = {
      session$flushReact()
      expect_equal(output$geometries_status, "pass")
      expect_equal(output$metadata_status,   "pass")
      expect_match(output$summary, "Layers|Polygons|Indicators")
    }
  )
})

test_that("app_validate_metadata_server: surfaces fail status for broken shapes", {
  broken_shp <- ukr_shp
  broken_shp$admin1_Oblast$admin1Pcod <- NULL
  testServer(
    app_validate_metadata_server,
    args = list(shp_dta = broken_shp, inp_dta = ukr_mtdt_full),
    expr = {
      session$flushReact()
      expect_equal(output$geometries_status, "fail")
    }
  )
})
