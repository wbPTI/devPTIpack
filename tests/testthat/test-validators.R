# Tier-1 tests for the geometry / metadata validators (arch-03 §1.4).
# Targets only permanent functions per .github/docs/arch-01-cleanup.md.
#
# Caveat: `validate_*` functions currently use `testthat::test_that()`
# *internally* for runtime validation (see issue #7). When they run
# inside our outer tests, their inner expectation failures bubble up
# as failures of our outer tests. `capture_validator()` absorbs the
# internal testthat-expectation-failures and exposes everything else
# (warnings, messages, errors) for assertion. The validators' final
# refactor (#7) will obsolete this helper.

capture_validator <- function(expr) {
  # Mock testthat::test_that to a no-op so the validator's *internal*
  # test_that blocks do not register expectation failures against the
  # outer test's reporter. Defer-scoped to the calling test_that.
  testthat::local_mocked_bindings(
    test_that = function(desc, code) invisible(),
    .package = "testthat"
  )
  warns <- character()
  msgs  <- character()
  err   <- NULL
  tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) {
      err <<- conditionMessage(e)
      NULL
    }
  )
  list(warnings = warns, messages = msgs, error = err)
}

# ---------------------------------------------------------------------------
# validate_single_geom(focus_geom, full_geom)
# ---------------------------------------------------------------------------

test_that("validate_single_geom: a valid layer produces no error", {
  out <- capture_validator(
    validate_single_geom(ukr_shp["admin1_Oblast"], ukr_shp)
  )
  expect_null(out$error)
})

test_that("validate_single_geom: missing Pcod column aborts via rlang", {
  bad <- list(admin1_Oblast = dplyr::select(
    ukr_shp$admin1_Oblast, -admin1Pcod
  ))
  out <- capture_validator(validate_single_geom(bad, bad))
  expect_match(out$error, "admin1Pcod")
})

test_that("validate_single_geom: duplicate Pcod values warn", {
  layer <- ukr_shp$admin1_Oblast
  layer$admin1Pcod[2] <- layer$admin1Pcod[1]
  bad <- list(admin1_Oblast = layer)
  out <- capture_validator(validate_single_geom(bad, ukr_shp))
  expect_true(any(grepl("unique identifiers", out$warnings)))
})

test_that("validate_single_geom: NA in the Pcod column warns", {
  layer <- ukr_shp$admin1_Oblast
  layer$admin1Pcod[1] <- NA_character_
  bad <- list(admin1_Oblast = layer)
  out <- capture_validator(validate_single_geom(bad, ukr_shp))
  expect_true(any(grepl("'NA'", out$warnings)))
})

test_that("validate_single_geom: orphan child Pcods warn", {
  # admin2 layer with one row whose admin1Pcod doesn't exist in admin1.
  layer <- ukr_shp$admin2_Rayon
  layer$admin1Pcod[1] <- "NOT_A_REAL_REGION"
  bad <- ukr_shp
  bad$admin2_Rayon <- layer
  out <- capture_validator(
    validate_single_geom(bad["admin2_Rayon"], bad)
  )
  expect_true(any(grepl("not present", out$warnings)))
})

test_that("validate_single_geom: root level (admin0) skips parent checks", {
  # admin0 has order 0, so the cross-level mapping branch is bypassed
  # entirely — the only signals should concern the layer's own structure.
  out <- capture_validator(
    validate_single_geom(ukr_shp["admin0_Country"], ukr_shp)
  )
  expect_null(out$error)
  expect_false(any(grepl("not present", out$warnings)))
})

# ---------------------------------------------------------------------------
# validate_geometries(existing_shapes)
# ---------------------------------------------------------------------------

test_that("validate_geometries: bundled ukr_shp does not abort", {
  out <- capture_validator(validate_geometries(ukr_shp))
  expect_null(out$error)
})

test_that("validate_geometries: layer with missing Pcod aborts", {
  bad <- ukr_shp
  bad$admin1_Oblast <- dplyr::select(bad$admin1_Oblast, -admin1Pcod)
  out <- capture_validator(validate_geometries(bad))
  expect_match(out$error, "admin1Pcod")
})

# ---------------------------------------------------------------------------
# validate_read_shp(shp_path)  +  validate_read_metadata(mtdt_path)
# ---------------------------------------------------------------------------

test_that("validate_read_shp: perfect shapefile passes round-trip", {
  # ukr_shp has no extra admin codes (every admin{N}Pcod has a matching
  # admin{N}_* slot). Pre PR #N this exercised a buggy str_detect on a
  # character(0) pattern inside the function's internal test_that block;
  # that error was swallowed by testthat::test_that and never surfaced
  # to the caller, but it left the inner expectation in a failed state.
  # The fix guards the diagnostic-label construction so the inner
  # expectation now passes cleanly. arch-01 still flags the broader
  # runtime-test_that refactor as a future cleanup target -- separate
  # PR.
  tmp <- withr::local_tempfile(fileext = ".rds")
  saveRDS(ukr_shp, tmp)
  out <- capture_validator(validate_read_shp(tmp))
  expect_null(out$error)
})

test_that("validate_read_metadata: bundled sample xlsx does not error", {
  samp <- system.file(
    "sample_pti/app-data/sample-metadata.xlsx",
    package = "devPTIpack"
  )
  skip_if(samp == "", "bundled sample-metadata.xlsx not installed")
  out <- capture_validator(validate_read_metadata(samp))
  expect_null(out$error)
})

# ---------------------------------------------------------------------------
# validate_metadata(shp_path, mtdt_path)
# ---------------------------------------------------------------------------

test_that("validate_metadata: bundled sample data passes end-to-end", {
  samp_xlsx <- system.file(
    "sample_pti/app-data/sample-metadata.xlsx",
    package = "devPTIpack"
  )
  skip_if(samp_xlsx == "", "bundled sample-metadata.xlsx not installed")

  tmp_rds <- withr::local_tempfile(fileext = ".rds")
  saveRDS(ukr_shp, tmp_rds)

  out <- capture_validator(validate_metadata(tmp_rds, samp_xlsx))
  expect_null(out$error)
})
