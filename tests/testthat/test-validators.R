# Tier-1 tests for the geometry / metadata validators (arch-03 §1.4).
# Targets only permanent functions per .github/docs/arch-01-cleanup.md.
#
# After the validator UX pass (arch-06 §6), each validator emits cli
# alerts as it runs and returns invisible(list(status, summary, issues)).
# When `error_on_fail = FALSE`, the function never throws — callers
# branch on `result$status` instead. These tests use that escape hatch
# to inspect the structured result rather than tangling with thrown
# errors.

# Silence the cli alerts so test output stays focused on assertions.
quiet <- function(expr) {
  suppressMessages(suppressWarnings(invisible(force(expr))))
}

# ---------------------------------------------------------------------------
# validate_single_geom(focus_geom, full_geom)
# ---------------------------------------------------------------------------

test_that("validate_single_geom: a valid layer passes", {
  res <- quiet(devPTIpack:::validate_single_geom(
    ukr_shp["admin1_Oblast"], ukr_shp,
    error_on_fail = FALSE
  ))
  expect_equal(res$status, "pass")
  expect_length(res$issues, 0)
})

test_that("validate_single_geom: missing Pcod column produces a fail-level issue", {
  bad <- list(admin1_Oblast = dplyr::select(
    ukr_shp$admin1_Oblast, -admin1Pcod
  ))
  res <- quiet(devPTIpack:::validate_single_geom(
    bad, bad, error_on_fail = FALSE
  ))
  expect_equal(res$status, "fail")
  fail_issues <- Filter(function(x) x$level == "fail", res$issues)
  expect_true(any(vapply(fail_issues, function(x) x$check, character(1)) == "pcod-name-present"))
})

test_that("validate_single_geom: duplicate Pcod values produce a warn issue", {
  layer <- ukr_shp$admin1_Oblast
  layer$admin1Pcod[2] <- layer$admin1Pcod[1]
  bad <- list(admin1_Oblast = layer)
  res <- quiet(devPTIpack:::validate_single_geom(
    bad, ukr_shp, error_on_fail = FALSE
  ))
  expect_equal(res$status, "warn")
  expect_true(any(vapply(res$issues, function(x) x$check, character(1)) == "pcod-unique"))
})

test_that("validate_single_geom: NA in the Pcod column produces a warn issue", {
  layer <- ukr_shp$admin1_Oblast
  layer$admin1Pcod[1] <- NA_character_
  bad <- list(admin1_Oblast = layer)
  res <- quiet(devPTIpack:::validate_single_geom(
    bad, ukr_shp, error_on_fail = FALSE
  ))
  expect_equal(res$status, "warn")
  expect_true(any(vapply(res$issues, function(x) x$check, character(1)) == "pcod-na"))
})

test_that("validate_single_geom: orphan child Pcods produce a warn issue", {
  # admin2 layer with one row whose admin1Pcod doesn't exist in admin1.
  layer <- ukr_shp$admin2_Rayon
  layer$admin1Pcod[1] <- "NOT_A_REAL_REGION"
  bad <- ukr_shp
  bad$admin2_Rayon <- layer
  res <- quiet(devPTIpack:::validate_single_geom(
    bad["admin2_Rayon"], bad, error_on_fail = FALSE
  ))
  expect_equal(res$status, "warn")
  expect_true(any(vapply(res$issues, function(x) x$check, character(1)) == "parent-pcod-cascade"))
})

test_that("validate_single_geom: root level (admin0) skips parent checks", {
  res <- quiet(devPTIpack:::validate_single_geom(
    ukr_shp["admin0_Country"], ukr_shp, error_on_fail = FALSE
  ))
  expect_equal(res$status, "pass")
  expect_false(any(vapply(res$issues, function(x) x$check, character(1)) == "parent-pcod-cascade"))
})

# ---------------------------------------------------------------------------
# validate_geometries(existing_shapes)
# ---------------------------------------------------------------------------

test_that("validate_geometries: bundled ukr_shp passes", {
  res <- quiet(validate_geometries(ukr_shp, error_on_fail = FALSE))
  expect_equal(res$status, "pass")
})

test_that("validate_geometries: layer with missing Pcod fails", {
  bad <- ukr_shp
  bad$admin1_Oblast <- dplyr::select(bad$admin1_Oblast, -admin1Pcod)
  res <- quiet(validate_geometries(bad, error_on_fail = FALSE))
  expect_equal(res$status, "fail")
})

test_that("validate_geometries: error_on_fail=TRUE throws on a fail", {
  bad <- ukr_shp
  bad$admin1_Oblast <- dplyr::select(bad$admin1_Oblast, -admin1Pcod)
  expect_error(quiet(validate_geometries(bad, error_on_fail = TRUE)))
})

# ---------------------------------------------------------------------------
# validate_read_shp(shp_path)  +  validate_read_metadata(mtdt_path)
# ---------------------------------------------------------------------------

test_that("validate_read_shp: perfect shapefile passes round-trip", {
  tmp <- withr::local_tempfile(fileext = ".rds")
  saveRDS(ukr_shp, tmp)
  res <- quiet(validate_read_shp(tmp, error_on_fail = FALSE))
  expect_equal(res$status, "pass")
})

test_that("validate_read_shp: unreadable path produces fail issue", {
  res <- quiet(validate_read_shp(
    "/path/that/does/not/exist.rds",
    error_on_fail = FALSE
  ))
  expect_equal(res$status, "fail")
  expect_true(any(vapply(res$issues, function(x) x$check, character(1)) == "rds-readable"))
})

test_that("validate_read_metadata: bundled sample xlsx passes", {
  samp <- system.file(
    "sample_pti/app-data/sample-metadata.xlsx",
    package = "devPTIpack"
  )
  skip_if(samp == "", "bundled sample-metadata.xlsx not installed")
  res <- quiet(validate_read_metadata(samp, error_on_fail = FALSE))
  expect_equal(res$status, "pass")
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

  res <- quiet(validate_metadata(tmp_rds, samp_xlsx, error_on_fail = FALSE))
  expect_equal(res$status, "pass")
})
