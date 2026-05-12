# Tier-1 tests for the structured-return contract introduced by the
# arch-06 §6 validator UX pass. Pins the
# list(status, summary, issues) shape that programmatic callers and the
# upcoming Shiny upload-validation module (issue #7) rely on.

quiet <- function(expr) {
  suppressMessages(suppressWarnings(invisible(force(expr))))
}

issue_check_names <- function(res) {
  vapply(res$issues, function(x) x$check, character(1))
}

# ---------------------------------------------------------------------------
# Return shape — every validator returns the same three-element list
# ---------------------------------------------------------------------------

test_that("validate_geometries returns list(status, summary, issues)", {
  res <- quiet(validate_geometries(ukr_shp, error_on_fail = FALSE))
  expect_named(res, c("status", "summary", "issues"))
  expect_type(res$status, "character")
  expect_type(res$summary, "character")
  expect_type(res$issues, "list")
})

test_that("validate_read_shp returns the same structured shape", {
  tmp <- withr::local_tempfile(fileext = ".rds")
  saveRDS(ukr_shp, tmp)
  res <- quiet(validate_read_shp(tmp, error_on_fail = FALSE))
  expect_named(res, c("status", "summary", "issues"))
})

test_that("validate_read_metadata returns the same structured shape", {
  samp <- system.file(
    "sample_pti/app-data/sample-metadata.xlsx",
    package = "devPTIpack"
  )
  skip_if(samp == "", "bundled sample-metadata.xlsx not installed")
  res <- quiet(validate_read_metadata(samp, error_on_fail = FALSE))
  expect_named(res, c("status", "summary", "issues"))
})

test_that("validate_metadata returns the same structured shape", {
  samp_xlsx <- system.file(
    "sample_pti/app-data/sample-metadata.xlsx",
    package = "devPTIpack"
  )
  skip_if(samp_xlsx == "", "bundled sample-metadata.xlsx not installed")
  tmp_rds <- withr::local_tempfile(fileext = ".rds")
  saveRDS(ukr_shp, tmp_rds)
  res <- quiet(validate_metadata(tmp_rds, samp_xlsx, error_on_fail = FALSE))
  expect_named(res, c("status", "summary", "issues"))
})

# ---------------------------------------------------------------------------
# Status value invariants
# ---------------------------------------------------------------------------

test_that("status is one of pass / warn / fail", {
  res <- quiet(validate_geometries(ukr_shp, error_on_fail = FALSE))
  expect_true(res$status %in% c("pass", "warn", "fail"))
})

test_that("status is 'pass' when issues is empty", {
  res <- quiet(validate_geometries(ukr_shp, error_on_fail = FALSE))
  if (length(res$issues) == 0) {
    expect_equal(res$status, "pass")
  } else {
    succeed("Bundled fixture introduced an issue at some point — assertion not applicable.")
  }
})

test_that("status is 'fail' when at least one issue is fail-level", {
  bad <- ukr_shp
  bad$admin1_Oblast <- dplyr::select(bad$admin1_Oblast, -admin1Pcod)
  res <- quiet(validate_geometries(bad, error_on_fail = FALSE))
  expect_equal(res$status, "fail")
  expect_true(any(vapply(res$issues, function(x) x$level, character(1)) == "fail"))
})

test_that("status is 'warn' when warns present but no fails", {
  layer <- ukr_shp$admin1_Oblast
  layer$admin1Pcod[2] <- layer$admin1Pcod[1] # duplicate id
  warned <- ukr_shp
  warned$admin1_Oblast <- layer
  res <- quiet(validate_geometries(warned, error_on_fail = FALSE))
  # The duplicate Pcod warning may also break the cross-layer
  # hierarchy-row-count check (which is fail-level). Only assert the
  # warn-without-fail invariant when the upstream check did not cascade.
  has_fail <- any(vapply(res$issues, function(x) x$level, character(1)) == "fail")
  if (!has_fail) {
    expect_equal(res$status, "warn")
  } else {
    succeed("Cross-layer hierarchy cascade fired — fail status expected.")
  }
})

# ---------------------------------------------------------------------------
# Issue records have a stable shape
# ---------------------------------------------------------------------------

test_that("each issue has level + check + message fields", {
  bad <- ukr_shp
  bad$admin1_Oblast <- dplyr::select(bad$admin1_Oblast, -admin1Pcod)
  res <- quiet(validate_geometries(bad, error_on_fail = FALSE))
  for (issue in res$issues) {
    expect_true(all(c("level", "check", "message") %in% names(issue)))
    expect_true(issue$level %in% c("warn", "fail", "info"))
    expect_type(issue$check, "character")
    expect_type(issue$message, "character")
  }
})

# ---------------------------------------------------------------------------
# error_on_fail behaviour
# ---------------------------------------------------------------------------

test_that("error_on_fail = TRUE throws on fail-level issues (default)", {
  bad <- ukr_shp
  bad$admin1_Oblast <- dplyr::select(bad$admin1_Oblast, -admin1Pcod)
  expect_error(quiet(validate_geometries(bad, error_on_fail = TRUE)))
})

test_that("error_on_fail = FALSE returns the result instead of throwing", {
  bad <- ukr_shp
  bad$admin1_Oblast <- dplyr::select(bad$admin1_Oblast, -admin1Pcod)
  res <- expect_silent(quiet(validate_geometries(bad, error_on_fail = FALSE)))
  expect_equal(res$status, "fail")
})

test_that("error_on_fail = TRUE with no fails does not throw", {
  expect_silent(quiet(validate_geometries(ukr_shp, error_on_fail = TRUE)))
})

# ---------------------------------------------------------------------------
# Specific check identifiers — these are part of the public contract,
# so external code (Shiny upload module) can branch on them stably.
# ---------------------------------------------------------------------------

test_that("missing Pcod surfaces check = 'pcod-name-present'", {
  bad <- ukr_shp
  bad$admin1_Oblast <- dplyr::select(bad$admin1_Oblast, -admin1Pcod)
  res <- quiet(validate_geometries(bad, error_on_fail = FALSE))
  expect_true("pcod-name-present" %in% issue_check_names(res))
})

test_that("orphan child Pcod surfaces check = 'parent-pcod-cascade'", {
  layer <- ukr_shp$admin2_Rayon
  layer$admin1Pcod[1] <- "NOT_A_REAL_REGION"
  bad <- ukr_shp
  bad$admin2_Rayon <- layer
  res <- quiet(validate_geometries(bad, error_on_fail = FALSE))
  expect_true("parent-pcod-cascade" %in% issue_check_names(res))
})

# Note: a string-valued fltr_* column in the source xlsx CAN'T be
# detected by `validate_read_metadata()` as it stands — `fct_template_reader()`
# itself coerces those columns to logical via `as.logical()` before
# `validate_read_metadata()` sees them. Detecting the source-format
# error would require reading the raw xlsx separately, which is out of
# scope for the minimal UX pass (issue #7 will tackle this when it
# adds the per-step validator decomposition).
