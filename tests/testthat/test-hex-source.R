# Tier-1 tests for pti_hex_var() + pti_hex_source() (arch-11 §"Variable
# descriptors"). Both are internal S3 constructors; tested via `:::`.

# ---------------------------------------------------------------------------
# pti_hex_var()
# ---------------------------------------------------------------------------

test_that("pti_hex_var: constructs a valid descriptor with defaults", {
  v <- devPTIpack:::pti_hex_var(
    source_col     = "pop_flood",
    canonical_name = "flood_exposure",
    var_name       = "Flood Exposure"
  )
  expect_s3_class(v, "pti_hex_var")
  expect_identical(v$source_col, "pop_flood")
  expect_identical(v$canonical_name, "flood_exposure")
  expect_identical(v$var_name, "Flood Exposure")
  expect_identical(v$weight, "none")
  expect_identical(v$fun, "mean")
  expect_false(v$legend_revert_colours)
  expect_false(v$fltr_exclude_pti)
  expect_false(v$internal)
  expect_identical(v$years, integer(0))
})

test_that("pti_hex_var: errors on missing / empty mandatory fields", {
  expect_error(
    devPTIpack:::pti_hex_var(source_col = "x", canonical_name = "n",
                             var_name = ""),
    regexp = "var_name"
  )
  expect_error(
    devPTIpack:::pti_hex_var(source_col = "", canonical_name = "n",
                             var_name = "X"),
    regexp = "source_col"
  )
  expect_error(
    devPTIpack:::pti_hex_var(source_col = "x", canonical_name = NA,
                             var_name = "X"),
    regexp = "canonical_name"
  )
})

test_that("pti_hex_var: validates weight is in {none, pop, area}", {
  expect_error(
    devPTIpack:::pti_hex_var(source_col = "x", canonical_name = "n",
                             var_name = "X", weight = "bogus"),
    regexp = "none.*pop.*area|pop.*area|should be one of"
  )
})

test_that("pti_hex_var: validates fun is in {mean, median, sum, min, max}", {
  expect_error(
    devPTIpack:::pti_hex_var(source_col = "x", canonical_name = "n",
                             var_name = "X", fun = "geom_mean"),
    regexp = "mean.*median|median.*sum|should be one of"
  )
})

test_that("pti_hex_var: coerces NULL optional fields to NA", {
  v <- devPTIpack:::pti_hex_var(
    source_col      = "x",
    canonical_name  = "n",
    var_name        = "X",
    var_description = NULL,
    var_units       = NULL,
    pillar_group    = NULL,
    pillar_name     = NULL,
    time_col        = NULL
  )
  expect_true(is.na(v$var_description))
  expect_true(is.na(v$var_units))
  expect_true(is.na(v$pillar_group))
  expect_true(is.na(v$pillar_name))
  expect_true(is.na(v$time_col))
})

test_that("pti_hex_var: internal flag propagates", {
  v <- devPTIpack:::pti_hex_var(
    source_col = "x", canonical_name = "n", var_name = "X",
    internal = TRUE
  )
  expect_true(v$internal)
})

# ---------------------------------------------------------------------------
# pti_hex_source()
# ---------------------------------------------------------------------------

test_that("pti_hex_source: constructs a valid descriptor", {
  v <- devPTIpack:::pti_hex_var(
    source_col = "x", canonical_name = "n", var_name = "X"
  )
  s <- devPTIpack:::pti_hex_source(
    label   = "Test",
    path    = "https://example.org/data.parquet",
    hex_col = "hex_id",
    vars    = list(n = v)
  )
  expect_s3_class(s, "pti_hex_source")
  expect_identical(s$label, "Test")
  expect_identical(s$hex_col, "hex_id")
  expect_length(s$vars, 1L)
  expect_null(s$pop_var)
})

test_that("pti_hex_source: errors on non-pti_hex_var entries in vars", {
  expect_error(
    devPTIpack:::pti_hex_source(
      label   = "Test",
      path    = "https://example.org/x.parquet",
      hex_col = "hex_id",
      vars    = list(n = list("not", "a", "pti_hex_var"))
    ),
    regexp = "pti_hex_var"
  )
})

test_that("pti_hex_source: errors on missing / empty mandatory fields", {
  v <- devPTIpack:::pti_hex_var(
    source_col = "x", canonical_name = "n", var_name = "X"
  )
  expect_error(
    devPTIpack:::pti_hex_source(label = "", path = "https://x",
                                hex_col = "h", vars = list(n = v)),
    regexp = "label"
  )
  expect_error(
    devPTIpack:::pti_hex_source(label = "L", path = "",
                                hex_col = "h", vars = list(n = v)),
    regexp = "path"
  )
  expect_error(
    devPTIpack:::pti_hex_source(label = "L", path = "https://x",
                                hex_col = "", vars = list(n = v)),
    regexp = "hex_col"
  )
})

test_that("pti_hex_source: pop_var must be NULL or pti_hex_var", {
  v <- devPTIpack:::pti_hex_var(
    source_col = "x", canonical_name = "n", var_name = "X"
  )
  expect_error(
    devPTIpack:::pti_hex_source(
      label = "L", path = "https://x", hex_col = "h",
      vars = list(n = v),
      pop_var = list("not", "a", "var")
    ),
    regexp = "pop_var"
  )
})

test_that("pti_hex_source: tolerates empty vars list", {
  s <- devPTIpack:::pti_hex_source(
    label = "L", path = "https://x", hex_col = "h", vars = list()
  )
  expect_s3_class(s, "pti_hex_source")
  expect_length(s$vars, 0L)
})
