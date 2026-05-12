# Tier-1 tests for the bundled hex variable registry (arch-11
# §"Registry"). Targets the three exported reader fns + the internal
# `read_hex_registry()` helper.
#
# The bundled `inst/hex_vars_registry.yaml` ships with one source
# (`wb_flood_exposure`) declaring two variables: `population` (the
# registry-tagged pop_var) and `flood_exposure_15cm_1in100`.

# ---------------------------------------------------------------------------
# read_hex_registry() (internal)
# ---------------------------------------------------------------------------

test_that("read_hex_registry: returns named list of pti_hex_source", {
  reg <- devPTIpack:::read_hex_registry()
  expect_type(reg, "list")
  expect_true(all(vapply(
    reg, inherits, logical(1L), what = "pti_hex_source"
  )))
  expect_true("wb_flood_exposure" %in% names(reg))
})

test_that("read_hex_registry: bundled wb_flood_exposure source has the verified schema", {
  reg <- devPTIpack:::read_hex_registry()
  src <- reg$wb_flood_exposure
  expect_identical(src$hex_col, "hex_id")
  expect_setequal(names(src$vars), c("population", "flood_exposure_15cm_1in100"))
  expect_s3_class(src$pop_var, "pti_hex_var")
  expect_identical(src$pop_var$canonical_name, "population")
  expect_true(src$pop_var$internal)
})

test_that("read_hex_registry: exactly one source declares pop_var", {
  reg <- devPTIpack:::read_hex_registry()
  n_pop <- sum(vapply(reg, function(s) !is.null(s$pop_var), logical(1L)))
  expect_equal(n_pop, 1L)
})

test_that("read_hex_registry: attaches registry_version attribute", {
  reg <- devPTIpack:::read_hex_registry()
  expect_match(
    attr(reg, "registry_version"),
    "^[0-9]+\\.[0-9]+\\.[0-9]+$"
  )
})

# ---------------------------------------------------------------------------
# list_hex_vars()
# ---------------------------------------------------------------------------

test_that("list_hex_vars: returns a tibble with the expected columns", {
  res <- list_hex_vars()
  expect_s3_class(res, "tbl_df")
  expect_setequal(
    names(res),
    c("source_id", "source_label", "canonical_name", "var_name",
      "var_units", "time_col", "available_years", "weight", "fun",
      "is_population")
  )
})

test_that("list_hex_vars: shows the bundled wb_flood_exposure variables", {
  res <- list_hex_vars()
  expect_true("flood_exposure_15cm_1in100" %in% res$canonical_name)
  expect_true("population" %in% res$canonical_name)
})

test_that("list_hex_vars: flags the population variable", {
  res <- list_hex_vars()
  pop_row <- res[res$canonical_name == "population", ]
  expect_equal(nrow(pop_row), 1L)
  expect_true(pop_row$is_population)
})

test_that("list_hex_vars: available_years is a list-column", {
  res <- list_hex_vars()
  expect_type(res$available_years, "list")
})

# ---------------------------------------------------------------------------
# use_hex_vars()
# ---------------------------------------------------------------------------

test_that("use_hex_vars: returns list of pti_hex_var", {
  v <- use_hex_vars("flood_exposure_15cm_1in100")
  expect_type(v, "list")
  expect_true(all(vapply(v, inherits, logical(1L), what = "pti_hex_var")))
})

test_that("use_hex_vars: auto-injects population alongside requested vars", {
  v <- use_hex_vars("flood_exposure_15cm_1in100")
  expect_true("flood_exposure_15cm_1in100" %in% names(v))
  expect_true("population" %in% names(v))
})

test_that("use_hex_vars: injected population is tagged internal = TRUE", {
  v <- use_hex_vars("flood_exposure_15cm_1in100")
  expect_true(v$population$internal)
  expect_false(v$flood_exposure_15cm_1in100$internal)
})

test_that("use_hex_vars: explicit population request -> internal = FALSE", {
  v <- use_hex_vars("population")
  expect_length(v, 1L)
  expect_false(v$population$internal)
})

test_that("use_hex_vars: with no args returns just the injected population", {
  v <- use_hex_vars()
  expect_length(v, 1L)
  expect_true(v$population$internal)
})

test_that("use_hex_vars: bare symbols and quoted names both accepted", {
  v_sym  <- use_hex_vars(flood_exposure_15cm_1in100)
  v_chr  <- use_hex_vars("flood_exposure_15cm_1in100")
  expect_identical(names(v_sym), names(v_chr))
})

test_that("use_hex_vars: errors immediately on unknown variable name", {
  expect_error(
    use_hex_vars("nonexistent_var"),
    regexp = "Unknown hex variable"
  )
})

test_that("use_hex_vars: `years` propagates to every returned descriptor", {
  v <- use_hex_vars("flood_exposure_15cm_1in100", years = c(2018L, 2022L))
  expect_identical(v$flood_exposure_15cm_1in100$years, c(2018L, 2022L))
  expect_identical(v$population$years, c(2018L, 2022L))
})

test_that("use_hex_vars: errors on non-numeric / NA years", {
  expect_error(
    use_hex_vars("flood_exposure_15cm_1in100", years = "two thousand"),
    regexp = "'years'"
  )
  expect_error(
    use_hex_vars("flood_exposure_15cm_1in100", years = c(2020L, NA_integer_)),
    regexp = "'years'"
  )
})

# ---------------------------------------------------------------------------
# get_available_years()
# ---------------------------------------------------------------------------

test_that("get_available_years: non-temporal variable returns integer(0)", {
  # wb_flood_exposure is a single non-temporal snapshot; arrow is
  # never called.
  expect_identical(
    get_available_years("flood_exposure_15cm_1in100"),
    integer(0)
  )
})

test_that("get_available_years: accepts a resolved pti_hex_var", {
  v <- use_hex_vars("flood_exposure_15cm_1in100")
  expect_identical(
    get_available_years(v$flood_exposure_15cm_1in100),
    integer(0)
  )
})

test_that("get_available_years: errors on missing / NULL input", {
  expect_error(get_available_years(), regexp = "is required")
  expect_error(get_available_years(NULL), regexp = "is required")
})

test_that("get_available_years: errors on unknown canonical name", {
  expect_error(
    get_available_years("nonexistent_var"),
    regexp = "Unknown hex variable"
  )
})

# ---------------------------------------------------------------------------
# make_safe_label() (internal)
# ---------------------------------------------------------------------------

test_that("make_safe_label: lowercases + collapses non-alphanumerics to _", {
  expect_identical(
    devPTIpack:::make_safe_label("WB Space2Stats Flood Exposure"),
    "wb_space2stats_flood_exposure"
  )
  expect_identical(
    devPTIpack:::make_safe_label("  Trim & Collapse!! "),
    "trim_collapse"
  )
})
