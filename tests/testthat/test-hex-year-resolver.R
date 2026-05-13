# Tier-1 tests for arch-11 §"Year resolution" / issue #110.
# Targets the three internal helpers in R/fct_hex_year_resolver.R:
#   resolve_years()                  -- pure, per-variable
#   resolve_years_for_vars()         -- walks a vars list, emits one warning
#   prompt_or_error_for_years()      -- handles years = NULL
#
# Network-free by construction: tests using resolve_years_for_vars()
# pass `available_years_lookup` instead of letting it call
# get_available_years().

# ---------------------------------------------------------------------------
# resolve_years() (pure)
# ---------------------------------------------------------------------------

test_that("resolve_years: exact match returns input verbatim with no substitution", {
  out <- devPTIpack:::resolve_years(
    requested_years = 2020L,
    available_years = c(2015L, 2018L, 2020L, 2022L),
    var_name = "x"
  )
  expect_identical(out$resolved, 2020L)
  expect_equal(nrow(out$substitutions), 0L)
})

test_that("resolve_years: nearest available year selected when exact is absent", {
  out <- devPTIpack:::resolve_years(
    requested_years = 2019L,
    available_years = c(2015L, 2018L, 2022L),
    var_name = "x"
  )
  expect_identical(out$resolved, 2018L)
  expect_identical(out$substitutions$requested, 2019L)
  expect_identical(out$substitutions$resolved, 2018L)
})

test_that("resolve_years: later year wins on equidistant tie", {
  out <- devPTIpack:::resolve_years(
    requested_years = 2017L,
    available_years = c(2015L, 2019L),
    var_name = "x"
  )
  expect_identical(out$resolved, 2019L)
})

test_that("resolve_years: errors when no year within 7-year tolerance", {
  expect_error(
    devPTIpack:::resolve_years(
      requested_years = 2010L,
      available_years = c(2020L, 2022L),
      var_name = "flood_exposure"
    ),
    regexp = "flood_exposure.*more than 7 years.*2020, 2022"
  )
})

test_that("resolve_years: exact 7-year gap is allowed (boundary inclusive)", {
  out <- devPTIpack:::resolve_years(
    requested_years = 2015L,
    available_years = 2022L,
    var_name = "x"
  )
  expect_identical(out$resolved, 2022L)
  expect_equal(nrow(out$substitutions), 1L)
})

test_that("resolve_years: multiple requested years map to multiple resolved, all substitutions recorded", {
  out <- devPTIpack:::resolve_years(
    requested_years = c(2018L, 2020L, 2021L),
    available_years = c(2015L, 2019L, 2022L),
    var_name = "x"
  )
  # 2018 -> 2019 (substitution)
  # 2020 -> 2019 (substitution; ties broken by max -- 2019 is closer than 2022)
  # 2021 -> 2022 (substitution; equidistant 2019/2022 tie -> later wins)
  expect_setequal(out$resolved, c(2019L, 2022L))
  expect_equal(nrow(out$substitutions), 3L)
})

test_that("resolve_years: empty requested_years returns empty resolved + empty substitutions", {
  out <- devPTIpack:::resolve_years(
    requested_years = integer(0),
    available_years = c(2018L, 2020L),
    var_name = "x"
  )
  expect_identical(out$resolved, integer(0))
  expect_equal(nrow(out$substitutions), 0L)
})

test_that("resolve_years: errors when available_years is empty", {
  expect_error(
    devPTIpack:::resolve_years(
      requested_years = 2020L,
      available_years = integer(0),
      var_name = "ghost"
    ),
    regexp = "ghost.*no available years"
  )
})

# ---------------------------------------------------------------------------
# resolve_years_for_vars()
# ---------------------------------------------------------------------------

# Test fixture builder -- avoids any registry / parquet round-trip.
make_temporal_var <- function(canonical, time_col = "year",
                              years = integer(0)) {
  devPTIpack:::pti_hex_var(
    source_col     = canonical,
    canonical_name = canonical,
    var_name       = paste0(canonical, " ({year})"),
    time_col       = time_col,
    years          = years,
    weight         = "none",
    fun            = "mean"
  )
}

make_static_var <- function(canonical) {
  devPTIpack:::pti_hex_var(
    source_col     = canonical,
    canonical_name = canonical,
    var_name       = canonical,
    time_col       = NA_character_,
    years          = integer(0),
    weight         = "none",
    fun            = "mean"
  )
}

test_that("resolve_years_for_vars: non-temporal variables pass through unchanged", {
  vars <- list(
    flood = make_static_var("flood"),
    pop   = make_static_var("population")
  )
  out <- devPTIpack:::resolve_years_for_vars(
    vars,
    available_years_lookup = list()  # unused -- everything is non-temporal
  )
  expect_identical(out$flood$years, integer(0))
  expect_identical(out$pop$years, integer(0))
})

test_that("resolve_years_for_vars: stamps resolved years onto temporal vars", {
  v <- make_temporal_var("nightlights", years = c(2019L, 2021L))
  vars <- list(nl = v)
  # 2019 -> 2020 (|d|=1 closest); 2021 equidistant to 2020/2022 so
  # later wins -> 2022.
  expect_warning(
    out <- devPTIpack:::resolve_years_for_vars(
      vars,
      available_years_lookup = list(nightlights = c(2015L, 2020L, 2022L))
    ),
    regexp = "substituted"
  )
  expect_identical(out$nl$years, c(2020L, 2022L))
})

test_that("resolve_years_for_vars: emits one consolidated warning listing all substitutions", {
  vars <- list(
    nl  = make_temporal_var("nightlights", years = 2019L),
    pov = make_temporal_var("poverty",     years = 2017L)
  )
  expect_warning(
    out <- devPTIpack:::resolve_years_for_vars(
      vars,
      available_years_lookup = list(
        nightlights = c(2015L, 2020L),
        poverty     = c(2016L, 2019L)
      )
    ),
    regexp = "nightlights.*2019 -> 2020"
  )
  expect_identical(out$nl$years, 2020L)
  # 2017 equidistant to 2016 and 2019? No: |2017-2016|=1, |2017-2019|=2.
  # Closer is 2016.
  expect_identical(out$pov$years, 2016L)
})

test_that("resolve_years_for_vars: no warning when every requested year matches exactly", {
  vars <- list(nl = make_temporal_var("nightlights", years = 2020L))
  expect_warning(
    out <- devPTIpack:::resolve_years_for_vars(
      vars,
      available_years_lookup = list(nightlights = c(2015L, 2020L, 2022L))
    ),
    regexp = NA  # no warning expected
  )
  expect_identical(out$nl$years, 2020L)
})

test_that("resolve_years_for_vars: strips __label suffix when looking up available years", {
  v <- make_temporal_var("poverty", years = 2020L)
  v$canonical_name <- "poverty__dec"
  vars <- list(poverty__dec = v)
  expect_warning(
    out <- devPTIpack:::resolve_years_for_vars(
      vars,
      # Note: keyed by the UNsuffixed canonical name.
      available_years_lookup = list(poverty = c(2018L, 2020L, 2022L))
    ),
    regexp = NA  # exact match -> no warning
  )
  expect_identical(out$poverty__dec$years, 2020L)
})

test_that("resolve_years_for_vars: errors when a temporal var has no available_years entry", {
  vars <- list(nl = make_temporal_var("nightlights", years = 2020L))
  expect_error(
    devPTIpack:::resolve_years_for_vars(
      vars,
      available_years_lookup = list()  # missing entry
    ),
    regexp = "nightlights.*no available years"
  )
})

test_that("resolve_years_for_vars: errors when a temporal var has no years AND session is non-interactive", {
  # No years passed; under non-interactive R CMD check this hits the
  # prompt_or_error_for_years() non-interactive branch via stop().
  skip_if(interactive(), "skipped when run interactively")
  vars <- list(nl = make_temporal_var("nightlights", years = integer(0)))
  expect_error(
    devPTIpack:::resolve_years_for_vars(
      vars,
      available_years_lookup = list(nightlights = c(2015L, 2020L))
    ),
    regexp = "No 'years' supplied.*nightlights.*2015, 2020"
  )
})

# ---------------------------------------------------------------------------
# prompt_or_error_for_years()
# ---------------------------------------------------------------------------

test_that("prompt_or_error_for_years: non-interactive session errors with the available-years list", {
  skip_if(interactive(), "skipped when run interactively")
  expect_error(
    devPTIpack:::prompt_or_error_for_years(
      var_name = "nightlights",
      available_years = c(2020L, 2015L)  # input order arbitrary
    ),
    # Message should list years sorted ascending.
    regexp = "nightlights.*available: 2015, 2020"
  )
})

test_that("prompt_or_error_for_years: interactive path returns the selected year (mocked menu)", {
  testthat::skip_if_not_installed("testthat", minimum_version = "3.2.0")
  testthat::local_mocked_bindings(
    menu = function(choices, title = NULL) 2L,
    .package = "utils"
  )
  testthat::local_mocked_bindings(
    is_interactive = function() TRUE,
    .package = "devPTIpack"
  )
  pick <- devPTIpack:::prompt_or_error_for_years(
    var_name = "nightlights",
    available_years = c(2015L, 2020L, 2022L)
  )
  expect_identical(pick, 2020L)
})

test_that("prompt_or_error_for_years: interactive cancellation (menu returns 0) errors", {
  testthat::skip_if_not_installed("testthat", minimum_version = "3.2.0")
  testthat::local_mocked_bindings(
    menu = function(choices, title = NULL) 0L,
    .package = "utils"
  )
  testthat::local_mocked_bindings(
    is_interactive = function() TRUE,
    .package = "devPTIpack"
  )
  expect_error(
    devPTIpack:::prompt_or_error_for_years(
      var_name = "nightlights",
      available_years = c(2015L, 2020L)
    ),
    regexp = "No year selected.*nightlights"
  )
})
