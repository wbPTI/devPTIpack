# Tier-1 tests for fetch_hex_data() (arch-11 §"Fetching" / issue #112).
#
# All tests are network-free: the `dataset_loader` seam accepts a function
# f(path) -> tibble, so no real parquet or H3 index service is contacted.
#
# Real h3jsr calls (get_res, get_children) are used -- h3jsr is a pure
# C-library wrapper with no network requirement.

# ---------------------------------------------------------------------------
# Rwanda test cells (computed with h3jsr locally)
# H5 cell and its 7 H6 children.
# ---------------------------------------------------------------------------
RWA_H5     <- "856ad8d7fffffff"
RWA_H6S    <- c(
  "866ad8d47ffffff", "866ad8d4fffffff", "866ad8d57ffffff",
  "866ad8d5fffffff", "866ad8d67ffffff", "866ad8d6fffffff",
  "866ad8d77ffffff"
)
RWA_H6_ONE <- "866ad8d47ffffff"   # single H6 cell for simple tests
RWA_H6_TWO <- "866ad8d4fffffff"

# Minimal vars list: one static non-temporal variable + population.
make_flood_vars <- function() {
  pop_v <- devPTIpack:::pti_hex_var(
    source_col     = "pop",
    canonical_name = "population",
    var_name       = "Population",
    time_col       = NA_character_,
    weight         = "none",
    fun            = "sum",
    internal       = TRUE,
    path           = "fake://registry.parquet",
    hex_col        = "hex_id"
  )
  flood_v <- devPTIpack:::pti_hex_var(
    source_col     = "pop_flood",
    canonical_name = "flood_exposure_15cm_1in100",
    var_name       = "Flood Exposure",
    time_col       = NA_character_,
    weight         = "none",
    fun            = "sum",
    internal       = FALSE,
    path           = "fake://registry.parquet",
    hex_col        = "hex_id"
  )
  list(flood_exposure_15cm_1in100 = flood_v, population = pop_v)
}

# Minimal mock dataset for a given set of H6 cells.
make_mock_loader <- function(h6_cells = RWA_H6S,
                              pop_vals = seq(100, by = 50,
                                            length.out = length(h6_cells)),
                              flood_vals = seq(10, by = 5,
                                              length.out = length(h6_cells))) {
  tbl <- tibble::tibble(
    hex_id    = h6_cells,
    pop       = as.double(pop_vals),
    pop_flood = as.double(flood_vals)
  )
  function(path) tbl
}

# ---------------------------------------------------------------------------
# Basic H6 direct fetch
# ---------------------------------------------------------------------------

test_that("fetch_hex_data: returns tibble with hex_id + population + indicator", {
  vars <- make_flood_vars()
  ldr  <- make_mock_loader(h6_cells = c(RWA_H6_ONE, RWA_H6_TWO),
                            pop_vals   = c(100, 200),
                            flood_vals = c(10, 20))

  result <- fetch_hex_data(c(RWA_H6_ONE, RWA_H6_TWO), vars,
                            dataset_loader = ldr)

  expect_s3_class(result, "tbl_df")
  expect_true("hex_id"   %in% names(result))
  expect_true("population" %in% names(result))
  expect_true("flood_exposure_15cm_1in100" %in% names(result))
})

test_that("fetch_hex_data: population is the first column after hex_id", {
  vars   <- make_flood_vars()
  ldr    <- make_mock_loader(h6_cells = RWA_H6_ONE,
                              pop_vals = 100, flood_vals = 10)

  result <- fetch_hex_data(RWA_H6_ONE, vars, dataset_loader = ldr)

  expect_identical(names(result)[1L], "hex_id")
  expect_identical(names(result)[2L], "population")
})

test_that("fetch_hex_data: numeric values match the mock data", {
  vars <- make_flood_vars()
  ldr  <- make_mock_loader(h6_cells = c(RWA_H6_ONE, RWA_H6_TWO),
                            pop_vals   = c(111, 222),
                            flood_vals = c(11, 22))

  result <- fetch_hex_data(c(RWA_H6_ONE, RWA_H6_TWO), vars,
                            dataset_loader = ldr)
  result <- result[order(result$hex_id), ]

  expect_equal(result$population, c(111, 222), tolerance = 1e-6)
  expect_equal(result$flood_exposure_15cm_1in100, c(11, 22), tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# Temporal variable: pivot wide
# ---------------------------------------------------------------------------

test_that("fetch_hex_data: temporal variable pivots to <canonical>_<year> columns", {
  # A temporal variable with two resolved years.
  nl_var <- devPTIpack:::pti_hex_var(
    source_col     = "ntl",
    canonical_name = "nightlights",
    var_name       = "Night Lights ({year})",
    time_col       = "year",
    years          = c(2020L, 2022L),
    weight         = "none",
    fun            = "mean",
    internal       = FALSE,
    path           = "fake://ntl.parquet",
    hex_col        = "h3id"
  )
  pop_v <- devPTIpack:::pti_hex_var(
    source_col     = "pop",
    canonical_name = "population",
    var_name       = "Population",
    time_col       = NA_character_,
    years          = integer(0),
    weight         = "none",
    fun            = "sum",
    internal       = TRUE,
    path           = "fake://ntl.parquet",
    hex_col        = "h3id"
  )
  vars <- list(nightlights = nl_var, population = pop_v)

  # Long mock data: hex × year
  long_tbl <- tibble::tibble(
    h3id = c(RWA_H6_ONE, RWA_H6_ONE, RWA_H6_TWO, RWA_H6_TWO),
    year = c(2020L, 2022L, 2020L, 2022L),
    ntl  = c(1.1, 1.2, 2.1, 2.2),
    pop  = c(100.0, 100.0, 200.0, 200.0)  # same pop both years
  )
  ldr <- function(path) long_tbl

  result <- fetch_hex_data(
    c(RWA_H6_ONE, RWA_H6_TWO), vars,
    dataset_loader        = ldr,
    available_years_lookup = list(nightlights = c(2020L, 2022L))
  )

  expect_true("nightlights_2020" %in% names(result))
  expect_true("nightlights_2022" %in% names(result))
  expect_false("ntl" %in% names(result))  # raw column gone
  expect_false("year" %in% names(result)) # time col gone
})

# ---------------------------------------------------------------------------
# H5 → H6 bridge
# ---------------------------------------------------------------------------

test_that("fetch_hex_data: H5 hex_ids trigger bridge; result has H5 IDs", {
  vars <- make_flood_vars()
  # Mock returns all 7 H6 children of RWA_H5.
  ldr  <- make_mock_loader(h6_cells   = RWA_H6S,
                            pop_vals   = rep(100, 7),
                            flood_vals = rep(10, 7))

  result <- fetch_hex_data(RWA_H5, vars, dataset_loader = ldr)

  expect_identical(result$hex_id, RWA_H5)
  # Population: sum of 7 × 100 = 700
  expect_equal(result$population, 700, tolerance = 1e-6)
  # Flood exposure: sum of 7 × 10 = 70
  expect_equal(result$flood_exposure_15cm_1in100, 70, tolerance = 1e-6)
})

test_that("fetch_hex_data: bridge with two H5 cells aggregates each independently", {
  # Second Rwanda H5 cell.
  RWA_H5B <- "856ad8c7fffffff"
  h6s_b   <- unlist(h3jsr::get_children(RWA_H5B, res = 6L))

  vars <- make_flood_vars()
  all_h6 <- c(RWA_H6S, h6s_b)
  ldr <- make_mock_loader(
    h6_cells   = all_h6,
    pop_vals   = c(rep(100, 7), rep(200, 7)),
    flood_vals = c(rep(10,  7), rep(20,  7))
  )

  result <- fetch_hex_data(c(RWA_H5, RWA_H5B), vars, dataset_loader = ldr)
  result <- result[order(result$hex_id), ]

  # Two H5 rows.
  expect_equal(nrow(result), 2L)
  # Each row aggregated from 7 children (sum).
  expect_setequal(result$population, c(700, 1400))
})

# ---------------------------------------------------------------------------
# Resolution errors
# ---------------------------------------------------------------------------

test_that("fetch_hex_data: H7 hex_ids produce an error mentioning HEX_RESOLUTION", {
  # Get an H7 child of the first H6 cell.
  h7 <- unlist(h3jsr::get_children(RWA_H6_ONE, res = 7L))[[1L]]
  vars <- make_flood_vars()
  ldr  <- make_mock_loader()

  expect_error(
    fetch_hex_data(h7, vars, dataset_loader = ldr),
    regexp = "HEX_RESOLUTION|H6|H7"
  )
})

test_that("fetch_hex_data: H4 hex_ids produce an unsupported-resolution error", {
  h4 <- h3jsr::get_parent(RWA_H5, res = 4L)
  vars <- make_flood_vars()
  ldr  <- make_mock_loader()

  expect_error(
    fetch_hex_data(h4, vars, dataset_loader = ldr),
    regexp = "not supported|H4|HEX_RESOLUTION"
  )
})

# ---------------------------------------------------------------------------
# Large hex set warning
# ---------------------------------------------------------------------------

test_that("fetch_hex_data: warns when hex set exceeds 10k cells", {
  # We need >10k valid-looking H6 cell IDs, but we just need get_res to work.
  # Use a single real H6 cell and a mock hex_ids vector.
  # Instead, we pass 10001 copies of the same H6 cell -- get_res only checks
  # the first element, so resolution detection works fine.
  big_ids <- rep(RWA_H6_ONE, 10001L)
  vars <- make_flood_vars()

  # Mock returns only one row (duplicate hex_ids collapse on join but that
  # is fine -- we're only testing the warning, not the result).
  ldr <- function(path) {
    tibble::tibble(hex_id = RWA_H6_ONE, pop = 100.0, pop_flood = 10.0)
  }

  expect_warning(
    fetch_hex_data(big_ids, vars, dataset_loader = ldr),
    regexp = "10.001|10,001|Large hex"
  )
})

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("fetch_hex_data: errors on empty hex_ids", {
  expect_error(
    fetch_hex_data(character(0), make_flood_vars(),
                   dataset_loader = make_mock_loader()),
    regexp = "hex_ids"
  )
})

test_that("fetch_hex_data: errors on empty vars list", {
  expect_error(
    fetch_hex_data(RWA_H6_ONE, list(),
                   dataset_loader = make_mock_loader()),
    regexp = "vars"
  )
})

test_that("fetch_hex_data: errors when vars contains non-pti_hex_var", {
  bad_vars <- list(x = list(source_col = "pop"))
  expect_error(
    fetch_hex_data(RWA_H6_ONE, bad_vars,
                   dataset_loader = make_mock_loader()),
    regexp = "pti_hex_var"
  )
})
