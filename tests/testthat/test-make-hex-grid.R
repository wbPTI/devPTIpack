# Tier-1 tests for make_hex_grid() (arch-10 §2 / arch-10 §8).
# Uses res = 5 throughout (~100 cells over Rwanda) so the suite stays fast.
# Live tests rely on the bundled rwa_shp.

data("rwa_shp", package = "devPTIpack", envir = environment())

skip_if_no_h3jsr <- function() {
  testthat::skip_if_not_installed("h3jsr")
}


# ---------------------------------------------------------------------------
# Output contract
# ---------------------------------------------------------------------------

test_that("make_hex_grid: returns sf with the admin9_Hexagon column contract", {
  skip_if_no_h3jsr()

  hex <- make_hex_grid(rwa_shp$admin0_Country, resolution = 5)

  expect_s3_class(hex, "sf")
  expect_setequal(
    names(hex),
    c("admin0Pcod", "admin9Pcod", "admin9Name", "area", "geometry")
  )
  expect_type(hex$admin9Pcod, "character")
  expect_type(hex$admin9Name, "character")
  expect_type(hex$area, "double")
  expect_gt(nrow(hex), 0L)
})

test_that("make_hex_grid: admin9Pcod values are unique", {
  skip_if_no_h3jsr()

  hex <- make_hex_grid(rwa_shp$admin0_Country, resolution = 5)
  expect_equal(anyDuplicated(hex$admin9Pcod), 0L)
})

test_that("make_hex_grid: admin9Name mirrors admin9Pcod", {
  skip_if_no_h3jsr()

  hex <- make_hex_grid(rwa_shp$admin0_Country, resolution = 5)
  expect_identical(hex$admin9Name, hex$admin9Pcod)
})

test_that("make_hex_grid: area is positive numeric in km^2 (~250 km^2 at H5)", {
  skip_if_no_h3jsr()

  hex <- make_hex_grid(rwa_shp$admin0_Country, resolution = 5)
  expect_true(all(is.finite(hex$area)))
  expect_true(all(hex$area > 0))
  # H5 cells are ~252 km^2; allow a generous tolerance for the s2 / planar
  # round-trip and CRS rounding.
  expect_lt(abs(stats::median(hex$area) - 252), 50)
})

test_that("make_hex_grid: output is in EPSG:4326", {
  skip_if_no_h3jsr()

  hex <- make_hex_grid(rwa_shp$admin0_Country, resolution = 5)
  expect_equal(sf::st_crs(hex), sf::st_crs(4326))
})


# ---------------------------------------------------------------------------
# Input handling
# ---------------------------------------------------------------------------

test_that("make_hex_grid: passes with single-row admin0_Country (no union)", {
  skip_if_no_h3jsr()

  hex <- make_hex_grid(rwa_shp$admin0_Country, resolution = 5)
  expect_gt(nrow(hex), 0L)
  expect_true(all(hex$admin0Pcod == rwa_shp$admin0_Country$admin0Pcod[[1]]))
})

test_that("make_hex_grid: passes with multi-row admin1_Province (triggers union)", {
  skip_if_no_h3jsr()

  hex_country  <- make_hex_grid(rwa_shp$admin0_Country, resolution = 5)
  hex_provinces <- make_hex_grid(rwa_shp$admin1_Province, resolution = 5)

  # Province union should produce ~the same hex count as the country layer.
  # Both polygons cover the same physical extent.
  expect_gt(nrow(hex_provinces), 0L)
  expect_lt(
    abs(nrow(hex_provinces) - nrow(hex_country)) / nrow(hex_country),
    0.1  # within 10%
  )
})

test_that("make_hex_grid: inherits admin0Pcod from input", {
  skip_if_no_h3jsr()

  hex <- make_hex_grid(rwa_shp$admin0_Country, resolution = 5)
  expect_true(all(hex$admin0Pcod == "RWA"))
})

test_that("make_hex_grid: warns and sets admin0Pcod = NA when input lacks the column", {
  skip_if_no_h3jsr()

  no_pcod <- rwa_shp$admin0_Country
  no_pcod$admin0Pcod <- NULL

  expect_warning(
    hex <- make_hex_grid(no_pcod, resolution = 5),
    regexp = "admin0Pcod"
  )
  expect_true(all(is.na(hex$admin0Pcod)))
})


# ---------------------------------------------------------------------------
# Error handling
# ---------------------------------------------------------------------------

test_that("make_hex_grid: errors when country_polygon is missing or NULL", {
  expect_error(make_hex_grid(), regexp = "missing or NULL")
  expect_error(make_hex_grid(NULL), regexp = "missing or NULL")
})

test_that("make_hex_grid: errors when country_polygon is not an sf object", {
  expect_error(make_hex_grid(data.frame(x = 1)), regexp = "must be an sf object")
})

test_that("make_hex_grid: errors when country_polygon has 0 rows", {
  skip_if_no_h3jsr()
  empty <- rwa_shp$admin0_Country[0, ]
  expect_error(make_hex_grid(empty, resolution = 5), regexp = "empty")
})

test_that("make_hex_grid: errors when resolution is not 5, 6, or 7", {
  expect_error(make_hex_grid(rwa_shp$admin0_Country, resolution = 4),
               regexp = "5, 6, or 7")
  expect_error(make_hex_grid(rwa_shp$admin0_Country, resolution = 8),
               regexp = "5, 6, or 7")
  expect_error(make_hex_grid(rwa_shp$admin0_Country, resolution = "high"),
               regexp = "single integer")
})


# ---------------------------------------------------------------------------
# s2 fallback path
# ---------------------------------------------------------------------------

test_that("make_hex_grid: s2 fallback path produces a valid result when st_within errors under s2", {
  skip_if_no_h3jsr()
  # local_mocked_bindings() landed in testthat 3.2.0; skip on older.
  testthat::skip_if_not_installed("testthat", minimum_version = "3.2.0")

  # Force sf::st_within to throw once (simulating an s2 error). The s2
  # fallback wrapper should disable s2 and retry, after which the call
  # succeeds and the function returns a populated hex grid.
  call_count <- 0L
  real_st_within <- sf::st_within
  testthat::local_mocked_bindings(
    st_within = function(x, y, ...) {
      call_count <<- call_count + 1L
      if (call_count == 1L) {
        stop("simulated s2 error")
      }
      real_st_within(x, y, ...)  # second call: fall through to real impl
    },
    .package = "sf"
  )

  hex <- make_hex_grid(rwa_shp$admin0_Country, resolution = 5)
  expect_gt(nrow(hex), 0L)
  expect_gte(call_count, 2L)  # at least one failed call + one retry
})

test_that("make_hex_grid: sf_use_s2() global state is restored after the call", {
  skip_if_no_h3jsr()

  initial <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(initial), add = TRUE)

  # Run twice with different starting states; verify each restores.
  sf::sf_use_s2(TRUE)
  hex_a <- make_hex_grid(rwa_shp$admin0_Country, resolution = 5)
  expect_true(sf::sf_use_s2())

  sf::sf_use_s2(FALSE)
  hex_b <- make_hex_grid(rwa_shp$admin0_Country, resolution = 5)
  expect_false(sf::sf_use_s2())

  expect_gt(nrow(hex_a), 0L)
  expect_gt(nrow(hex_b), 0L)
})
