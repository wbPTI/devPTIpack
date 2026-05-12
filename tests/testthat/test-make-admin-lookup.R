# Tier-1 tests for make_admin_lookup() (arch-10 §3 / arch-10 §8).
# Bundled rwa_shp ships with the cascade columns already populated, so
# these tests strip them to simulate raw admin layers and verify the
# function rebuilds the cascade against the ground truth.

data("rwa_shp", package = "devPTIpack", envir = environment())

skip_if_no_h3jsr <- function() {
  testthat::skip_if_not_installed("h3jsr")
}

# ----- helpers --------------------------------------------------------------

strip_cascade <- function(layer, level) {
  keep <- c(
    paste0("admin", level, "Pcod"),
    paste0("admin", level, "Name"),
    "area", "geometry"
  )
  layer[, intersect(keep, names(layer)), drop = FALSE]
}

raw_rwa <- function() {
  list(
    admin0_Country  = rwa_shp$admin0_Country,
    admin1_Province = strip_cascade(rwa_shp$admin1_Province, 1L),
    admin2_District = strip_cascade(rwa_shp$admin2_District, 2L)
  )
}


# ---------------------------------------------------------------------------
# Happy path: enrichment + cascade correctness
# ---------------------------------------------------------------------------

test_that("make_admin_lookup: enriches admin1 and admin2 with all ancestor Pcods", {
  enriched <- make_admin_lookup(raw_rwa())

  expect_setequal(
    names(enriched$admin1_Province),
    c("admin0Pcod", "admin1Pcod", "admin1Name", "area", "geometry")
  )
  expect_setequal(
    names(enriched$admin2_District),
    c("admin0Pcod", "admin1Pcod", "admin2Pcod", "admin2Name", "area", "geometry")
  )
})

test_that("make_admin_lookup: cascade matches the bundled rwa_shp ground truth", {
  enriched <- make_admin_lookup(raw_rwa())

  gt <- sf::st_drop_geometry(rwa_shp$admin2_District)[,
    c("admin2Pcod", "admin1Pcod", "admin0Pcod")
  ]
  res <- sf::st_drop_geometry(enriched$admin2_District)[,
    c("admin2Pcod", "admin1Pcod", "admin0Pcod")
  ]
  merged <- merge(gt, res, by = "admin2Pcod", suffixes = c(".gt", ".res"))
  expect_equal(merged$admin1Pcod.gt, merged$admin1Pcod.res)
  expect_equal(merged$admin0Pcod.gt, merged$admin0Pcod.res)
})

test_that("make_admin_lookup: coarsest layer is returned unchanged", {
  raw <- raw_rwa()
  enriched <- make_admin_lookup(raw)
  expect_identical(enriched$admin0_Country, raw$admin0_Country)
})

test_that("make_admin_lookup: out-of-order input is sorted coarsest -> finest", {
  raw <- raw_rwa()
  shuffled <- raw[c("admin2_District", "admin0_Country", "admin1_Province")]
  enriched <- make_admin_lookup(shuffled)

  expect_equal(
    names(enriched),
    c("admin0_Country", "admin1_Province", "admin2_District")
  )
})

test_that("make_admin_lookup: single-layer input is returned unchanged", {
  raw <- list(admin0_Country = rwa_shp$admin0_Country)
  enriched <- make_admin_lookup(raw)
  expect_identical(enriched, raw)
})


# ---------------------------------------------------------------------------
# admin9_Hexagon support (arch-10 §3.4)
# ---------------------------------------------------------------------------

test_that("make_admin_lookup: admin9_Hexagon receives full ancestor cascade", {
  skip_if_no_h3jsr()

  hex <- make_hex_grid(rwa_shp$admin0_Country, resolution = 5)
  raw <- c(raw_rwa(), list(admin9_Hexagon = hex))

  enriched <- make_admin_lookup(raw)

  expect_setequal(
    names(enriched$admin9_Hexagon),
    c("admin0Pcod", "admin1Pcod", "admin2Pcod", "admin9Pcod", "admin9Name",
      "area", "geometry")
  )
  expect_true(all(enriched$admin9_Hexagon$admin0Pcod == "RWA"))
  # Every hex cell must have a non-NA parent at every level.
  expect_false(any(is.na(enriched$admin9_Hexagon$admin1Pcod)))
  expect_false(any(is.na(enriched$admin9_Hexagon$admin2Pcod)))
})

test_that("make_admin_lookup: non-contiguous level numbers (0, 1, 9) are handled", {
  skip_if_no_h3jsr()

  hex <- make_hex_grid(rwa_shp$admin0_Country, resolution = 5)
  raw <- list(
    admin0_Country  = rwa_shp$admin0_Country,
    admin1_Province = strip_cascade(rwa_shp$admin1_Province, 1L),
    admin9_Hexagon  = hex
  )

  enriched <- make_admin_lookup(raw)

  expect_setequal(
    names(enriched$admin9_Hexagon),
    c("admin0Pcod", "admin1Pcod", "admin9Pcod", "admin9Name", "area", "geometry")
  )
})


# ---------------------------------------------------------------------------
# Pre-flight validation (arch-10 §3.3 step 2)
# ---------------------------------------------------------------------------

test_that("make_admin_lookup: errors when a layer is not an sf object", {
  raw <- raw_rwa()
  raw$admin1_Province <- as.data.frame(raw$admin1_Province)
  expect_error(make_admin_lookup(raw), regexp = "must be an sf object")
})

test_that("make_admin_lookup: errors when admin<N>Pcod is missing", {
  raw <- raw_rwa()
  raw$admin1_Province$admin1Pcod <- NULL
  expect_error(make_admin_lookup(raw), regexp = "admin1Pcod")
})

test_that("make_admin_lookup: errors when admin<N>Name is missing", {
  raw <- raw_rwa()
  raw$admin1_Province$admin1Name <- NULL
  expect_error(make_admin_lookup(raw), regexp = "admin1Name")
})

test_that("make_admin_lookup: errors when admin<N>Pcod contains NA", {
  raw <- raw_rwa()
  raw$admin1_Province$admin1Pcod[1] <- NA_character_
  expect_error(make_admin_lookup(raw), regexp = "NA values")
})

test_that("make_admin_lookup: errors when admin<N>Pcod has duplicates", {
  raw <- raw_rwa()
  raw$admin1_Province$admin1Pcod[2] <- raw$admin1_Province$admin1Pcod[1]
  expect_error(make_admin_lookup(raw), regexp = "duplicate")
})

test_that("make_admin_lookup: errors when admin<N>Name has duplicates", {
  raw <- raw_rwa()
  raw$admin1_Province$admin1Name[2] <- raw$admin1_Province$admin1Name[1]
  expect_error(make_admin_lookup(raw), regexp = "duplicate")
})

test_that("make_admin_lookup: warns + computes area in km^2 when missing", {
  raw <- raw_rwa()
  raw$admin1_Province$area <- NULL

  expect_warning(
    enriched <- make_admin_lookup(raw),
    regexp = "area"
  )
  expect_true(is.numeric(enriched$admin1_Province$area))
  expect_true(all(enriched$admin1_Province$area > 0))
  # Rwanda's provinces are ~5000 km^2 on average; just verify the order of
  # magnitude is correct (km^2, not m^2 which would be 1,000,000x larger).
  expect_lt(stats::median(enriched$admin1_Province$area), 50000)
})


# ---------------------------------------------------------------------------
# Cascade validation: orphan / many-to-one errors
# ---------------------------------------------------------------------------

test_that("make_admin_lookup: errors when a child has no parent match (orphan)", {
  raw <- raw_rwa()
  # Add a fake admin2 polygon over the open ocean -- no admin1 contains it.
  fake_geom <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, -45), c(1, -45), c(1, -44), c(0, -44), c(0, -45)
    ))),
    crs = 4326
  )
  fake_row <- sf::st_sf(
    admin2Pcod = "RWA_OCEAN", admin2Name = "Ocean", area = 100,
    geometry = fake_geom
  )
  raw$admin2_District <- rbind(raw$admin2_District, fake_row)

  expect_error(make_admin_lookup(raw), regexp = "Orphan")
})


# ---------------------------------------------------------------------------
# Top-level error handling
# ---------------------------------------------------------------------------

test_that("make_admin_lookup: errors on missing / NULL / non-list / empty input", {
  expect_error(make_admin_lookup(), regexp = "missing or NULL")
  expect_error(make_admin_lookup(NULL), regexp = "missing or NULL")
  expect_error(make_admin_lookup("not a list"), regexp = "must be a list")
  expect_error(make_admin_lookup(list()), regexp = "empty")
})

test_that("make_admin_lookup: errors on unnamed list", {
  raw <- raw_rwa()
  names(raw) <- NULL
  expect_error(make_admin_lookup(raw), regexp = "named list")
})

test_that("make_admin_lookup: errors on slot names that don't match admin<N>_", {
  raw <- raw_rwa()
  names(raw)[2] <- "province"
  expect_error(make_admin_lookup(raw), regexp = "admin<N>")
})


# ---------------------------------------------------------------------------
# s2 fallback (arch-10 §3.5)
# ---------------------------------------------------------------------------

test_that("make_admin_lookup: s2 fallback path is exercised when st_join errors under s2", {
  # local_mocked_bindings() landed in testthat 3.2.0; skip on older.
  testthat::skip_if_not_installed("testthat", minimum_version = "3.2.0")

  call_count <- 0L
  real_st_join <- sf::st_join
  testthat::local_mocked_bindings(
    st_join = function(x, y, ...) {
      call_count <<- call_count + 1L
      if (call_count == 1L) {
        stop("simulated s2 error")
      }
      real_st_join(x, y, ...)
    },
    .package = "sf"
  )

  enriched <- make_admin_lookup(raw_rwa())
  expect_gte(call_count, 2L)
  expect_true("admin1Pcod" %in% names(enriched$admin2_District))
})

test_that("make_admin_lookup: sf_use_s2() global state is restored after the call", {
  initial <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(initial), add = TRUE)

  sf::sf_use_s2(TRUE)
  enriched_a <- make_admin_lookup(raw_rwa())
  expect_true(sf::sf_use_s2())

  sf::sf_use_s2(FALSE)
  enriched_b <- make_admin_lookup(raw_rwa())
  expect_false(sf::sf_use_s2())

  expect_true("admin1Pcod" %in% names(enriched_a$admin2_District))
  expect_true("admin1Pcod" %in% names(enriched_b$admin2_District))
})
