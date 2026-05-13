# Tier-1 tests for aggregate_hex_to_shapes() (arch-11 §"Aggregation" / issue #113).
#
# All tests use plain tibbles instead of sf objects — sf::st_drop_geometry()
# returns non-sf inputs unchanged, so no geometry is needed.
#
# Fixture: 6 hexes in 2 provinces (P1: h1-h3, P2: h4-h6), 1 country (C1).

make_hex_layer_agg <- function() {
  tibble::tibble(
    admin9Pcod = paste0("h", 1:6),
    admin9Name = paste0("Hex", 1:6),
    area       = c(10, 10, 10, 20, 20, 20),
    admin1Pcod = c("P1", "P1", "P1", "P2", "P2", "P2"),
    admin0Pcod = rep("C1", 6)
  )
}

make_shp_dta_agg <- function() {
  list(
    admin0_Country = tibble::tibble(
      admin0Pcod = "C1",
      admin0Name = "Country1"
    ),
    admin1_Province = tibble::tibble(
      admin1Pcod = c("P1", "P2"),
      admin1Name = c("Province1", "Province2"),
      admin0Pcod = c("C1", "C1")
    ),
    admin9_Hexagon = make_hex_layer_agg()
  )
}

make_hex_data_agg <- function(
    pop   = c(100, 200, 300, 400, 500, 600),
    flood = c(10,  20,  30,  40,  50,  60)) {
  tibble::tibble(
    hex_id     = paste0("h", 1:6),
    population = as.double(pop),
    flood      = as.double(flood)
  )
}

# ---------------------------------------------------------------------------
# Return structure
# ---------------------------------------------------------------------------

test_that("aggregate_hex_to_shapes: returns named list with same names as shp_dta", {
  result <- aggregate_hex_to_shapes(
    hex_data  = make_hex_data_agg(),
    hex_layer = make_hex_layer_agg(),
    shp_dta   = make_shp_dta_agg(),
    strategy  = list(.default = c(weight = "none", fun = "sum"))
  )
  expect_type(result, "list")
  expect_setequal(names(result), names(make_shp_dta_agg()))
})

test_that("aggregate_hex_to_shapes: each output tibble is a tbl_df", {
  result <- aggregate_hex_to_shapes(
    hex_data  = make_hex_data_agg(),
    hex_layer = make_hex_layer_agg(),
    shp_dta   = make_shp_dta_agg(),
    strategy  = list(.default = c(weight = "none", fun = "sum"))
  )
  for (nm in names(result)) {
    expect_s3_class(result[[nm]], "tbl_df")
  }
})

# ---------------------------------------------------------------------------
# Column structure
# ---------------------------------------------------------------------------

test_that("aggregate_hex_to_shapes: admin1 tibble has correct columns", {
  result <- aggregate_hex_to_shapes(
    hex_data  = make_hex_data_agg(),
    hex_layer = make_hex_layer_agg(),
    shp_dta   = make_shp_dta_agg(),
    strategy  = list(.default = c(weight = "none", fun = "sum"))
  )
  admin1 <- result$admin1_Province
  expect_true("admin1Pcod"  %in% names(admin1))
  expect_true("admin1Name"  %in% names(admin1))
  expect_true("admin0Pcod"  %in% names(admin1))
  expect_true("population"  %in% names(admin1))
  expect_true("flood"       %in% names(admin1))
})

# ---------------------------------------------------------------------------
# Unweighted sum aggregation
# ---------------------------------------------------------------------------

test_that("aggregate_hex_to_shapes: admin1 sums population and flood correctly", {
  result <- aggregate_hex_to_shapes(
    hex_data  = make_hex_data_agg(),
    hex_layer = make_hex_layer_agg(),
    shp_dta   = make_shp_dta_agg(),
    strategy  = list(.default = c(weight = "none", fun = "sum"))
  )
  admin1 <- result$admin1_Province
  admin1 <- admin1[order(admin1$admin1Pcod), ]

  # P1: pop = 100+200+300 = 600; flood = 10+20+30 = 60
  # P2: pop = 400+500+600 = 1500; flood = 40+50+60 = 150
  expect_equal(admin1$population, c(600, 1500), tolerance = 1e-9)
  expect_equal(admin1$flood,      c(60,  150),  tolerance = 1e-9)
})

test_that("aggregate_hex_to_shapes: admin0 aggregates all 6 hexes", {
  result <- aggregate_hex_to_shapes(
    hex_data  = make_hex_data_agg(),
    hex_layer = make_hex_layer_agg(),
    shp_dta   = make_shp_dta_agg(),
    strategy  = list(.default = c(weight = "none", fun = "sum"))
  )
  admin0 <- result$admin0_Country

  expect_equal(nrow(admin0), 1L)
  expect_equal(admin0$population, 2100, tolerance = 1e-9)  # 100+200+300+400+500+600
  expect_equal(admin0$flood,      210,  tolerance = 1e-9)  # 10+20+30+40+50+60
})

test_that("aggregate_hex_to_shapes: admin9 passthrough has one row per hex", {
  result <- aggregate_hex_to_shapes(
    hex_data  = make_hex_data_agg(),
    hex_layer = make_hex_layer_agg(),
    shp_dta   = make_shp_dta_agg(),
    strategy  = list(.default = c(weight = "none", fun = "sum"))
  )
  admin9 <- result$admin9_Hexagon
  expect_equal(nrow(admin9), 6L)
  admin9s <- admin9[order(admin9$admin9Pcod), ]
  expect_equal(admin9s$population, c(100, 200, 300, 400, 500, 600), tolerance = 1e-9)
  expect_equal(admin9s$flood,      c(10,  20,  30,  40,  50,  60),  tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# Population-weighted mean
# ---------------------------------------------------------------------------

test_that("aggregate_hex_to_shapes: pop-weighted mean computes correctly for admin1", {
  result <- aggregate_hex_to_shapes(
    hex_data  = make_hex_data_agg(),
    hex_layer = make_hex_layer_agg(),
    shp_dta   = make_shp_dta_agg(),
    strategy  = list(.default = c(weight = "pop", fun = "mean"))
  )
  admin1 <- result$admin1_Province
  admin1 <- admin1[order(admin1$admin1Pcod), ]

  # P1: weighted.mean(c(10,20,30), c(100,200,300))
  #   = (10*100 + 20*200 + 30*300) / (100+200+300) = 14000/600 ≈ 23.333
  p1_expected <- stats::weighted.mean(c(10, 20, 30), c(100, 200, 300))
  expect_equal(admin1$flood[[1L]], p1_expected, tolerance = 1e-9)

  # P2: weighted.mean(c(40,50,60), c(400,500,600))
  p2_expected <- stats::weighted.mean(c(40, 50, 60), c(400, 500, 600))
  expect_equal(admin1$flood[[2L]], p2_expected, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# Area-weighted mean
# ---------------------------------------------------------------------------

test_that("aggregate_hex_to_shapes: area-weighted mean computes correctly", {
  result <- aggregate_hex_to_shapes(
    hex_data  = make_hex_data_agg(),
    hex_layer = make_hex_layer_agg(),
    shp_dta   = make_shp_dta_agg(),
    strategy  = list(.default = c(weight = "area", fun = "mean"))
  )
  admin1 <- result$admin1_Province
  admin1 <- admin1[order(admin1$admin1Pcod), ]

  # P1 hexes: area all 10. flood = 10,20,30 → equal weights → mean = 20
  expect_equal(admin1$flood[[1L]], 20, tolerance = 1e-9)

  # P2 hexes: area all 20. flood = 40,50,60 → equal weights → mean = 50
  expect_equal(admin1$flood[[2L]], 50, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# All-NA handling
# ---------------------------------------------------------------------------

test_that("aggregate_hex_to_shapes: all-NA indicator returns NA + warns", {
  hex_data_na <- make_hex_data_agg(flood = rep(NA_real_, 6))

  expect_warning(
    result <- aggregate_hex_to_shapes(
      hex_data  = hex_data_na,
      hex_layer = make_hex_layer_agg(),
      shp_dta   = make_shp_dta_agg(),
      strategy  = list(.default = c(weight = "none", fun = "sum"))
    ),
    regexp = "All hexes are NA|NA"
  )

  expect_true(all(is.na(result$admin1_Province$flood)))
  expect_true(all(is.na(result$admin0_Country$flood)))
})

test_that("aggregate_hex_to_shapes: partial-NA indicator treated as 0", {
  # h1 NA, rest have values
  pop   <- c(100, 200, 300, 400, 500, 600)
  flood <- c(NA, 20, 30, 40, 50, 60)

  result <- aggregate_hex_to_shapes(
    hex_data  = make_hex_data_agg(pop = pop, flood = flood),
    hex_layer = make_hex_layer_agg(),
    shp_dta   = make_shp_dta_agg(),
    strategy  = list(.default = c(weight = "none", fun = "sum"))
  )
  admin1 <- result$admin1_Province
  admin1 <- admin1[order(admin1$admin1Pcod), ]

  # P1: h1 NA→0, h2=20, h3=30 → sum = 50
  expect_equal(admin1$flood[[1L]], 50, tolerance = 1e-9)
  # P2: 40+50+60 = 150
  expect_equal(admin1$flood[[2L]], 150, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# Temporal variable stem lookup
# ---------------------------------------------------------------------------

test_that("aggregate_hex_to_shapes: temporal column uses stem from strategy", {
  hex_data_temporal <- tibble::tibble(
    hex_id          = paste0("h", 1:6),
    population      = as.double(c(100, 200, 300, 400, 500, 600)),
    nightlights_2020 = as.double(c(1, 2, 3, 4, 5, 6))
  )
  result <- aggregate_hex_to_shapes(
    hex_data  = hex_data_temporal,
    hex_layer = make_hex_layer_agg(),
    shp_dta   = make_shp_dta_agg(),
    strategy  = list(
      .default    = c(weight = "pop",  fun = "mean"),
      nightlights = c(weight = "none", fun = "sum")
    )
  )
  admin1 <- result$admin1_Province
  admin1 <- admin1[order(admin1$admin1Pcod), ]

  expect_true("nightlights_2020" %in% names(admin1))
  # Stem "nightlights" found in strategy → weight="none", fun="sum"
  # P1: 1+2+3 = 6; P2: 4+5+6 = 15
  expect_equal(admin1$nightlights_2020, c(6, 15), tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# Strategy validation warnings / errors
# ---------------------------------------------------------------------------

test_that("aggregate_hex_to_shapes: 'population' in strategy emits warning", {
  expect_warning(
    aggregate_hex_to_shapes(
      hex_data  = make_hex_data_agg(),
      hex_layer = make_hex_layer_agg(),
      shp_dta   = make_shp_dta_agg(),
      strategy  = list(
        .default   = c(weight = "none", fun = "sum"),
        population = c(weight = "none", fun = "sum")
      )
    ),
    regexp = "population.*ignored|ignored.*population"
  )
})

test_that("aggregate_hex_to_shapes: weight='pop' without population column errors", {
  hex_no_pop <- tibble::tibble(
    hex_id = paste0("h", 1:6),
    flood  = as.double(1:6)
  )
  expect_error(
    aggregate_hex_to_shapes(
      hex_data  = hex_no_pop,
      hex_layer = make_hex_layer_agg(),
      shp_dta   = make_shp_dta_agg(),
      strategy  = list(.default = c(weight = "pop", fun = "mean"))
    ),
    regexp = "weight.*pop|population.*absent"
  )
})

test_that("aggregate_hex_to_shapes: weight='area' without area column errors", {
  hex_layer_no_area <- make_hex_layer_agg()
  hex_layer_no_area$area <- NULL

  expect_error(
    aggregate_hex_to_shapes(
      hex_data  = make_hex_data_agg(),
      hex_layer = hex_layer_no_area,
      shp_dta   = make_shp_dta_agg(),
      strategy  = list(.default = c(weight = "area", fun = "mean"))
    ),
    regexp = "weight.*area|area.*absent"
  )
})

# ---------------------------------------------------------------------------
# Input validation errors
# ---------------------------------------------------------------------------

test_that("aggregate_hex_to_shapes: errors on missing hex_id column", {
  expect_error(
    aggregate_hex_to_shapes(
      hex_data  = tibble::tibble(population = 1),
      hex_layer = make_hex_layer_agg(),
      shp_dta   = make_shp_dta_agg(),
      strategy  = list(.default = c(weight = "none", fun = "sum"))
    ),
    regexp = "hex_id"
  )
})

test_that("aggregate_hex_to_shapes: errors on strategy without .default", {
  expect_error(
    aggregate_hex_to_shapes(
      hex_data  = make_hex_data_agg(),
      hex_layer = make_hex_layer_agg(),
      shp_dta   = make_shp_dta_agg(),
      strategy  = list(flood = c(weight = "none", fun = "sum"))
    ),
    regexp = "\\.default"
  )
})

test_that("aggregate_hex_to_shapes: warns on .x/.y column names", {
  hex_collision <- make_hex_data_agg()
  hex_collision$flood.x <- 1
  hex_collision$flood.y <- 2

  expect_warning(
    aggregate_hex_to_shapes(
      hex_data  = hex_collision,
      hex_layer = make_hex_layer_agg(),
      shp_dta   = make_shp_dta_agg(),
      strategy  = list(.default = c(weight = "none", fun = "sum"))
    ),
    regexp = "\\.x.*\\.y|\\.y.*\\.x|collision"
  )
})
