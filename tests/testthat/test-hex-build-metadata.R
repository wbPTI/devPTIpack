# Tier-1 tests for build_hex_metadata() (arch-11 §"Metadata Excel output" / issue #115).
#
# Fixtures use flood_exposure_15cm_1in100 (in registry) + population.
# Xlsx files are written to withr::local_tempdir() and read back with readxl.

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

make_aggregated_meta <- function(extra_col = NULL) {
  base_tbl <- function(n_rows = 2, admin_level = 1) {
    tbl <- tibble::tibble(
      !!paste0("admin", admin_level, "Pcod") := paste0("A", seq_len(n_rows)),
      !!paste0("admin", admin_level, "Name") := paste0("Admin", seq_len(n_rows)),
      population = as.double(seq_len(n_rows) * 100),
      flood_exposure_15cm_1in100 = as.double(seq_len(n_rows) * 10)
    )
    if (admin_level > 0) {
      tbl$admin0Pcod <- "C1"
    }
    if (!is.null(extra_col)) tbl[[extra_col]] <- as.double(seq_len(n_rows))
    tbl
  }

  list(
    admin0_Country = tibble::tibble(
      admin0Pcod = "C1",
      admin0Name = "Country1",
      area       = 100,
      population = 2100,
      flood_exposure_15cm_1in100 = 210
    ),
    admin1_Province = base_tbl(2L, 1L),
    admin9_Hexagon  = tibble::tibble(
      admin9Pcod = paste0("h", 1:6),
      admin9Name = paste0("H", 1:6),
      admin1Pcod = c("A1", "A1", "A1", "A2", "A2", "A2"),
      admin0Pcod = rep("C1", 6),
      area       = rep(10, 6),
      population = as.double(c(100, 200, 300, 400, 500, 600)),
      flood_exposure_15cm_1in100 = as.double(c(10, 20, 30, 40, 50, 60))
    )
  )
}

make_shp_dta_meta <- function() {
  list(
    admin0_Country  = tibble::tibble(admin0Pcod = "C1", admin0Name = "Country1"),
    admin1_Province = tibble::tibble(admin1Pcod = c("A1", "A2"),
                                     admin1Name = c("Admin1", "Admin2"),
                                     admin0Pcod = c("C1", "C1")),
    admin9_Hexagon  = tibble::tibble(admin9Pcod = paste0("h", 1:6),
                                     admin9Name = paste0("H", 1:6),
                                     admin0Pcod = rep("C1", 6),
                                     area       = rep(10, 6))
  )
}

tmp_xlsx <- function() tempfile(fileext = ".xlsx")

# ---------------------------------------------------------------------------
# Return value and file creation
# ---------------------------------------------------------------------------

test_that("build_hex_metadata: returns output_path invisibly and file exists", {
  out <- tmp_xlsx()
  ret <- build_hex_metadata(
    aggregated   = make_aggregated_meta(),
    shp_dta      = make_shp_dta_meta(),
    country_name = "Rwanda",
    output_path  = out,
    include_hex  = FALSE
  )
  expect_equal(ret, out)
  expect_true(file.exists(out))
})

# ---------------------------------------------------------------------------
# Sheet names
# ---------------------------------------------------------------------------

test_that("build_hex_metadata: include_hex=FALSE omits hex sheet", {
  out <- tmp_xlsx()
  build_hex_metadata(
    aggregated   = make_aggregated_meta(),
    shp_dta      = make_shp_dta_meta(),
    country_name = "Rwanda",
    output_path  = out,
    include_hex  = FALSE
  )
  sheets <- readxl::excel_sheets(out)
  expect_true("general"         %in% sheets)
  expect_true("metadata"        %in% sheets)
  expect_true("admin0_Country"  %in% sheets)
  expect_true("admin1_Province" %in% sheets)
  expect_false("admin9_Hexagon" %in% sheets)
})

test_that("build_hex_metadata: include_hex=TRUE includes hex sheet", {
  out <- tmp_xlsx()
  build_hex_metadata(
    aggregated   = make_aggregated_meta(),
    shp_dta      = make_shp_dta_meta(),
    country_name = "Rwanda",
    output_path  = out,
    include_hex  = TRUE
  )
  sheets <- readxl::excel_sheets(out)
  expect_true("admin9_Hexagon" %in% sheets)
})

test_that("build_hex_metadata: include_hex missing + small grid defaults to TRUE", {
  out <- tmp_xlsx()
  build_hex_metadata(
    aggregated   = make_aggregated_meta(),
    shp_dta      = make_shp_dta_meta(),
    country_name = "Rwanda",
    output_path  = out
    # include_hex intentionally omitted; fixture has 6 hex rows < 5000
  )
  sheets <- readxl::excel_sheets(out)
  expect_true("admin9_Hexagon" %in% sheets)
})

# ---------------------------------------------------------------------------
# general sheet
# ---------------------------------------------------------------------------

test_that("build_hex_metadata: general sheet has country and registry_version", {
  out <- tmp_xlsx()
  build_hex_metadata(
    aggregated   = make_aggregated_meta(),
    shp_dta      = make_shp_dta_meta(),
    country_name = "Rwanda",
    output_path  = out,
    include_hex  = FALSE
  )
  gen <- readxl::read_excel(out, sheet = "general")
  expect_true("country"          %in% names(gen))
  expect_true("registry_version" %in% names(gen))
  expect_equal(gen$country[[1L]], "Rwanda")
})

# ---------------------------------------------------------------------------
# metadata sheet columns and registry auto-population
# ---------------------------------------------------------------------------

test_that("build_hex_metadata: metadata sheet has required columns", {
  out <- tmp_xlsx()
  build_hex_metadata(
    aggregated   = make_aggregated_meta(),
    shp_dta      = make_shp_dta_meta(),
    country_name = "Rwanda",
    output_path  = out,
    include_hex  = FALSE
  )
  meta <- readxl::read_excel(out, sheet = "metadata")
  required <- c("var_code", "var_name", "var_description", "var_order",
                "var_units", "spatial_level", "pillar_group", "pillar_name",
                "pillar_description", "fltr_exclude_pti", "fltr_exclude_explorer",
                "fltr_overlay_pti", "fltr_overlay_explorer", "legend_revert_colours")
  for (col in required) {
    expect_true(col %in% names(meta), info = paste("missing column:", col))
  }
})

test_that("build_hex_metadata: metadata sheet has one row for flood indicator", {
  out <- tmp_xlsx()
  build_hex_metadata(
    aggregated   = make_aggregated_meta(),
    shp_dta      = make_shp_dta_meta(),
    country_name = "Rwanda",
    output_path  = out,
    include_hex  = FALSE
  )
  meta <- readxl::read_excel(out, sheet = "metadata")
  expect_equal(nrow(meta), 1L)
  expect_equal(meta$var_code[[1L]], "flood_exposure_15cm_1in100")
})

test_that("build_hex_metadata: registry var_name auto-populated", {
  out <- tmp_xlsx()
  build_hex_metadata(
    aggregated   = make_aggregated_meta(),
    shp_dta      = make_shp_dta_meta(),
    country_name = "Rwanda",
    output_path  = out,
    include_hex  = FALSE
  )
  meta <- readxl::read_excel(out, sheet = "metadata")
  expect_equal(meta$var_name[[1L]], "Flood Exposure (15 cm, 1-in-100)")
})

test_that("build_hex_metadata: registry legend_revert_colours propagates", {
  out <- tmp_xlsx()
  build_hex_metadata(
    aggregated   = make_aggregated_meta(),
    shp_dta      = make_shp_dta_meta(),
    country_name = "Rwanda",
    output_path  = out,
    include_hex  = FALSE
  )
  meta <- readxl::read_excel(out, sheet = "metadata")
  # flood_exposure_15cm_1in100 has legend_revert_colours: true in registry
  expect_true(as.logical(meta$legend_revert_colours[[1L]]))
})

# ---------------------------------------------------------------------------
# include_population flag
# ---------------------------------------------------------------------------

test_that("build_hex_metadata: include_population=FALSE excludes population row", {
  out <- tmp_xlsx()
  build_hex_metadata(
    aggregated        = make_aggregated_meta(),
    shp_dta           = make_shp_dta_meta(),
    country_name      = "Rwanda",
    output_path       = out,
    include_hex       = FALSE,
    include_population = FALSE
  )
  meta <- readxl::read_excel(out, sheet = "metadata")
  expect_false("population" %in% meta$var_code)
})

test_that("build_hex_metadata: include_population=TRUE includes population row", {
  out <- tmp_xlsx()
  build_hex_metadata(
    aggregated         = make_aggregated_meta(),
    shp_dta            = make_shp_dta_meta(),
    country_name       = "Rwanda",
    output_path        = out,
    include_hex        = FALSE,
    include_population = TRUE
  )
  meta <- readxl::read_excel(out, sheet = "metadata")
  expect_true("population" %in% meta$var_code)
})

# ---------------------------------------------------------------------------
# Admin sheet structure
# ---------------------------------------------------------------------------

test_that("build_hex_metadata: admin1 sheet has year column and indicator column", {
  out <- tmp_xlsx()
  build_hex_metadata(
    aggregated   = make_aggregated_meta(),
    shp_dta      = make_shp_dta_meta(),
    country_name = "Rwanda",
    output_path  = out,
    include_hex  = FALSE
  )
  adm1 <- readxl::read_excel(out, sheet = "admin1_Province")
  expect_true("year" %in% names(adm1))
  expect_true("flood_exposure_15cm_1in100" %in% names(adm1))
  expect_true("admin1Pcod" %in% names(adm1))
})

# ---------------------------------------------------------------------------
# indicator_config override
# ---------------------------------------------------------------------------

test_that("build_hex_metadata: indicator_config override emits warning and wins", {
  out <- tmp_xlsx()
  cfg <- tibble::tibble(
    canonical_name = "flood_exposure_15cm_1in100",
    var_name       = "My Custom Flood Name"
  )
  expect_warning(
    build_hex_metadata(
      aggregated       = make_aggregated_meta(),
      shp_dta          = make_shp_dta_meta(),
      country_name     = "Rwanda",
      output_path      = out,
      include_hex      = FALSE,
      indicator_config = cfg
    ),
    regexp = "override"
  )
  meta <- readxl::read_excel(out, sheet = "metadata")
  expect_equal(meta$var_name[[1L]], "My Custom Flood Name")
})

# ---------------------------------------------------------------------------
# Non-registry indicator without indicator_config → error
# ---------------------------------------------------------------------------

test_that("build_hex_metadata: unknown indicator without indicator_config errors", {
  agg <- make_aggregated_meta()
  # add unknown column to the hex slot (ref_tbl for all_cols detection)
  agg$admin9_Hexagon$conflict <- as.double(1:6)

  expect_error(
    build_hex_metadata(
      aggregated   = agg,
      shp_dta      = make_shp_dta_meta(),
      country_name = "Rwanda",
      output_path  = tmp_xlsx(),
      include_hex  = FALSE
    ),
    regexp = "indicator_config"
  )
})

# ---------------------------------------------------------------------------
# Input validation errors
# ---------------------------------------------------------------------------

test_that("build_hex_metadata: errors on empty aggregated list", {
  expect_error(
    build_hex_metadata(
      aggregated   = list(),
      shp_dta      = make_shp_dta_meta(),
      country_name = "Rwanda",
      output_path  = tmp_xlsx(),
      include_hex  = FALSE
    ),
    regexp = "aggregated"
  )
})

test_that("build_hex_metadata: errors on non-string country_name", {
  expect_error(
    build_hex_metadata(
      aggregated   = make_aggregated_meta(),
      shp_dta      = make_shp_dta_meta(),
      country_name = 42L,
      output_path  = tmp_xlsx(),
      include_hex  = FALSE
    ),
    regexp = "country_name"
  )
})

test_that("build_hex_metadata: errors on non-existent output directory", {
  expect_error(
    build_hex_metadata(
      aggregated   = make_aggregated_meta(),
      shp_dta      = make_shp_dta_meta(),
      country_name = "Rwanda",
      output_path  = "/nonexistent/path/metadata-hex.xlsx",
      include_hex  = FALSE
    ),
    regexp = "directory|exist"
  )
})

test_that("build_hex_metadata: errors on indicator_config without canonical_name", {
  cfg <- tibble::tibble(var_name = "something")
  expect_error(
    build_hex_metadata(
      aggregated       = make_aggregated_meta(),
      shp_dta          = make_shp_dta_meta(),
      country_name     = "Rwanda",
      output_path      = tmp_xlsx(),
      include_hex      = FALSE,
      indicator_config = cfg
    ),
    regexp = "canonical_name"
  )
})
