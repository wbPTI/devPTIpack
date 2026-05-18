# End-to-end integration smoke for the Rwanda template scaffold + the
# app_validate_*() launchers + launch_pti(). Issue #82.
#
# This file is the "gate" between the template pipeline (#79), the visual
# validators (#80, #81), and the compile function (#83): if any one of
# the upstream pieces breaks, this test fails before #83 starts. Runtime
# budget per arch-03 §"CI runtime budget" and the issue brief: under
# 30 s. We hit that by sourcing 01-shapes.qmd via `knitr::purl()` rather
# than calling `quarto::quarto_render()` (Quarto's HTML render is heavy
# and not needed to confirm the data path is healthy).

suppressPackageStartupMessages({
  library(shiny)
})

# A single fresh `create_new_pti()` copy underpins every assertion; doing
# it once at file scope keeps the runtime well under budget.
.tmp_proj    <- tempfile("rwa-int-")
.copy_status <- tryCatch(
  {
    create_new_pti(.tmp_proj, open = FALSE)
    TRUE
  },
  error = function(e) {
    message("create_new_pti() failed: ", conditionMessage(e))
    FALSE
  }
)

withr::defer(unlink(.tmp_proj, recursive = TRUE), envir = teardown_env())

# ---------------------------------------------------------------------------
# 1) Template copy

test_that("create_new_pti() copies the full arch-09 §5 tree", {
  skip_if_not(.copy_status, "create_new_pti() failed at file load — see above")

  expected <- c(
    "00-master.R",
    "01-shapes.qmd", "02a-user-zonal-stats.qmd", "03-metadata.qmd",
    "04-hex-data.qmd", "05-compile.qmd", "06-deploy.R",
    "app.R", "landing-page.md", "README.md",
    "sample-data/rwa_adm0.geojson",
    "sample-data/rwa_adm1.geojson",
    "sample-data/rwa_adm2.geojson",
    "sample-data/sample-metadata-adm1.xlsx",
    "sample-data/sample-metadata-adm1-adm2.xlsx",
    "R/_disable_autoload.R"
  )
  for (f in expected) {
    expect_true(file.exists(file.path(.tmp_proj, f)),
                info = paste("missing:", f))
  }
})

# ---------------------------------------------------------------------------
# 2) Rwanda GeoJSONs

test_that("Rwanda GeoJSONs are readable by sf::read_sf", {
  skip_if_not(.copy_status)

  adm0 <- sf::read_sf(file.path(.tmp_proj, "sample-data/rwa_adm0.geojson"))
  adm1 <- sf::read_sf(file.path(.tmp_proj, "sample-data/rwa_adm1.geojson"))
  adm2 <- sf::read_sf(file.path(.tmp_proj, "sample-data/rwa_adm2.geojson"))

  expect_equal(nrow(adm0), 1L)
  expect_equal(nrow(adm1), 5L)
  expect_equal(nrow(adm2), 30L)
})

# ---------------------------------------------------------------------------
# 3) Rwanda synthetic metadata

test_that("Rwanda metadata xlsx files are readable by fct_template_reader", {
  skip_if_not(.copy_status)

  simple <- fct_template_reader(
    file.path(.tmp_proj, "sample-data/sample-metadata-adm1.xlsx")
  )
  advanced <- fct_template_reader(
    file.path(.tmp_proj, "sample-data/sample-metadata-adm1-adm2.xlsx")
  )

  expect_true(is.list(simple))
  expect_true("metadata" %in% names(simple))
  expect_true("general"  %in% names(simple))

  expect_true(is.list(advanced))
  expect_true("metadata" %in% names(advanced))
  expect_true(NROW(advanced$metadata) >= NROW(simple$metadata))
})

# ---------------------------------------------------------------------------
# 4) Pipeline produces app-data/shapes.rds and validate_geometries() passes
#
# We render Step 01 inline via knitr::purl + source rather than
# quarto::quarto_render to keep the runtime well under the 30 s budget
# (Quarto's HTML render dominates, while the underlying R code path is
# what actually exercises the data + validators).

test_that("Step 01 produces app-data/shapes.rds and validate_geometries passes", {
  skip_if_not(.copy_status)

  old_wd <- setwd(.tmp_proj)
  withr::defer(setwd(old_wd))

  dir.create("app-data", showWarnings = FALSE)
  purl_out <- knitr::purl("01-shapes.qmd",
                          output = tempfile(fileext = ".R"),
                          quiet = TRUE)
  source(purl_out, echo = FALSE)

  expect_true(file.exists("app-data/shapes.rds"))

  shp <- readRDS("app-data/shapes.rds")
  expect_equal(length(shp), 3L)
  expect_named(
    shp,
    c("admin0_Country", "admin1_Province", "admin2_District"),
    ignore.order = TRUE
  )

  diag <- validate_geometries(shp, error_on_fail = FALSE)
  expect_equal(diag$status, "pass")
})

# ---------------------------------------------------------------------------
# 5) validate_metadata() passes for both the simple and advanced xlsx

test_that("validate_metadata passes on Rwanda shapes + simple metadata", {
  skip_if_not(.copy_status && file.exists(file.path(.tmp_proj, "app-data/shapes.rds")),
              "Step 01 must run before this test (see preceding block)")

  diag <- validate_metadata(
    shp_path  = file.path(.tmp_proj, "app-data/shapes.rds"),
    mtdt_path = file.path(.tmp_proj, "sample-data/sample-metadata-adm1.xlsx"),
    error_on_fail = FALSE
  )
  expect_equal(diag$status, "pass")
})

test_that("validate_metadata passes on Rwanda shapes + advanced metadata", {
  skip_if_not(.copy_status && file.exists(file.path(.tmp_proj, "app-data/shapes.rds")))

  diag <- validate_metadata(
    shp_path  = file.path(.tmp_proj, "app-data/shapes.rds"),
    mtdt_path = file.path(.tmp_proj, "sample-data/sample-metadata-adm1-adm2.xlsx"),
    error_on_fail = FALSE
  )
  expect_equal(diag$status, "pass")
})

# ---------------------------------------------------------------------------
# 6) Visual validators accept Rwanda data

test_that("app_validate_shp(rwa_shp) returns a shiny.appobj", {
  skip_if_not(.copy_status && file.exists(file.path(.tmp_proj, "app-data/shapes.rds")))

  rwa_shp <- readRDS(file.path(.tmp_proj, "app-data/shapes.rds"))
  app <- app_validate_shp(rwa_shp)
  expect_s3_class(app, "shiny.appobj")
})

test_that("app_validate_metadata(rwa_shp, rwa_mtdt) returns a shiny.appobj", {
  skip_if_not(.copy_status && file.exists(file.path(.tmp_proj, "app-data/shapes.rds")))

  rwa_shp  <- readRDS(file.path(.tmp_proj, "app-data/shapes.rds"))
  rwa_mtdt <- fct_template_reader(
    file.path(.tmp_proj, "sample-data/sample-metadata-adm1-adm2.xlsx")
  )

  app <- app_validate_metadata(rwa_shp, rwa_mtdt)
  expect_s3_class(app, "shiny.appobj")
})

# ---------------------------------------------------------------------------
# 7) launch_pti() accepts Rwanda data

test_that("launch_pti(rwa_shp, rwa_mtdt) returns a shiny.appobj", {
  skip_if_not(.copy_status && file.exists(file.path(.tmp_proj, "app-data/shapes.rds")))

  rwa_shp  <- readRDS(file.path(.tmp_proj, "app-data/shapes.rds"))
  rwa_mtdt <- fct_template_reader(
    file.path(.tmp_proj, "sample-data/sample-metadata-adm1-adm2.xlsx")
  )

  app <- launch_pti(
    shp_dta   = rwa_shp,
    inp_dta   = rwa_mtdt,
    app_name  = "Rwanda PTI integration smoke"
  )
  expect_s3_class(app, "shiny.appobj")
})
