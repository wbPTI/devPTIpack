# Tier-1 tests for I/O and scaffolding (arch-03 §1.3 + neighbours).
# Targets only permanent functions per .github/docs/arch-01-cleanup.md.
#
# Functions under test:
#   - fct_template_reader(...)            (R/fct_template_reader.R)
#   - fct_convert_weight_to_clean(dta)    (R/fct_template_reader.R)
#   - get_shape(...)                      (R/mod_load_shapes.R)
#   - create_new_pti(path, ...)           (R/fct_create_new_pti.R)

# ---------------------------------------------------------------------------
# fct_template_reader(...)
# ---------------------------------------------------------------------------

test_that("fct_template_reader: bundled xlsx returns the right structure", {
  samp <- system.file(
    "sample_pti/app-data/sample-metadata.xlsx",
    package = "devPTIpack"
  )
  skip_if(samp == "", "bundled sample-metadata.xlsx not installed")

  out <- fct_template_reader(samp)
  expect_type(out, "list")
  expect_true(
    all(c("metadata", "admin1_Oblast", "admin2_Rayon") %in% names(out))
  )
  expect_s3_class(out$metadata, "tbl_df")
})

test_that("fct_template_reader: fltr_* and legend_revert_colours are logical", {
  samp <- system.file(
    "sample_pti/app-data/sample-metadata.xlsx",
    package = "devPTIpack"
  )
  skip_if(samp == "", "bundled sample-metadata.xlsx not installed")

  out <- fct_template_reader(samp)
  flag_cols <- c(
    "fltr_exclude_pti", "fltr_exclude_explorer",
    "fltr_overlay_pti", "fltr_overlay_explorer",
    "legend_revert_colours"
  )
  for (col in flag_cols) {
    expect_type(out$metadata[[col]], "logical")
    expect_false(anyNA(out$metadata[[col]]))
  }
})

test_that("fct_template_reader: weights_clean is NULL when no weights_table", {
  samp <- system.file(
    "sample_pti/app-data/sample-metadata.xlsx",
    package = "devPTIpack"
  )
  skip_if(samp == "", "bundled sample-metadata.xlsx not installed")

  out <- fct_template_reader(samp)
  expect_null(out$weights_clean)
})

test_that("fct_template_reader: admin sheets retain only declared var_codes", {
  samp <- system.file(
    "sample_pti/app-data/sample-metadata.xlsx",
    package = "devPTIpack"
  )
  skip_if(samp == "", "bundled sample-metadata.xlsx not installed")

  out <- fct_template_reader(samp)
  declared <- unique(out$metadata$var_code)
  for (sheet in grep("^admin\\d", names(out), value = TRUE)) {
    indicator_cols <- setdiff(
      names(out[[sheet]]),
      c("admin0Pcod", "admin1Pcod", "admin2Pcod", "admin4Pcod",
        "admin1Name", "admin2Name", "admin4Name", "year", "area")
    )
    extra <- setdiff(indicator_cols, declared)
    expect_equal(extra, character(0))
  }
})

# ---------------------------------------------------------------------------
# fct_convert_weight_to_clean(dta)
# ---------------------------------------------------------------------------

test_that("fct_convert_weight_to_clean: builds a list named by scheme name", {
  weights_table <- tibble::tibble(
    var_code        = c("ind_a", "ind_b", "ind_c"),
    `ws1..name`     = "Equal",
    `ws1..weight`   = c(1, 1, 1),
    `ws2..name`     = "Skewed",
    `ws2..weight`   = c(2, 1, 0)
  )
  out <- fct_convert_weight_to_clean(weights_table)
  expect_named(out, c("Equal", "Skewed"))
  expect_equal(length(out), 2L)
})

test_that("fct_convert_weight_to_clean: element has only var_code+weight", {
  weights_table <- tibble::tibble(
    var_code      = c("ind_a", "ind_b"),
    `ws1..name`   = "S1",
    `ws1..weight` = c(0.5, 0.7)
  )
  out <- fct_convert_weight_to_clean(weights_table)
  expect_named(out[[1]], c("var_code", "weight"))
  expect_equal(out[["S1"]]$weight, c(0.5, 0.7))
})

test_that("fct_convert_weight_to_clean: a single scheme -> 1-element list", {
  weights_table <- tibble::tibble(
    var_code      = "ind_a",
    `ws1..name`   = "Only",
    `ws1..weight` = 1
  )
  out <- fct_convert_weight_to_clean(weights_table)
  expect_equal(length(out), 1L)
  expect_named(out, "Only")
})

# ---------------------------------------------------------------------------
# get_shape(shapes_fldr, shape_country, shape_path, shape_dta)
# ---------------------------------------------------------------------------

test_that("get_shape: returns shape_dta verbatim when supplied", {
  out <- get_shape(
    shapes_fldr = "irrelevant/",
    shape_country = "anything",
    shape_path = NULL,
    shape_dta = ukr_shp
  )
  expect_identical(out, ukr_shp)
})

test_that("get_shape: reads from shape_path when the file exists", {
  tmp <- withr::local_tempfile(fileext = ".rds")
  saveRDS(ukr_shp, tmp)
  out <- get_shape(
    shapes_fldr = "irrelevant/",
    shape_country = "anything",
    shape_path = tmp
  )
  expect_named(out, names(ukr_shp))
})

test_that("get_shape: falls back to shapes_fldr when shape_path is missing", {
  tmp_dir <- withr::local_tempdir()
  rds_name <- "Ukraine.rds"
  saveRDS(ukr_shp, file.path(tmp_dir, rds_name))
  out <- get_shape(
    shapes_fldr = paste0(tmp_dir, "/"),
    shape_country = "Ukraine",
    shape_path = NULL,
    shape_dta = NULL
  )
  expect_named(out, names(ukr_shp))
})

# ---------------------------------------------------------------------------
# create_new_pti(path, ...)
# ---------------------------------------------------------------------------

test_that("create_new_pti: scaffolds template files into a fresh path", {
  # Avoid the yesno() interactive prompt by targeting a fresh subdir,
  # and skip RStudio integration with open = FALSE.
  # cli::cat_* writes to stdout; capture.output silences it.
  parent <- withr::local_tempdir()
  target <- file.path(parent, "demo-pti")

  invisible(capture.output(
    create_new_pti(target, open = FALSE)
  ))
  expect_true(dir.exists(target))

  template_files <- list.files(
    system.file("template_pti", package = "devPTIpack"),
    recursive = TRUE
  )
  copied <- list.files(target, recursive = TRUE)
  expect_true(all(template_files %in% copied))
})
