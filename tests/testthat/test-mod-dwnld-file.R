# Tier-2 module-server tests for mod_dwnld_file_server (arch-03 §2.X).
# Source: R/mod_dwnld_dta.R. Permanent function per arch-01.
#
# This module wires a Shiny downloadHandler that streams a single
# file. Pre PR #N invalid filepaths (NULL, ".", missing, directory)
# silently produced broken downloads -- the browser fell back to URL
# path + content-type sniffing and saved the empty response as an
# .html. The fix validates `filepath` at registration: when invalid,
# the link is greyed out via `shinyjs::disable()` and the content
# callback ships an explanatory `unavailable-<date>.txt` placeholder
# if a click sneaks through anyway (so the user never receives a
# silent broken `.html` again). When valid, the handler streams the
# file under its base name.

suppressPackageStartupMessages(library(shiny))

# ---------------------------------------------------------------------------
# Tier-1: materialize_dwnld_paths
# ---------------------------------------------------------------------------

test_that("materialize_dwnld_paths: NULL paths produce tempfile fallbacks", {
  shp <- ukr_shp
  inp <- ukr_mtdt_full
  paths <- devPTIpack:::materialize_dwnld_paths(
    shp_dta = shp,
    inp_dta = inp
  )
  expect_true(file.exists(paths$shapes_path))
  expect_true(file.exists(paths$data_path))
  expect_null(paths$mtdtpdf_path)
  expect_match(basename(paths$shapes_path), "^pti-shapes-.*\\.rds$")
  expect_match(basename(paths$data_path), "^pti-data-export-.*\\.xlsx$")
})

test_that("materialize_dwnld_paths: explicit paths are returned normalized", {
  tmp_shp <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(stub = "shapes"), tmp_shp)
  tmp_pdf <- withr::local_tempfile(fileext = ".pdf")
  writeLines("stub", tmp_pdf)

  paths <- devPTIpack:::materialize_dwnld_paths(
    shp_dta      = ukr_shp,
    inp_dta      = ukr_mtdt_full,
    shapes_path  = tmp_shp,
    mtdtpdf_path = tmp_pdf
  )
  expect_equal(paths$shapes_path, normalizePath(tmp_shp, mustWork = FALSE))
  expect_equal(paths$mtdtpdf_path, normalizePath(tmp_pdf, mustWork = FALSE))
  # data_path NULL -> tempfile fallback even when shapes/pdf are explicit.
  expect_true(file.exists(paths$data_path))
})

# ---------------------------------------------------------------------------
# Tier-2: mod_dwnld_file_server
# ---------------------------------------------------------------------------

test_that("mod_dwnld_file_server: valid filepath does not disable the link", {
  tmp <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(stub = "x"), tmp)

  disable_calls <- list()
  local_mocked_bindings(
    disable = function(...) {
      disable_calls[[length(disable_calls) + 1L]] <<- list(...)
      invisible(NULL)
    },
    .package = "shinyjs"
  )

  testServer(
    mod_dwnld_file_server,
    args = list(outputId = "dta.dwld", filepath = tmp),
    expr = { session$flushReact() }
  )
  expect_length(disable_calls, 0L)
})

test_that("mod_dwnld_file_server: NULL filepath disables the link", {
  disable_calls <- list()
  local_mocked_bindings(
    disable = function(...) {
      disable_calls[[length(disable_calls) + 1L]] <<- list(...)
      invisible(NULL)
    },
    .package = "shinyjs"
  )

  testServer(
    mod_dwnld_file_server,
    args = list(outputId = "dta.dwld", filepath = NULL),
    expr = { session$flushReact() }
  )
  expect_length(disable_calls, 1L)
  expect_equal(disable_calls[[1]]$id, "dta.dwld")
})

test_that("mod_dwnld_file_server: '.' (directory) filepath disables the link", {
  disable_calls <- list()
  local_mocked_bindings(
    disable = function(...) {
      disable_calls[[length(disable_calls) + 1L]] <<- list(...)
      invisible(NULL)
    },
    .package = "shinyjs"
  )

  testServer(
    mod_dwnld_file_server,
    args = list(outputId = "dta.dwld", filepath = "."),
    expr = { session$flushReact() }
  )
  expect_length(disable_calls, 1L)
})

test_that("mod_dwnld_file_server: nonexistent path disables the link", {
  disable_calls <- list()
  local_mocked_bindings(
    disable = function(...) {
      disable_calls[[length(disable_calls) + 1L]] <<- list(...)
      invisible(NULL)
    },
    .package = "shinyjs"
  )

  testServer(
    mod_dwnld_file_server,
    args = list(
      outputId = "dta.dwld",
      filepath = file.path(tempdir(), "definitely-not-a-real-file-xyz.dat")
    ),
    expr = { session$flushReact() }
  )
  expect_length(disable_calls, 1L)
})
