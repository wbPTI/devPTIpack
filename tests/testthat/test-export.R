# Tier-1 tests for export helpers (arch-03 §1.8).
# Targets only permanent functions per arch-01.
#
# Functions under test:
#   - get_pti_scores_export(plotted_dta)         (R/mod_export_pti_data.R)
#   - get_pti_weights_export(wghts_dta, indic_dta)  (same file)
#   - fct_inp_for_exp(dta)                       (R/fct_inp_for_exp.R)
#   - fct_internal_wt_to_exp(weights_clean, indicators_list)  (same file)

# ---------------------------------------------------------------------------
# get_pti_scores_export
# ---------------------------------------------------------------------------

test_that("get_pti_scores_export: returns one tibble per admin level", {
  preplot  <- preplot_reshape_wghtd_dta(test_pipeline_out)
  with_leg <- add_legend_paras(preplot, nbins = 5)
  out <- get_pti_scores_export(with_leg)
  expect_type(out, "list")
  for (x in out) expect_s3_class(x, "tbl_df")
  # 4 admin levels in ukr_shp.
  expect_equal(length(out), 4L)
})

test_that("get_pti_scores_export: list names end with ' PTI Scores'", {
  preplot  <- preplot_reshape_wghtd_dta(test_pipeline_out)
  with_leg <- add_legend_paras(preplot, nbins = 5)
  out <- get_pti_scores_export(with_leg)
  expect_true(all(grepl(" PTI Scores$", names(out))))
})

test_that("get_pti_scores_export: no geometry / pti_label / area columns", {
  preplot  <- preplot_reshape_wghtd_dta(test_pipeline_out)
  with_leg <- add_legend_paras(preplot, nbins = 5)
  out <- get_pti_scores_export(with_leg)
  for (x in out) {
    expect_false("geometry" %in% names(x))
    expect_false(any(grepl("pti_label", names(x))))
    expect_false("area" %in% names(x))
  }
})

test_that("get_pti_scores_export: each scheme has Score + Priority columns", {
  preplot  <- preplot_reshape_wghtd_dta(test_pipeline_out)
  with_leg <- add_legend_paras(preplot, nbins = 5)
  out <- get_pti_scores_export(with_leg)
  schemes <- names(test_weights_clean)
  for (x in out) {
    for (sch in schemes) {
      expect_true(paste0(sch, " - PTI Score") %in% names(x))
      expect_true(paste0(sch, " - PTI Priority") %in% names(x))
    }
  }
})

# ---------------------------------------------------------------------------
# get_pti_weights_export
# ---------------------------------------------------------------------------

test_that("get_pti_weights_export: returns a tibble with row per indicator", {
  out <- get_pti_weights_export(test_weights_clean, test_indicators)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), nrow(test_indicators))
})

test_that("get_pti_weights_export: drops var_code, renames Variable / Pillar", {
  out <- get_pti_weights_export(test_weights_clean, test_indicators)
  expect_false("var_code" %in% names(out))
  expect_true("Variable name" %in% names(out))
  expect_true("Pillar" %in% names(out))
})

test_that("get_pti_weights_export: one 'Weights - X' column per scheme", {
  out <- get_pti_weights_export(test_weights_clean, test_indicators)
  for (sch in names(test_weights_clean)) {
    expect_true(paste0("Weights - ", sch) %in% names(out))
  }
})

# ---------------------------------------------------------------------------
# fct_internal_wt_to_exp
# ---------------------------------------------------------------------------

test_that("fct_internal_wt_to_exp: returns a long tibble keyed by scheme", {
  out <- fct_internal_wt_to_exp(test_weights_clean, test_indicators)
  expect_s3_class(out, "tbl_df")
  expect_setequal(unique(out$weight_scheme), names(test_weights_clean))
})

test_that("fct_internal_wt_to_exp: row count = vars x schemes", {
  out <- fct_internal_wt_to_exp(test_weights_clean, test_indicators)
  expect_equal(
    nrow(out),
    nrow(test_indicators) * length(test_weights_clean)
  )
})

test_that("fct_internal_wt_to_exp: empty list errors at left_join (PINNED)", {
  # PINNED: imap_dfr(list()) yields a 0x0 tibble, so the downstream
  # left_join cannot find its `var_code` join key. Worth normalising
  # later (early return on length-0 input) but pinned for now.
  expect_error(
    fct_internal_wt_to_exp(list(), test_indicators),
    regexp = "var_code"
  )
})

test_that("fct_internal_wt_to_exp: empty scheme tibble -> 0-row tibble", {
  empty_scheme <- list(empty = tibble::tibble(
    var_code = character(),
    weight   = numeric()
  ))
  out <- fct_internal_wt_to_exp(empty_scheme, test_indicators)
  expect_equal(nrow(out), 0L)
})

# ---------------------------------------------------------------------------
# fct_inp_for_exp
# ---------------------------------------------------------------------------

test_that("fct_inp_for_exp: returns a named list with general/admin/metadata", {
  out <- fct_inp_for_exp(ukr_mtdt_full)
  expect_type(out, "list")
  expect_true("general" %in% names(out))
  expect_true("metadata" %in% names(out))
  expect_true(any(grepl("^admin\\d_", names(out))))
})

test_that("fct_inp_for_exp: admin sheets drop adminXPcod columns", {
  out <- fct_inp_for_exp(ukr_mtdt_full)
  for (nm in grep("^admin\\d_", names(out), value = TRUE)) {
    expect_false(any(grepl("admin\\d+Pcod", names(out[[nm]]))))
  }
})

test_that("fct_inp_for_exp: admin sheets rename indicators by var_name", {
  out <- fct_inp_for_exp(ukr_mtdt_full)
  declared_codes <- ukr_mtdt_full$metadata$var_code
  declared_names <- ukr_mtdt_full$metadata$var_name
  # In the bundled fixture var_code == var_name for these indicators,
  # so just confirm: every indicator column appearing in admin sheets
  # is one of the declared var_names — i.e. nothing is renamed away.
  for (nm in grep("^admin\\d_", names(out), value = TRUE)) {
    cols <- names(out[[nm]])
    indicator_cols <- setdiff(cols, c("admin1Name", "admin2Name",
                                      "admin4Name", "year", "area"))
    expect_true(all(indicator_cols %in% declared_names))
  }
})

test_that("fct_inp_for_exp: metadata sheet drops fltr_* columns", {
  out <- fct_inp_for_exp(ukr_mtdt_full)
  expect_false(any(grepl("^fltr_", names(out$metadata))))
})
