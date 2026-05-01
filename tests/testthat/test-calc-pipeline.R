# Calculation-pipeline tests — Tier 1 (pure functions, no Shiny).
# Source-of-truth specs:
#   .github/docs/arch-02.01-testing-calc-pipeline.md
# Targets only permanent functions per
#   .github/docs/arch-01-cleanup.md.
#
# Fixtures and pipeline intermediates come from helper-test-data.R.

# ---------------------------------------------------------------------------
# Level A.1 — get_mt(country_shapes)
# ---------------------------------------------------------------------------

test_that("get_mt: returns a tbl_df mapping every admin level", {
  mt <- get_mt(ukr_shp)
  expect_s3_class(mt, "tbl_df")
  expect_setequal(
    names(mt),
    c("admin0Pcod", "admin1Pcod", "admin2Pcod", "admin4Pcod")
  )
})

test_that("get_mt: row count equals the finest admin layer", {
  mt <- get_mt(ukr_shp)
  finest_layer <- ukr_shp[["admin4_Hexagon"]]
  expect_equal(nrow(mt), nrow(finest_layer))
})

test_that("get_mt: drops rows with any NA Pcod", {
  expect_true(all(!is.na(get_mt(ukr_shp))))
})

test_that("get_mt: ignores list element names", {
  # Renaming the list elements should not change the output —
  # only column names drive the join.
  shuffled <- ukr_shp
  names(shuffled) <- paste0("renamed_", seq_along(shuffled))
  expect_equal(get_mt(shuffled), get_mt(ukr_shp))
})

# ---------------------------------------------------------------------------
# Level A.2 — get_adm_levels(dta)
# ---------------------------------------------------------------------------

test_that("get_adm_levels: returns sorted admin ids from a mapping table", {
  out <- get_adm_levels(test_mt)
  expect_equal(unname(out), c("admin0", "admin1", "admin2", "admin4"))
  expect_equal(names(out), unname(out))
})

test_that("get_adm_levels: empty when no admin columns are present", {
  out <- get_adm_levels(tibble::tibble(x = 1))
  expect_type(out, "character")
  expect_length(out, 0)
})

test_that("get_adm_levels: sort is lexicographic, not numeric (PINNED)", {
  # arch-03 §"Known Issues to Pin": result for mixed single/double-digit
  # admin levels is lexicographic, so admin1 < admin10 < admin2.
  # Pinned, not skipped — downstream code (`expand_adm_levels`,
  # `agg_pti_scores`) compares levels by first-digit only, so the
  # codebase is internally consistent with this quirk.
  jumbled <- tibble::tibble(
    admin2Pcod  = character(),
    admin10Pcod = character(),
    admin1Pcod  = character()
  )
  expect_equal(
    unname(get_adm_levels(jumbled)),
    c("admin1", "admin10", "admin2")
  )
})

# ---------------------------------------------------------------------------
# Level A.3 — clean_geoms(country_shapes)
# ---------------------------------------------------------------------------

test_that("clean_geoms: drops geometry from every layer", {
  cg <- clean_geoms(ukr_shp)
  for (x in cg) expect_false(inherits(x, "sf"))
})

test_that("clean_geoms: shortens list element names to admin{N}", {
  cg <- clean_geoms(ukr_shp)
  expect_equal(names(cg), c("admin0", "admin1", "admin2", "admin4"))
})

test_that("clean_geoms: preserves Pcod and Name columns", {
  cg <- clean_geoms(ukr_shp)
  for (lvl in c("admin1", "admin2", "admin4")) {
    expect_true(paste0(lvl, "Pcod") %in% names(cg[[lvl]]))
    expect_true(paste0(lvl, "Name") %in% names(cg[[lvl]]))
  }
})

# ---------------------------------------------------------------------------
# Level A.4 — pivot_pti_dta(input_dta, indicators_list)
# ---------------------------------------------------------------------------

test_that("pivot_pti_dta: one long tibble per admin sheet", {
  expect_setequal(
    names(test_pivoted),
    c("admin1_Oblast", "admin2_Rayon", "admin4_Hexagon")
  )
  for (x in test_pivoted) {
    expect_s3_class(x, "tbl_df")
    expect_true(all(c("var_code", "value") %in% names(x)))
  }
})

test_that("pivot_pti_dta: drops rows with NA value", {
  for (x in test_pivoted) expect_false(anyNA(x$value))
})

test_that("pivot_pti_dta: only var_codes from indicators_list pivot", {
  declared <- test_indicators$var_code
  for (x in test_pivoted) {
    expect_true(all(unique(x$var_code) %in% declared))
  }
})

# ---------------------------------------------------------------------------
# Level A.5 — get_weighted_data(wt_list, vars_dta_list, indicators_list)
# ---------------------------------------------------------------------------

test_that("get_weighted_data: returns a scheme x admin nested list", {
  expect_equal(length(test_weighted), length(test_weights_clean))
  for (scheme in test_weighted) {
    expect_equal(length(scheme), length(test_pivoted))
  }
})

test_that("get_weighted_data: drops the weight column", {
  for (scheme in test_weighted) {
    for (x in scheme) expect_false("weight" %in% names(x))
  }
})

test_that("get_weighted_data: weight = 1 leaves values unchanged", {
  # test_weights_clean is all 1s, so weighted == original for every
  # var_code x admin combo present in both.
  scheme <- test_weighted[["all_ones"]]
  for (lvl in names(test_pivoted)) {
    pre  <- test_pivoted[[lvl]]
    post <- scheme[[lvl]]
    if (nrow(post) == 0) next
    join_keys <- intersect(names(pre), names(post)) |> setdiff("value")
    joined <- dplyr::inner_join(
      pre, post,
      by = join_keys,
      suffix = c("_pre", "_post")
    )
    expect_equal(joined$value_post, joined$value_pre * 1)
  }
})

test_that("get_weighted_data: all-zero weights keep rows but zero values", {
  zero_wts <- list(zero = tibble::tibble(
    var_code = test_indicators$var_code,
    weight   = 0
  ))
  out <- get_weighted_data(zero_wts, test_pivoted, test_indicators)
  for (x in out[["zero"]]) {
    if (nrow(x) > 0) expect_true(all(x$value == 0))
  }
})

# ---------------------------------------------------------------------------
# Level A.6 — get_scores_data(wt_dta_list)
# ---------------------------------------------------------------------------

test_that("get_scores_data: per-group mean of standardised values is ~0", {
  scored <- test_scored[["all_ones"]]
  for (x in scored) {
    if (nrow(x) <= 1) next
    means <- x |>
      dplyr::group_by(var_code) |>
      dplyr::summarise(m = mean(value, na.rm = TRUE), .groups = "drop")
    # NA can happen for single-row groups within a year x var_code combo —
    # see PINNED QUIRK below. Filter those out and check the rest.
    means <- means[!is.na(means$m), ]
    if (nrow(means) > 0) expect_true(all(abs(means$m) < 1e-10))
  }
})

test_that("get_scores_data: structure (scheme x admin) is preserved", {
  expect_equal(length(test_scored), length(test_weighted))
  for (i in seq_along(test_scored)) {
    expect_equal(length(test_scored[[i]]), length(test_weighted[[i]]))
  }
})

test_that("get_scores_data: 1-row groups produce NA, not 0 (PINNED)", {
  # arch-03 §"Known Issues to Pin": sd() of a length-1 vector returns NA
  # (not NaN), so the `is.nan` replacement leaves the value as NA.
  one_row <- list(scheme = list(adm = tibble::tibble(
    var_code = "x",
    year     = 2020,
    value    = 7
  )))
  out <- get_scores_data(one_row)
  expect_true(is.na(out$scheme$adm$value))
})

# ---------------------------------------------------------------------------
# Level A.7 — expand_adm_levels(wtd_scrd_dta, mt)
# ---------------------------------------------------------------------------

test_that("expand_adm_levels: returns nested list source x target", {
  expect_setequal(
    names(test_expanded),
    c("admin0", "admin1", "admin2", "admin4")
  )
  for (src in test_expanded) {
    expect_setequal(names(src), c("admin0", "admin1", "admin2", "admin4"))
  }
})

test_that("expand_adm_levels: same-level branch has no `._._.` suffix", {
  same_level <- test_expanded$admin1$admin1
  expect_s3_class(same_level, "tbl_df")
  expect_true("admin1Pcod" %in% names(same_level))
  expect_false(any(grepl("\\._\\._\\.", names(same_level))))
})

test_that("expand_adm_levels: upward branch suffixes cols with source level", {
  # source admin1 -> target admin2: indicator columns carry `._._.admin1`
  upward <- test_expanded$admin1$admin2
  expect_true("admin2Pcod" %in% names(upward))
  indicator_cols <- setdiff(names(upward), "admin2Pcod")
  expect_true(all(grepl("\\._\\._\\.admin1$", indicator_cols)))
})

test_that("expand_adm_levels: downward branch suffixes cols with src level", {
  # source admin2 -> target admin1: indicator columns carry `._._.admin2`
  downward <- test_expanded$admin2$admin1
  expect_true("admin1Pcod" %in% names(downward))
  indicator_cols <- setdiff(names(downward), "admin1Pcod")
  expect_true(all(grepl("\\._\\._\\.admin2$", indicator_cols)))
})

test_that("expand_adm_levels: downward aggregation averages children", {
  # admin2 -> admin1 should average all admin2 values per admin1 parent.
  # Use raw (unscored) admin2 values to make the arithmetic transparent.
  # The function ignores admin1Pcod in the input long tibble (the
  # `select(contains("admin2"))` strips it) and instead derives parents
  # from the mapping table. So we reproduce that logic to compute the
  # expected mean.
  long <- ukr_mtdt_full$admin2_Rayon |>
    dplyr::filter(!is.na(var_nval6_na_adm12)) |>
    dplyr::transmute(
      admin2Pcod,
      year,
      var_code = "var_nval6_na_adm12",
      value    = var_nval6_na_adm12
    )
  expanded <- expand_adm_levels(list(admin2 = long), test_mt)
  out <- expanded$admin2$admin1

  mt_link <- dplyr::distinct(
    dplyr::select(test_mt, admin1Pcod, admin2Pcod)
  )
  expected <- long |>
    dplyr::left_join(mt_link, by = "admin2Pcod") |>
    dplyr::group_by(admin1Pcod) |>
    dplyr::summarise(mean_val = mean(value), .groups = "drop")

  joined <- dplyr::inner_join(out, expected, by = "admin1Pcod")
  expect_equal(
    joined[["var_nval6_na_adm12._._.admin2"]],
    joined$mean_val
  )
})

test_that("expand_adm_levels: empty input -> all NULL outputs", {
  empty_in <- list(admin1 = test_scored$all_ones$admin1_Oblast[0, ])
  out <- expand_adm_levels(empty_in, test_mt)
  for (src in out) {
    for (tgt in src) expect_null(tgt)
  }
})

test_that("expand_adm_levels: >1 element matches a level -> NULLs (PINNED)", {
  # arch-02.01 §A.7 case 10: when two list elements both contain the same
  # admin level string in their names, the source-loop guard `length(...)
  # == 1` fails and the entire iteration returns nested NULLs.
  src1 <- test_scored$all_ones$admin1_Oblast
  duped <- list(admin1_a = src1, admin1_b = src1)
  out <- expand_adm_levels(duped, test_mt)
  # admin1 source iteration short-circuits because two names match
  for (tgt in out$admin1) expect_null(tgt)
})

test_that("expand_adm_levels: missing `year` column errors at all_of()", {
  # arch-02.01 §A.7 case 11: pivot survives without year, but
  # expand_adm_levels requires it via `all_of(c("year", ...))`.
  no_year <- test_scored$all_ones$admin1_Oblast |>
    dplyr::select(-dplyr::any_of("year"))
  expect_error(
    expand_adm_levels(list(admin1 = no_year), test_mt),
    regexp = "year"
  )
})

# ---------------------------------------------------------------------------
# Level A.8 — merge_expandedn_adm_levels(dta)
# ---------------------------------------------------------------------------

test_that("merge_expandedn_adm_levels: one wide tibble per target admin", {
  expect_setequal(names(test_merged), names(test_expanded))
  for (lvl in names(test_merged)) {
    pcod_col <- paste0(lvl, "Pcod")
    expect_true(pcod_col %in% names(test_merged[[lvl]]))
    # multi-source merge: more than just the Pcod column
    expect_gt(ncol(test_merged[[lvl]]), 1L)
  }
})

test_that("merge_expandedn_adm_levels: column names are unique", {
  for (m in test_merged) expect_equal(anyDuplicated(names(m)), 0L)
})

test_that("merge_expandedn_adm_levels: row count matches mapping table", {
  # Each target admin's wide tibble should have one row per Pcod present
  # in the mapping table.
  for (lvl in names(test_merged)) {
    pcod_col <- paste0(lvl, "Pcod")
    expected_rows <- dplyr::n_distinct(test_mt[[pcod_col]])
    expect_equal(nrow(test_merged[[lvl]]), expected_rows)
  }
})

test_that("merge_expandedn_adm_levels: NULL elements are filtered out", {
  # Inject NULLs for admin4 source across all targets — merge should still
  # produce valid output for the remaining sources.
  with_nulls <- test_expanded
  for (tgt in names(with_nulls$admin4)) with_nulls$admin4[[tgt]] <- NULL
  out <- merge_expandedn_adm_levels(with_nulls)
  expect_equal(names(out), names(test_merged))
  # admin1 target should still have its native admin1 values
  expect_true("var_nval3_skewd_adm1" %in% names(out$admin1))
})

# ---------------------------------------------------------------------------
# Level A.9 — agg_pti_scores(extrap_dta, adm_ids, na_rm_pti2)
# ---------------------------------------------------------------------------

test_that("agg_pti_scores: output is a list keyed by admin level", {
  expect_setequal(
    names(test_aggregated),
    c("admin0", "admin1", "admin2", "admin4")
  )
  for (x in test_aggregated) expect_s3_class(x, "tbl_df")
})

test_that("agg_pti_scores: every tibble has the expected score columns", {
  for (x in test_aggregated) {
    expect_true(all(
      c("pti_score", "pti_name", "spatial_name") %in% names(x)
    ))
  }
})

test_that("agg_pti_scores: pti_score equals rowSums of indicator columns", {
  # Synthetic, single-level input avoids the foreign-vs-native dedup logic
  # so the arithmetic is transparent.
  extrap <- list(scheme1 = list(admin0 = tibble::tibble(
    admin0Pcod = c("A", "B"),
    ind_x = c(1, 2),
    ind_y = c(10, 20)
  )))
  ids <- list(admin0 = tibble::tibble(
    admin0Pcod = c("A", "B"),
    admin0Name = c("Alpha", "Beta")
  ))
  out <- agg_pti_scores(extrap, ids)$admin0
  expect_equal(out$pti_score, c(11, 22))
})

test_that("agg_pti_scores: schemes appear as rows under each admin level", {
  for (x in test_aggregated) {
    expect_setequal(unique(x$pti_name), names(test_weights_clean))
  }
})

test_that("agg_pti_scores: pti_name preserves scheme names verbatim", {
  extrap <- list(
    A = list(admin0 = tibble::tibble(admin0Pcod = "X", v = 1)),
    B = list(admin0 = tibble::tibble(admin0Pcod = "X", v = 2))
  )
  ids <- list(admin0 = tibble::tibble(
    admin0Pcod = "X", admin0Name = "ex"
  ))
  out <- agg_pti_scores(extrap, ids)$admin0
  expect_equal(sort(out$pti_name), c("A", "B"))
})

test_that("agg_pti_scores: spatial_name is sourced from adm_ids", {
  for (lvl in names(test_aggregated)) {
    name_col <- paste0(lvl, "Name")
    if (!name_col %in% names(test_clean_geoms[[lvl]])) next
    expect_true(all(
      test_aggregated[[lvl]]$spatial_name %in%
        test_clean_geoms[[lvl]][[name_col]]
    ))
  }
})

test_that("agg_pti_scores: na_rm_pti2 = FALSE propagates NAs to pti_score", {
  na_extrap <- list(scheme1 = list(admin0 = tibble::tibble(
    admin0Pcod = c("A", "B"),
    ind_x = c(NA_real_, 3),
    ind_y = c(NA_real_, 4)
  )))
  ids <- list(admin0 = tibble::tibble(
    admin0Pcod = c("A", "B"),
    admin0Name = c("Alpha", "Beta")
  ))
  out <- agg_pti_scores(na_extrap, ids, na_rm_pti2 = FALSE)$admin0
  expect_true(is.na(out$pti_score[out$admin0Pcod == "A"]))
  expect_equal(out$pti_score[out$admin0Pcod == "B"], 7)
})

test_that("agg_pti_scores: na_rm_pti2 = TRUE drops NAs and zero-sums all-NA", {
  # arch-02.01 §A.9.4: all-NA row with na_rm = TRUE -> pti_score = 0
  # (rowSums of an empty numeric vector).
  na_extrap <- list(scheme1 = list(admin0 = tibble::tibble(
    admin0Pcod = c("A", "B"),
    ind_x = c(NA_real_, 3),
    ind_y = c(NA_real_, NA_real_)
  )))
  ids <- list(admin0 = tibble::tibble(
    admin0Pcod = c("A", "B"),
    admin0Name = c("Alpha", "Beta")
  ))
  out <- agg_pti_scores(na_extrap, ids, na_rm_pti2 = TRUE)$admin0
  expect_equal(out$pti_score[out$admin0Pcod == "A"], 0)
  expect_equal(out$pti_score[out$admin0Pcod == "B"], 3)
})

test_that("agg_pti_scores: drops rows whose admin Pcod is NA", {
  # arch-02.01 §A.9: filter_at(contains(nonsum_cols), all_vars(!is.na)).
  extrap <- list(scheme1 = list(admin0 = tibble::tibble(
    admin0Pcod = c("A", NA_character_),
    v = c(1, 2)
  )))
  ids <- list(admin0 = tibble::tibble(
    admin0Pcod = c("A", "B"),
    admin0Name = c("Alpha", "Beta")
  ))
  out <- agg_pti_scores(extrap, ids)$admin0
  expect_equal(out$admin0Pcod, "A")
  expect_equal(nrow(out), 1L)
})

# ---------------------------------------------------------------------------
# Level C — End-to-end via run_pti_pipeline()
# ---------------------------------------------------------------------------

test_that("run_pti_pipeline: admin-keyed list with the expected slots", {
  result <- test_pipeline_out
  expect_setequal(names(result), c("admin0", "admin1", "admin2", "admin4"))
  for (x in result) {
    expect_setequal(names(x), c("pti_data", "pti_codes", "admin_level"))
    expect_s3_class(x$pti_data, "sf")
    expect_gt(nrow(x$pti_data), 0)
  }
})

test_that("run_pti_pipeline: pti_codes count == number of weight schemes", {
  for (x in test_pipeline_out) {
    expect_equal(length(x$pti_codes), length(test_weights_clean))
  }
})

test_that("run_pti_pipeline: deterministic — same input yields same output", {
  args <- list(
    weights_clean   = test_weights_clean,
    inp_dta         = ukr_mtdt_full,
    shp_dta         = ukr_shp,
    indicators_list = test_indicators
  )
  r1 <- do.call(run_pti_pipeline, args)
  r2 <- do.call(run_pti_pipeline, args)
  for (lvl in names(r1)) {
    expect_equal(
      sf::st_drop_geometry(r1[[lvl]]$pti_data),
      sf::st_drop_geometry(r2[[lvl]]$pti_data)
    )
  }
})

test_that("run_pti_pipeline: all-zero weights produce zero PTI scores", {
  zero_wts <- list(zero = tibble::tibble(
    var_code = test_indicators$var_code,
    weight   = 0
  ))
  result <- run_pti_pipeline(
    weights_clean   = zero_wts,
    inp_dta         = ukr_mtdt_full,
    shp_dta         = ukr_shp,
    indicators_list = test_indicators
  )
  for (x in result) {
    score_col <- grep("^pti_score", names(x$pti_data), value = TRUE)[[1]]
    scores <- x$pti_data[[score_col]]
    non_na <- scores[!is.na(scores)]
    if (length(non_na) > 0) expect_true(all(non_na == 0))
  }
})

test_that("run_pti_pipeline: handles multiple distinct weight schemes", {
  ind <- test_indicators$var_code
  multi <- list(
    scheme_a = tibble::tibble(var_code = ind, weight = 1),
    scheme_b = tibble::tibble(
      var_code = ind,
      weight   = c(1, rep(0, length(ind) - 1))
    ),
    scheme_c = tibble::tibble(var_code = ind, weight = rev(seq_along(ind)))
  )
  result <- run_pti_pipeline(
    weights_clean   = multi,
    inp_dta         = ukr_mtdt_full,
    shp_dta         = ukr_shp,
    indicators_list = test_indicators
  )
  for (x in result) {
    score_cols <- grep(
      "^pti_score\\.\\.pti_ind", names(x$pti_data), value = TRUE
    )
    expect_equal(length(score_cols), 3L)
    expect_equal(length(x$pti_codes), 3L)
  }
})
