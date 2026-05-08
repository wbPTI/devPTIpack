# Tier-1 tests for the drop-invalid-admin helpers (arch-03 §1.7).
# Source: R/mod_drop_inval_adm.R. Permanent functions per arch-01.
#
# Functions under test:
#   - get_vars_un_avbil(ind_list, admin_levels = NULL)
#   - get_min_admin_wght(un_available_vars, wght_list)
#   - drop_inval_adm(dta, adm_to_drom)

# ---------------------------------------------------------------------------
# get_vars_un_avbil
# ---------------------------------------------------------------------------

test_that("get_vars_un_avbil: returns a tbl_df with var_code + admin_level", {
  out <- get_vars_un_avbil(test_indicators)
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("var_code", "admin_level") %in% names(out)))
})

test_that("get_vars_un_avbil: each row identifies a missing var x admin pair", {
  out <- get_vars_un_avbil(test_indicators)
  # All entries must reference a real var_code from the indicator list.
  expect_true(all(out$var_code %in% test_indicators$var_code))
  expect_true(all(grepl("^admin\\d", out$admin_level)))
})

test_that("get_vars_un_avbil: respects an explicit admin_levels argument", {
  full   <- get_vars_un_avbil(test_indicators)
  scoped <- get_vars_un_avbil(
    test_indicators, admin_levels = c("admin1", "admin2", "admin4")
  )
  expect_s3_class(scoped, "tbl_df")
  # Every level mentioned must be in the requested set.
  expect_true(all(scoped$admin_level %in% c("admin1", "admin2", "admin4")))
})

test_that("get_vars_un_avbil: flags missing levels symmetrically", {
  # Contract: a (var, admin) pair is unavailable iff the indicator has
  # no native data at that admin level. Both directions must surface --
  # the prior `lag()`-based fill silently treated admin1-only indicators
  # as available at admin2/admin4 (PR #34's pinned asymmetry).
  ind_list <- tibble::tibble(
    var_code = c("only_admin1", "only_admin2", "everywhere"),
    admin_levels_years = list(
      tibble::tibble(
        admin_level = "admin1", admin_level_name = "Oblast",
        years = list(2020)
      ),
      tibble::tibble(
        admin_level = "admin2", admin_level_name = "Rayon",
        years = list(2020)
      ),
      tibble::tibble(
        admin_level = c("admin1", "admin2"),
        admin_level_name = c("Oblast", "Rayon"),
        years = list(2020, 2020)
      )
    )
  )
  out <- get_vars_un_avbil(ind_list, admin_levels = c("admin1", "admin2"))

  # only_admin1 must surface at admin2 (the previously-broken direction).
  expect_true(any(out$var_code == "only_admin1" & out$admin_level == "admin2"))
  # only_admin2 must surface at admin1 (the previously-working direction).
  expect_true(any(out$var_code == "only_admin2" & out$admin_level == "admin1"))
  # everywhere must not surface at all.
  expect_false(any(out$var_code == "everywhere"))
})

# ---------------------------------------------------------------------------
# get_min_admin_wght
# ---------------------------------------------------------------------------

test_that("get_min_admin_wght: zero-weight schemes drop nothing", {
  unavail <- get_vars_un_avbil(test_indicators)
  zero <- list(scheme = tibble::tibble(
    var_code = test_indicators$var_code,
    weight   = 0
  ))
  out <- get_min_admin_wght(unavail, zero)
  expect_named(out, "scheme")
  expect_null(out$scheme)
})

test_that("get_min_admin_wght: a fully-available var drops nothing", {
  unavail <- get_vars_un_avbil(test_indicators)
  # var_nvalinf_unif_adm124 is the only fixture indicator with native
  # data at every admin level (admin1, admin2, admin4). Weighting it
  # alone must produce no drops.
  fully_available <- list(scheme = tibble::tibble(
    var_code = test_indicators$var_code,
    weight   = ifelse(
      test_indicators$var_code == "var_nvalinf_unif_adm124", 1, 0
    )
  ))
  out <- get_min_admin_wght(unavail, fully_available)
  expect_null(out$scheme)
})

test_that("get_min_admin_wght: weighting an unavailable var produces drops", {
  unavail <- get_vars_un_avbil(test_indicators)
  # var_nval4_small_skewd_adm4 is missing at admin1 and admin2 in the
  # fixture, so weighting it should mark those admins for dropping.
  weighted_unavail <- list(scheme = tibble::tibble(
    var_code = test_indicators$var_code,
    weight   = ifelse(
      test_indicators$var_code == "var_nval4_small_skewd_adm4", 1, 0
    )
  ))
  out <- get_min_admin_wght(unavail, weighted_unavail)
  expect_type(out$scheme, "character")
  expect_true(all(c("admin1", "admin2") %in% out$scheme))
})

test_that("get_min_admin_wght: result list mirrors weight-scheme names", {
  unavail <- get_vars_un_avbil(test_indicators)
  multi <- list(
    a = tibble::tibble(var_code = test_indicators$var_code, weight = 0),
    b = tibble::tibble(var_code = test_indicators$var_code, weight = 0),
    c = tibble::tibble(var_code = test_indicators$var_code, weight = 0)
  )
  out <- get_min_admin_wght(unavail, multi)
  expect_named(out, c("a", "b", "c"))
})

# ---------------------------------------------------------------------------
# drop_inval_adm
# ---------------------------------------------------------------------------

test_that("drop_inval_adm: empty drop list returns input unchanged", {
  preplot <- preplot_reshape_wghtd_dta(test_pipeline_out)
  empty_drops <- list(all_ones = character(0), uniform = character(0))
  out <- drop_inval_adm(preplot, empty_drops)
  expect_equal(length(out), length(preplot))
})

test_that("drop_inval_adm: drops only entries whose scheme + admin match", {
  preplot <- preplot_reshape_wghtd_dta(test_pipeline_out)
  drops <- list(all_ones = c("admin0", "admin1"))
  out <- drop_inval_adm(preplot, drops)

  # all_ones admin0 + admin1 should be gone; uniform admin0 + admin1 stay.
  for (x in out) {
    if (unname(x$pti_codes) == "all_ones") {
      expect_false(names(x$admin_level) %in% c("admin0", "admin1"))
    }
  }
  # uniform retains its admin0 + admin1
  uniform_levels <- vapply(
    out[vapply(out, function(x) unname(x$pti_codes) == "uniform", logical(1))],
    function(x) names(x$admin_level), character(1)
  )
  expect_true(all(c("admin0", "admin1") %in% uniform_levels))
})

test_that("drop_inval_adm: dropping all admins for every scheme empties list", {
  preplot <- preplot_reshape_wghtd_dta(test_pipeline_out)
  all_admins <- c("admin0", "admin1", "admin2", "admin4")
  drops <- list(all_ones = all_admins, uniform = all_admins)
  out <- drop_inval_adm(preplot, drops)
  expect_equal(length(out), 0L)
})
