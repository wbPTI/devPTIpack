# Tier-2 module-server tests for mod_calc_pti2_server (arch-03 §2.1).
# Source: R/mod_calc_pti2.R. Permanent function per arch-01.
#
# This module wires the reactive chain that the live app uses to
# calculate PTI scores. We exercise it via shiny::testServer, which
# spins up a fake reactive context. Calling session$flushReact() is
# required to drive the observeEvent that copies wt_dta() into the
# internal wt_dta_local() reactiveVal — without it, calc_pti() runs
# once with NULL weights and returns an empty list.

suppressPackageStartupMessages(library(shiny))

# Shared input builder. wt_dta is a list combining admin sheets with
# explicit indicators_list + weights_clean (the live app passes the
# same shape).
.build_wt_dta <- function(weights = test_weights_clean,
                          indicators = test_indicators) {
  c(
    ukr_mtdt_full[grepl("admin", names(ukr_mtdt_full))],
    list(
      indicators_list = indicators,
      weights_clean   = weights
    )
  )
}

# ---------------------------------------------------------------------------
# Happy path
# ---------------------------------------------------------------------------

test_that("mod_calc_pti2_server: returns admin-keyed list of pti_data slots", {
  testServer(
    mod_calc_pti2_server,
    args = list(
      shp_dta   = reactive(ukr_shp),
      input_dta = reactive(ukr_mtdt_full),
      wt_dta    = reactive(.build_wt_dta())
    ),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      expect_setequal(
        names(out), c("admin0", "admin1", "admin2", "admin4")
      )
      for (lvl in out) {
        expect_setequal(
          names(lvl), c("pti_data", "pti_codes", "admin_level")
        )
        expect_s3_class(lvl$pti_data, "sf")
        expect_true(any(grepl(
          "^pti_score", names(lvl$pti_data)
        )))
      }
    }
  )
})

# ---------------------------------------------------------------------------
# All-zero weights
# ---------------------------------------------------------------------------

test_that("mod_calc_pti2_server: all-zero weights produce zero PTI scores", {
  zero_wts <- list(zero = tibble::tibble(
    var_code = test_indicators$var_code,
    weight   = 0
  ))
  testServer(
    mod_calc_pti2_server,
    args = list(
      shp_dta   = reactive(ukr_shp),
      input_dta = reactive(ukr_mtdt_full),
      wt_dta    = reactive(.build_wt_dta(weights = zero_wts))
    ),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      for (lvl in out) {
        score_col <- grep("^pti_score", names(lvl$pti_data),
                          value = TRUE)[[1]]
        scores <- lvl$pti_data[[score_col]]
        non_na <- scores[!is.na(scores)]
        if (length(non_na) > 0) expect_true(all(non_na == 0))
      }
    }
  )
})

# ---------------------------------------------------------------------------
# Single indicator weight list
# ---------------------------------------------------------------------------

test_that("mod_calc_pti2_server: single-indicator weight list works", {
  # Use an indicator that has values across all admin levels so the
  # expand/merge stages don't choke on missing source data. Single-
  # admin-only indicators are the topic of a separate bug pinned in
  # PLAN.md §12 (expand_adm_levels NULL short-circuit).
  ind1 <- test_indicators[
    test_indicators$var_code == "var_nvalinf_unif_adm124", ]
  single <- list(only = tibble::tibble(
    var_code = ind1$var_code, weight = 1
  ))
  testServer(
    mod_calc_pti2_server,
    args = list(
      shp_dta   = reactive(ukr_shp),
      input_dta = reactive(ukr_mtdt_full),
      wt_dta    = reactive(.build_wt_dta(
        weights = single, indicators = ind1
      ))
    ),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      for (lvl in out) {
        expect_s3_class(lvl$pti_data, "sf")
        expect_equal(length(lvl$pti_codes), 1L)
      }
    }
  )
})

# ---------------------------------------------------------------------------
# Determinism / dedup of identical wt_dta values
# ---------------------------------------------------------------------------

test_that("mod_calc_pti2_server: identical wt_dta yields identical output", {
  rv <- reactiveVal(.build_wt_dta())
  testServer(
    mod_calc_pti2_server,
    args = list(
      shp_dta   = reactive(ukr_shp),
      input_dta = reactive(ukr_mtdt_full),
      wt_dta    = reactive(rv())
    ),
    expr = {
      session$flushReact()
      first <- session$getReturned()()

      # Re-set the same value. The internal observer's identical() guard
      # should suppress the re-assignment, and the returned reactive
      # should yield the same output.
      rv(.build_wt_dta())
      session$flushReact()
      second <- session$getReturned()()

      for (lvl in names(first)) {
        expect_equal(
          sf::st_drop_geometry(first[[lvl]]$pti_data),
          sf::st_drop_geometry(second[[lvl]]$pti_data)
        )
      }
    }
  )
})

# ---------------------------------------------------------------------------
# Weight change triggers recalc
# ---------------------------------------------------------------------------

test_that("mod_calc_pti2_server: weight change recomputes the output", {
  zero_wts <- list(zero = tibble::tibble(
    var_code = test_indicators$var_code, weight = 0
  ))
  rv <- reactiveVal(.build_wt_dta())
  testServer(
    mod_calc_pti2_server,
    args = list(
      shp_dta   = reactive(ukr_shp),
      input_dta = reactive(ukr_mtdt_full),
      wt_dta    = reactive(rv())
    ),
    expr = {
      session$flushReact()
      first <- session$getReturned()()
      first_codes <- first[[1]]$pti_codes

      rv(.build_wt_dta(weights = zero_wts))
      session$flushReact()
      second <- session$getReturned()()
      second_codes <- second[[1]]$pti_codes

      # Scheme names changed -> pti_codes must reflect that.
      expect_false(identical(first_codes, second_codes))
      expect_setequal(unname(second_codes), names(zero_wts))
    }
  )
})
