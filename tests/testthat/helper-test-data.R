# Auto-loaded by testthat before each test file (see ?test_dir).
# Provides bundled package data and a deterministic set of derived
# intermediates for the calc pipeline so tests can assert against
# stable inputs without each rebuilding the chain.

# ---- Bundled data ----------------------------------------------------------
data("ukr_shp",       package = "devPTIpack", envir = environment())
data("ukr_mtdt_full", package = "devPTIpack", envir = environment())

# ---- Indicator metadata ----------------------------------------------------
test_indicators <- get_indicators_list(ukr_mtdt_full)

# ---- Deterministic weights_clean ------------------------------------------
# ukr_mtdt_full$weights_clean is NULL by design (the bundled data simulates
# an app *before* the user picks a scheme). Tests need a stable input.
# `all_ones` weights every indicator equally; `uniform` is identical and
# kept as a second scheme so list-iterating code (e.g. agg_pti_scores)
# exercises the multi-scheme path.
test_weights_clean <- list(
  all_ones = tibble::tibble(
    var_code = test_indicators$var_code,
    weight   = 1
  ),
  uniform = tibble::tibble(
    var_code = test_indicators$var_code,
    weight   = 1
  )
)

# ---- Pipeline intermediates -----------------------------------------------
# Computed once per test file. Matches the order in mod_calc_pti2_server().
test_mt          <- get_mt(ukr_shp)
test_adm_levels  <- get_adm_levels(test_mt)
test_clean_geoms <- clean_geoms(ukr_shp)
test_pivoted     <- pivot_pti_dta(ukr_mtdt_full, test_indicators)
test_weighted    <- get_weighted_data(
  test_weights_clean, test_pivoted, test_indicators
)
test_scored      <- get_scores_data(test_weighted)

# Cross-level expansion + merge for the `all_ones` scheme. These are the
# inputs for tests of `agg_pti_scores`, `label_generic_pti`, and
# `structure_pti_data` so they live in the helper to avoid duplication.
test_expanded    <- expand_adm_levels(test_scored[["all_ones"]], test_mt)
test_merged      <- merge_expandedn_adm_levels(test_expanded)

# ---- Full pipeline output via the orchestrator ----------------------------
test_pipeline_out <- run_pti_pipeline(
  weights_clean   = test_weights_clean,
  inp_dta         = ukr_mtdt_full,
  shp_dta         = ukr_shp,
  indicators_list = test_indicators
)
