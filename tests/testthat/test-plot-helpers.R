# Tier-1 tests for plot helpers (arch-03 §1.6).
# Source: R/plot_pti_helpers.R. Permanent functions per arch-01.
#
# Functions under test (Tier 1):
#   - preplot_reshape_wghtd_dta
#   - get_current_levels
#   - filter_admin_levels
#   - add_legend_paras
#   - complete_pti_labels
#   - check_existing_groups
#
# `plot_pti_polygons` and friends interact with leaflet maps and are
# covered by Tier 3 manual tests, not here.

# ---------------------------------------------------------------------------
# preplot_reshape_wghtd_dta
# ---------------------------------------------------------------------------

test_that("preplot_reshape_wghtd_dta: one entry per admin x scheme", {
  preplot <- preplot_reshape_wghtd_dta(test_pipeline_out)
  # 4 admin levels x 2 schemes (test_weights_clean) = 8 entries.
  expect_equal(length(preplot), 4 * length(test_weights_clean))
})

test_that("preplot_reshape_wghtd_dta: entry shape has the expected slots", {
  preplot <- preplot_reshape_wghtd_dta(test_pipeline_out)
  for (x in preplot) {
    expect_named(x, c("pti_dta", "pti_codes", "admin_level"))
    expect_s3_class(x$pti_dta, "sf")
    expect_type(x$pti_codes, "character")
  }
})

test_that("preplot_reshape_wghtd_dta: 1-scheme input -> 1 entry per admin", {
  one_scheme <- list(all_ones = test_weights_clean[["all_ones"]])
  one_pipeline <- run_pti_pipeline(
    weights_clean   = one_scheme,
    inp_dta         = ukr_mtdt_full,
    shp_dta         = ukr_shp,
    indicators_list = test_indicators
  )
  preplot <- preplot_reshape_wghtd_dta(one_pipeline)
  expect_equal(length(preplot), length(one_pipeline))
})

# ---------------------------------------------------------------------------
# get_current_levels
# ---------------------------------------------------------------------------

test_that("get_current_levels: returns one named entry per distinct admin", {
  preplot <- preplot_reshape_wghtd_dta(test_pipeline_out)
  out <- get_current_levels(preplot)
  expect_type(out, "character")
  # Names are admin0..admin4; values are the friendly labels.
  expect_setequal(names(out), c("admin0", "admin1", "admin2", "admin4"))
  expect_setequal(unname(out), c("Country", "Oblast", "Rayon", "Hexagon"))
})

# ---------------------------------------------------------------------------
# filter_admin_levels
# ---------------------------------------------------------------------------

test_that("filter_admin_levels: 'all' returns input unchanged", {
  preplot <- preplot_reshape_wghtd_dta(test_pipeline_out)
  expect_equal(filter_admin_levels(preplot, "all"), preplot)
})

test_that("filter_admin_levels: filters by admin_level value (not name)", {
  # arch-03 §1.6 case "Specific level" expects passing 'admin1' to keep
  # admin1 entries. The current implementation matches against the
  # `admin_level` *value* ("Oblast"), not its name ("admin1"). Pin both
  # behaviours so any future refactor under #7-style work catches the
  # asymmetry.
  preplot <- preplot_reshape_wghtd_dta(test_pipeline_out)
  by_value <- filter_admin_levels(preplot, "Oblast")
  expect_equal(length(by_value), length(test_weights_clean))
  for (x in by_value) {
    expect_equal(unname(x$admin_level), "Oblast")
  }
})

test_that("filter_admin_levels: name-only filter matches the same entries as value-only", {
  # Pre PR #69 this returned 0 entries because the keep() predicate
  # compared `x$admin_level` (the display value, e.g. "Oblast")
  # against `to_fltr` (here the admin key "admin1"). The fix extends
  # the predicate to also match `names(x$admin_level)`, so passing
  # "admin1" filters to the same entries as passing "Oblast".
  preplot  <- preplot_reshape_wghtd_dta(test_pipeline_out)
  by_name  <- filter_admin_levels(preplot, "admin1")
  by_value <- filter_admin_levels(preplot, "Oblast")
  expect_equal(by_name, by_value)
})

test_that("filter_admin_levels: NULL filter -> NULL output", {
  preplot <- preplot_reshape_wghtd_dta(test_pipeline_out)
  expect_null(filter_admin_levels(preplot, NULL))
})

test_that("filter_admin_levels: non-matching filter -> NULL output", {
  preplot <- preplot_reshape_wghtd_dta(test_pipeline_out)
  expect_null(filter_admin_levels(preplot, "admin99"))
})

# ---------------------------------------------------------------------------
# add_legend_paras
# ---------------------------------------------------------------------------

test_that("add_legend_paras: attaches a $leg slot with palette + labels", {
  preplot <- preplot_reshape_wghtd_dta(test_pipeline_out)
  out <- add_legend_paras(preplot, nbins = 5)
  for (x in out) {
    expect_true("leg" %in% names(x))
    expect_true(all(
      c("pal", "our_labels", "recode_function") %in% names(x$leg)
    ))
  }
})

test_that("add_legend_paras: NULL input -> NULL output", {
  expect_null(add_legend_paras(NULL))
})

test_that("add_legend_paras: respects nbins for the continuous branch", {
  # Force the continuous branch by mutating the score column.
  preplot <- preplot_reshape_wghtd_dta(test_pipeline_out)
  preplot[[1]]$pti_dta$pti_score <- runif(nrow(preplot[[1]]$pti_dta))
  out <- add_legend_paras(preplot, nbins = 3)
  expect_equal(out[[1]]$leg$selected_groups, 3L)
})

# ---------------------------------------------------------------------------
# complete_pti_labels
# ---------------------------------------------------------------------------

test_that("complete_pti_labels: appends <strong>{priority_rank}</strong> per entry", {
  # arch-03 §1.6: pti_label is enriched with the priority-rank category
  # by concatenating the entry's legend `recode_function(pti_score)`
  # inside <strong>...</strong>.
  preplot <- preplot_reshape_wghtd_dta(test_pipeline_out)
  with_leg <- add_legend_paras(preplot, nbins = 3)
  out <- complete_pti_labels(with_leg)
  for (i in seq_along(with_leg)) {
    expected <- stringr::str_c(
      with_leg[[i]]$pti_dta$pti_label,
      "<strong>",
      with_leg[[i]]$leg$recode_function(with_leg[[i]]$pti_dta$pti_score),
      "</strong>"
    )
    expect_equal(out[[i]]$pti_dta$pti_label, expected)
  }
})

test_that("complete_pti_labels: NULL input -> NULL output", {
  expect_null(complete_pti_labels(NULL))
})

# ---------------------------------------------------------------------------
# check_existing_groups
# ---------------------------------------------------------------------------

test_that("check_existing_groups: prior selection is kept when still present", {
  cur <- c("all_ones (Country)", "all_ones (Oblast)", "uniform (Oblast)")
  old <- c("all_ones (Oblast)")
  out <- check_existing_groups(cur, old, priority_group = "Oblast")
  expect_equal(out$out_show, "all_ones (Oblast)")
  expect_equal(
    sort(out$out_hide),
    sort(c("all_ones (Country)", "uniform (Oblast)"))
  )
})

test_that("check_existing_groups: empty old -> first of current shown", {
  # arch-03 §1.6 contract: empty old_grps -> first currently rendered
  # group is shown, the rest hidden. Pre-fix this errored in
  # `str_detect(., character(0))` with a vctrs size error.
  out <- check_existing_groups(
    c("a (Country)", "b (Oblast)"),
    character(0),
    priority_group = "Oblast"
  )
  expect_equal(out$out_show, "a (Country)")
  expect_equal(out$out_hide, "b (Oblast)")
})

test_that("check_existing_groups: disjoint old -> first current shown", {
  cur <- c("foo (Country)", "bar (Oblast)")
  old <- c("baz (Country)")
  out <- check_existing_groups(cur, old, priority_group = "Country")
  expect_equal(out$out_show, "foo (Country)")
})

test_that("check_existing_groups: empty current -> NULL show / hide", {
  out <- check_existing_groups(
    cur_grps = character(0),
    old_grps = c("a (Country)"),
    priority_group = "Country"
  )
  expect_null(out$out_show)
  expect_null(out$out_hide)
})
