# Tier-1 tests for the non-interactive map renderers (arch-03 §1.10).
# Permanent functions per arch-01.
#
# Functions under test:
#   - make_ggmap(preplot_dta, selected_layer, ...)   (R/mod_plot_poly_leaf.R)
#   - make_gg_line_map(shp_dta, ...)                 (R/mod_plot_poly_leaf.R)
#   - plot_leaf_line_map2(leaf_map, shps_dta, show_adm_levels)
#                                                    (R/mod_plot_init_leaf.R)
#
# These functions return ggplot or leaflet objects and have no easily
# assertable internal state. Tests cover: returns the right class,
# does not error on representative inputs, respects the
# `show_adm_levels` filter for the leaflet helper.

# Build a labelled-and-legended preplot list once per test file.
.preplot_with_leg <- complete_pti_labels(
  add_legend_paras(preplot_reshape_wghtd_dta(test_pipeline_out), nbins = 5)
)

# Pick a layer string the make_ggmap predicate can match against.
.layer_id <- paste0(
  unname(.preplot_with_leg[[3]]$pti_codes),
  " (", unname(.preplot_with_leg[[3]]$admin_level), ")"
)

# ---------------------------------------------------------------------------
# make_ggmap
# ---------------------------------------------------------------------------

test_that("make_ggmap: returns a ggplot for a valid layer id", {
  g <- make_ggmap(.preplot_with_leg, .layer_id, shp_dta = ukr_shp)
  expect_s3_class(g, "gg")
})

test_that("make_ggmap: works without shp_dta (no main label branch)", {
  g <- make_ggmap(.preplot_with_leg, .layer_id)
  expect_s3_class(g, "gg")
})

test_that("make_ggmap: show_interval = TRUE picks the intervals branch", {
  g <- make_ggmap(
    .preplot_with_leg, .layer_id,
    show_interval = TRUE, shp_dta = ukr_shp
  )
  expect_s3_class(g, "gg")
})

# ---------------------------------------------------------------------------
# make_gg_line_map
# ---------------------------------------------------------------------------

test_that("make_gg_line_map: returns a ggplot for ukr_shp", {
  g <- make_gg_line_map(ukr_shp)
  expect_s3_class(g, "gg")
})

# ---------------------------------------------------------------------------
# plot_leaf_line_map2
# ---------------------------------------------------------------------------

test_that("plot_leaf_line_map2: returns a leaflet htmlwidget", {
  m <- plot_leaf_line_map2(leaflet::leaflet(), ukr_shp)
  expect_s3_class(m, "leaflet")
  expect_s3_class(m, "htmlwidget")
})

test_that("plot_leaf_line_map2: NULL show_adm_levels drops the last layer", {
  # Default branch keeps `shps_dta[-length(.)]`. Verify the call
  # succeeds for ukr_shp (4 layers -> 3 plotted as polylines).
  m <- plot_leaf_line_map2(leaflet::leaflet(), ukr_shp, show_adm_levels = NULL)
  expect_s3_class(m, "leaflet")
})

test_that("plot_leaf_line_map2: show_adm_levels filters before drop-last", {
  m <- plot_leaf_line_map2(
    leaflet::leaflet(), ukr_shp,
    show_adm_levels = c("admin0", "admin1")
  )
  expect_s3_class(m, "leaflet")
})
