# Tier-1 tests for legend / palette helpers (arch-03 §1.5).
# Source: R/fct_legend_map_satelites.R. Permanent functions per arch-01.
#
# Functions under test:
#   - legend_map_satelite(leg_vals, n_groups, legend_paras, ...)
#   - recode_val_base(our_breaks, our_labels_category, no_data_label)
#
# `legend_map_satelite` has two coloring branches: a "factor-style"
# branch for binary or up-to-12-distinct-integer inputs, and a
# "continuous" branch for everything else. arch-03 §1.5 is partially
# out of date — these tests pin actual behaviour.

# ---------------------------------------------------------------------------
# legend_map_satelite — output structure
# ---------------------------------------------------------------------------

test_that("legend_map_satelite: returns the documented slots", {
  out <- legend_map_satelite(
    leg_vals = c(1.1, 2.5, 3.7, 4.2, 5.6),
    n_groups = 3,
    legend_paras = list()
  )
  expect_true(all(c(
    "pal", "recode_function", "recode_function_intervals",
    "radius_function", "our_labels", "our_labels_category",
    "our_values", "selected_groups"
  ) %in% names(out)))
  expect_type(out$pal, "closure")
  expect_type(out$recode_function, "closure")
  expect_type(out$radius_function, "closure")
})

# ---------------------------------------------------------------------------
# legend_map_satelite — branch behaviour
# ---------------------------------------------------------------------------

test_that("legend_map_satelite: <=12 distinct integers -> factor branch", {
  out <- legend_map_satelite(
    leg_vals = c(1, 5, 3, 7, 2),
    n_groups = 3,
    legend_paras = list()
  )
  # Factor branch: labels are the sorted-descending unique values, NOT
  # bin intervals. arch-03's "length 3" expectation is for the
  # continuous branch only — this input takes the integer branch.
  expect_equal(sort(out$our_labels, decreasing = TRUE), c(7, 5, 3, 2, 1))
})

test_that("legend_map_satelite: continuous values produce interval labels", {
  out <- legend_map_satelite(
    leg_vals = c(1.1, 2.5, 3.7, 4.2, 5.6),
    n_groups = 3,
    legend_paras = list()
  )
  # Continuous branch produces "low : high" labels.
  expect_equal(length(out$our_labels), 3)
  expect_true(all(grepl(" : ", out$our_labels)))
})

test_that("legend_map_satelite: single unique value -> 1 label, constant pal", {
  out <- legend_map_satelite(
    leg_vals = c(5, 5, 5),
    n_groups = 3,
    legend_paras = list()
  )
  expect_equal(length(out$our_labels), 1L)
  # The constant-palette branch returns the same colour for every input.
  colours <- out$pal(c(5, 5, 5))
  expect_equal(length(unique(colours)), 1L)
})

test_that("legend_map_satelite: binary 0/1 -> factor branch with two labels", {
  out <- legend_map_satelite(
    leg_vals = c(0, 1, 0, 1),
    n_groups = 3,
    legend_paras = list()
  )
  expect_equal(sort(out$our_labels), c(0, 1))
})

test_that("legend_map_satelite: NA inputs add a 'No data' entry", {
  out <- legend_map_satelite(
    leg_vals = c(1, 2, 3, NA),
    n_groups = 3,
    legend_paras = list()
  )
  expect_true("No data" %in% out$our_labels)
})

test_that("legend_map_satelite: legend_revert_colours flips the palette", {
  vals <- c(1.1, 2.5, 3.7, 4.2, 5.6)
  fwd <- legend_map_satelite(
    leg_vals = vals, n_groups = 3,
    legend_paras = list(legend_revert_colours = FALSE)
  )$pal(vals)
  rev <- legend_map_satelite(
    leg_vals = vals, n_groups = 3,
    legend_paras = list(legend_revert_colours = TRUE)
  )$pal(vals)
  expect_false(identical(fwd, rev))
})

# ---------------------------------------------------------------------------
# recode_val_base
# ---------------------------------------------------------------------------

test_that("recode_val_base: maps numeric values to category labels", {
  fn <- recode_val_base(c(0, 5, 10), c("Low", "High"), "No data")
  expect_equal(fn(3), "Low")
  expect_equal(fn(7), "High")
})

test_that("recode_val_base: handles a vector of mixed values", {
  fn <- recode_val_base(c(0, 5, 10), c("Low", "High"), "No data")
  expect_equal(fn(c(2, 4, 6, 8)), c("Low", "Low", "High", "High"))
})

test_that("recode_val_base: NA_real_ input -> no_data_label", {
  fn <- recode_val_base(c(0, 5, 10), c("Low", "High"), "No data")
  expect_equal(fn(NA_real_), "No data")
})

test_that("recode_val_base: a partial-NA vector handles each element", {
  fn <- recode_val_base(c(0, 5, 10), c("Low", "High"), "No data")
  expect_equal(fn(c(3, NA_real_, 7)), c("Low", "No data", "High"))
})

test_that("recode_val_base: a single break is patched, not erroring", {
  # arch-03 §1.5: "Single break -> patched to avoid cut() error".
  # Internal patch: c(b, b + 1) is used so cut() has a valid range.
  fn <- recode_val_base(5, "OnlyLabel", "No data")
  expect_equal(fn(5), "OnlyLabel")
  expect_equal(fn(c(3, 5, 7)), c("No data", "OnlyLabel", "No data"))
})
