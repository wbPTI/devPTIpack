# Tier-2 module-server tests for mod_drop_inval_adm (arch-03 §2.3).
# Source: R/mod_drop_inval_adm.R. Permanent function per arch-01.
#
# This module filters out admin-level slots from the calc-pipeline
# output whenever the user's chosen scheme weights a variable that
# has no data at that admin level. It also fires a `showNotification`
# message announcing the dropped levels.
#
# We exercise it with synthetic minimal inputs (one indicator, two
# admin slots) to keep the assertions deterministic. Bundled fixtures
# work too, but the synthetic shape lets each test surgically set
# whether a drop is required. `shiny::showNotification` is mocked via
# `testthat::local_mocked_bindings()` so the test can observe whether
# the message was emitted without a real Shiny client.

suppressPackageStartupMessages(library(shiny))

# Builds the (dta, wt_dta) pair the module consumes. When
# `missing_admin` is set, the single indicator `v1` only has data at
# the OTHER admin level, so the listed level should be dropped from
# the output (since `v1` is weighted 1 in scheme `s1`).
.build_drop_inputs <- function(missing_admin = NULL) {
  ind_levels <-
    if (is.null(missing_admin)) c("admin1", "admin2")
    else setdiff(c("admin1", "admin2"), missing_admin)
  ind_names <-
    if (is.null(missing_admin)) c("Region1", "Region2")
    else setdiff(c("Region1", "Region2"),
                 if (missing_admin == "admin1") "Region1" else "Region2")

  ind <- tibble::tibble(
    var_code              = "v1",
    pillar_group          = "1",
    pillar_name           = "P1",
    pillar_description    = "p1",
    var_name              = "V1",
    var_description       = "",
    var_order             = 1,
    var_units             = "",
    legend_revert_colours = FALSE,
    admin_levels_years    = list(tibble::tibble(
      admin_level      = ind_levels,
      admin_level_name = ind_names,
      years            = rep(list(2020), length(ind_levels))
    ))
  )

  wt_clean <- list(s1 = tibble::tibble(var_code = "v1", weight = 1))

  dta <- list(
    admin1 = list(
      pti_data    = tibble::tibble(Pcod = "A", pti_score = 0.5),
      pti_codes   = c(pti_score = "s1"),
      admin_level = c(admin1 = "Region1")
    ),
    admin2 = list(
      pti_data    = tibble::tibble(Pcod = "B", pti_score = 0.7),
      pti_codes   = c(pti_score = "s1"),
      admin_level = c(admin2 = "Region2")
    )
  )

  list(
    dta    = dta,
    wt_dta = list(indicators_list = ind, weights_clean = wt_clean)
  )
}

# ---------------------------------------------------------------------------
# No drops needed
# ---------------------------------------------------------------------------

test_that("mod_drop_inval_adm: all data available -> output equals input", {
  inp <- .build_drop_inputs(missing_admin = NULL)
  testServer(
    mod_drop_inval_adm,
    args = list(
      dta    = reactive(inp$dta),
      wt_dta = reactive(inp$wt_dta)
    ),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      expect_identical(names(out), names(inp$dta))
      expect_identical(out, inp$dta)
    }
  )
})

# ---------------------------------------------------------------------------
# Drop required
# ---------------------------------------------------------------------------

test_that("mod_drop_inval_adm: indicator missing at admin1 -> admin1 removed", {
  inp <- .build_drop_inputs(missing_admin = "admin1")
  testServer(
    mod_drop_inval_adm,
    args = list(
      dta    = reactive(inp$dta),
      wt_dta = reactive(inp$wt_dta)
    ),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      expect_identical(names(out), "admin2")
      expect_identical(out$admin2, inp$dta$admin2)
    }
  )
})

test_that("mod_drop_inval_adm: indicator missing at admin2 -> admin2 removed", {
  # Reverse-direction sibling of the test above. Pre PR #N this was
  # the silently-broken case: get_vars_un_avbil's `lag()`-based fill
  # treated an admin1-only indicator as available at admin2.
  inp <- .build_drop_inputs(missing_admin = "admin2")
  testServer(
    mod_drop_inval_adm,
    args = list(
      dta    = reactive(inp$dta),
      wt_dta = reactive(inp$wt_dta)
    ),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      expect_identical(names(out), "admin1")
      expect_identical(out$admin1, inp$dta$admin1)
    }
  )
})

# ---------------------------------------------------------------------------
# Notification side effect
# ---------------------------------------------------------------------------

test_that("mod_drop_inval_adm: showNotification fires when admin levels are dropped", {
  inp <- .build_drop_inputs(missing_admin = "admin1")
  notif_calls <- list()
  local_mocked_bindings(
    showNotification = function(ui, ...) {
      notif_calls[[length(notif_calls) + 1L]] <<-
        list(ui = ui, args = list(...))
      invisible(NULL)
    },
    .package = "shiny"
  )

  testServer(
    mod_drop_inval_adm,
    args = list(
      dta    = reactive(inp$dta),
      wt_dta = reactive(inp$wt_dta)
    ),
    expr = {
      session$flushReact()
      session$getReturned()()  # force evaluation
    }
  )

  expect_length(notif_calls, 1L)
  msg <- as.character(notif_calls[[1]]$ui)
  # The message names the dropped scheme + the admin level display
  # name (not the admin key).
  expect_match(msg, "s1")
  expect_match(msg, "Region1")
  expect_equal(notif_calls[[1]]$args$id, "dropped-admin-levels")
})

test_that("mod_drop_inval_adm: showNotification does NOT fire when nothing is dropped", {
  inp <- .build_drop_inputs(missing_admin = NULL)
  notif_calls <- list()
  local_mocked_bindings(
    showNotification = function(ui, ...) {
      notif_calls[[length(notif_calls) + 1L]] <<-
        list(ui = ui, args = list(...))
      invisible(NULL)
    },
    .package = "shiny"
  )

  testServer(
    mod_drop_inval_adm,
    args = list(
      dta    = reactive(inp$dta),
      wt_dta = reactive(inp$wt_dta)
    ),
    expr = {
      session$flushReact()
      session$getReturned()()
    }
  )

  expect_length(notif_calls, 0L)
})

# ---------------------------------------------------------------------------
# Zero-weighted indicator does NOT trigger a drop
# ---------------------------------------------------------------------------

test_that("mod_drop_inval_adm: missing indicator with weight 0 does not drop its admin level", {
  # The indicator is unavailable at admin1 but its weight is 0 in the
  # scheme — `get_min_admin_wght` filters by weight != 0, so no drop.
  inp <- .build_drop_inputs(missing_admin = "admin1")
  inp$wt_dta$weights_clean$s1$weight <- 0

  testServer(
    mod_drop_inval_adm,
    args = list(
      dta    = reactive(inp$dta),
      wt_dta = reactive(inp$wt_dta)
    ),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      expect_identical(names(out), names(inp$dta))
    }
  )
})
