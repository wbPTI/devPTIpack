# Tier-2 module-server tests for mod_export_pti_data_server (arch-03 §2.7).
# Source: R/mod_export_pti_data.R. Permanent function per arch-01.
#
# This module wraps `get_pti_scores_export()` and
# `get_pti_weights_export()` into a single reactive that emits a
# named list ready to feed an Excel writer:
#   - `Country`            <- weights_dta()$general,
#   - `Weighting schemes`  <- get_pti_weights_export(...),
#   - <admin name> PTI Scores tibbles, one per admin level (reversed
#     order: finer admin first).
#
# Inputs are synthetic and minimal — the wrapper does no calc work
# of its own, so we just need shapes the two helpers accept. The
# plotted_dta slots use `pti_dta` (with the typo) because that is
# what `get_pti_scores_export` reads (production passes data through
# `mod_plot_pti2_srv` which renames `pti_data` -> `pti_dta`).

suppressPackageStartupMessages(library(shiny))

.make_slot <- function(admin_key, admin_name) {
  pcod_col <- paste0(admin_key, "Pcod")
  name_col <- paste0(admin_key, "Name")
  pti_dta <- sf::st_sf(
    setNames(list("A1"), pcod_col),
    setNames(list(admin_name), name_col),
    pti_score = 0.5,
    geometry  = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  )
  list(
    pti_dta     = pti_dta,
    pti_codes   = c(pti_score = "scheme_a"),
    leg         = list(recode_function = function(x) "High"),
    admin_level = setNames(admin_name, admin_key)
  )
}

.build_export_inputs <- function(schemes = "scheme_a",
                                 admins = list(admin1 = "Oblast",
                                               admin2 = "Raion")) {
  plotted <- mapply(.make_slot, names(admins), unname(admins),
                    SIMPLIFY = FALSE)

  ind <- tibble::tibble(
    var_code           = c("v1", "v2"),
    var_name           = c("V1", "V2"),
    pillar_name        = c("Pillar A", "Pillar A"),
    pillar_group       = c("1", "1"),
    pillar_description = c("d", "d"),
    var_description    = c("", "")
  )
  wt_clean <- setNames(
    lapply(seq_along(schemes), function(i) {
      tibble::tibble(var_code = c("v1", "v2"), weight = c(i / 10, 1 - i / 10))
    }),
    schemes
  )

  list(
    plotted_dta = plotted,
    weights_dta = list(
      weights_clean   = wt_clean,
      indicators_list = ind,
      general         = list(country = "Ukraine")
    )
  )
}

# ---------------------------------------------------------------------------
# Returns named list
# ---------------------------------------------------------------------------

test_that("mod_export_pti_data_server: returns named list with Country, Weighting schemes, and per-admin scores", {
  inp <- .build_export_inputs()
  testServer(
    mod_export_pti_data_server,
    args = list(
      plotted_dta = reactive(inp$plotted_dta),
      weights_dta = reactive(inp$weights_dta)
    ),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      expect_type(out, "list")
      expect_named(
        out,
        c("Country", "Weighting schemes", "Raion PTI Scores",
          "Oblast PTI Scores")
      )
    }
  )
})

# ---------------------------------------------------------------------------
# Country slot
# ---------------------------------------------------------------------------

test_that("mod_export_pti_data_server: Country slot mirrors weights_dta$general", {
  inp <- .build_export_inputs()
  testServer(
    mod_export_pti_data_server,
    args = list(
      plotted_dta = reactive(inp$plotted_dta),
      weights_dta = reactive(inp$weights_dta)
    ),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      expect_identical(out$Country, inp$weights_dta$general)
    }
  )
})

# ---------------------------------------------------------------------------
# Weighting schemes
# ---------------------------------------------------------------------------

test_that("mod_export_pti_data_server: 'Weighting schemes' is a tibble with one column per scheme", {
  inp <- .build_export_inputs(schemes = c("scheme_a", "scheme_b"))
  testServer(
    mod_export_pti_data_server,
    args = list(
      plotted_dta = reactive(inp$plotted_dta),
      weights_dta = reactive(inp$weights_dta)
    ),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      ws <- out[["Weighting schemes"]]
      expect_s3_class(ws, "tbl_df")
      expect_named(
        ws,
        c("Variable name", "Pillar",
          "Weights - scheme_a", "Weights - scheme_b")
      )
      # Two indicators -> two rows.
      expect_equal(nrow(ws), 2L)
    }
  )
})

# ---------------------------------------------------------------------------
# Scores per admin
# ---------------------------------------------------------------------------

test_that("mod_export_pti_data_server: each admin level produces a '<name> PTI Scores' tibble", {
  inp <- .build_export_inputs(
    admins = list(admin1 = "Oblast", admin2 = "Raion")
  )
  testServer(
    mod_export_pti_data_server,
    args = list(
      plotted_dta = reactive(inp$plotted_dta),
      weights_dta = reactive(inp$weights_dta)
    ),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      score_keys <- grep(" PTI Scores$", names(out), value = TRUE)
      expect_setequal(score_keys, c("Oblast PTI Scores", "Raion PTI Scores"))
      for (k in score_keys) {
        expect_s3_class(out[[k]], "data.frame")
        expect_gt(nrow(out[[k]]), 0L)
      }
    }
  )
})

test_that("mod_export_pti_data_server: per-admin score slots are reversed (finer first)", {
  # `get_pti_scores_export()` walks `get_current_levels()` (admin1
  # then admin2) but the wrapper applies `rev()` so admin2 (the finer
  # level) appears first in the final list — what an Excel writer
  # would naturally show top-to-bottom.
  inp <- .build_export_inputs(
    admins = list(admin1 = "Oblast", admin2 = "Raion")
  )
  testServer(
    mod_export_pti_data_server,
    args = list(
      plotted_dta = reactive(inp$plotted_dta),
      weights_dta = reactive(inp$weights_dta)
    ),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      score_keys <- grep(" PTI Scores$", names(out), value = TRUE)
      expect_equal(score_keys, c("Raion PTI Scores", "Oblast PTI Scores"))
    }
  )
})

# ---------------------------------------------------------------------------
# req() guard
# ---------------------------------------------------------------------------

test_that("mod_export_pti_data_server: NULL plotted_dta -> reactive does not emit", {
  # The body opens with `req(plotted_dta()); req(weights_dta())`, so
  # the reactive halts before producing a value. `getReturned()()`
  # therefore errors with the silent shiny "evaluation halted" path.
  inp <- .build_export_inputs()
  testServer(
    mod_export_pti_data_server,
    args = list(
      plotted_dta = reactive(NULL),
      weights_dta = reactive(inp$weights_dta)
    ),
    expr = {
      session$flushReact()
      expect_error(session$getReturned()())
    }
  )
})
