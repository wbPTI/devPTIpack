# Tier-2 module-server tests for mod_get_admin_levels_srv (arch-03 §2.4).
# Source: R/mod_pti_map_side_pan.R. Permanent function per arch-01.
#
# This module narrows the named admin-level vector that the calc
# pipeline produces (e.g. `c(admin1 = "Region1", admin2 = "Region2")`)
# down to the levels the user actually wants to plot. Filtering happens
# either via `default_adm_level` (precedence) or `show_adm_levels`,
# matched against either *names* (admin keys) or *values* (display
# names). The legacy radioButtons code path is commented out — only
# the reactive-return path is exercised here.
#
# arch-03 §2.4 cases: default selection, show_adm_levels filtering,
# update on data change. The non-matching-default fallback ("return
# last element") is also pinned because it's an easy regression.

suppressPackageStartupMessages(library(shiny))

# ---------------------------------------------------------------------------
# Default selection
# ---------------------------------------------------------------------------

test_that("mod_get_admin_levels_srv: no filters -> returns cur_levels unchanged", {
  cur <- c(admin1 = "Region1", admin2 = "Region2", admin3 = "Region3")
  testServer(
    mod_get_admin_levels_srv,
    args = list(cur_levels = reactive(cur)),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      expect_identical(out, cur)
    }
  )
})

test_that("mod_get_admin_levels_srv: default_adm_level matches admin key -> filters by name", {
  cur <- c(admin1 = "Region1", admin2 = "Region2", admin3 = "Region3")
  testServer(
    mod_get_admin_levels_srv,
    args = list(cur_levels = reactive(cur), default_adm_level = "admin2"),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      expect_identical(out, c(admin2 = "Region2"))
    }
  )
})

test_that("mod_get_admin_levels_srv: default_adm_level matches display value -> filters by value", {
  cur <- c(admin1 = "Region1", admin2 = "Region2")
  testServer(
    mod_get_admin_levels_srv,
    args = list(cur_levels = reactive(cur), default_adm_level = "Region1"),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      expect_identical(out, c(admin1 = "Region1"))
    }
  )
})

test_that("mod_get_admin_levels_srv: default_adm_level == 'All' -> no filtering", {
  cur <- c(admin1 = "Region1", admin2 = "Region2", admin3 = "Region3")
  testServer(
    mod_get_admin_levels_srv,
    args = list(cur_levels = reactive(cur), default_adm_level = "All"),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      expect_identical(out, cur)
    }
  )
})

test_that("mod_get_admin_levels_srv: 'all' default match is case-insensitive", {
  cur <- c(admin1 = "Region1", admin2 = "Region2")
  testServer(
    mod_get_admin_levels_srv,
    args = list(cur_levels = reactive(cur), default_adm_level = "all"),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      expect_identical(out, cur)
    }
  )
})

test_that("mod_get_admin_levels_srv: default_adm_level matches nothing -> last element only", {
  cur <- c(admin1 = "Region1", admin2 = "Region2", admin3 = "Region3")
  testServer(
    mod_get_admin_levels_srv,
    args = list(cur_levels = reactive(cur), default_adm_level = "nonexistent"),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      expect_identical(out, c(admin3 = "Region3"))
    }
  )
})

# ---------------------------------------------------------------------------
# show_adm_levels filtering
# ---------------------------------------------------------------------------

test_that("mod_get_admin_levels_srv: show_adm_levels filters by name", {
  cur <- c(admin1 = "Region1", admin2 = "Region2", admin3 = "Region3")
  testServer(
    mod_get_admin_levels_srv,
    args = list(
      cur_levels      = reactive(cur),
      show_adm_levels = c("admin1", "admin3")
    ),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      expect_identical(out, c(admin1 = "Region1", admin3 = "Region3"))
    }
  )
})

test_that("mod_get_admin_levels_srv: show_adm_levels filters by value", {
  cur <- c(admin1 = "Region1", admin2 = "Region2")
  testServer(
    mod_get_admin_levels_srv,
    args = list(
      cur_levels      = reactive(cur),
      show_adm_levels = "Region2"
    ),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      expect_identical(out, c(admin2 = "Region2"))
    }
  )
})

test_that("mod_get_admin_levels_srv: single non-matching show_adm_level -> last element only", {
  cur <- c(admin1 = "Region1", admin2 = "Region2")
  testServer(
    mod_get_admin_levels_srv,
    args = list(
      cur_levels      = reactive(cur),
      show_adm_levels = "admin99"
    ),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      expect_identical(out, c(admin2 = "Region2"))
    }
  )
})

test_that("mod_get_admin_levels_srv: default_adm_level wins over show_adm_levels", {
  # When both are set and `default_adm_level` matches, `show_adm_levels`
  # is ignored. (The source's else-branch on `isTruthy(default_adm_level)`
  # gates the show_adm_levels logic.)
  cur <- c(admin1 = "Region1", admin2 = "Region2", admin3 = "Region3")
  testServer(
    mod_get_admin_levels_srv,
    args = list(
      cur_levels        = reactive(cur),
      default_adm_level = "admin2",
      show_adm_levels   = c("admin1", "admin3")
    ),
    expr = {
      session$flushReact()
      out <- session$getReturned()()
      expect_identical(out, c(admin2 = "Region2"))
    }
  )
})

# ---------------------------------------------------------------------------
# Update on data change
# ---------------------------------------------------------------------------

test_that("mod_get_admin_levels_srv: cur_levels change -> output updates", {
  rv <- reactiveVal(c(admin1 = "Region1", admin2 = "Region2"))
  testServer(
    mod_get_admin_levels_srv,
    args = list(cur_levels = reactive(rv())),
    expr = {
      session$flushReact()
      first <- session$getReturned()()
      expect_identical(first, c(admin1 = "Region1", admin2 = "Region2"))

      rv(c(admin3 = "X", admin4 = "Y"))
      session$flushReact()
      second <- session$getReturned()()
      expect_identical(second, c(admin3 = "X", admin4 = "Y"))
    }
  )
})
