#' Resolve a requested year vector against the years a parquet actually has
#'
#' Pure helper used by `fetch_hex_data()` (arch-11 §"Fetching") to map
#' deployer-requested years to the nearest available years in the source
#' parquet, applying the resolution rules in arch-11 §"Year resolution":
#'
#' - Exact match → return as-is, no substitution recorded.
#' - Otherwise the nearest available year is used. On equidistant tie
#'   the **later** year wins.
#' - Maximum tolerance is **7 years**; beyond that, this function errors
#'   with the variable name, requested year, and the full list of
#'   available years.
#' - Duplicates produced by collapsing multiple requested years onto
#'   the same resolved year are de-duplicated, preserving the order in
#'   which they first appeared.
#'
#' This helper never emits warnings or hits the network. The caller
#' (`resolve_years_for_vars()`) is responsible for surfacing
#' substitutions and for sourcing `available_years` from
#' [get_available_years()] or a test stub.
#'
#' @param requested_years Integer vector. Years the deployer asked for.
#'   `NULL` is *not* an accepted input here -- the NULL branch is
#'   handled separately by [prompt_or_error_for_years()].
#' @param available_years Integer vector. Years actually present in the
#'   source parquet for this variable. Must be non-empty.
#' @param var_name Character. Variable name, used only to format error
#'   messages.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{`resolved`}{Integer vector of resolved years, de-duplicated
#'       and in the order produced by walking `requested_years`.}
#'     \item{`substitutions`}{A `data.frame` with columns `requested`
#'       and `resolved`, one row per *substituted* year. Empty when
#'       every requested year matched exactly.}
#'   }
#'
#' @seealso [resolve_years_for_vars()], [prompt_or_error_for_years()].
#'
#' @noRd
resolve_years <- function(requested_years, available_years, var_name) {
  if (length(available_years) == 0L) {
    stop(
      "Variable '", var_name, "' has no available years to resolve ",
      "against. Check the source parquet.",
      call. = FALSE
    )
  }
  if (length(requested_years) == 0L) {
    return(list(
      resolved = integer(0),
      substitutions = empty_substitutions()
    ))
  }
  requested_years <- as.integer(requested_years)
  available_years <- as.integer(available_years)

  resolved <- integer(length(requested_years))
  sub_req <- integer(0)
  sub_res <- integer(0)

  for (i in seq_along(requested_years)) {
    r <- requested_years[i]
    if (r %in% available_years) {
      resolved[i] <- r
      next
    }
    dists <- abs(available_years - r)
    min_d <- min(dists)
    if (min_d > 7L) {
      stop(
        "Variable '", var_name, "': requested year ", r,
        " is more than 7 years from any available year (",
        paste(sort(available_years), collapse = ", "), ").",
        call. = FALSE
      )
    }
    candidates <- available_years[dists == min_d]
    # Equidistant tie -> prefer the later year.
    pick <- max(candidates)
    resolved[i] <- pick
    sub_req <- c(sub_req, r)
    sub_res <- c(sub_res, pick)
  }

  # De-duplicate while preserving first-occurrence order.
  keep <- !duplicated(resolved)
  resolved <- resolved[keep]

  list(
    resolved = resolved,
    substitutions = data.frame(
      requested = sub_req,
      resolved = sub_res,
      stringsAsFactors = FALSE
    )
  )
}


# Empty-substitution frame shared by the two paths that need one.
empty_substitutions <- function() {
  data.frame(
    requested = integer(0),
    resolved = integer(0),
    stringsAsFactors = FALSE
  )
}


#' Resolve years across a `vars` list returned by `use_hex_vars()`
#'
#' Walks a list of `pti_hex_var` descriptors, runs [resolve_years()] on
#' each temporal variable (`time_col` not `NA`), stamps the resolved
#' years back onto `var$years`, and emits **one consolidated**
#' `cli::cli_warn()` summarising every substitution made. Non-temporal
#' variables are passed through untouched.
#'
#' Available years are looked up via [get_available_years()] unless the
#' caller supplies `available_years_lookup` -- a named list of integer
#' vectors keyed by the variable's *unsuffixed* canonical name. The
#' lookup parameter exists primarily so unit tests can stay
#' network-free; production code should leave it `NULL` and let the
#' resolver consult the parquet.
#'
#' When a variable's stored `years` is `integer(0)` (i.e. the deployer
#' did not pass a `years` hint), this function delegates to
#' [prompt_or_error_for_years()]: interactive sessions get a menu;
#' non-interactive sessions get an actionable error.
#'
#' @param vars A list of `pti_hex_var` objects, as returned by
#'   [use_hex_vars()].
#' @param available_years_lookup Optional named list of integer
#'   vectors. Keys are unsuffixed canonical names; values are the
#'   available years for that variable. When `NULL`,
#'   [get_available_years()] is called per variable.
#'
#' @return The input `vars` list with each temporal variable's
#'   `$years` replaced by the resolved years. Object class and order
#'   are preserved.
#'
#' @seealso [resolve_years()], [prompt_or_error_for_years()],
#'   [get_available_years()].
#'
#' @importFrom cli cli_warn
#' @noRd
resolve_years_for_vars <- function(vars, available_years_lookup = NULL) {
  if (!is.list(vars)) {
    stop("'vars' must be a list of pti_hex_var objects.", call. = FALSE)
  }
  all_subs <- list()

  for (nm in names(vars)) {
    v <- vars[[nm]]
    if (!inherits(v, "pti_hex_var")) {
      stop(
        "Entry '", nm, "' in 'vars' is not a pti_hex_var.",
        call. = FALSE
      )
    }
    if (is.na(v$time_col)) next  # non-temporal -> pass through

    lookup_key <- sub("__[^_].*$", "", v$canonical_name)
    available <- if (is.null(available_years_lookup)) {
      get_available_years(v)
    } else {
      available_years_lookup[[lookup_key]]
    }
    if (is.null(available) || length(available) == 0L) {
      stop(
        "Variable '", v$canonical_name, "' has no available years to ",
        "resolve against. Check the source parquet.",
        call. = FALSE
      )
    }
    available <- as.integer(available)

    if (length(v$years) == 0L) {
      v$years <- prompt_or_error_for_years(v$canonical_name, available)
    }

    out <- resolve_years(v$years, available, v$canonical_name)
    v$years <- out$resolved
    if (nrow(out$substitutions) > 0L) {
      # Keyed by canonical_name (user-facing label), not the list slot,
      # so the warning reads sensibly even when callers construct
      # vars-list keys that diverge from canonical_name.
      all_subs[[v$canonical_name]] <- out$substitutions
    }
    vars[[nm]] <- v
  }

  if (length(all_subs) > 0L) {
    lines <- vapply(names(all_subs), function(canon) {
      df <- all_subs[[canon]]
      pairs <- paste0(df$requested, " -> ", df$resolved, collapse = ", ")
      paste0("{.field ", canon, "}: ", pairs)
    }, character(1L))
    cli::cli_warn(c(
      "Some requested years had no exact match and were substituted:",
      stats::setNames(lines, rep("*", length(lines)))
    ))
  }

  vars
}


#' Prompt for years when the deployer passed `years = NULL`
#'
#' Interactive sessions get a `cli::cli_inform()` listing the available
#' years followed by a `utils::menu()` single-select prompt.
#' Non-interactive sessions get a `stop()` that names the variable and
#' tells the deployer to pass `years = ...`.
#'
#' v1 is intentionally single-select: nothing downstream in arch-11
#' currently consumes more than one resolved year per variable, and
#' adding multi-select via `readline()` parsing is cheap to layer on
#' later if a real use case emerges.
#'
#' @param var_name Character. Variable being resolved (used in messages).
#' @param available_years Integer vector. Years to choose from.
#'
#' @return Integer scalar -- the chosen year.
#'
#' @importFrom cli cli_inform
#' @importFrom utils menu
#' @noRd
is_interactive <- function() interactive()

prompt_or_error_for_years <- function(var_name, available_years) {
  available_years <- sort(as.integer(available_years))
  if (!is_interactive()) {
    stop(
      "No 'years' supplied for temporal variable '", var_name,
      "'. Pass years = c(...) (available: ",
      paste(available_years, collapse = ", "), ").",
      call. = FALSE
    )
  }
  cli::cli_inform(c(
    "No years supplied for {.field {var_name}}.",
    "i" = "Available years: {.val {available_years}}"
  ))
  pick <- utils::menu(
    choices = as.character(available_years),
    title = paste0("Choose a year for '", var_name, "':")
  )
  if (pick == 0L) {
    stop(
      "No year selected for '", var_name, "'.",
      call. = FALSE
    )
  }
  available_years[pick]
}
