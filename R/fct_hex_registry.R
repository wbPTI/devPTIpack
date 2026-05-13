#' Read the bundled hex variable registry
#'
#' Loads `inst/hex_vars_registry.yaml` from the installed package
#' and converts each entry into the S3 descriptor objects
#' `pti_hex_source()` / `pti_hex_var()`. Used by [list_hex_vars()]
#' and [use_hex_vars()] -- deployers do not normally call this
#' directly.
#'
#' @return Named list of `pti_hex_source` objects, keyed by source-id.
#'
#' @noRd
#' @importFrom yaml read_yaml
read_hex_registry <- function() {
  path <- system.file("hex_vars_registry.yaml", package = "devPTIpack")
  if (!nzchar(path)) {
    stop(
      "Could not locate 'hex_vars_registry.yaml' in the installed ",
      "devPTIpack package. Reinstall the package.",
      call. = FALSE
    )
  }
  raw <- yaml::read_yaml(path)

  if (!is.list(raw$sources) || length(raw$sources) == 0L) {
    stop(
      "'hex_vars_registry.yaml' has no 'sources' entries; the ",
      "registry is unusable.",
      call. = FALSE
    )
  }

  sources <- lapply(names(raw$sources), function(src_id) {
    s <- raw$sources[[src_id]]

    vars_list <- lapply(names(s$variables), function(canon) {
      v <- s$variables[[canon]]
      pti_hex_var(
        source_col            = v$source_col,
        canonical_name        = canon,
        var_name              = v$var_name,
        var_description       = v$var_description,
        var_units             = v$var_units,
        pillar_group          = v$pillar_group,
        pillar_name           = v$pillar_name,
        legend_revert_colours = isTRUE(v$legend_revert_colours),
        fltr_exclude_pti      = isTRUE(v$fltr_exclude_pti),
        time_col              = v$time_col,
        years                 = if (is.null(v$available_years))
                                  integer(0)
                                else as.integer(v$available_years),
        weight                = v$weight %||% "none",
        fun                   = v$fun %||% "mean",
        internal              = FALSE
      )
    })
    names(vars_list) <- names(s$variables)

    pop_var_obj <- if (!is.null(s$pop_var) && nzchar(s$pop_var)) {
      pv <- vars_list[[s$pop_var]]
      if (is.null(pv)) {
        stop(
          "Source '", src_id, "' declares pop_var = '", s$pop_var,
          "' but no matching variable entry exists.",
          call. = FALSE
        )
      }
      pv$internal <- TRUE
      pv
    } else {
      NULL
    }

    pti_hex_source(
      label   = s$label,
      path    = s$path,
      hex_col = s$hex_col,
      vars    = vars_list,
      pop_var = pop_var_obj
    )
  })
  names(sources) <- names(raw$sources)

  # Enforce: exactly one source across the registry declares pop_var.
  n_pop <- sum(vapply(sources, function(s) !is.null(s$pop_var),
                      logical(1L)))
  if (n_pop != 1L) {
    stop(
      "Registry contract violated: exactly one source must declare ",
      "pop_var (got ", n_pop, ").",
      call. = FALSE
    )
  }

  attr(sources, "registry_version") <-
    if (is.null(raw$registry_version)) NA_character_
    else as.character(raw$registry_version)

  sources
}


# Internal default helper -- R's %||% landed in base 4.4 but we still
# support 4.5+, so this is a no-op safety net.
`%||%` <- function(a, b) if (is.null(a)) b else a


#' List the bundled hex variables available for the PTI pipeline
#'
#' Returns a tibble summarising every variable declared in the
#' bundled `inst/hex_vars_registry.yaml`. Call this at the top of
#' Step 4 to browse what indicators a deployer can pull from the
#' registry without leaving R. The output is the entry point that
#' decides what to pass to [use_hex_vars()].
#'
#' When the same canonical name appears in multiple sources, the
#' tibble shows one row per (source, canonical name) pair so the
#' deployer can pick the source they want; [use_hex_vars()] will then
#' disambiguate via a `__<source-label>` suffix on the
#' `canonical_name` column.
#'
#' @return A tibble with one row per (source, canonical name). Columns:
#'   `source_id`, `source_label`, `canonical_name`, `var_name`,
#'   `var_units`, `time_col`, `available_years` (a list-column),
#'   `weight`, `fun`, `is_population` (logical, `TRUE` for the
#'   registry-declared population variable).
#'
#' @seealso [use_hex_vars()] to resolve specific variables;
#'   [get_available_years()] to query the live parquet for ground
#'   truth on temporal coverage.
#'
#' @importFrom tibble tibble
#' @family data-input
#' @export
#'
#' @examples
#' list_hex_vars()
list_hex_vars <- function() {
  sources <- read_hex_registry()
  rows <- list()
  for (src_id in names(sources)) {
    s <- sources[[src_id]]
    pop_name <- if (is.null(s$pop_var)) NA_character_
                else s$pop_var$canonical_name
    for (canon in names(s$vars)) {
      v <- s$vars[[canon]]
      rows[[length(rows) + 1L]] <- tibble::tibble(
        source_id       = src_id,
        source_label    = s$label,
        canonical_name  = canon,
        var_name        = v$var_name,
        var_units       = v$var_units,
        time_col        = v$time_col,
        available_years = list(v$years),
        weight          = v$weight,
        fun             = v$fun,
        is_population   = identical(canon, pop_name)
      )
    }
  }
  if (length(rows) == 0L) {
    return(tibble::tibble(
      source_id       = character(0),
      source_label    = character(0),
      canonical_name  = character(0),
      var_name        = character(0),
      var_units       = character(0),
      time_col        = character(0),
      available_years = list(),
      weight          = character(0),
      fun             = character(0),
      is_population   = logical(0)
    ))
  }
  do.call(rbind, rows)
}


#' Resolve hex variable names against the registry
#'
#' Takes canonical variable names (unquoted, quoted, or both via
#' `...`) and returns a list of `pti_hex_var` descriptors ready to
#' pass to `fetch_hex_data()`. **Always injects the population
#' variable** (tagged `internal = TRUE`) regardless of what the
#' deployer requests -- downstream aggregation needs it.
#'
#' When the same canonical name appears in multiple sources, the
#' returned descriptor's `canonical_name` is suffixed with
#' `__<source-label>` (e.g. `poverty_rate__DEC`,
#' `poverty_rate__local`) so each becomes a distinct entry in the
#' returned list and a distinct row downstream in
#' `metadata-hex.xlsx`.
#'
#' @param ... Canonical variable names. Either bare symbols, quoted
#'   strings, or a mix. Duplicates are removed silently. Pass no names
#'   to get just the auto-injected population variable.
#' @param years Optional integer vector of requested years for
#'   temporal variables. Stored on each `pti_hex_var` for the year
#'   resolver in [fetch_hex_data()] (arch-11 §"Year resolution" /
#'   issue \href{https://github.com/worldbank/devPTIpack/issues/110}{#110})
#'   to consult; this function does **not** resolve nearest-year
#'   substitutions itself.
#'
#' @return A list of `pti_hex_var` objects, named by their (possibly
#'   suffixed) canonical name. Population always appears in the list,
#'   tagged `internal = TRUE`.
#'
#' @seealso [list_hex_vars()] to browse what is available;
#'   [get_available_years()] to confirm temporal coverage.
#'
#' @importFrom rlang ensyms
#' @family data-input
#' @export
#'
#' @examples
#' \dontrun{
#' # Resolve a single variable; population is auto-injected.
#' vars <- use_hex_vars("flood_exposure_15cm_1in100")
#' names(vars)
#' # Pass a year hint for temporal variables (year resolver runs in
#' # fetch_hex_data() per arch-11 #110).
#' vars <- use_hex_vars("flood_exposure_15cm_1in100", years = 2020L)
#' }
use_hex_vars <- function(..., years = NULL) {

  # Accept bare symbols, character strings, or both.
  exprs <- rlang::ensyms(...)
  requested <- vapply(exprs, rlang::as_string, character(1L))
  requested <- unique(requested[nzchar(requested)])

  if (!is.null(years)) {
    if (!is.numeric(years) || any(is.na(years))) {
      stop("'years' must be a numeric vector of years (no NA).",
           call. = FALSE)
    }
    years <- as.integer(years)
  }

  sources <- read_hex_registry()

  # Build a lookup of canonical_name -> list(source_id, source_label)
  # so we can detect duplicates and add suffixes.
  occurrences <- list()
  for (src_id in names(sources)) {
    s <- sources[[src_id]]
    for (canon in names(s$vars)) {
      occurrences[[canon]] <- c(
        occurrences[[canon]],
        list(list(src_id = src_id, src_label = s$label))
      )
    }
  }

  resolved <- list()
  pop_injected <- NULL

  # Locate the source whose pop_var is non-NULL (registry guarantees
  # exactly one).
  pop_src_id <- names(sources)[
    vapply(sources, function(s) !is.null(s$pop_var), logical(1L))
  ][1L]
  pop_src <- sources[[pop_src_id]]
  pop_var_obj <- pop_src$pop_var
  pop_canonical <- pop_var_obj$canonical_name
  if (!is.null(years)) {
    pop_var_obj$years <- years
  }
  pop_injected <- list(pop_var_obj)
  names(pop_injected) <- pop_canonical

  for (name in requested) {
    if (identical(name, pop_canonical)) {
      # Deployer explicitly requested population -- promote out of
      # the internal-injected slot (override the internal=TRUE tag).
      explicit_pop <- pop_var_obj
      explicit_pop$internal <- FALSE
      explicit_pop$path    <- pop_src$path
      explicit_pop$hex_col <- pop_src$hex_col
      resolved[[name]] <- explicit_pop
      pop_injected <- list()
      next
    }

    occ <- occurrences[[name]]
    if (is.null(occ)) {
      stop(
        "Unknown hex variable: '", name, "'. Use list_hex_vars() to ",
        "browse available variables.",
        call. = FALSE
      )
    }

    for (entry in occ) {
      src <- sources[[entry$src_id]]
      v <- src$vars[[name]]
      v$years  <- if (is.null(years)) v$years else years
      v$path   <- src$path
      v$hex_col <- src$hex_col

      out_name <- if (length(occ) > 1L) {
        paste0(name, "__", make_safe_label(entry$src_label))
      } else {
        name
      }
      v$canonical_name <- out_name
      resolved[[out_name]] <- v
    }
  }

  # Embed path/hex_col on the auto-injected population descriptor too.
  if (length(pop_injected) > 0L) {
    pop_injected[[1L]]$path    <- pop_src$path
    pop_injected[[1L]]$hex_col <- pop_src$hex_col
  }

  # Append the auto-injected population if the deployer didn't ask
  # for it explicitly.
  c(resolved, pop_injected)
}


# Internal helper -- collapse a source label to something safe to use
# as a __<suffix> on canonical names (no spaces / punctuation that
# would break xlsx column names downstream).
make_safe_label <- function(label) {
  s <- tolower(label)
  s <- gsub("[^a-z0-9]+", "_", s, perl = TRUE)
  s <- gsub("^_+|_+$", "", s, perl = TRUE)
  s
}


#' Query a parquet endpoint for the actual available years of a variable
#'
#' Reads the registry to find the parquet path and `time_col` for the
#' supplied variable, opens the dataset via [arrow::open_dataset()],
#' and returns the distinct values found in `time_col`. Useful when
#' the `available_years` hint in the YAML is stale (or absent) and
#' you want ground truth before passing `years = ...` to
#' [use_hex_vars()].
#'
#' Non-temporal variables (`time_col` is `NA`) return
#' `integer(0)`.
#'
#' @param var A single canonical variable name, or a `pti_hex_var`
#'   already resolved via [use_hex_vars()]. Required.
#'
#' @return Integer vector of years available in the source parquet,
#'   sorted ascending. `integer(0)` for non-temporal variables.
#'
#' @importFrom rlang is_missing
#' @family data-input
#' @export
#'
#' @examples
#' \dontrun{
#' # Non-temporal variable -> integer(0) without hitting the network.
#' get_available_years("flood_exposure_15cm_1in100")
#' }
get_available_years <- function(var) {

  if (rlang::is_missing(var) || is.null(var)) {
    stop(
      "'var' is required: pass a canonical variable name (character) ",
      "or a pti_hex_var returned by use_hex_vars().",
      call. = FALSE
    )
  }

  # Accept either a name or a resolved pti_hex_var.
  if (inherits(var, "pti_hex_var")) {
    canonical <- var$canonical_name
    # Strip any `__<source-label>` suffix added by use_hex_vars() for
    # the registry lookup.
    canonical <- sub("__[^_].*$", "", canonical)
  } else if (is.character(var) && length(var) == 1L && nzchar(var)) {
    canonical <- var
  } else {
    stop(
      "'var' must be a single canonical name or a pti_hex_var.",
      call. = FALSE
    )
  }

  sources <- read_hex_registry()
  match_src <- NULL
  match_var <- NULL
  for (src_id in names(sources)) {
    s <- sources[[src_id]]
    if (canonical %in% names(s$vars)) {
      match_src <- s
      match_var <- s$vars[[canonical]]
      break
    }
  }
  if (is.null(match_var)) {
    stop("Unknown hex variable: '", canonical, "'.", call. = FALSE)
  }

  if (is.na(match_var$time_col)) {
    return(integer(0))
  }

  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop(
      "Package 'arrow' is required to query live parquet endpoints. ",
      "Install it via install.packages('arrow').",
      call. = FALSE
    )
  }

  ds <- arrow::open_dataset(match_src$path)
  years <- sort(unique(dplyr::collect(
    ds[, match_var$time_col]
  )[[match_var$time_col]]))
  as.integer(years)
}
