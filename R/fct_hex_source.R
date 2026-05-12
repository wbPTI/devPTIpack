#' Construct a hex variable descriptor (S3)
#'
#' Internal S3 constructor for per-variable configuration in the hex
#' data registry. Carries everything `fetch_hex_data()`,
#' `aggregate_hex_to_shapes()`, and `build_hex_metadata()` need to know
#' about one variable without further registry lookups. Built by
#' [use_hex_vars()] from a parsed `inst/hex_vars_registry.yaml` entry;
#' deployers do not normally construct these directly.
#'
#' @param source_col Character. Column name in the parquet source.
#' @param canonical_name Character. The deployer-facing name (e.g.
#'   `"flood_exposure_15cm_1in100"`). When the same canonical name
#'   appears in multiple sources, [use_hex_vars()] disambiguates by
#'   appending `__<source-label>`.
#' @param var_name Character. Display name. For temporal variables
#'   this is a glue template with `{year}` (e.g.
#'   `"Night Lights ({year})"`).
#' @param var_description Character or `NA`. Long-form description for
#'   the indicator atlas / data explorer.
#' @param var_units Character or `NA`. Unit string (e.g. `"count"`,
#'   `"%"`).
#' @param pillar_group Integer or `NA`. Numeric pillar code; left `NA`
#'   when the deployer should assign it at
#'   `build_hex_metadata()` time.
#' @param pillar_name Character or `NA`. Pillar label paired with
#'   `pillar_group`.
#' @param legend_revert_colours Logical. `TRUE` when high values are
#'   bad (e.g. flood exposure, poverty rate).
#' @param fltr_exclude_pti Logical. `TRUE` to keep the variable out of
#'   the composite PTI score by default (display-only).
#' @param time_col Character or `NA`. Column carrying the year /
#'   period in the parquet, or `NA` for non-temporal snapshots.
#' @param years Integer vector. Resolved year(s) for this variable;
#'   `integer(0)` for non-temporal variables. Populated by
#'   `use_hex_vars()`'s per-variable year resolver (forthcoming in
#'   arch-11 §"Year resolution" / issue \href{https://github.com/worldbank/devPTIpack/issues/110}{#110}).
#' @param weight Character. Aggregation weight hint: one of
#'   `"pop"` / `"area"` / `"none"`. Documentation-only on the
#'   variable -- the deployer picks the actual strategy at
#'   `aggregate_hex_to_shapes()` time.
#' @param fun Character. Aggregation function hint: one of
#'   `"mean"` / `"median"` / `"sum"` / `"min"` / `"max"`.
#'   Documentation-only.
#' @param internal Logical. `TRUE` when the variable was auto-injected
#'   by [use_hex_vars()] (population) rather than requested by the
#'   deployer. `build_hex_metadata()` reads this tag to exclude
#'   population from the metadata Excel by default.
#'
#' @return A list with class `"pti_hex_var"`.
#'
#' @seealso [pti_hex_source()], [use_hex_vars()], [list_hex_vars()].
#'
#' @noRd
pti_hex_var <- function(
  source_col,
  canonical_name,
  var_name,
  var_description = NA_character_,
  var_units = NA_character_,
  pillar_group = NA_integer_,
  pillar_name = NA_character_,
  legend_revert_colours = FALSE,
  fltr_exclude_pti = FALSE,
  time_col = NA_character_,
  years = integer(0),
  weight = c("none", "pop", "area"),
  fun = c("mean", "median", "sum", "min", "max"),
  internal = FALSE
) {
  if (!is.character(source_col) || length(source_col) != 1L ||
        is.na(source_col) || !nzchar(source_col)) {
    stop("'source_col' must be a single non-empty character.", call. = FALSE)
  }
  if (!is.character(canonical_name) || length(canonical_name) != 1L ||
        is.na(canonical_name) || !nzchar(canonical_name)) {
    stop("'canonical_name' must be a single non-empty character.",
         call. = FALSE)
  }
  if (!is.character(var_name) || length(var_name) != 1L || is.na(var_name) ||
        !nzchar(var_name)) {
    stop("'var_name' must be a single non-empty character.", call. = FALSE)
  }

  weight <- match.arg(weight)
  fun <- match.arg(fun)

  if (!is.logical(legend_revert_colours) ||
        length(legend_revert_colours) != 1L) {
    stop("'legend_revert_colours' must be a single TRUE/FALSE.", call. = FALSE)
  }
  if (!is.logical(fltr_exclude_pti) || length(fltr_exclude_pti) != 1L) {
    stop("'fltr_exclude_pti' must be a single TRUE/FALSE.", call. = FALSE)
  }
  if (!is.logical(internal) || length(internal) != 1L) {
    stop("'internal' must be a single TRUE/FALSE.", call. = FALSE)
  }

  structure(
    list(
      source_col = source_col,
      canonical_name = canonical_name,
      var_name = var_name,
      var_description = if (is.null(var_description)) NA_character_
                        else as.character(var_description),
      var_units = if (is.null(var_units)) NA_character_
                  else as.character(var_units),
      pillar_group = if (is.null(pillar_group)) NA_integer_
                     else as.integer(pillar_group),
      pillar_name = if (is.null(pillar_name)) NA_character_
                    else as.character(pillar_name),
      legend_revert_colours = legend_revert_colours,
      fltr_exclude_pti = fltr_exclude_pti,
      time_col = if (is.null(time_col)) NA_character_
                 else as.character(time_col),
      years = as.integer(years),
      weight = weight,
      fun = fun,
      internal = internal
    ),
    class = "pti_hex_var"
  )
}


#' Construct a hex source descriptor (S3)
#'
#' Internal S3 constructor for one parquet endpoint -- the unit of
#' organisation in `inst/hex_vars_registry.yaml`. A source bundles a
#' parquet URL, the column in that parquet carrying the H3 cell index,
#' the list of `pti_hex_var` descriptors that source publishes, an
#' optional `pop_var` (the `pti_hex_var` describing the population
#' column in this source -- only one source across the registry may
#' set this), and a human-readable label.
#'
#' @param label Character. Human-readable source name shown in
#'   [list_hex_vars()] and used as the disambiguation suffix when two
#'   sources expose the same canonical name.
#' @param path Character. HTTPS URL of the parquet (or a partitioned
#'   directory path eventually).
#' @param hex_col Character. Column name in the parquet carrying the
#'   H3 cell index string.
#' @param vars Named list of `pti_hex_var` objects keyed by canonical
#'   name. May be empty (a source with no published variables is
#'   degenerate but tolerated for forward compatibility).
#' @param pop_var Either `NULL` or a `pti_hex_var` describing the
#'   population column in this source. Exactly one source across the
#'   full registry must declare a non-`NULL` `pop_var`; this is
#'   enforced by the registry reader, not by this constructor.
#'
#' @return A list with class `"pti_hex_source"`.
#'
#' @seealso [pti_hex_var()].
#'
#' @noRd
pti_hex_source <- function(label, path, hex_col, vars, pop_var = NULL) {
  if (!is.character(label) || length(label) != 1L || is.na(label) ||
        !nzchar(label)) {
    stop("'label' must be a single non-empty character.", call. = FALSE)
  }
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
        !nzchar(path)) {
    stop("'path' must be a single non-empty character.", call. = FALSE)
  }
  if (!is.character(hex_col) || length(hex_col) != 1L || is.na(hex_col) ||
        !nzchar(hex_col)) {
    stop("'hex_col' must be a single non-empty character.", call. = FALSE)
  }
  if (!is.list(vars)) {
    stop("'vars' must be a list of pti_hex_var objects.", call. = FALSE)
  }
  bad_vars <- !vapply(vars, inherits, logical(1L), what = "pti_hex_var")
  if (any(bad_vars)) {
    stop(
      "'vars' must contain only pti_hex_var objects; offending entries: ",
      paste(shQuote(names(vars)[bad_vars]), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (!is.null(pop_var) && !inherits(pop_var, "pti_hex_var")) {
    stop("'pop_var' must be NULL or a pti_hex_var.", call. = FALSE)
  }

  structure(
    list(
      label = label,
      path = path,
      hex_col = hex_col,
      vars = vars,
      pop_var = pop_var
    ),
    class = "pti_hex_source"
  )
}
