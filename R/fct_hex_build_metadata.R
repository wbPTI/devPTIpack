#' Build the hex-pipeline metadata Excel workbook
#'
#' Takes the tibble list from [aggregate_hex_to_shapes()] and writes a
#' `metadata-hex.xlsx` workbook in the same format as the Step-3 user
#' metadata template. Metadata for registry variables is auto-populated
#' from `inst/hex_vars_registry.yaml`; non-registry indicators must be
#' declared in `indicator_config`.
#'
#' Population is excluded from the output by default (`include_population
#' = FALSE`); set it to `TRUE` to include population as a visible map
#' layer. When the hex grid exceeds 5,000 cells and `include_hex` is not
#' supplied, the function prompts interactively (default N) or silently
#' excludes the hex sheet in non-interactive sessions.
#'
#' @param aggregated Named list of tibbles as returned by
#'   [aggregate_hex_to_shapes()]. Each tibble is one admin level
#'   (including `admin9_Hexagon`).
#' @param shp_dta Named list of sf tibbles (or plain tibbles), the same
#'   list that was passed to [aggregate_hex_to_shapes()]. Used only to
#'   identify the hex-level slot name when it is absent from `aggregated`.
#' @param indicator_config Optional tibble. Must contain a
#'   `canonical_name` column matching the stem of each non-registry
#'   indicator (e.g. `"conflict"` for `conflict_2020`). Additional
#'   columns (`var_name`, `var_description`, `var_units`, `pillar_group`,
#'   `pillar_name`, `legend_revert_colours`, `fltr_exclude_pti`,
#'   `pillar_description`) override registry defaults when the indicator
#'   is also in the registry; a warning is emitted per override.
#' @param country_name Character. Written to the `general` sheet.
#' @param output_path Character. Filesystem path for the output
#'   `.xlsx` file. Directories must already exist.
#' @param include_hex Logical. `TRUE` includes the `admin9_Hexagon` sheet;
#'   `FALSE` omits it. When not supplied and the hex grid has more than
#'   5,000 cells: interactive sessions prompt (default N); non-interactive
#'   sessions warn and default to `FALSE`.
#' @param include_population Logical (default `FALSE`). Set to `TRUE` to
#'   write `population` as a visible indicator row in `metadata` and as a
#'   column in all admin sheets.
#'
#' @return Invisibly, `output_path`. The xlsx is validated with
#'   [validate_read_metadata()] before returning; an error is thrown on
#'   validation failure.
#'
#' @importFrom writexl write_xlsx
#' @importFrom cli cli_abort cli_warn cli_alert_info cli_alert_success
#' @importFrom tibble tibble as_tibble
#' @importFrom glue glue
#' @family data-input
#' @export
#'
#' @examples
#' \dontrun{
#' my_shp    <- readRDS("app-data/shapes.rds")
#' hex_layer <- my_shp$admin9_Hexagon
#' vars      <- use_hex_vars("flood_exposure_15cm_1in100")
#' hex_data  <- fetch_hex_data(hex_layer$admin9Pcod, vars)
#' aggregated <- aggregate_hex_to_shapes(
#'   hex_data  = hex_data,
#'   hex_layer = hex_layer,
#'   shp_dta   = my_shp,
#'   strategy  = list(.default = c(weight = "pop", fun = "mean"),
#'                    flood_exposure_15cm_1in100 = c(weight = "none", fun = "sum"))
#' )
#' build_hex_metadata(
#'   aggregated   = aggregated,
#'   shp_dta      = my_shp,
#'   country_name = "Rwanda",
#'   output_path  = "app-data/metadata-hex.xlsx",
#'   include_hex  = FALSE
#' )
#' }
build_hex_metadata <- function(aggregated, shp_dta, indicator_config = NULL,
                                country_name, output_path,
                                include_hex, include_population = FALSE) {

  # ── 1. Input validation ──────────────────────────────────────────────
  if (!is.list(aggregated) || length(aggregated) == 0L) {
    cli::cli_abort("{.arg aggregated} must be a non-empty named list of tibbles.")
  }
  if (!is.character(country_name) || length(country_name) != 1L || !nzchar(country_name)) {
    cli::cli_abort("{.arg country_name} must be a non-empty string.")
  }
  if (!is.character(output_path) || length(output_path) != 1L || !nzchar(output_path)) {
    cli::cli_abort("{.arg output_path} must be a non-empty string.")
  }
  if (!is.null(indicator_config)) {
    if (!is.data.frame(indicator_config) ||
        !"canonical_name" %in% names(indicator_config)) {
      cli::cli_abort(c(
        "{.arg indicator_config} must be a data frame with a {.col canonical_name} column.",
        "i" = "Each row declares metadata for one non-registry (or override) indicator."
      ))
    }
  }

  # ── 2. Identify hex level ────────────────────────────────────────────
  slot_lvls <- as.integer(sub("admin([0-9]+).*", "\\1", names(aggregated)))
  slot_lvls[is.na(slot_lvls)] <- 0L
  hex_slot  <- names(aggregated)[which.max(slot_lvls)]

  # ── 3. Registry lookup ───────────────────────────────────────────────
  reg_lookup  <- hex_meta_registry_lookup()
  reg_version <- attr(reg_lookup, "registry_version")

  # ── 4. Resolve indicator columns ────────────────────────────────────
  ref_tbl    <- aggregated[[hex_slot]]
  struct_pat <- "^admin[0-9]+(Pcod|Name)$"
  all_cols   <- setdiff(names(ref_tbl),
                        c(grep(struct_pat, names(ref_tbl), value = TRUE), "area"))
  if (!include_population) {
    all_cols <- setdiff(all_cols, "population")
  }
  if (length(all_cols) == 0L) {
    cli::cli_abort("No indicator columns found in {.arg aggregated} (after population filter).")
  }

  # ── 5. Handle include_hex ────────────────────────────────────────────
  if (missing(include_hex)) {
    n_hex <- nrow(aggregated[[hex_slot]])
    if (n_hex > 5000L) {
      if (is_interactive()) {
        cli::cli_warn(
          "Hex grid has {.val {n_hex}} cells (>5,000). App performance may suffer."
        )
        ans <- utils::menu(c("Yes", "No"),
                           title = "Include hex level in metadata-hex.xlsx?")
        include_hex <- ans == 1L
      } else {
        cli::cli_warn(c(
          "!" = "Hex grid has {.val {n_hex}} cells (>5,000). Hex-level data excluded.",
          "i" = "Set {.code include_hex = TRUE} to override."
        ))
        include_hex <- FALSE
      }
    } else {
      include_hex <- TRUE
    }
  }

  # ── 6. Build metadata rows (one per output column) ───────────────────
  meta_rows <- lapply(seq_along(all_cols), function(i) {
    col  <- all_cols[[i]]
    stem <- sub("_[0-9]{4}$", "", col)
    year <- if (stem != col) as.integer(sub(".*_([0-9]{4})$", "\\1", col)) else NA_integer_

    reg  <- reg_lookup[[stem]] %||% reg_lookup[[col]]
    usr  <- hex_meta_user_row(indicator_config, stem, col)

    if (is.null(reg) && is.null(usr)) {
      cli::cli_abort(c(
        "Indicator {.val {col}} is not in the registry and has no entry in \\
{.arg indicator_config}.",
        "i" = "Add a row with {.code canonical_name = {dQuote(stem)}} to \\
{.arg indicator_config}."
      ))
    }

    if (!is.null(reg) && !is.null(usr)) {
      cli::cli_warn(
        "{.arg indicator_config} overrides registry defaults for {.val {stem}}."
      )
    }

    meta <- hex_meta_merge(reg, usr)

    vname <- meta$var_name %||% col
    if (!is.na(year) && grepl("\\{year\\}", vname)) {
      vname <- as.character(glue::glue(vname, year = year))
    }

    tibble::tibble(
      var_code              = col,
      var_name              = vname,
      var_description       = as.character(meta$var_description %||% NA_character_),
      var_order             = i,
      var_units             = as.character(meta$var_units %||% NA_character_),
      spatial_level         = hex_slot,
      pillar_group          = as.character(meta$pillar_group %||% NA_character_),
      pillar_name           = as.character(meta$pillar_name %||% NA_character_),
      pillar_description    = as.character(meta$pillar_description %||% NA_character_),
      fltr_exclude_pti      = isTRUE(meta$fltr_exclude_pti),
      fltr_exclude_explorer = FALSE,
      fltr_overlay_pti      = FALSE,
      fltr_overlay_explorer = FALSE,
      legend_revert_colours = isTRUE(meta$legend_revert_colours)
    )
  })
  metadata_sheet <- do.call(rbind, meta_rows)

  # ── 7. Build admin sheets ────────────────────────────────────────────
  sheets <- list()

  sheets[["general"]] <- tibble::tibble(
    country          = country_name,
    registry_version = as.character(reg_version %||% NA_character_)
  )

  for (slot in names(aggregated)) {
    if (!include_hex && identical(slot, hex_slot)) next

    tbl <- aggregated[[slot]]
    lvl <- as.integer(sub("admin([0-9]+).*", "\\1", slot))

    pcod_cols <- grep(struct_pat, names(tbl), value = TRUE)
    pcod_cols <- pcod_cols[grepl("Pcod", pcod_cols)]
    name_cols <- pcod_cols
    name_cols <- grep("Name$", names(tbl), value = TRUE)
    pcod_cols <- grep("Pcod$", names(tbl), value = TRUE)

    pcod_lvls <- as.integer(sub("admin([0-9]+)Pcod", "\\1", pcod_cols))
    pcod_cols <- pcod_cols[order(pcod_lvls)]

    data_cols  <- intersect(all_cols, names(tbl))
    sheet_cols <- c(pcod_cols, name_cols)
    sheet_tbl  <- as.data.frame(tbl[, sheet_cols, drop = FALSE],
                                 stringsAsFactors = FALSE)
    sheet_tbl$year <- NA_character_
    for (dc in data_cols) sheet_tbl[[dc]] <- tbl[[dc]]

    sheets[[slot]] <- tibble::as_tibble(sheet_tbl)
  }

  sheets[["metadata"]] <- metadata_sheet

  # ── 8. Write Excel ───────────────────────────────────────────────────
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    cli::cli_abort(
      "Output directory {.path {output_dir}} does not exist.",
      "i" = "Create it with {.code dir.create('{output_dir}', recursive = TRUE)}."
    )
  }

  writexl::write_xlsx(sheets, output_path)
  cli::cli_alert_success("Written {.file {output_path}}.")
  cli::cli_alert_info(
    "Review {.field fltr_exclude_pti} in the metadata sheet for any \\
display-only indicators."
  )

  # ── 9. Validate ──────────────────────────────────────────────────────
  validate_read_metadata(output_path, error_on_fail = TRUE)

  invisible(output_path)
}


# ── Internal helpers ─────────────────────────────────────────────────────────

# Build a named list (canonical_name -> list) for all registry variables.
hex_meta_registry_lookup <- function() {
  sources  <- read_hex_registry()
  rv       <- attr(sources, "registry_version")
  out      <- list()

  for (src_id in names(sources)) {
    s <- sources[[src_id]]
    for (canon in names(s$vars)) {
      v <- s$vars[[canon]]
      out[[canon]] <- list(
        var_name              = v$var_name,
        var_description       = v$var_description,
        var_units             = v$var_units,
        pillar_group          = v$pillar_group,
        pillar_name           = v$pillar_name,
        legend_revert_colours = isTRUE(v$legend_revert_colours),
        fltr_exclude_pti      = isTRUE(v$fltr_exclude_pti),
        pillar_description    = NA_character_
      )
    }
  }

  attr(out, "registry_version") <- rv
  out
}


# Extract the indicator_config row for a given stem or full column name.
# Returns a named list or NULL.
hex_meta_user_row <- function(indicator_config, stem, col) {
  if (is.null(indicator_config)) return(NULL)
  idx <- which(indicator_config$canonical_name == stem |
               indicator_config$canonical_name == col)
  if (length(idx) == 0L) return(NULL)
  as.list(indicator_config[idx[[1L]], setdiff(names(indicator_config), "canonical_name"),
                            drop = FALSE])
}


# Merge registry and user metadata; user wins on any field it provides.
hex_meta_merge <- function(reg, usr) {
  out <- if (!is.null(reg)) reg else list()
  if (is.null(usr)) return(out)
  for (field in names(usr)) {
    val <- usr[[field]]
    if (!is.null(val) && length(val) == 1L && !is.na(val)) {
      out[[field]] <- val
    }
  }
  out
}
