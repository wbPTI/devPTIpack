#' Compile PTI deployment artefacts from intermediate files
#'
#' Final stage of the PTI build pipeline. Reads the compiled shapes
#' RDS plus one or more intermediate metadata Excel files (typically
#' `metadata-user.xlsx` from Step 3 and / or `metadata-hex.xlsx` from
#' Step 4), merges them into a single canonical metadata workbook,
#' validates the combined inputs, renders the printable indicator
#' atlas (`pti-metadata.pdf`), and bundles the boundary GeoJSONs into
#' `shapefiles.zip` so the deployment package is a single self-
#' contained directory.
#'
#' Used by Step 5 of the deployer template
#' (`inst/template_pti/05-compile.qmd`). The CLI summary mirrors the
#' style of the existing validators: a `cli_h1` per phase, alerts as
#' work proceeds, and a final `cli_alert_success` / `cli_alert_danger`
#' depending on the validators' verdict.
#'
#' ## Merge contract
#'
#' For 1+ inputs:
#'
#' - The `general` sheet is taken from the **first** input file
#'   (assumption: same country across all inputs).
#' - The `metadata` sheet is row-bound across all inputs and
#'   deduplicated by `var_code` (last writer wins on conflict; a
#'   warning is emitted naming the duplicates).
#' - Each `admin<N>_*` sheet is `dplyr::full_join`ed across all input
#'   files on the admin-code / area / year keys, so an indicator
#'   present at a level in only one input is preserved alongside
#'   indicators from the other inputs.
#' - The `weights_table` sheet, if any input has it with non-zero
#'   rows, is taken from the first such input (others ignored;
#'   warning emitted if multiple).
#'
#' ## Output files (in `output_dir`)
#'
#' - `metadata.xlsx` -- canonical merged metadata workbook, readable
#'   by [fct_template_reader()].
#' - `shapefiles.zip` -- one GeoJSON per admin level (filename =
#'   slot name, e.g. `admin1_Province.geojson`).
#' - `pti-metadata.pdf` -- rendered from the bundled
#'   `inst/metadata.Rmd` template; one choropleth map per indicator.
#'   Skipped (with a warning) when LaTeX is not available -- the rest
#'   of the artefacts still produced.
#'
#' @param shp_path Character. Path to the compiled shapes `.rds`
#'   produced by Step 1 (`01-shapes.qmd`). Must be readable by
#'   [base::readRDS()] and validate via [validate_geometries()].
#' @param metadata_paths Character vector of length 1 or more --
#'   filesystem paths to intermediate metadata Excel files (typically
#'   the Step 3 output `metadata-user.xlsx` and / or the Step 4 output
#'   `metadata-hex.xlsx`). Each must be readable by
#'   [fct_template_reader()].
#' @param output_dir Character. Directory where the three output
#'   artefacts (`metadata.xlsx`, `shapefiles.zip`, `pti-metadata.pdf`)
#'   are written. Created if it does not exist.
#' @param error_on_fail Logical. When `TRUE` (the default), throws if
#'   either [validate_geometries()] or [validate_metadata()] reports
#'   `status = "fail"` on the combined inputs. When `FALSE`, returns
#'   the structured result and lets the caller decide (matches the
#'   existing validator convention).
#'
#' @return Invisibly, a list with the same shape as the existing
#'   validators:
#'   - `status` -- `"pass"` / `"warn"` / `"fail"`, the worse of the
#'     two underlying validators' statuses.
#'   - `summary` -- character vector of free-text summary lines.
#'   - `issues` -- list of all validation issues (concatenation of the
#'     two validators' `issues`).
#'   Plus three extra fields:
#'   - `metadata_path` -- path to the canonical merged xlsx.
#'   - `shapefiles_path` -- path to the shapefiles zip.
#'   - `pdf_path` -- path to the rendered PDF, or `NA_character_` if
#'     PDF rendering was skipped.
#'
#' @seealso [validate_geometries()], [validate_metadata()],
#'   [fct_template_reader()], [launch_pti()].
#'
#' @importFrom cli cli_h1 cli_h2 cli_alert_info cli_alert_success
#'   cli_alert_warning cli_alert_danger cli_bullets
#' @importFrom dplyr full_join
#' @importFrom purrr map map_dfr keep
#' @importFrom rlang is_missing
#' @importFrom sf st_write
#' @importFrom stats setNames
#' @importFrom writexl write_xlsx
#' @importFrom zip zipr
#' @importFrom rmarkdown render
#' @family pti-pipeline
#' @export
#'
#' @examples
#' \dontrun{
#' # After running Steps 1 + 3 of the template:
#' compile_pti_data(
#'   shp_path        = "app-data/shapes.rds",
#'   metadata_paths  = "app-data/metadata-user.xlsx",
#'   output_dir      = "app-data"
#' )
#'
#' # Combine user metadata + HEX metadata into a single deployment bundle:
#' compile_pti_data(
#'   shp_path        = "app-data/shapes.rds",
#'   metadata_paths  = c("app-data/metadata-user.xlsx",
#'                       "app-data/metadata-hex.xlsx"),
#'   output_dir      = "app-data"
#' )
#' }
compile_pti_data <- function(
  shp_path,
  metadata_paths,
  output_dir,
  error_on_fail = TRUE
) {
  # ----- 1) input gate -----------------------------------------------------

  if (
    rlang::is_missing(shp_path) ||
      !is.character(shp_path) ||
      length(shp_path) != 1L ||
      !nzchar(shp_path)
  ) {
    stop("'shp_path' must be a single non-empty character path.", call. = FALSE)
  }
  if (!file.exists(shp_path)) {
    stop("'shp_path' does not exist: ", shp_path, call. = FALSE)
  }
  if (
    rlang::is_missing(metadata_paths) ||
      !is.character(metadata_paths) ||
      length(metadata_paths) == 0L ||
      any(!nzchar(metadata_paths))
  ) {
    stop(
      "'metadata_paths' must be a non-empty character vector of paths.",
      call. = FALSE
    )
  }
  missing_mtdt <- metadata_paths[!file.exists(metadata_paths)]
  if (length(missing_mtdt) > 0L) {
    stop(
      "'metadata_paths' entry does not exist: ",
      paste(missing_mtdt, collapse = ", "),
      call. = FALSE
    )
  }
  if (
    rlang::is_missing(output_dir) ||
      !is.character(output_dir) ||
      length(output_dir) != 1L ||
      !nzchar(output_dir)
  ) {
    stop(
      "'output_dir' must be a single non-empty character path.",
      call. = FALSE
    )
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  cli::cli_h1("Compiling PTI deployment artefacts")

  # ----- 2) read inputs ----------------------------------------------------

  shp_dta <- readRDS(shp_path)
  cli::cli_alert_info(
    "Read shapes: {length(shp_dta)} layer{?s} ({sum(vapply(shp_dta, NROW, integer(1)))} polygons total)."
  )

  parsed_inputs <- purrr::map(metadata_paths, fct_template_reader)
  cli::cli_alert_info("Read metadata: {length(metadata_paths)} input file{?s}.")

  # Derive source labels from file basenames.
  source_labels <- sub(
    "\\.xlsx$",
    "",
    basename(metadata_paths),
    ignore.case = TRUE
  )
  source_labels <- sub("^metadata-", "", source_labels)

  # ----- 3) merge ----------------------------------------------------------

  merged <- compile_merge_metadata(parsed_inputs, source_labels)
  n_indicators <- NROW(merged$metadata)
  n_pillars <- length(unique(merged$metadata$pillar_group))
  cli::cli_alert_info(
    "Merged metadata: {n_indicators} indicator{?s} across {n_pillars} pillar{?s}."
  )

  # ----- 4) write canonical metadata.xlsx ----------------------------------

  out_xlsx <- file.path(output_dir, "metadata.xlsx")
  compile_write_metadata_xlsx(merged, out_xlsx)
  cli::cli_alert_success("Wrote {.file metadata.xlsx}.")

  # ----- 5) write shapefiles.zip -------------------------------------------

  out_zip <- file.path(output_dir, "shapefiles.zip")
  compile_write_shapefiles_zip(shp_dta, out_zip)
  cli::cli_alert_success("Wrote {.file shapefiles.zip}.")

  # ----- 6) validate combined inputs ---------------------------------------

  cli::cli_h2("Validating combined inputs")
  geom_diag <- validate_geometries(shp_dta, error_on_fail = FALSE)
  mtdt_diag <- validate_metadata(
    shp_path = shp_path,
    mtdt_path = out_xlsx,
    error_on_fail = FALSE
  )

  # Combined status: worst-of-two.
  combined_status <- pick_worst_status(geom_diag$status, mtdt_diag$status)

  # ----- 7) render PDF (best-effort) ---------------------------------------

  cli::cli_h2("Rendering pti-metadata.pdf")
  pdf_path <- tryCatch(
    compile_render_metadata_pdf(
      shp_path = shp_path,
      mtdt_path = out_xlsx,
      output_dir = output_dir
    ),
    error = function(e) {
      cli::cli_alert_warning(
        "PDF render skipped: {conditionMessage(e)}"
      )
      NA_character_
    }
  )
  if (!is.na(pdf_path) && file.exists(pdf_path)) {
    cli::cli_alert_success("Wrote {.file pti-metadata.pdf}.")
  }

  # ----- 8) summary --------------------------------------------------------

  summary_lines <- c(
    paste0("Layers: ", length(shp_dta)),
    paste0("Polygons (total): ", sum(vapply(shp_dta, NROW, integer(1)))),
    paste0("Indicators: ", n_indicators),
    paste0("Pillars: ", n_pillars),
    paste0("Metadata sources: ", length(metadata_paths))
  )

  cli::cli_h2("Summary")
  cli::cli_bullets(setNames(summary_lines, rep("*", length(summary_lines))))

  if (combined_status == "pass") {
    cli::cli_alert_success("compile_pti_data: all checks passed.")
  } else if (combined_status == "warn") {
    cli::cli_alert_warning(
      "compile_pti_data: passed with warnings ({length(geom_diag$issues) + length(mtdt_diag$issues)} issue{?s})."
    )
  } else {
    cli::cli_alert_danger(
      "compile_pti_data: validation failed."
    )
  }

  result <- list(
    status = combined_status,
    summary = summary_lines,
    issues = c(geom_diag$issues, mtdt_diag$issues),
    metadata_path = out_xlsx,
    shapefiles_path = out_zip,
    pdf_path = pdf_path
  )

  if (isTRUE(error_on_fail) && combined_status == "fail") {
    stop(
      "compile_pti_data: validation failed. ",
      "Set `error_on_fail = FALSE` to inspect the structured result.",
      call. = FALSE
    )
  }

  invisible(result)
}


#' Merge per-input parsed metadata lists into a single canonical list
#'
#' See the merge contract under [compile_pti_data()]. Returns a list
#' with `general`, per-admin tibbles, `metadata`, and (optionally)
#' `weights_table`.
#'
#' When the same `var_code` appears in more than one input, both are
#' kept. Each is renamed `<var_code>__<source_label>` where
#' `source_label` is derived from the input file's basename by stripping
#' the leading `metadata-` prefix (if present) and the `.xlsx` extension
#' (e.g. `metadata-hex.xlsx` -> `hex`, `my-indicators.xlsx` ->
#' `my-indicators`). The same renaming is applied to the matching
#' indicator columns in every `admin<N>_*` sheet so column names stay
#' in sync with `var_code` values.
#'
#' @param parsed_list List of lists; each element is the return value
#'   of [fct_template_reader()] on one input file.
#' @param source_labels Character vector the same length as
#'   `parsed_list`; used to build disambiguation suffixes. Derived from
#'   file basenames by the caller.
#'
#' @return A single list of tibbles, ready to write via
#'   [writexl::write_xlsx()].
#' @noRd
compile_merge_metadata <- function(parsed_list, source_labels) {
  if (length(parsed_list) == 0L) {
    stop("internal: empty parsed_list passed to compile_merge_metadata.")
  }

  # general -- first wins.
  general <- parsed_list[[1]]$general

  # metadata -- identify colliding var_codes and suffix both sides.
  all_metadata <- mapply(
    function(p, lbl) {
      tbl <- p$metadata
      tbl[[".source_label"]] <- lbl
      tbl
    },
    parsed_list,
    source_labels,
    SIMPLIFY = FALSE
  )
  metadata_raw <- do.call(rbind, all_metadata)

  dup_codes <- unique(metadata_raw$var_code[
    duplicated(metadata_raw$var_code)
  ])

  # Per-input rename maps: var_code -> renamed_var_code (for collisions only)
  rename_maps <- vector("list", length(parsed_list))
  names(rename_maps) <- source_labels
  for (i in seq_along(source_labels)) {
    rename_maps[[i]] <- character(0L)
  }

  if (length(dup_codes) > 0L) {
    cli::cli_alert_warning(
      c(
        "Duplicate var_code{?s} across metadata inputs -- both kept with source suffix:",
        "*" = "{.val {dup_codes}}"
      )
    )
    for (code in dup_codes) {
      rows <- which(metadata_raw$var_code == code)
      for (r in rows) {
        lbl <- metadata_raw$.source_label[[r]]
        new_code <- paste0(code, "__", lbl)
        rename_maps[[lbl]][[code]] <- new_code
        metadata_raw$var_code[[r]] <- new_code
      }
    }
  }
  metadata_raw$.source_label <- NULL
  metadata <- metadata_raw

  # admin sheets -- full_join on key columns; apply same var_code renames.
  admin_slots <- unique(unlist(lapply(parsed_list, function(p) {
    grep("^admin\\d", names(p), value = TRUE)
  })))

  admin_combined <- list()
  for (slot in admin_slots) {
    parts <- mapply(
      function(p, lbl) {
        tbl <- p[[slot]]
        if (is.null(tbl)) {
          return(NULL)
        }
        # Rename any colliding indicator columns in this admin sheet.
        rmap <- rename_maps[[lbl]]
        if (length(rmap) > 0L) {
          cols_to_rename <- intersect(names(tbl), names(rmap))
          if (length(cols_to_rename) > 0L) {
            names(tbl)[match(cols_to_rename, names(tbl))] <-
              unname(rmap[cols_to_rename])
          }
        }
        tbl
      },
      parsed_list,
      source_labels,
      SIMPLIFY = FALSE
    )
    parts <- Filter(Negate(is.null), parts)
    if (length(parts) == 0L) {
      next
    }
    if (length(parts) == 1L) {
      admin_combined[[slot]] <- parts[[1L]]
      next
    }
    key_pattern <- "^(admin\\d+(Pcod|Name)|area|year)$"
    key_cols <- grep(key_pattern, names(parts[[1L]]), value = TRUE)
    if (length(key_cols) == 0L) {
      stop(
        "internal: no join keys found in slot '",
        slot,
        "' (expected admin<N>Pcod / admin<N>Name / area / year)."
      )
    }
    admin_combined[[slot]] <- Reduce(
      function(a, b) {
        shared <- intersect(intersect(names(a), names(b)), key_cols)
        dplyr::full_join(a, b, by = shared)
      },
      parts
    )
  }

  # weights_table -- first non-empty wins.
  weights_table <- NULL
  weights_count <- 0L
  for (p in parsed_list) {
    if (!is.null(p$weights_table) && NROW(p$weights_table) > 0L) {
      weights_count <- weights_count + 1L
      if (is.null(weights_table)) weights_table <- p$weights_table
    }
  }
  if (weights_count > 1L) {
    cli::cli_alert_warning(
      "Multiple inputs carry a non-empty weights_table; using the first."
    )
  }

  out <- c(
    list(general = general),
    admin_combined,
    list(metadata = metadata)
  )
  if (!is.null(weights_table)) {
    out$weights_table <- weights_table
  }
  out
}


#' Write the merged metadata list to an xlsx workbook
#'
#' Drops `weights_clean` (an internal field added by
#' [fct_template_reader()] that is recomputed from `weights_table` on
#' read) before writing.
#'
#' @param merged List as returned by [compile_merge_metadata()].
#' @param path Output path.
#'
#' @return The path, invisibly.
#' @noRd
compile_write_metadata_xlsx <- function(merged, path) {
  to_write <- merged
  to_write$weights_clean <- NULL
  writexl::write_xlsx(to_write, path = path)
  invisible(path)
}


#' Write a zip of GeoJSONs (one per admin layer) to `path`
#'
#' One GeoJSON per slot of `shp_dta`, file basename = slot name. The
#' GeoJSONs themselves are written to a tempdir then zipped, so the
#' caller never sees the staging directory.
#'
#' @param shp_dta Named list of `sf` tibbles.
#' @param path Output zip path.
#'
#' @return The path, invisibly.
#' @noRd
compile_write_shapefiles_zip <- function(shp_dta, path) {
  if (!is.list(shp_dta) || length(shp_dta) == 0L) {
    stop("internal: shp_dta must be a non-empty list of sf tibbles.")
  }
  tmpdir <- tempfile("pti-shapes-")
  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  geojson_paths <- vapply(
    seq_along(shp_dta),
    function(i) {
      out <- file.path(tmpdir, paste0(names(shp_dta)[i], ".geojson"))
      sf::st_write(
        shp_dta[[i]],
        out,
        driver = "GeoJSON",
        append = FALSE,
        quiet = TRUE
      )
      out
    },
    character(1)
  )

  zip::zipr(zipfile = path, files = geojson_paths)
  invisible(path)
}


#' Render the parameterised `inst/metadata.Rmd` to a PDF in `output_dir`
#'
#' Copies the bundled Rmd to a tempfile and renders it with the path
#' params filled in. Errors propagate to the caller, which wraps the
#' call in `tryCatch()` so PDF failure (e.g. LaTeX missing) does not
#' abort the rest of the compile.
#'
#' @param shp_path,mtdt_path Paths into the rendered Rmd's params.
#' @param output_dir Where to write `pti-metadata.pdf`.
#'
#' @return Path to the rendered PDF.
#' @noRd
compile_render_metadata_pdf <- function(shp_path, mtdt_path, output_dir) {
  rmd_template <- system.file("metadata.Rmd", package = "devPTIpack")
  if (!nzchar(rmd_template)) {
    stop("internal: metadata.Rmd not found in installed package.")
  }
  tmprmd <- tempfile(fileext = ".Rmd")
  file.copy(rmd_template, tmprmd, overwrite = TRUE)
  on.exit(unlink(tmprmd), add = TRUE)

  rmarkdown::render(
    input = tmprmd,
    output_format = "pdf_document",
    output_file = "pti-metadata.pdf",
    output_dir = normalizePath(output_dir, mustWork = TRUE),
    params = list(
      shp_path = normalizePath(shp_path, mustWork = TRUE),
      mtdt_path = normalizePath(mtdt_path, mustWork = TRUE)
    ),
    quiet = TRUE,
    envir = new.env()
  )

  file.path(output_dir, "pti-metadata.pdf")
}


#' Pick the "worse" of two validator status values
#'
#' Ordering: `pass` < `warn` < `fail`.
#'
#' @noRd
pick_worst_status <- function(a, b) {
  rank <- c(pass = 0L, warn = 1L, fail = 2L)
  worst <- max(rank[a], rank[b])
  names(rank)[match(worst, rank)]
}
