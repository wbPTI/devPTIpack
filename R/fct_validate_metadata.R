#' Verify that a shapes file and metadata file together produce valid PTI scores
#'
#' End-to-end pre-flight validator for a PTI deployment. Calls
#' [validate_read_shp()] and [validate_read_metadata()] on the inputs,
#' then runs the full calculation pipeline ([pivot_pti_dta()] ->
#' [get_weighted_data()] -> [get_scores_data()] -> [expand_adm_levels()] ->
#' [agg_pti_scores()]) under an all-equal weighting and asserts that the
#' number of scored pillars matches `nrow(indicators_list)`. Emits one
#' cli alert per check; returns the structured `list(status, summary,
#' issues)` result invisibly. With `error_on_fail = TRUE` (default),
#' throws if any `fail`-level issue is recorded.
#'
#' @param shp_path Character. Path to an `.rds` file containing the
#'   shapes list (the on-disk form of objects shaped like [ukr_shp]).
#' @param mtdt_path Character. Path to the metadata `.xlsx` template
#'   (the on-disk form of [ukr_mtdt_full]).
#' @inheritParams validate_geometries
#'
#' @return Invisibly, a list with components `status` (`"pass"`,
#'   `"warn"`, or `"fail"`), `summary` (one-line string), and `issues`
#'   (list of issue records).
#'
#' @importFrom readr read_rds
#' @importFrom purrr map_dfr
#' @importFrom dplyr count
#' @importFrom cli cli_h1 cli_alert_success
#' @family validation
#' @export
#'
#' @examples
#' \dontrun{
#' validate_metadata(
#'   shp_path  = "path/to/shapes.rds",
#'   mtdt_path = "path/to/metadata.xlsx"
#' )
#' }
validate_metadata <- function(shp_path, mtdt_path, error_on_fail = TRUE) {

  issues <- new_issues()

  cli::cli_h1("Reading inputs")
  shp_res  <- validate_read_shp(shp_path,  error_on_fail = FALSE)
  mtdt_res <- validate_read_metadata(mtdt_path, error_on_fail = FALSE)
  issues   <- c(issues, shp_res$issues, mtdt_res$issues)

  if (any(vapply(issues, function(x) x$level == "fail", logical(1)))) {
    return(finalize_validation(issues, "validate_metadata", error_on_fail))
  }

  cli::cli_h1("Pipeline dry run")

  pipeline_ok <- tryCatch({
    shp_dta <- read_rds(shp_path)
    imp_dta <- fct_template_reader(mtdt_path)

    imp_dta$indicators_list <- get_indicators_list(imp_dta)
    long_vars <- imp_dta %>% pivot_pti_dta(imp_dta$indicators_list)

    existing_shapes <- shp_dta %>% clean_geoms()
    mt              <- shp_dta %>% get_mt()

    imp_dta$weights_clean <-
      imp_dta$indicators_list$var_code %>%
      get_all_weights_combs(1)

    calc_pti_at_once <-
      get_weighted_data(imp_dta$weights_clean, long_vars, imp_dta$indicators_list) %>%
      get_scores_data() %>%
      imap(~ expand_adm_levels(.x, mt) %>% merge_expandedn_adm_levels())

    na_agg <-
      calc_pti_at_once %>%
      agg_pti_scores(existing_shapes)

    nrow_pti <-
      na_agg %>%
      map_dfr(~ .x %>% count(.data$pti_name)) %>%
      count(.data$pti_name) %>%
      nrow()

    expected <- nrow(imp_dta$indicators_list)

    if (nrow_pti != expected) {
      issues <<- emit_issue(
        issues, "fail", "pillar-count-mismatch",
        sprintf(
          "Pipeline produced %d scored pillar(s) but metadata declares %d indicator(s). Some indicators were silently dropped -- check var_code consistency between metadata and admin sheets.",
          nrow_pti, expected
        )
      )
      FALSE
    } else {
      cli::cli_alert_success(sprintf(
        "Calculation pipeline ran end-to-end (%d indicator%s, %d scored pillar%s).",
        expected, if (expected == 1) "" else "s",
        nrow_pti, if (nrow_pti == 1) "" else "s"
      ))
      TRUE
    }
  }, error = function(e) {
    issues <<- emit_issue(
      issues, "fail", "pipeline-error",
      sprintf("Calculation pipeline aborted: %s", conditionMessage(e))
    )
    FALSE
  })

  finalize_validation(issues, "validate_metadata", error_on_fail)
}


#' Validate a shapes `.rds` file in isolation
#'
#' Reads the `.rds` at `shp_path` and checks two invariants: (1) the
#' object is a non-empty named list, and (2) every `admin<N>Pcod` column
#' referenced inside any layer has a corresponding top-level `admin<N>_*`
#' element. Emits one cli alert per check; returns the structured
#' `list(status, summary, issues)` result invisibly.
#'
#' @inheritParams validate_metadata
#'
#' @return Invisibly, a list with components `status` (`"pass"`,
#'   `"warn"`, or `"fail"`), `summary` (one-line string), and `issues`
#'   (list of issue records).
#'
#' @importFrom readr read_rds
#' @importFrom purrr map keep
#' @importFrom stringr str_extract str_detect str_c
#' @importFrom glue glue
#' @importFrom cli cli_alert_success
#' @family validation
#' @export
#'
#' @examples
#' \dontrun{
#' validate_read_shp("path/to/shapes.rds")
#' }
validate_read_shp <- function(shp_path, error_on_fail = TRUE) {

  issues <- new_issues()

  shp_dta <- tryCatch(
    readr::read_rds(shp_path),
    error = function(e) {
      issues <<- emit_issue(
        issues, "fail", "rds-readable",
        sprintf("Could not read RDS at '%s': %s", shp_path, conditionMessage(e))
      )
      NULL
    }
  )

  if (is.null(shp_dta)) {
    return(finalize_validation(issues, "validate_read_shp", error_on_fail))
  }

  if (length(shp_dta) == 0) {
    issues <- emit_issue(
      issues, "fail", "non-empty-list",
      "Shapes RDS is an empty list. Provide at least one admin layer."
    )
    return(finalize_validation(issues, "validate_read_shp", error_on_fail))
  }

  cli::cli_alert_success(sprintf("Read shapes RDS (%d layer%s).",
    length(shp_dta), if (length(shp_dta) == 1) "" else "s"))

  all_admin_codes <-
    shp_dta %>%
    map(names) %>%
    unlist() %>%
    unique() %>%
    `[`(str_detect(., "admin\\dPcod")) %>%
    str_extract("admin\\d")

  all_admin_levels <-
    shp_dta %>%
    names() %>%
    str_extract("admin\\d")

  extra_levels <- setdiff(all_admin_codes, all_admin_levels)

  if (length(extra_levels) > 0) {
    extra_pat <- str_c(extra_levels, collapse = "|")
    probl_layers <-
      shp_dta %>%
      keep(function(x) any(str_detect(names(x), extra_pat))) %>%
      names() %>%
      str_c(collapse = ", ")

    issues <- emit_issue(
      issues, "warn", "admin-code-layer-coverage",
      glue(
        "Layer(s) [{probl_layers}] reference admin level(s) [{str_c(unique(extra_levels), collapse = ', ')}] via Pcod columns, but no top-level layer for those levels exists. Add the missing layer(s) or drop the orphan Pcod columns."
      )
    )
  } else {
    cli::cli_alert_success("Every admin<N>Pcod column has a matching top-level layer.")
  }

  finalize_validation(issues, "validate_read_shp", error_on_fail)
}


#' Validate a metadata `.xlsx` file in isolation
#'
#' Reads the metadata template at `mtdt_path` via [fct_template_reader()]
#' and checks two invariants: (1) the `metadata` sheet has at least one
#' row, and (2) every `fltr_*` column is read as logical (the template
#' rules require `TRUE`/`FALSE` rather than `1`/`0` strings). Emits one
#' cli alert per check; returns the structured `list(status, summary,
#' issues)` result invisibly.
#'
#' @inheritParams validate_metadata
#'
#' @return Invisibly, a list with components `status` (`"pass"`,
#'   `"warn"`, or `"fail"`), `summary` (one-line string), and `issues`
#'   (list of issue records).
#'
#' @importFrom dplyr select contains any_of
#' @importFrom purrr map_lgl is_logical
#' @importFrom cli cli_alert_success
#' @family validation
#' @export
#'
#' @examples
#' \dontrun{
#' validate_read_metadata("path/to/metadata.xlsx")
#' }
validate_read_metadata <- function(mtdt_path, error_on_fail = TRUE) {

  issues <- new_issues()

  imp_dta <- tryCatch(
    fct_template_reader(mtdt_path),
    error = function(e) {
      issues <<- emit_issue(
        issues, "fail", "xlsx-readable",
        sprintf("Could not read metadata at '%s': %s", mtdt_path, conditionMessage(e))
      )
      NULL
    }
  )

  if (is.null(imp_dta)) {
    return(finalize_validation(issues, "validate_read_metadata", error_on_fail))
  }

  if (is.null(imp_dta$metadata) || nrow(imp_dta$metadata) == 0) {
    issues <- emit_issue(
      issues, "fail", "metadata-sheet-non-empty",
      "The 'metadata' sheet has zero rows. Add one row per indicator (see ?validate_read_metadata)."
    )
  } else {
    cli::cli_alert_success(sprintf(
      "Metadata sheet has %d indicator row%s.",
      nrow(imp_dta$metadata), if (nrow(imp_dta$metadata) == 1) "" else "s"
    ))
  }

  fltr_cols <-
    imp_dta$metadata %>%
    dplyr::select(contains("fltr_"), any_of("legend_revert_colours"))

  if (ncol(fltr_cols) > 0) {
    is_logi <- purrr::map_lgl(fltr_cols, purrr::is_logical)
    bad_cols <- names(is_logi)[!is_logi]

    if (length(bad_cols) > 0) {
      issues <- emit_issue(
        issues, "fail", "fltr-cols-logical",
        sprintf(
          "Filter column(s) [%s] in 'metadata' are not logical. Excel sometimes saves TRUE/FALSE as strings -- write the .xlsx from R with `writexl::write_xlsx()` so the cells are true booleans.",
          paste(bad_cols, collapse = ", ")
        )
      )
    } else {
      cli::cli_alert_success("All fltr_* / legend_revert_colours columns are logical.")
    }
  }

  finalize_validation(issues, "validate_read_metadata", error_on_fail)
}
