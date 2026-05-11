#' Collect issues recorded during a validator run
#'
#' Internal helper used by the exported `validate_*` family. Each
#' validator builds up a list of issue records via [add_issue()] and
#' hands the list to [finalize_validation()] at the end, which prints a
#' cli summary and returns the structured result. Centralising the
#' bookkeeping here keeps the validators' control flow linear and makes
#' the per-issue level/check/message contract explicit.
#'
#' Issue record shape:
#'
#' \describe{
#'   \item{level}{One of `"pass"`, `"info"`, `"warn"`, `"fail"`. Only
#'     `"warn"` and `"fail"` are typically appended; `"pass"` checks
#'     are usually emitted as cli alerts and not stored, to keep the
#'     issues list readable.}
#'   \item{check}{Short kebab-case identifier for the check
#'     (e.g. `"pcod-unique"`, `"parent-mapping"`). Stable across
#'     versions so callers can branch on it.}
#'   \item{message}{One-line user-facing message.}
#'   \item{details}{Optional list of extra context (offending values,
#'     row counts, etc.). Keep small — large details belong in attached
#'     reports, not the issues list.}
#' }
#'
#' @noRd
new_issues <- function() {
  list()
}

#' Append a single issue record to a validation-issue list
#'
#' @param issues The current issues list (from [new_issues()] or a prior
#'   [add_issue()] call). Returned with the new record appended.
#' @param level Character. One of `"warn"` or `"fail"`.
#' @param check Character. Short stable check identifier.
#' @param message Character. One-line message.
#' @param details Optional list of extra context.
#'
#' @return The updated issues list.
#'
#' @noRd
add_issue <- function(issues, level, check, message, details = NULL) {
  c(issues, list(list(
    level = level,
    check = check,
    message = message,
    details = details
  )))
}

#' Print a cli summary, optionally abort, and return the structured result
#'
#' Wraps up a validator run. Computes overall status (`"pass"` /
#' `"warn"` / `"fail"`) from the collected issues, prints one cli alert
#' summarising the run, and returns the standard
#' `list(status, summary, issues)` shape. If any issue is `"fail"`-level
#' and `error_on_fail = TRUE` (the default for backward compatibility),
#' it `stop()`s with a short summary; otherwise it returns invisibly.
#'
#' @param issues The accumulated issues list.
#' @param label Character. Validator name (e.g. `"validate_geometries"`),
#'   used in the cli summary and the abort message.
#' @param error_on_fail Logical. If `TRUE` and any issue has
#'   `level == "fail"`, throw an error rather than return.
#'
#' @return Invisibly, a list with components `status`, `summary`, and
#'   `issues`. See `?validate_geometries` for the user-facing contract.
#'
#' @importFrom cli cli_alert_success cli_alert_warning cli_alert_danger
#' @noRd
finalize_validation <- function(issues, label, error_on_fail = TRUE) {
  levels <- vapply(issues, function(x) x$level, character(1))
  n_fail <- sum(levels == "fail")
  n_warn <- sum(levels == "warn")

  status <- if (n_fail > 0) "fail" else if (n_warn > 0) "warn" else "pass"

  summary <- sprintf(
    "%d failure%s, %d warning%s",
    n_fail, if (n_fail == 1) "" else "s",
    n_warn, if (n_warn == 1) "" else "s"
  )

  switch(
    status,
    pass = cli::cli_alert_success(sprintf("%s: all checks passed.", label)),
    warn = cli::cli_alert_warning(sprintf("%s: %s.", label, summary)),
    fail = cli::cli_alert_danger(sprintf("%s: %s.", label, summary))
  )

  res <- list(
    status = status,
    summary = summary,
    issues = issues
  )

  if (status == "fail" && isTRUE(error_on_fail)) {
    stop(
      sprintf(
        "%s failed: %s. Re-run with `error_on_fail = FALSE` and inspect `$issues` for details.",
        label, summary
      ),
      call. = FALSE
    )
  }

  invisible(res)
}

#' Emit a cli alert *and* append the same message as an issue record
#'
#' Convenience helper so the per-check sites stay one line. Always
#' returns the updated issues list; callers should re-bind:
#'
#' \preformatted{
#' issues <- emit_issue(issues, "warn", "pcod-unique", msg)
#' }
#'
#' @noRd
emit_issue <- function(issues, level, check, message, details = NULL) {
  switch(
    level,
    warn = cli::cli_alert_warning(message),
    fail = cli::cli_alert_danger(message),
    info = cli::cli_alert_info(message),
    cli::cli_alert_info(message)
  )
  add_issue(issues, level, check, message, details)
}
