#' Resolve a path inside the installed devPTIpack package
#'
#' Thin wrapper around [system.file()] that always sets
#' `package = "devPTIpack"`. Use it from app code or tests to locate
#' bundled resources (CSS, images, sample template) without having to
#' repeat the package name at every call site.
#'
#' @param ... Character path components passed through to
#'   [system.file()]. Joined with the system file separator.
#'
#' @return A single character string. The absolute path to the
#'   requested resource if it exists, or `""` if it does not (matching
#'   [system.file()]'s convention for missing files).
#'
#' @family package-utilities
#' @export
#'
#' @examples
#' app_sys("app", "www")
#' app_sys("template_pti")
app_sys <- function(...){
  system.file(..., package = "devPTIpack")
}
