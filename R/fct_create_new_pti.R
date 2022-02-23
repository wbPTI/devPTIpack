#' create_new_pti Create a directory with key apps' files and `app.R` needed for deployment
#'
#' @description Copies a template app folder structure to the location/name 
#'   specified. Creates `app.R` as well as the RStudio project to simplify
#'   the  workflow.
#'   
#' @param path full path to the place where the app's folder must be along with
#'   the not yet existing app folder name. The folder will be created automatically.
#' 
#' @param app_name optional. name of folder where the app should be.
#' 
#' @param open logical, if the app's folder should be opened as an RStudio project. 
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom  fs path_file dir_copy path_expand dir_create path_abs dir_exists
#' @importFrom  cli cat_rule cat_bullet
#' @importFrom  yesno yesno
#' @importFrom  rstudioapi isAvailable initializeProject openProject
#' @export
create_new_pti <- function(path, open = TRUE, app_name = basename(path)) {
  
  path <- fs::path_expand(path)
  
  if (path == "." & app_name == fs::path_file(path)) {
    app_name <- fs::path_file(getwd())
  }
  
  if (fs::dir_exists(path)) {
    res <- yesno::yesno(paste("The path", path, "already exists, override?"))
    if (!res) {
      return(invisible(NULL))
    }
  }
  
  cli::cat_rule("Creating dir")
  fs::dir_create(path, recurse = TRUE)
  cli::cat_bullet("Created package directory")
  
  if (rstudioapi::isAvailable()) {
    cli::cat_rule("Rstudio project initialisation")
    rproj_path <- rstudioapi::initializeProject(path = path)
  }
  
  cli::cat_rule("Copying package skeleton")
  
  from <- system.file("template_pti", package = "devPTIpack")
  
  fs::dir_copy(path = from, new_path = path, overwrite = TRUE)
  
  copied_files <- list.files(path = from, full.names = FALSE, 
                             all.files = TRUE, recursive = TRUE)

  cli::cat_bullet("Copied app skeleton")
  cli::cat_rule("Setting the default config")
  
  cli::cat_bullet("Configured app")
  if (open & rstudioapi::isAvailable()) {
    rstudioapi::openProject(path = path)
  }
  
  return(invisible(fs::path_abs(path)))
  
}
