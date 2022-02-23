#' Sample of a geometries data structure
#'
#' @description A nested list, where each sub-list is named in a standard way: 
#'   {admin level code}_{title of the administrative level}. 
#'   
#'   {admin level code} must be "admin0" - for country-level shapes and 
#'   "admin1", "admin2", "admin3" ... "admin9" and so on.
#'   
#'   {title of the administrative} level could be a single word.
#'   
#'   Examples of the complete names: "admin0_Country", "admin1_Oblast", 
#'   "admin2_Rayon", "admin4_Hexagon";
#' 
#' 
#' @format Each list element is a tibble and sf object
"ukr_shp"

#' @describeIn ukr_shp Sample of a metadata strucuture.
#' 
#' @format Each list element is a tibble
"ukr_mtdt_full"