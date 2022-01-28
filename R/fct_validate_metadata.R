#' generic function to check if the PTI can be computed
#'
#' @param shp_path,mtdt_path path to the metadata file and shapes files respectively
#'
#' @return The return value, if any, from executing the function.
#' @export
#' 
#' @importFrom testthat test_that expect_true expect_gt
#' @importFrom readr read_rds
#'
#' 
validate_metadata <- function(shp_path, mtdt_path) {
  
  validate_read_shp(shp_path)
  validate_read_metadata(mtdt_path)
  
  test_that("provided metadata and shape files can produce valid PTI scores", {
    
    testthat::expect_success({
      
      testthat::expect_true({
        shp_dta <- read_rds(shp_path)
        imp_dta <- fct_template_reader(mtdt_path)
        
        imp_dta$indicators_list <- get_indicators_list(imp_dta)
        long_vars <-
          imp_dta %>% pivot_pti_dta(imp_dta$indicators_list)
        existing_shapes <- shp_dta %>% clean_geoms()
        mt <- shp_dta %>% get_mt()
        adm_lvls <- mt %>% get_adm_levels()
        
        imp_dta$weights_clean <-
          imp_dta$indicators_list$var_code %>%
          get_all_weights_combs(1)
        
        calc_pti_at_once <-
          get_weighted_data(imp_dta$weights_clean, long_vars, imp_dta$indicators_list) %>%
          get_scores_data() %>%
          imap( ~ expand_adm_levels(.x, mt) %>% merge_expandedn_adm_levels())
        
        na_agg <-
          calc_pti_at_once %>%
          agg_pti_scores(existing_shapes)
        
        nrow_pti <- 
          na_agg %>%
          map_dfr(~ .x %>% count(pti_name)) %>%
          count(pti_name) %>%
          nrow() 
        
        nrow(imp_dta$indicators_list) ==  nrow_pti
      })
      
    })
    
    
  })
  
  
}





#' @describeIn validate_metadata Validate shapes reading
#' @export
#' @importFrom readr read_rds
#' @importFrom testthat test_that expect_gt
#' @importFrom purrr map keep
#' @importFrom stringr str_extract str_detect
validate_read_shp <- function(shp_path) {
  
  testthat::test_that("read shapefiles works", {
    shp_dta <- readr::read_rds(shp_path)
    
    testthat::expect_gt(
      length(shp_dta),
      expected = 0,
      label = "R object with boundaries should be a list with names that is greater than zero.")
  })
  
  
  testthat::test_that("In the admin boundaries, all 'admin{X}Pcod' fields have a corresponding dataframe with boundaries", {
    shp_dta <- readr::read_rds(shp_path)
    
    all_admin_codes <- 
      shp_dta %>% 
      map(names) %>% 
      unlist() %>% 
      unique() %>% 
      `[`(str_detect(., "admin\\dPcod")) %>% 
      str_extract( "admin\\d")
    
    all_admin_levels <- 
      shp_dta %>% 
      names %>% 
      str_extract( "admin\\d")
    
    extra_level <- 
      all_admin_codes[!all_admin_codes %in% all_admin_levels] %>% 
      str_c(collapse = "|")
    
    probl_ms <- 
      shp_dta %>% 
      keep(function(x) any(str_detect(names(x), extra_level))) %>% 
      names() %>% 
      str_c(collapse = ", ")
    
    testthat::expect_true(
      all(all_admin_codes %in% all_admin_levels),
      label = 
        glue::glue(
          "In admin bounds {probl_ms} there are variables with admin levels ({extra_level}), which are not present as separea admin bounds"
        ))
  })
  
  
  
  
}


#' @describeIn validate_metadata validate metadata reading
#' @export
#' @importFrom readr read_rds
#' @importFrom testthat test_that expect_gt
#' @importFrom dplyr select
#' @importFrom purrr map_lgl
validate_read_metadata <- function(mtdt_path) {
  
  test_that("read metadata works", {
    imp_dta <- fct_template_reader(mtdt_path)
    
    testthat::expect_gt(
      length(imp_dta$metadata),
      expected = 0,
      label = "metadata sheet in the metadata file has no valid rows, check it."
    )
    
    
    testthat::expect_true(
      {
        imp_dta <- fct_template_reader(mtdt_path)
        
        imp_dta$metadata %>% 
          dplyr::select(contains("fltr_"), any_of("legend_revert_colours ")) %>% 
          purrr::map_lgl(~purrr::is_logical(.x)) %>% 
          all
      },
      label = "filter columns in the metadata are not read as logical columns"
    )
    
    
  })
  
}
