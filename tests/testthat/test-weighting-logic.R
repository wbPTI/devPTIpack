

test_that("random weights are generated", {
  expect_success(
    expect_gt({
      devPTIpack::ukr_mtdt_full %>% 
        devPTIpack::get_indicators_list() %>% 
        devPTIpack::get_rand_weights() %>% 
        bind_rows() %>% 
        length()
    }, 0)
    
  )
  
})




test_that("generic weigthing of the PTI data works", {
  expect_success(
    expect_equal({
      # library(tidyverse)
      set.seed(1221)
      wgt <- 
        devPTIpack::ukr_mtdt_full %>% 
        devPTIpack::get_indicators_list() %>% 
        devPTIpack::get_rand_weights() 
      # 1 Weighting data according to the existing weighting scheme.
      
      imp_dta <- devPTIpack::ukr_mtdt_full
      imp_dta$indicators_list <- devPTIpack::get_indicators_list(imp_dta)
      imp_dta$weights_clean <- devPTIpack::get_rand_weights(imp_dta$indicators_list)
      long_vars <- imp_dta %>% devPTIpack::pivot_pti_dta(imp_dta$indicators_list)
      
      devPTIpack::get_weighted_data(imp_dta$weights_clean,
                                   long_vars,
                                   imp_dta$indicators_list) %>%
        class()
    }, "list")
  )
  
})




test_that("pti scores produce NA when some data are NA", {
  
  shp_dta <- devPTIpack::ukr_shp
  imp_dta <- devPTIpack::ukr_mtdt_full
  imp_dta$indicators_list <- get_indicators_list(imp_dta)
  long_vars <- imp_dta %>% pivot_pti_dta(imp_dta$indicators_list)
  existing_shapes <- shp_dta %>% clean_geoms()
  mt <- shp_dta %>% get_mt()
  adm_lvls <- mt %>% get_adm_levels()
  
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
  
  expect_equal(
    na_agg %>%
      map_dfr( ~ .x %>% filter(is.na(pti_score))) %>%
      count(pti_name)  %>%
      pull(pti_name) %>%
      length(),
    2
  )
  
  no_na_agg <- 
    calc_pti_at_once  %>% 
    agg_pti_scores(existing_shapes, na_rm_pti2 = T) 
  
  expect_equal(
    no_na_agg %>%
      map_dfr( ~ .x %>% filter(is.na(pti_score))) %>%
      nrow(),
    0
  )
  
})



test_that("strange weights produces some PTI values", {
  
  shp_dta <- devPTIpack::ukr_shp
  imp_dta <- devPTIpack::ukr_mtdt_full
  imp_dta$indicators_list <- get_indicators_list(imp_dta)
  long_vars <- imp_dta %>% pivot_pti_dta(imp_dta$indicators_list)
  existing_shapes <- shp_dta %>% clean_geoms()
  mt <- shp_dta %>% get_mt()
  adm_lvls <- mt %>% get_adm_levels()
  
  
  expect_equal({
    imp_dta$weights_clean <-
      list(allzeroweight = 
             imp_dta$indicators_list %>% 
             select(var_code) %>% 
             mutate(weight = 1))
    
    get_weighted_data(imp_dta$weights_clean, long_vars, imp_dta$indicators_list) %>%
      get_scores_data() %>%
      imap( ~ expand_adm_levels(.x, mt) %>% merge_expandedn_adm_levels()) %>%
      class()
  },
  "list", label = "Weights are 1.")
  
  
  
  expect_equal({
    imp_dta$weights_clean <-
      list(allzeroweight = 
             imp_dta$indicators_list %>% 
             select(var_code) %>% 
             mutate(weight = 0))
    
    get_weighted_data(imp_dta$weights_clean, long_vars, imp_dta$indicators_list) %>%
      get_scores_data() %>%
      imap( ~ expand_adm_levels(.x, mt) %>% merge_expandedn_adm_levels()) %>%
      class()
  },
  "list",  label = "Weights are 0")
  
  
})





