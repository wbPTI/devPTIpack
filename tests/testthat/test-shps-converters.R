test_that("maping table is generated from shapes file", {
  
  expect_gt({
    devPTIpack::ukr_shp %>% 
      clean_geoms() %>% 
      length()
    }, 0)
  
  
  expect_gt({
    devPTIpack::ukr_shp %>% get_mt() %>% length()
  }, 2)
  
  
  expect_equal({
    devPTIpack::ukr_shp %>% get_mt() %>% get_adm_levels() %>%  class
  }, "character")
  
  
})
