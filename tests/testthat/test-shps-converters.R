test_that("maping table is generated from shapes file", {
  
  expect_gt({
    devPTIpack::ukr_shps %>% 
      devPTIpack::clean_geoms() %>% 
      length()
    }, 0)
  
  
  expect_gt({
    devPTIpack::ukr_shps %>% devPTIpack::get_mt() %>% length()
  }, 2)
  
  
  expect_equal({
    devPTIpack::ukr_shps %>% devPTIpack::get_mt() %>% devPTIpack::get_adm_levels() %>%  class
  }, "character")
  
  
})
