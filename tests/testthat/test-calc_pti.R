# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

testthat::test_that("list of PTI indicators can be extracted", {
  
  expect_s3_class({
    get_indicators_list(devPTIpack::ukr_mtdt_full)
  }, 'tbl_df')

  expect_equal({
    length(get_indicators_list(devPTIpack::ukr_mtdt_full))
  }, 10)
  
})


testthat::test_that("PTI metadata can be pivoted", {
  
  expect_equal({
    devPTIpack::ukr_mtdt_full %>%
      pivot_pti_dta(
        get_indicators_list(., "fltr_exclude_pti")
        ) %>%
      class()
  }, "list")

  expect_gt({
    devPTIpack::ukr_mtdt_full %>%
      pivot_pti_dta(
        get_indicators_list(., "fltr_exclude_pti")
      ) %>%
      length()
  }, 0)
  
})
