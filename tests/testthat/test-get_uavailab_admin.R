

test_that("get unavailable adming levels works", {
  rn_wt_1 <- 
    list(#
      wt_admi4_1 = 
        tribble(
          ~var_code, ~weight,
          "var_nval15_small_skewd_adm12",   0,
          "var_nval3_skewd_adm1",          0,
          "var_nval4_small_skewd_adm4",    0,
          "var_nval6_na_adm12",             0,
          "var_nval60_na_adm4",            1,
          "var_nvalinf_norm_adm24",         0,
          "var_nvalinf_skewd_adm2",        0,
          "var_nvalinf_unif_adm124",         0,
          "var_nvalinf_huge_unif_adm24",    0,
        ),
      
      wt_admi4_2 = 
        tribble(
          ~var_code, ~weight,
          "var_nval15_small_skewd_adm12",   0,
          "var_nval3_skewd_adm1",          0,
          "var_nval4_small_skewd_adm4",    0,
          "var_nval6_na_adm12",             1,
          "var_nval60_na_adm4",            1,
          "var_nvalinf_norm_adm24",         0,
          "var_nvalinf_skewd_adm2",        0,
          "var_nvalinf_unif_adm124",         0,
          "var_nvalinf_huge_unif_adm24",    0,
        ),
      
      wt_admi2_1 = 
        tribble(
          ~var_code, ~weight,
          "var_nval15_small_skewd_adm12",   0,
          "var_nval3_skewd_adm1",          1,
          "var_nval4_small_skewd_adm4",    0,
          "var_nval6_na_adm12",             1,
          "var_nval60_na_adm4",            0,
          "var_nvalinf_norm_adm24",         1,
          "var_nvalinf_skewd_adm2",        1,
          "var_nvalinf_unif_adm124",         1,
          "var_nvalinf_huge_unif_adm24",    1,
        ),
      
      wt_admi2_2 = 
        tribble(
          ~var_code, ~weight,
          "var_nval15_small_skewd_adm12",   1,
          "var_nval3_skewd_adm1",          1,
          "var_nval4_small_skewd_adm4",    0,
          "var_nval6_na_adm12",             1,
          "var_nval60_na_adm4",            0,
          "var_nvalinf_norm_adm24",         0,
          "var_nvalinf_skewd_adm2",        0,
          "var_nvalinf_unif_adm124",         0,
          "var_nvalinf_huge_unif_adm24",    0,
        ),
      
      wt_admi1_1 = 
        tribble(
          ~var_code, ~weight,
          "var_nval15_small_skewd_adm12",   0,
          "var_nval3_skewd_adm1",          1,
          "var_nval4_small_skewd_adm4",    0,
          "var_nval6_na_adm12",             1,
          "var_nval60_na_adm4",            0,
          "var_nvalinf_norm_adm24",         0,
          "var_nvalinf_skewd_adm2",        0,
          "var_nvalinf_unif_adm124",         1,
          "var_nvalinf_huge_unif_adm24",    0,
        )
    )
  
  # testthat::expect_type(
  #   ukr_mtdt_full %>% get_indicators_list() %>% get_vars_nabil() ,
  #   "tbl_df")
  
  testthat::expect_equal(
    ukr_mtdt_full %>% get_indicators_list() %>% get_vars_un_avbil() %>% nrow(),
    7)
  
  
  testthat::expect_equal(
    ukr_mtdt_full %>%
      get_indicators_list() %>%
      get_vars_un_avbil() %>% 
      get_min_admin_wght(rn_wt_1) %>% 
      length(),
    5)
  
  
})
