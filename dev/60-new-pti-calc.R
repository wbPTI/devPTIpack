options(golem.app.prod = FALSE)
options(shiny.fullstacktrace = FALSE)
options(shiny.reactlog = FALSE)
golem::detach_all_attached()
golem::document_and_reload()
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)



# Same process in a manual functional way -----------------------------------

library(tidyverse)
library(profvis)

# shp_dta <- devPTIpack::ukr_shp
# imp_dta <- devPTIpack::ukr_mtdt_full

shp_dta <- read_rds("../other_countries/Mozambique.rds") %>% 
  map(~{.x %>% select(-any_of("admin3Pcod"))})
imp_dta <- fct_template_reader("../other_countries/Mozambique--metadata-2021-06-28.xlsx")

imp_dta$indicators_list <- devPTIpack::get_indicators_list(imp_dta)
imp_dta$weights_clean <- 
  devPTIpack::get_rand_weights(imp_dta$indicators_list)
# list(
#   wt_admi1_1 = 
#     tribble(
#       ~var_code, ~weight,
#       "var_nval15_small_skewd_adm12",   0,
#       "var_nval3_skewd_adm1",          0,
#       "var_nval4_small_skewd_adm4",    0,
#       "var_nval6_na_adm12",             1,
#       "var_nval60_na_adm4",            0,
#       "var_nvalinf_norm_adm24",         0,
#       "var_nvalinf_skewd_adm2",        0,
#       "var_nvalinf_unif_adm124",         0,
#       "var_nvalinf_huge_unif_adm24",    0,
#     )
# )


##
# Steps of the PTI weights calculations

## 0.1 All data in the long format
long_vars <- imp_dta %>% pivot_pti_dta(imp_dta$indicators_list)

## 0.2 Key ID columns of each admin level. 
existing_shapes <- shp_dta %>% clean_geoms()

## 0.3 Mapping table for administrative units 
mt <- shp_dta %>% get_mt()

## 0.4 Names of admin levels in use
adm_lvls <- mt %>% get_adm_levels()

# 1 Weighting data according to the existing weighting scheme.
weighted_data <-
    get_weighted_data(
      imp_dta$weights_clean,
      long_vars,
      imp_dta$indicators_list
  )

# 2 Calculate PTI scores
scores_data <- weighted_data %>% get_scores_data()

# 3 Extrapolate weights up and down.
# scores_data[[1]] %>%
#   expand_adm_levels( mt)

# profvis::profvis({
#   scores_data[[1]] %>%
#     expand_adm_levels( mt)
# })
# profvis::profvis({
#   scores_data %>%
#     imap(~ expand_adm_levels(.x, mt) %>%
#            merge_expandedn_adm_levels())
# })

extrapolated_data <-
  scores_data %>%
  imap(~ expand_adm_levels(.x, mt) %>% 
         merge_expandedn_adm_levels())

# 4 Clean agg pti data
# profvis::profvis({
#   extrapolated_data %>%
#     agg_pti_scores(existing_shapes)
# })

agged_pti_data <- 
  extrapolated_data %>% 
  agg_pti_scores(existing_shapes)

# 5 Providing data points labels 
agged_labelled_pti_data <-
  agged_pti_data %>% 
  label_generic_pti()

# 6 Restructuring PTI data to the mapping needs
structured_pti <- 
  agged_labelled_pti_data %>% 
  structure_pti_data(shp_dta)





# Profiling PTI calculations ----------------------------------------------

library(profvis)
profvis({
  
  calc_pti_at_once <- 
    
    # 1 Weighting data according to the existing weighting scheme.
    get_weighted_data(
      imp_dta$weights_clean,
      long_vars,
      imp_dta$indicators_list
    ) %>% 
    
    # 2 Calculate PTI scores
    get_scores_data() %>%
    
    # 3 Extrapolate weights up and down.
    imap(~ expand_adm_levels(.x, mt) %>% 
           merge_expandedn_adm_levels()) %>% 
    
    # 4 Clean agg pti data
    agg_pti_scores(existing_shapes) %>% 
    
    # 5 Proviging data points labels 
    label_generic_pti() %>% 
    
    # 6 Restructuring PTI data to the mapping needs
    structure_pti_data(shp_dta)
})


# Polygo labels ================================================
# Here is a placeholdes for developing data labels
# browser()
# globr[[pti_clean_list]] <-



# shapes <- globr[[shapes_list]]
# browser()
# plt_dta_semi_raw <-
#   globr[[pti_raw_list]]



# 
# scores_data$`Rand wt 2`$admin2_District
# weighted_data$`Rand wt 2`$admin2_District
# 
# imp_dta$admin2_District %>% 
#   select(any_of(imp_dta$weights_clean$`Rand wt 2`$var_code))
# 
# long_vars$admin2_District %>% 
#   filter(var_code %in% imp_dta$weights_clean$`Rand wt 2`$var_code) %>% 
#   distinct(var_code)
# 
# imp_dta$weights_clean$`Rand wt 2`$var_code


### ### ### ###### ### ### ###### ### ### ###### ### ### ###### ### ### ###
# Testing data flow logic in the app:


options(golem.app.prod = FALSE)
options(shiny.fullstacktrace = FALSE)
golem::detach_all_attached()
golem::document_and_reload()
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options(shiny.reactlog = TRUE)

devPTIpack::run_dev_pti_plot(
  pti.name = "Sample Country PTI",
  shape_dta = ukr_shp,
  data_dta = ukr_mtdt_full, 
  default_adm_level = "admin2",
  choose_adm_levels = TRUE,
  explorer_choose_adm = FALSE,
  explorer_default_adm = "all",
  explorer_multiple_var = TRUE,
  show_adm_levels = c("admin1", "admin2", "admin3", "admin4"),
  show_waiter = FALSE
)






