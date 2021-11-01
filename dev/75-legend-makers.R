options(golem.app.prod = FALSE)
options(shiny.fullstacktrace = FALSE)
options(shiny.reactlog = FALSE)
golem::detach_all_attached()
golem::document_and_reload()
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# devPTIpack::run_dev_map_pti(shape_dta = ukr_shps, data_dta = ukr_mtdt_full, show_waiter = FALSE)





# Same process in a manual functional way -----------------------------------

library(tidyverse)
library(leaflet)

# shp_dta <- #devPTIpack::ukr_shps

# imp_dta <- devPTIpack::ukr_mtdt_full
# imp_dta$indicators_list <- devPTIpack::get_indicators_list(imp_dta)
# imp_dta$weights_clean <- devPTIpack::get_rand_weights(imp_dta$indicators_list)



recode_val_base(1, 1, 1)


xxx <- c(0, 0, 0, 0, NA, 0, 0, 0, NA, 0, 0, 0, 0)
xxx <- c(0, NA)

def_leg <- legend_map_satelite(xxx, legend_paras = NULL)

def_leg$recode_function(xxx)

cut(
  xxx,
  breaks = c(0, 0+1),
  labels = our_labels_category[our_labels_category != no_data_label],
  include.lowest = T,
  ordered_result = FALSE
) 


cut(
  x,
  breaks = c(our_breaks, our_breaks),
  labels = our_labels_category[our_labels_category != no_data_label],
  include.lowest = T,
  ordered_result = FALSE
) 


# Weighting logic -----------------------------------
long_vars <- imp_dta %>% pivot_pti_dta(imp_dta$indicators_list) 
existing_shapes <- shp_dta %>% clean_geoms() 
mt <- shp_dta %>% get_mt()
adm_lvls <- mt %>% get_adm_levels()

calc_wght <- 
  imp_dta$weights_clean %>%
  get_weighted_data(long_vars, indicators_list = imp_dta$indicators_list) %>% 
  get_scores_data() %>% 
  imap(~ expand_adm_levels(.x, mt) %>%
         merge_expandedn_adm_levels()) %>% 
  agg_pti_scores(existing_shapes) %>% 
  label_generic_pti() %>% 
  structure_pti_data(shp_dta)

# Plotting logic -----------------------------

# Preparing data for plotting
preplot_dta <- 
  calc_wght %>% 
  preplot_reshape_wghtd_dta() %>%
  filter_admin_levels() %>% 
  add_legend_paras(nbins = 7) %>% 
  complete_pti_labels() %>% 
  rev()


preplot_dta %>% 
  get_current_levels() 



# Visualizing the data on leaflet map. ------------------------------------
library(leaflet)
# 
# leaflet() %>% 
#   plot_leaf_line_map2(shp_dta, show_adm_levels = NULL) %>% 
#   plot_pti_polygons(preplot_dta) %>% 
#   add_pti_poly_controls(preplot_dta)


# Data export module ---------------------------------------

library(tidyverse)


# Data explorer module logic -------------------------------------------

input_dta <- devPTIpack::ukr_mtdt_full
# input_dta$admin1_States <- 
#   input_dta$admin1_States %>% 
#   dplyr::mutate(sdn_pop_2020_un_const = sdn_pop_dens_2020_un_const)
shp_dta <- ukr_shps
  
indicators_list <- get_indicators_list(input_dta, "fltr_exclude_explorer")
# %>% 
#   filter(var_name %in% c(
#     "Poverty rate, %",                                                  
#     "Number of poor",                                                   
#     "Income inequality (Gini) index",                                   
#     "IPC Phase of food insecurity",                                     
#     "Population in 2020",                                               
#     "Population density in 2020, people per sq. km",                    
#     "Population density in 2020, people per sq. km of populated area"
#   ))

# filter(str_detect(var_name, "Population den"))
  

# indicators_list %>% View()

# Step 1
pre_map_dta_1 <-
  input_dta %>% 
  pivot_pti_dta(indicators_list) %>% 
  reshaped_explorer_dta(indicators_list) %>% 
  structure_pti_data(shp_dta) %>% 
  preplot_reshape_wghtd_dta() 

# Step 2 Variable filter
choices <- indicators_list %>% get_var_choices()
selected <- sample(choices$Needs, 2)

pre_map_dta_2 <- pre_map_dta_1 %>% filter_var_explorer(unlist(unname(choices)) %>% `[`(str_detect(., "poverty")) %>% names())
# unlist(unname(choices)) %>% str_detect(., "poverty")
  
# Step 3 Admin level and legends
pre_map_dta_3 <- 
  pre_map_dta_2 %>%
  filter_admin_levels("ALL") %>%
  add_legend_paras(nbins = 5) %>%
  # complete_pti_labels() %>%
  rev()

# Step 4 Plotting
library(leaflet)
leaflet() %>% 
  # clean_pti_polygons(pre_map_dta_3) %>% 
  # clean_pti_poly_controls(pre_map_dta_3) %>% 
  plot_leaf_line_map2(shp_dta, show_adm_levels = NULL) %>%
  plot_pti_polygons(pre_map_dta_3) %>%  
  add_pti_poly_controls(pre_map_dta_3) %>% 
  plot_pti_legend(pre_map_dta_3, "Poverty rate, % (District)")


pre_map_dta_1 %>% 
  map("pti_codes")

# Filtering variables


pre_map_dta_1 %>% 
  filter_var_explorer(selected) %>% 
  filter_admin_levels("States") %>% 
  add_legend_paras(nbins = 7) %>% 
  rev()



### ### ### ###### ### ### ###### ### ### ###### ### ### ###### ### ### ###
# Testing data flow logic in the app:


options(golem.app.prod = FALSE)
options(shiny.fullstacktrace = FALSE)
golem::detach_all_attached()
golem::document_and_reload()
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options(shiny.reactlog = TRUE)

input_dta <- devPTIpack::ukr_mtdt_full
input_dta$admin1_States <- 
  input_dta$admin1_States %>% 
  dplyr::mutate(sdn_pop_2020_un_const = sdn_pop_dens_2020_un_const)

devPTIpack::run_dev_pti_plot(
  pti.name = "Sample country PTI",
  shape_dta = ukr_shps,
  data_dta = input_dta, 
  default_adm_level = "admin2",
  choose_adm_levels = TRUE,
  explorer_choose_adm = FALSE,
  explorer_default_adm = "all",
  explorer_multiple_var = TRUE,
  show_adm_levels = c("admin1", "admin2", "admin3", "admin4"),
  show_waiter = FALSE
)
