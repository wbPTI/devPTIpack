options(golem.app.prod = FALSE)
options(shiny.fullstacktrace = FALSE)
options(shiny.reactlog = FALSE)
golem::detach_all_attached()
golem::document_and_reload()
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)





# Same process in a manual functional way -----------------------------------

library(tidyverse)

shp_dta <- devPTIpack::ukr_shp
imp_dta <- devPTIpack::ukr_mtdt_full
imp_dta$indicators_list <- devPTIpack::get_indicators_list(imp_dta)
imp_dta$weights_clean <- devPTIpack::get_rand_weights(imp_dta$indicators_list)


# Weighting logic -----------------------------------
long_vars <- imp_dta %>% pivot_pti_dta(imp_dta$indicators_list ) 
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


### ### ### ###### ### ### ###### ### ### ###### ### ### ###### ### ### ###
# Testing data flow logic in the app:

options(golem.app.prod = FALSE)
options(shiny.fullstacktrace = FALSE)
golem::detach_all_attached()
golem::document_and_reload()
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options(shiny.reactlog = TRUE)
devPTIpack::run_dev_pti_plot(
  pti.name = "Sample",
  shape_dta = ukr_shp, data_dta = ukr_mtdt_full, 
  default_adm_level = "admin2",
  choose_adm_levels = TRUE,
  show_adm_levels = c("admin1", "admin2", "admin3", "admin4"),
  show_waiter = TRUE
)
