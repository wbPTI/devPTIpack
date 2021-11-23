options(golem.app.prod = FALSE)
options(shiny.fullstacktrace = FALSE)
options(shiny.reactlog = FALSE)
golem::detach_all_attached()
golem::document_and_reload()
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)





# Same process in a manual functional way -----------------------------------

library(tidyverse)

shp_dta <-  ukr_shp 
imp_dta <-  ukr_mtdt_full
imp_dta$indicators_list <- devPTIpack::get_indicators_list(imp_dta)
imp_dta$weights_clean <- 
  imp_dta$indicators_list$var_code %>% get_all_weights_combs(1)
  # list(#
  #   wt_admi4_1 = 
  #     tribble(
  #       ~var_code, ~weight,
  #       "var_nval15_small_skewd_adm12",   0,
  #       "var_nval3_skewd_adm1",          0,
  #       "var_nval4_small_skewd_adm4",    0,
  #       "var_nval6_na_adm12",             0,
  #       "var_nval60_na_adm4",            1,
  #       "var_nvalinf_norm_adm24",         0,
  #       "var_nvalinf_skewd_adm2",        0,
  #       "var_nvalinf_unif_adm124",         0,
  #       "var_nvalinf_huge_unif_adm24",    0,
  #     ),
  #   
  #   wt_admi4_2 = 
  #     tribble(
  #       ~var_code, ~weight,
  #       "var_nval15_small_skewd_adm12",   0,
  #       "var_nval3_skewd_adm1",          0,
  #       "var_nval4_small_skewd_adm4",    0,
  #       "var_nval6_na_adm12",             1,
  #       "var_nval60_na_adm4",            1,
  #       "var_nvalinf_norm_adm24",         0,
  #       "var_nvalinf_skewd_adm2",        0,
  #       "var_nvalinf_unif_adm124",         0,
  #       "var_nvalinf_huge_unif_adm24",    0,
  #     ),
  #   
  #   wt_admi2_1 = 
  #     tribble(
  #       ~var_code, ~weight,
  #       "var_nval15_small_skewd_adm12",   0,
  #       "var_nval3_skewd_adm1",          1,
  #       "var_nval4_small_skewd_adm4",    0,
  #       "var_nval6_na_adm12",             1,
  #       "var_nval60_na_adm4",            0,
  #       "var_nvalinf_norm_adm24",         1,
  #       "var_nvalinf_skewd_adm2",        1,
  #       "var_nvalinf_unif_adm124",         1,
  #       "var_nvalinf_huge_unif_adm24",    1,
  #     ),
  #   
  #   wt_admi2_2 = 
  #     tribble(
  #       ~var_code, ~weight,
  #       "var_nval15_small_skewd_adm12",   1,
  #       "var_nval3_skewd_adm1",          1,
  #       "var_nval4_small_skewd_adm4",    0,
  #       "var_nval6_na_adm12",             1,
  #       "var_nval60_na_adm4",            0,
  #       "var_nvalinf_norm_adm24",         0,
  #       "var_nvalinf_skewd_adm2",        0,
  #       "var_nvalinf_unif_adm124",         0,
  #       "var_nvalinf_huge_unif_adm24",    0,
  #     ),
  #   
  #   wt_admi1_1 = 
  #     tribble(
  #       ~var_code, ~weight,
  #       "var_nval15_small_skewd_adm12",   0,
  #       "var_nval3_skewd_adm1",          1,
  #       "var_nval4_small_skewd_adm4",    0,
  #       "var_nval6_na_adm12",             1,
  #       "var_nval60_na_adm4",            0,
  #       "var_nvalinf_norm_adm24",         0,
  #       "var_nvalinf_skewd_adm2",        0,
  #       "var_nvalinf_unif_adm124",         1,
  #       "var_nvalinf_huge_unif_adm24",    0,
  #     )
  # )



imp_dta$weights_clean <-
  # imp_dta$indicators_list$var_code %>% get_all_weights_combs(1)
list(#
  wt_admi4_1 =
    tribble(
      ~var_code, ~weight,
      "var_nval15_small_skewd_adm12",   0,
      "var_nval3_skewd_adm1",           0,
      "var_nval4_small_skewd_adm4",     0,
      "var_nval6_na_adm12",             0,
      "var_nval60_na_adm4",             0,
      "var_nvalinf_norm_adm24",         0,
      "var_nvalinf_skewd_adm2",         0,
      "var_nvalinf_unif_adm124",        0,
      "var_nvalinf_huge_unif_adm24",    0,
    )
)

## 0.1 All data in the long format
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


# Initializing the map ----------------------------------------------------

# # Adding country boundaries
# library(leaflet)
# leaflet() %>% 
#   plot_leaf_line_map2(shp_dta, show_adm_levels = NULL)


# Plotting module logic -----------------------------

# Preparing data for plotting
preplot_dta0 <- 
  calc_wght %>% 
  preplot_reshape_wghtd_dta()

length(preplot_dta0)

# Filtering admin levels which are not available
adm_to_filter <- 
  imp_dta %>% 
  get_indicators_list() %>% 
  get_vars_un_avbil(names(get_current_levels(preplot_dta0))) %>% 
  get_min_admin_wght(imp_dta$weights_clean)


preplot_dta <-
  preplot_dta0 %>% 
  drop_inval_adm(adm_to_filter) %>% 
  # Placeholder for filtering data module to plot only specific admin levels
  # filter_admin_levels(c("Country", "Hexagons")) %>% 
  
  # Module on the number of bins and legend 
  add_legend_paras(nbins = 7) %>% 
  complete_pti_labels() %>% 
  rev()


preplot_dta %>% 
  get_current_levels() 


# Making a ggplot function to plot a map ----------------------------------

preplot_dta %>% 
  map(~{ str_c(.x$pti_codes, " (", .x$admin_level, ")") })

# Filter selected layer
selected_layer <-  "wt_admi4_1 (Rayon)"

dta_plot <- 
  shp_dta %>%
  `[`(-length(.)) %>% 
  list(.x = ., .y = names(.), .z = rev(seq_along(.)) / max(seq_along(.))) %>% 
  pmap_dfr(function(...) {..1 %>% mutate(line = ..2, width = ..3)}) 


dta_plot %>% 
  ggplot() +
  aes(group = line, linetype = line, colour = line, size = width) +
  geom_sf(fill = NA) + 
  coord_sf(crs = sf::st_crs(dta_plot), datum = sf::st_crs(dta_plot)) +
  scale_colour_brewer(palette = "Dark2") + 
  scale_size_continuous(range = c(0.15, 1.25)) +
  theme_bw()  + 
  theme(legend.position="none")

#' Plot the map of country using GG and knowing the layer to plot.  
make_gg_line_map <- function(shp_dta) {
  shp_dta %>%
    `[`(-length(.)) %>% 
    list(.x = ., .y = names(.), .z = rev(seq_along(.)) / max(seq_along(.))) %>% 
    pmap_dfr(function(...) {..1 %>% mutate(line = ..2, width = ..3)}) %>% 
    ggplot() +
    aes(group = line, linetype = line, colour = line, size = width) +
    geom_sf(fill = NA) +
    scale_colour_brewer(palette = "Dark2") + 
    scale_size_continuous(range = c(0.15, 1.25)) +
    theme_bw()  + 
    theme(legend.position="none")
  
}



#' Plot the map of country using GG and knowing the layer to plot.  
make_ggmap <- function(preplot_dta, selected_layer) {
  map_to_plot <-
    preplot_dta %>%
    purrr::keep(function(.x) {
      str_c(.x$pti_codes, " (", .x$admin_level, ")") %in% selected_layer
    }) %>%
    `[[`(1)
  
  layer_id <-
    str_c(map_to_plot$pti_codes, " (", map_to_plot$admin_level, ")")
  
  map_to_plot$leg$our_labels_category
  
  map_to_plot$pti_dta %>%
    mutate(
      pti_score_category = map_to_plot$leg$recode_function(pti_score),
      pti_score_category = factor(pti_score_category, levels = map_to_plot$leg$our_labels_category)
    ) %>%
    ggplot() +
    aes(fill = pti_score_category) +
    geom_sf() +
    scale_fill_manual(values = set_names(
      map_to_plot$leg$pal(map_to_plot$leg$our_values),
      map_to_plot$leg$our_labels_category
    )) +
    labs(fill = layer_id) +
    theme_bw()
  
}
# +
#   annotation_scale(location = "bl", width_hint = 0.5) +
#   annotation_north_arrow(location = "bl", which_north = "true", 
#                          pad_x = unit(0.15, "cm"), pad_y = unit(0.45, "cm"),
#                          style = north_arrow_fancy_orienteering) 
# 
# library(ggspatial)
# Visualizing the data on leaflet map. ------------------------------------

library(leaflet)
leaflet() %>%
  plot_leaf_line_map2(shp_dta, show_adm_levels = NULL) %>%
  plot_pti_polygons(preplot_dta) %>%
  add_pti_poly_controls(preplot_dta)


### ### ### ###### ### ### ###### ### ### ###### ### ### ###### ### ### ###
# Testing data flow logic in the app:

options(golem.app.prod = FALSE)
options(shiny.fullstacktrace = FALSE)
golem::detach_all_attached()
golem::document_and_reload()
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options(shiny.reactlog = TRUE)
devPTIpack::run_dev_pti_plot(
  pti.name = "Sample country",
  shape_dta = ukr_shp, data_dta = ukr_mtdt_full, show_waiter = FALSE,
  default_adm_level = "admin2",
  choose_adm_levels = TRUE,
  show_adm_levels = c("admin1", "admin2", "admin3")
  )





