# Launch the ShinyApp (Do not remove this comment)

library(devPTIpack)
library(tidyverse)
library(shiny)

shp_dta <-  devPTIpack::ukr_shp 
imp_dta <-  devPTIpack::ukr_mtdt_full
imp_dta$indicators_list <- devPTIpack::get_indicators_list(imp_dta)
imp_dta$weights_clean <- 
  imp_dta$indicators_list$var_code %>% get_all_weights_combs(1)

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


# Plotting module logic -----------------------------

# Preparing data for plotting
preplot_dta0 <- 
  calc_wght %>% 
  preplot_reshape_wghtd_dta()


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


# Making a ggplot function to plot a map ----------------------------------

to_plot <- 
  preplot_dta %>%
  map(~{ str_c(.x$pti_codes, " (", .x$admin_level, ")") })

# Filter selected layer
selected_layer <-  "Wght of 1 comb no. 7 (Hexagon)"

dta_plot <-
  shp_dta %>%
  `[`(-length(.)) %>%
  list(.x = ., .y = names(.), .z = rev(seq_along(.)) / max(seq_along(.))) %>%
  pmap_dfr(function(...) {..1 %>% mutate(line = ..2, width = ..3)})

# shp_dta %>% 
#   devPTIpack::make_gg_line_map()

# sp::as.SpatialPolygons.PolygonsList()
# 
# shp_dta$admin0_Country %>%
#   sf::as_Spatial() %>% 
#   sp::spplot()
# 
# 
# 
# map_file <- tempfile(fileext = ".png")
# 
# 
# print(spplot(trees, "value"))


# map_to_plot <-
#   preplot_dta %>%
#   purrr::keep(function(.x) {
#     str_c(.x$pti_codes, " (", .x$admin_level, ")") %in% selected_layer
#   }) %>%
#   `[[`(1)
# 
# # layer_id <-
# #   str_c(map_to_plot$pti_codes, " (", map_to_plot$admin_level, ")")
# 
# hgh <- get_size_coef(map_to_plot$pti_dta)
# 
# png(filename="map_file.png", height = 21, width = 21 * hgh, units = "cm", res = 300)
# 
# map_to_plot$pti_dta %>%
#   mutate(
#     pti_score_category = map_to_plot$leg$recode_function(pti_score),
#     pti_score_category = factor(pti_score_category, levels = map_to_plot$leg$our_labels_category)
#   ) %>%
#   select(pti_score_category) %>%
#   sf::as_Spatial() %>%
#   sp::spplot(col.regions = map_to_plot$leg$pal(map_to_plot$leg$our_values))
# 
# dev.off()
# 
# 

shp_dta %>% 
  make_sp_line_map



shinyApp(
  ui =
    fluidPage(fluidRow(selectInput(
      "layerID", "layer",  unname(to_plot)
    )),
    fluidRow(plotOutput("plotID"))),
  server = function(input, output) {
    output$plotID <- renderPlot({
      
      map_to_plot <-
        preplot_dta %>%
        purrr::keep(function(.x) {
          str_c(.x$pti_codes, " (", .x$admin_level, ")") %in% input$layerID
        }) %>%
        `[[`(1)
      
      # layer_id <-
      #   str_c(map_to_plot$pti_codes, " (", map_to_plot$admin_level, ")")
      
      map_to_plot$pti_dta %>%
        mutate(
          pti_score_category = map_to_plot$leg$recode_function(pti_score),
          pti_score_category = factor(pti_score_category, levels = map_to_plot$leg$our_labels_category)
        ) %>% 
        select(pti_score_category) %>%
        sf::as_Spatial() %>% 
        sp::spplot(col.regions = map_to_plot$leg$pal(map_to_plot$leg$our_values))
      
      
    })
    
  }
)
