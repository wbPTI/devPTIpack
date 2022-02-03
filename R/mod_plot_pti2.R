#' map_pti2 key module for actual plotting of the PTI data.
#'
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @export
#' @importFrom shiny NS tagList 
mod_plot_pti2_srv <- function(id, shp_dta, map_dta, wt_dta, active_tab, target_tabs, 
                              default_adm_level = NULL, 
                              show_adm_levels = NULL,
                              metadata_path = NULL,  ...) {
  
  # Check if the tab is opened at first
  first_open <- mod_first_open_count_server(id, active_tab, target_tabs)
  
  # Clean plot dta
  pre_map_dta_1 <- reactive({map_dta() %>% preplot_reshape_wghtd_dta()})
  
  # Removing admin levels, which are irrelevant
  pre_map_dta_2 <- mod_drop_inval_adm(id, pre_map_dta_1, wt_dta)

  # N bins and selected admin levels modules
  sel_adm_levels <- mod_get_admin_levels_srv(id, 
                                             reactive(get_current_levels(pre_map_dta_2())), 
                                             default_adm_level = default_adm_level,
                                             show_adm_levels = show_adm_levels)
  n_bins <- mod_get_nbins_srv(id)
  
  # Filtering not relevant admin levels
  # Computing legend based on data
  pre_map_dta_3 <- reactive({
    # req(sel_adm_levels())
    req(n_bins())
    req(first_open())
    
    pre_map_dta_2() %>%
      filter_admin_levels(sel_adm_levels()) %>%
      add_legend_paras(nbins = n_bins()) %>%
      complete_pti_labels() %>% 
      rev()
  })
  
  # Initialize the map and fly to it.
  init_leaf <- mod_plot_init_leaf_server(id, shp_dta, first_open)
  
  # Plotting of the map
  out_leaf <- mod_plot_poly_leaf_server(id, pre_map_dta_3, shp_dta, init_leaf)
  
  # Map download server functions
  mod_map_dwnld_srv(id, out_leaf, metadata_path = metadata_path)
  
  # Data download 
  reactive({list(pre_map_dta = pre_map_dta_3)})#, init_leaf = init_leaf)})
  
}

#' @describeIn mod_plot_pti2_srv server side function for side-by-side PTI plotting
#' 
mod_plot_pti_comparison_srv  <-
  function(id, shp_dta, map_dta, wt_dta, active_tab, target_tabs, metadata_path = NULL,  ...){
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        mod_plot_pti2_srv("first_leaf", shp_dta, map_dta, wt_dta, active_tab, target_tabs, metadata_path , ...)
        
        mod_plot_pti2_srv("second_leaf", shp_dta, map_dta, wt_dta, active_tab, target_tabs, metadata_path , ...)
        
      }
    )
  }



