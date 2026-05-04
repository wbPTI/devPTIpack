#' Main PTI map orchestration module
#'
#' Server module that ties the PTI rendering stack together: detects the
#' first time the host tab is opened, reshapes the weighted PTI data,
#' drops admin levels with insufficient indicator coverage, exposes the
#' n-bins and admin-level-selector controls, computes the legend, and
#' delegates polygon rendering and download wiring to the polygon /
#' download sub-modules.
#'
#' @param id Character. Shiny module namespace ID.
#' @param shp_dta Reactive yielding a named list of `sf` tibbles (one per
#'   admin level).
#' @param map_dta Reactive yielding the structured PTI list (output of
#'   [mod_ptipage_newsrv()] -- one element per admin level).
#' @param wt_dta Reactive yielding the weights list (output of the
#'   weights-input module). Used by [mod_drop_inval_adm()] to identify
#'   admin levels that lack the indicators carrying non-zero weight.
#' @param active_tab Reactive character indicating the currently selected
#'   tab. Forwarded to `mod_first_open_count_server()`.
#' @param target_tabs Character vector of tab names this module should
#'   render for. Used by `mod_first_open_count_server()` to defer
#'   rendering until the user actually opens one.
#' @param default_adm_level Character or NULL. Default admin level in the
#'   side-panel selector (passed to `mod_get_admin_levels_srv()`).
#' @param show_adm_levels Character vector or NULL. Admin levels to expose
#'   in the selector; passed to `mod_get_admin_levels_srv()`.
#' @param metadata_path Character or NULL. Filesystem path to the metadata
#'   PDF served via the side-panel download link.
#' @param shapes_path Character or NULL. Filesystem path to the shapes
#'   archive served via the side-panel download link.
#' @param ... Forwarded to nested sub-modules.
#'
#' @return A `reactive()` yielding `list(pre_map_dta = pre_map_dta_3)`,
#'   the post-filter / post-legend reshape used by callers that need to
#'   share the rendered data (e.g. the data-export module).
#'
#' @importFrom shiny reactive req
#' @export
mod_plot_pti2_srv <- function(id, shp_dta, map_dta, wt_dta, active_tab, target_tabs,
                              default_adm_level = NULL,
                              show_adm_levels = NULL,
                              metadata_path = NULL,
                              shapes_path = NULL, ...) {

  first_open <- mod_first_open_count_server(id, active_tab, target_tabs)

  pre_map_dta_1 <- reactive({map_dta() %>% preplot_reshape_wghtd_dta()})

  pre_map_dta_2 <- mod_drop_inval_adm(id, pre_map_dta_1, wt_dta)

  sel_adm_levels <- mod_get_admin_levels_srv(id,
                                             reactive(get_current_levels(pre_map_dta_2())),
                                             default_adm_level = default_adm_level,
                                             show_adm_levels = show_adm_levels)
  n_bins <- mod_get_nbins_srv(id)

  pre_map_dta_3 <- reactive({
    req(n_bins())
    req(first_open())

    pre_map_dta_2() %>%
      filter_admin_levels(sel_adm_levels()) %>%
      add_legend_paras(nbins = n_bins()) %>%
      complete_pti_labels() %>%
      rev()
  })

  init_leaf <- mod_plot_init_leaf_server(id, shp_dta, first_open)

  out_leaf <- mod_plot_poly_leaf_server(id, pre_map_dta_3, shp_dta, init_leaf, leg_type = "priority")

  mod_map_dwnld_srv(id, out_leaf, metadata_path = metadata_path, shapes_path = shapes_path)
  mod_dwnld_file_server(id, "mtdt.files.side", filepath = metadata_path)
  mod_dwnld_file_server(id, "shp.files.side", filepath = shapes_path)


  reactive({list(pre_map_dta = pre_map_dta_3)})

}
