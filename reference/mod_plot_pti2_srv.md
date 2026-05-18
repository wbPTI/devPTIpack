# Main PTI map orchestration module

Server module that ties the PTI rendering stack together: detects the
first time the host tab is opened, reshapes the weighted PTI data, drops
admin levels with insufficient indicator coverage, exposes the n-bins
and admin-level-selector controls, computes the legend, and delegates
polygon rendering and download wiring to the polygon / download
sub-modules.

## Usage

``` r
mod_plot_pti2_srv(
  id,
  shp_dta,
  map_dta,
  wt_dta,
  active_tab,
  target_tabs,
  default_adm_level = NULL,
  show_adm_levels = NULL,
  metadata_path = NULL,
  shapes_path = NULL,
  ...
)
```

## Arguments

- id:

  Character. Shiny module namespace ID.

- shp_dta:

  Reactive yielding a named list of \`sf\` tibbles (one per admin
  level).

- map_dta:

  Reactive yielding the structured PTI list (output of
  \[mod_ptipage_newsrv()\] – one element per admin level).

- wt_dta:

  Reactive yielding the weights list (output of the weights-input
  module). Used by \[mod_drop_inval_adm()\] to identify admin levels
  that lack the indicators carrying non-zero weight.

- active_tab:

  Reactive character indicating the currently selected tab. Forwarded to
  \`mod_first_open_count_server()\`.

- target_tabs:

  Character vector of tab names this module should render for. Used by
  \`mod_first_open_count_server()\` to defer rendering until the user
  actually opens one.

- default_adm_level:

  Character or NULL. Default admin level in the side-panel selector
  (passed to \`mod_get_admin_levels_srv()\`).

- show_adm_levels:

  Character vector or NULL. Admin levels to expose in the selector;
  passed to \`mod_get_admin_levels_srv()\`.

- metadata_path:

  Character or NULL. Filesystem path to the metadata PDF served via the
  side-panel download link.

- shapes_path:

  Character or NULL. Filesystem path to the shapes archive served via
  the side-panel download link.

- ...:

  Forwarded to nested sub-modules.

## Value

A \`reactive()\` yielding \`list(pre_map_dta = pre_map_dta_3)\`, the
post-filter / post-legend reshape used by callers that need to share the
rendered data (e.g. the data-export module).

## See also

Other shiny-modules:
[`mod_dta_explorer2_server()`](https://worldbank.github.io/devPTIpack/reference/mod_dta_explorer2_server.md),
[`mod_dta_explorer2_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_dta_explorer2_ui.md),
[`mod_leaf_side_panel_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_leaf_side_panel_ui.md),
[`mod_map_pti_leaf_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_map_pti_leaf_ui.md),
[`mod_pti_comparepage_newsrv()`](https://worldbank.github.io/devPTIpack/reference/mod_pti_comparepage_newsrv.md),
[`mod_pti_comparepage_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_pti_comparepage_ui.md),
[`mod_ptipage_newsrv()`](https://worldbank.github.io/devPTIpack/reference/mod_ptipage_newsrv.md),
[`mod_ptipage_twocol_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_ptipage_twocol_ui.md),
[`mod_tab_open_first_newserv()`](https://worldbank.github.io/devPTIpack/reference/mod_tab_open_first_newserv.md)
