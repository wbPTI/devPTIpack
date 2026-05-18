# Data-explorer page server module

Server module powering the data-explorer page. Computes the indicator
list (excluding indicators tagged \`fltr_exclude_explorer\`), reshapes
the metadata into per-admin explorer data, wires the variable selector
(\`mod_fltr_sel_var2_srv()\`), the admin-level selector
(\`mod_get_admin_levels_srv()\`), and the n-bins selector
(\`mod_get_nbins_srv()\`). Defers map rendering until the host tab is
opened (via \`mod_first_open_count_server()\`).

## Usage

``` r
mod_dta_explorer2_server(
  id,
  shp_dta,
  input_dta,
  active_tab,
  target_tabs,
  mtdtpdf_path = NULL,
  shapes_path = NULL,
  data_path = NULL,
  expl_show_adm_levels = NULL,
  expl_default_adm_level = NULL,
  add_selected = reactive(NULL),
  ...
)
```

## Arguments

- id:

  Character. Shiny module namespace ID. Must match the \`id\` passed to
  \[mod_dta_explorer2_ui()\].

- shp_dta:

  Reactive yielding a named list of \`sf\` tibbles, one per admin level.

- input_dta:

  Reactive yielding the metadata list (output of
  \[fct_template_reader()\]).

- active_tab:

  Reactive character indicating the currently selected tab.

- target_tabs:

  Character vector of tab names this module should render for.

- mtdtpdf_path, shapes_path, data_path:

  Character or \`NULL\`. Filesystem paths to the metadata PDF, shapes
  archive, and metadata xlsx served via the side-panel download links.

- expl_show_adm_levels, expl_default_adm_level:

  Forwarded to \`mod_get_admin_levels_srv()\` (\`show_adm_levels\` and
  \`default_adm_level\`). Take precedence over the \`explorer\_\*\`
  Golem options.

- add_selected:

  Reactive yielding character vector of indicator names to
  programmatically add to the variable picker (e.g. triggered by another
  tab). Defaults to \`reactive(NULL)\`.

- ...:

  Unused; retained for forward compatibility.

## Value

A \`reactive()\` yielding \`list(pre_map_dta, init_leaf)\`. Other
modules subscribing to the explorer's state read this reactive.

## See also

Other shiny-modules:
[`mod_dta_explorer2_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_dta_explorer2_ui.md),
[`mod_leaf_side_panel_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_leaf_side_panel_ui.md),
[`mod_map_pti_leaf_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_map_pti_leaf_ui.md),
[`mod_plot_pti2_srv()`](https://worldbank.github.io/devPTIpack/reference/mod_plot_pti2_srv.md),
[`mod_pti_comparepage_newsrv()`](https://worldbank.github.io/devPTIpack/reference/mod_pti_comparepage_newsrv.md),
[`mod_pti_comparepage_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_pti_comparepage_ui.md),
[`mod_ptipage_newsrv()`](https://worldbank.github.io/devPTIpack/reference/mod_ptipage_newsrv.md),
[`mod_ptipage_twocol_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_ptipage_twocol_ui.md),
[`mod_tab_open_first_newserv()`](https://worldbank.github.io/devPTIpack/reference/mod_tab_open_first_newserv.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mod_dta_explorer2_server(
  "explorer",
  shp_dta     = shp_dta,
  input_dta   = input_dta,
  active_tab  = active_tab,
  target_tabs = "Explore data"
)
} # }
```
