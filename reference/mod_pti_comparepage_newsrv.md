# Comparison-page server module

Server module powering the comparison page. Instantiates two
\[mod_plot_pti2_srv()\] sub-modules (\`first_leaf\` / \`second_leaf\`)
against the same shapes, structured PTI data, and weights. The two maps
share inputs but render independently so the user can pan / zoom each
side separately while comparing the same underlying weighting.

## Usage

``` r
mod_pti_comparepage_newsrv(
  id,
  shp_dta,
  map_dta,
  wt_dta,
  active_tab,
  target_tabs,
  mtdtpdf_path = ".",
  shapes_path = ".",
  show_adm_levels = NULL,
  ...
)
```

## Arguments

- id:

  Character. Shiny module namespace ID. Must match the \`id\` passed to
  \[mod_pti_comparepage_ui()\].

- shp_dta:

  Reactive yielding a named list of \`sf\` tibbles (one per admin
  level). Forwarded to both \[mod_plot_pti2_srv()\] children.

- map_dta:

  Reactive yielding the structured PTI list (output of
  \[mod_ptipage_newsrv()\]).

- wt_dta:

  Reactive yielding the weights list (output of the weights-input
  module).

- active_tab:

  Reactive character indicating the currently selected tab.

- target_tabs:

  Character vector of tab names this module should render for.

- mtdtpdf_path:

  Character. Filesystem path to the metadata PDF served via the
  side-panel download link in each map.

- shapes_path:

  Character. Filesystem path to the shapes archive served via the
  side-panel download link in each map.

- show_adm_levels:

  Character vector or NULL. Admin levels to expose in the side-panel
  selector of each map.

- ...:

  Forwarded to both \[mod_plot_pti2_srv()\] children.

## Value

No explicit return value. Called for side effects (renders two
side-by-side leaflet maps within the Shiny session).

## See also

Other shiny-modules:
[`mod_dta_explorer2_server()`](https://worldbank.github.io/devPTIpack/reference/mod_dta_explorer2_server.md),
[`mod_dta_explorer2_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_dta_explorer2_ui.md),
[`mod_leaf_side_panel_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_leaf_side_panel_ui.md),
[`mod_map_pti_leaf_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_map_pti_leaf_ui.md),
[`mod_plot_pti2_srv()`](https://worldbank.github.io/devPTIpack/reference/mod_plot_pti2_srv.md),
[`mod_pti_comparepage_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_pti_comparepage_ui.md),
[`mod_ptipage_newsrv()`](https://worldbank.github.io/devPTIpack/reference/mod_ptipage_newsrv.md),
[`mod_ptipage_twocol_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_ptipage_twocol_ui.md),
[`mod_tab_open_first_newserv()`](https://worldbank.github.io/devPTIpack/reference/mod_tab_open_first_newserv.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mod_pti_comparepage_newsrv(
  "compare",
  shp_dta     = shp_dta,
  map_dta     = map_dta,
  wt_dta      = wt_dta,
  active_tab  = active_tab,
  target_tabs = "Compare PTIs"
)
} # }
```
