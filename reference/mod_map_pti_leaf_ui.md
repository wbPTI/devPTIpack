# Map container UI for PTI page modules

Composes a Leaflet output and the \[mod_leaf_side_panel_ui()\] side
panel into a single positioned container. Used by
\[mod_ptipage_twocol_ui()\], \[mod_ptipage_box_ui()\], and
\[mod_pti_comparepage_ui()\] to render the map area within a PTI page
layout.

## Usage

``` r
mod_map_pti_leaf_ui(
  id,
  side_width = 350,
  side_ui = NULL,
  map_dwnld_options = c("shapes", "metadata"),
  ...
)
```

## Arguments

- id:

  Character. Shiny module namespace ID. Wired to the matching
  server-side module.

- side_width:

  Numeric. Side-panel width in pixels.

- side_ui:

  Tag list or NULL. Additional UI inserted at the top of the side panel
  (passed through to \[mod_leaf_side_panel_ui()\]).

- map_dwnld_options:

  Character vector of download options to expose in the side panel;
  passed to \[mod_leaf_side_panel_ui()\].

- ...:

  Further arguments forwarded to \`leaflet::leafletOutput()\` (e.g.
  \`height\`, \`width\`).

## Value

A \`shiny::tags\$div\` containing the leaflet output and the side panel,
positioned for absolute-panel overlay.

## See also

Other shiny-modules:
[`mod_dta_explorer2_server()`](https://worldbank.github.io/devPTIpack/reference/mod_dta_explorer2_server.md),
[`mod_dta_explorer2_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_dta_explorer2_ui.md),
[`mod_leaf_side_panel_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_leaf_side_panel_ui.md),
[`mod_plot_pti2_srv()`](https://worldbank.github.io/devPTIpack/reference/mod_plot_pti2_srv.md),
[`mod_pti_comparepage_newsrv()`](https://worldbank.github.io/devPTIpack/reference/mod_pti_comparepage_newsrv.md),
[`mod_pti_comparepage_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_pti_comparepage_ui.md),
[`mod_ptipage_newsrv()`](https://worldbank.github.io/devPTIpack/reference/mod_ptipage_newsrv.md),
[`mod_ptipage_twocol_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_ptipage_twocol_ui.md),
[`mod_tab_open_first_newserv()`](https://worldbank.github.io/devPTIpack/reference/mod_tab_open_first_newserv.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Inside a Shiny UI:
mod_map_pti_leaf_ui("page1", side_width = 350)
} # }
```
