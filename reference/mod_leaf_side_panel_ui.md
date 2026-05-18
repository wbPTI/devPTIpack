# Map side-panel UI for a PTI page

Composes the side-panel of a PTI map: an optional caller-supplied
\`side_ui\` block (typically the weights-input box), the n-bins selector
(\`mod_get_nbins_ui()\`), the admin-level selector
(\`mod_get_admin_levels_ui()\`), and the per-format download links
(\`mod_map_dwnld_ui()\`). The whole panel is rendered inside a floating
\`shiny::absolutePanel()\` overlaid on the leaflet output.

## Usage

``` r
mod_leaf_side_panel_ui(
  id,
  side_width = 200,
  side_ui = NULL,
  map_dwnld_options = c("shapes", "metadata"),
  ...
)
```

## Arguments

- id:

  Character. Shiny module namespace ID. Wired to the matching per-widget
  servers (\`mod_get_nbins_srv()\`, \`mod_get_admin_levels_srv()\`,
  \`mod_map_dwnld_srv()\`).

- side_width:

  Numeric. Side-panel width in pixels.

- side_ui:

  Tag list or \`NULL\`. Caller-supplied content placed at the top of the
  panel (e.g. the compact weights-input UI).

- map_dwnld_options:

  Character vector of download options to expose. Any subset of
  \`c("data", "weights", "shapes", "metadata")\`. \`NULL\` or empty
  means no download options.

- ...:

  Unused; retained for forward compatibility.

## Value

A \`shiny::absolutePanel()\` containing the assembled side panel.

## See also

Other shiny-modules:
[`mod_dta_explorer2_server()`](https://worldbank.github.io/devPTIpack/reference/mod_dta_explorer2_server.md),
[`mod_dta_explorer2_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_dta_explorer2_ui.md),
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
mod_leaf_side_panel_ui("page_pti", side_width = 250)
} # }
```
