# Comparison-page UI for two side-by-side PTI maps

Returns a two-column \[shiny::fluidRow()\] in which each column hosts a
\[mod_map_pti_leaf_ui()\] container, allowing the user to compare two
weighting schemes (or admin-level renderings) on adjacent maps. Each map
gets its own namespace (\`first_leaf\` / \`second_leaf\`) so the two
instances are addressable independently from the server side.

## Usage

``` r
mod_pti_comparepage_ui(id)
```

## Arguments

- id:

  Character. Shiny module namespace ID. Wired to the matching
  \[mod_pti_comparepage_newsrv()\] call.

## Value

A \`shiny::bootstrapPage()\` containing the two-column compare layout.

## See also

Other shiny-modules:
[`mod_dta_explorer2_server()`](https://worldbank.github.io/devPTIpack/reference/mod_dta_explorer2_server.md),
[`mod_dta_explorer2_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_dta_explorer2_ui.md),
[`mod_leaf_side_panel_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_leaf_side_panel_ui.md),
[`mod_map_pti_leaf_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_map_pti_leaf_ui.md),
[`mod_plot_pti2_srv()`](https://worldbank.github.io/devPTIpack/reference/mod_plot_pti2_srv.md),
[`mod_pti_comparepage_newsrv()`](https://worldbank.github.io/devPTIpack/reference/mod_pti_comparepage_newsrv.md),
[`mod_ptipage_newsrv()`](https://worldbank.github.io/devPTIpack/reference/mod_ptipage_newsrv.md),
[`mod_ptipage_twocol_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_ptipage_twocol_ui.md),
[`mod_tab_open_first_newserv()`](https://worldbank.github.io/devPTIpack/reference/mod_tab_open_first_newserv.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mod_pti_comparepage_ui("compare")
} # }
```
