# Data-explorer page UI

Builds the explorer page layout: a full-width Leaflet output overlaid by
the explorer side panel (\`mod_dta_explorer2_side_ui()\`), wrapped in a
\[shiny::fluidRow()\] and a Cicerone-tour-friendly stack of nested
\`div\`s.

## Usage

``` r
mod_dta_explorer2_ui(
  id,
  multi_choice,
  max_choice = 3,
  map_dwnld_options = c("shapes", "metadata"),
  ...
)
```

## Arguments

- id:

  Character. Shiny module namespace ID. Must match the \`id\` passed to
  \[mod_dta_explorer2_server()\].

- multi_choice:

  Logical or \`NULL\`. Whether the indicator picker accepts multiple
  selections. Forwarded to \`mod_select_var_ui()\`; may be overridden by
  the \`explorer_multiple_var\` Golem option.

- max_choice:

  Integer. Max number of indicators a user may pick when \`multi_choice
  = TRUE\`.

- map_dwnld_options:

  Character vector of download options exposed in the side panel. Any
  subset of \`c("data", "weights", "shapes", "metadata")\`.

- ...:

  Forwarded to \`leaflet::leafletOutput()\`.

## Value

A \`shiny::tagList()\` containing the explorer layout.

## See also

Other shiny-modules:
[`mod_dta_explorer2_server()`](https://worldbank.github.io/devPTIpack/reference/mod_dta_explorer2_server.md),
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
mod_dta_explorer2_ui("explorer", multi_choice = FALSE)
} # }
```
