# Two-column UI for a PTI page

Builds a \[shiny::bootstrapPage()\] that splits the viewport into a
weights-input column and a map column. Used by single-page apps and
multi-tab apps where each PTI scheme gets its own tab. The split width
is configurable via \`cols\`; the inner UIs are wired to the matching
\[mod_ptipage_newsrv()\] module via the shared \`id\`.

## Usage

``` r
mod_ptipage_twocol_ui(
  id,
  cols = c(4, 8),
  full_ui = FALSE,
  show_waiter = FALSE,
  map_height = "calc(99vh)",
  wt_height = "inherit",
  dt_style = "zoom:0.95; height: calc(95vh - 250px);",
  wt_style = NULL,
  wt_dwnld_options = c("data", "weights", "shapes", "metadata"),
  map_dwnld_options = c("shapes", "metadata"),
  ...
)

mod_ptipage_box_ui(
  id,
  full_ui = FALSE,
  show_waiter = FALSE,
  map_height = "calc(100vh)",
  wt_height = "inherit",
  dt_style = "zoom:0.9; height: calc(35vh);",
  wt_style = "zoom:0.9;",
  side_width = 450,
  wt_dwnld_options = c("data", "weights", "shapes", "metadata"),
  map_dwnld_options = NULL,
  ...
)
```

## Arguments

- id:

  Character. Shiny module namespace ID. Must match the \`id\` passed to
  \[mod_ptipage_newsrv()\]. The weights-input column is rendered via
  \`mod_wt_inp_ui()\`.

- cols:

  Integer vector of length 2 summing to \<= 12 – column widths
  (Bootstrap grid units) for the weights-input column and the map
  column.

- full_ui:

  Logical. If \`TRUE\`, the full weights-input UI is shown; otherwise
  the compact variant is used. Forwarded to \[mod_wt_inp_ui()\].

- show_waiter:

  Logical. If \`TRUE\`, wraps the page in a waiter / spinner overlay
  shown until the first PTI calculation completes.

- map_height, wt_height:

  CSS height values for the map and weights columns respectively (e.g.
  \`"calc(95vh - 60px)"\`).

- dt_style, wt_style:

  Character or NULL. Additional CSS applied to the weights
  \`DT::datatable\` and to the surrounding weights container.

- wt_dwnld_options, map_dwnld_options:

  Character vector of download options exposed in the weights side and
  on the map respectively. Any subset of \`c("data", "weights",
  "shapes", "metadata")\`. \`NULL\` or empty means no download options.

- ...:

  Additional arguments forwarded to \`mod_wt_inp_ui()\`.

- side_width:

  Numeric. Width in pixels of the side panel in the box variant
  (\[mod_ptipage_box_ui()\]).

## Value

A \`shiny::tagList\` / \`shiny::bootstrapPage\` wrapping the page
layout, optionally wrapped by a waiter overlay.

## Functions

- `mod_ptipage_box_ui()`: PTI page where the weights input sits inside
  an absolute side panel overlaid on a full-viewport map. Used when a
  single PTI is the only thing on screen.

## See also

Other shiny-modules:
[`mod_dta_explorer2_server()`](https://worldbank.github.io/devPTIpack/reference/mod_dta_explorer2_server.md),
[`mod_dta_explorer2_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_dta_explorer2_ui.md),
[`mod_leaf_side_panel_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_leaf_side_panel_ui.md),
[`mod_map_pti_leaf_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_map_pti_leaf_ui.md),
[`mod_plot_pti2_srv()`](https://worldbank.github.io/devPTIpack/reference/mod_plot_pti2_srv.md),
[`mod_pti_comparepage_newsrv()`](https://worldbank.github.io/devPTIpack/reference/mod_pti_comparepage_newsrv.md),
[`mod_pti_comparepage_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_pti_comparepage_ui.md),
[`mod_ptipage_newsrv()`](https://worldbank.github.io/devPTIpack/reference/mod_ptipage_newsrv.md),
[`mod_tab_open_first_newserv()`](https://worldbank.github.io/devPTIpack/reference/mod_tab_open_first_newserv.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mod_ptipage_twocol_ui("page_pti", cols = c(4, 8))
} # }
```
