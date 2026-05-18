# Tab-opening invalidator for custom PTI page layouts

Returns a reactive that emits a fresh \`Sys.time()\` timestamp the first
time the user opens any tab whose name appears in \`target_tabs\`.
Subsequent visits to the same tab do not re-trigger the invalidator.
Used by page modules to defer expensive rendering (e.g. the
\`mod_pti_comparepage_newsrv\` map pair) until the tab is actually shown
for the first time.

## Usage

``` r
mod_tab_open_first_newserv(id, active_tab, target_tabs, ...)
```

## Arguments

- id:

  Character. Shiny module namespace ID.

- active_tab:

  Reactive returning the currently selected tab name (typically
  \`reactive(input\$tabpan)\`). May yield \`NULL\` for single-tab
  layouts.

- target_tabs:

  Character vector of tab names whose first-open event should
  invalidate. May be \`NULL\` for single-tab layouts.

- ...:

  Currently unused; reserved for future extensions.

## Value

A reactive returning either \`NULL\` (before any qualifying tab open) or
a \`POSIXct\` timestamp updated on each first open.

## Details

When both \`active_tab()\` and \`target_tabs\` are not truthy, the
invalidator fires once on init – this covers the single-tab
(\`launch_pti_onepage\`) layout where there is no \`navbarPage\`
selection at all.

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
[`mod_ptipage_twocol_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_ptipage_twocol_ui.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# In a server function:
first_open <- mod_tab_open_first_newserv(
  id          = "first_open",
  active_tab  = reactive(input$tabpan),
  target_tabs = c("PTI", "PTI comparison")
)
observeEvent(first_open(), {
  message("Tab opened for the first time at ", first_open())
})
} # }
```
