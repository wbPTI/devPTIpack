# Page-level PTI server module

Orchestrates the weights-input, PTI calculation, and map-visualisation
sub-modules for a single PTI page (one tab in a multi-tab app, or the
whole app in a single-page deployment). Wires their reactives together
and returns the joined results so a sibling page (e.g. a comparison tab)
can read the same inputs and outputs.

## Usage

``` r
mod_ptipage_newsrv(
  id,
  inp_dta = reactive(NULL),
  shp_dta = reactive(NULL),
  active_tab = reactive(NULL),
  target_tabs = NULL,
  default_adm_level = NULL,
  show_adm_levels = NULL,
  shapes_path = "",
  mtdtpdf_path = "",
  show_waiter = FALSE,
  ...
)
```

## Arguments

- id:

  Character. Shiny module namespace ID. Must match the \`id\` passed to
  \[mod_ptipage_twocol_ui()\] or \[mod_ptipage_box_ui()\].

- inp_dta:

  Reactive yielding the metadata list (output of
  \[fct_template_reader()\]).

- shp_dta:

  Reactive yielding a named list of \`sf\` tibbles, one per admin level.

- active_tab:

  Reactive character indicating the currently selected tab. Used by the
  deferred-render guard so the map only renders on first open of a
  \`target_tabs\` tab.

- target_tabs:

  Character vector of tab names this module should render for.

- default_adm_level:

  Character or NULL. Default admin level shown in the side-panel
  selector.

- show_adm_levels:

  Character vector or NULL. Admin levels exposed in the selector.
  \`NULL\` shows all levels with data; a single value pins to that level
  (falling back to the most disaggregated level that has data); a vector
  restricts to the listed levels.

- shapes_path, mtdtpdf_path:

  Character. Filesystem paths to the shapes archive and metadata PDF
  served via download links.

- show_waiter:

  Logical. Whether to render the waiter overlay wrapping the page.

- ...:

  Additional arguments forwarded to nested sub-modules.

## Value

A \`shiny::reactiveValues\` object with elements \`plotted_dta\`,
\`map_dta\`, and \`wt_dta\`, allowing sibling modules to consume the
same outputs.

## See also

Other shiny-modules:
[`mod_dta_explorer2_server()`](https://worldbank.github.io/devPTIpack/reference/mod_dta_explorer2_server.md),
[`mod_dta_explorer2_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_dta_explorer2_ui.md),
[`mod_leaf_side_panel_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_leaf_side_panel_ui.md),
[`mod_map_pti_leaf_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_map_pti_leaf_ui.md),
[`mod_plot_pti2_srv()`](https://worldbank.github.io/devPTIpack/reference/mod_plot_pti2_srv.md),
[`mod_pti_comparepage_newsrv()`](https://worldbank.github.io/devPTIpack/reference/mod_pti_comparepage_newsrv.md),
[`mod_pti_comparepage_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_pti_comparepage_ui.md),
[`mod_ptipage_twocol_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_ptipage_twocol_ui.md),
[`mod_tab_open_first_newserv()`](https://worldbank.github.io/devPTIpack/reference/mod_tab_open_first_newserv.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mod_ptipage_newsrv(
  "page_pti",
  inp_dta     = inp_dta,
  shp_dta     = shp_dta,
  active_tab  = active_tab,
  target_tabs = "PTI"
)
} # }
```
