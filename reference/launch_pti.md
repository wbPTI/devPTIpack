# Launch a multi-tab PTI Shiny app

Starts a full PTI app with a top-level navbar that can include any
combination of an info landing tab, the PTI page itself, a PTI compare
tab, a data explorer tab, and a how-it-works tab. Use this when
stakeholders need the comparison and explorer surfaces; for a focused
single-page viewer use \[launch_pti_onepage()\] instead.

## Usage

``` r
launch_pti(
  shp_dta,
  inp_dta,
  ui_type = c("twocol", "box"),
  app_name = "Some app",
  tabs = c("info", "compare", "explorer"),
  show_waiter = TRUE,
  show_adm_levels = NULL,
  wt_dwnld_options = c("data", "weights", "shapes", "metadata"),
  map_dwnld_options = c("shapes", "metadata"),
  shapes_path = NULL,
  mtdtpdf_path = NULL,
  data_path = NULL,
  map_height = "calc(100vh - 60px)",
  dt_style = "zoom:0.9; height: calc(95vh - 250px);",
  ...
)
```

## Arguments

- shp_dta:

  Named list of \`sf\` tibbles, one per admin level (e.g. the bundled
  \[ukr_shp\]). Each element must carry the \`adminNPcod\` /
  \`adminNName\` / \`geometry\` columns.

- inp_dta:

  Named list of metadata tibbles in the package format (e.g. the bundled
  \[ukr_mtdt_full\]); typically built by \[fct_template_reader()\].

- ui_type:

  Character. Either \`"twocol"\` (default) for the side-by-side weights
  / map layout or \`"box"\` for the boxed layout. Only the first element
  is used.

- app_name:

  Character. Title rendered in the navbar via \`add_logo()\`; also
  stored as \`pti.name\` in the golem options so downstream modules can
  reach it through \[golem::get_golem_options()\].

- tabs:

  Character vector picking which top-level tabs to show. Subset of
  \`c("info", "compare", "explorer", "how")\`. The PTI tab is always
  rendered. Defaults to \`c("info", "compare", "explorer")\`.

- show_waiter:

  Logical. If \`TRUE\` (default) a loading spinner covers the page until
  the first map render completes.

- show_adm_levels:

  Optional character vector restricting which admin levels appear in the
  level selector. \`NULL\` (default) shows every level present in
  \`shp_dta\`.

- wt_dwnld_options:

  Character vector of weights-tab download buttons to expose. Subset of
  \`c("data", "weights", "shapes", "metadata")\`.

- map_dwnld_options:

  Character vector of map-tab download buttons. Subset of \`c("shapes",
  "metadata")\`. Forced to \`NULL\` when \`ui_type = "box"\`.

- shapes_path, mtdtpdf_path:

  Character or \`NULL\`. Filesystem paths served by the download
  handlers. When \`shapes_path\` is \`NULL\` (the default), \`shp_dta\`
  is auto-written to a tempfile so the "Download shapes" button serves
  the in-memory data as an \`.rds\`. When \`mtdtpdf_path\` is \`NULL\`
  (the default), the metadata PDF link is disabled (no in-memory
  equivalent to materialize). To serve a specific source file, pass the
  path explicitly.

- data_path:

  Character or \`NULL\`. Filesystem path served by the data-explorer's
  "Download data" button. When \`NULL\` (the default), \`inp_dta\` is
  auto-written to a tempfile so the button serves the in-memory data as
  an \`.xlsx\`. To serve the original source xlsx instead (preserving
  its formatting), pass the path explicitly.

- map_height, dt_style:

  CSS strings forwarded to the map container and the weights DataTable
  respectively.

- ...:

  Additional arguments forwarded to \[mod_ptipage_newsrv()\] and to
  \[golem::with_golem_options()\].

## Value

A \[shiny::shinyApp()\] object wrapped in
\[golem::with_golem_options()\]. Called primarily for its side effect of
starting the Shiny app.

## See also

Other pti-launch:
[`create_new_pti()`](https://worldbank.github.io/devPTIpack/reference/create_new_pti.md),
[`launch_pti_onepage()`](https://worldbank.github.io/devPTIpack/reference/launch_pti_onepage.md)

## Examples

``` r
if (FALSE) { # \dontrun{
launch_pti(shp_dta = rwa_shp,
           inp_dta = rwa_mtdt_full,
           app_name = "Rwanda PTI demo")

launch_pti(shp_dta = rwa_shp,
           inp_dta = rwa_mtdt_full,
           tabs = c("info", "compare", "explorer", "how"))
} # }
```
