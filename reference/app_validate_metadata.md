# Standalone validation app – geometry + metadata + Data Explorer

Launches a Shiny app that runs both \[validate_geometries()\] and
\[validate_metadata()\] on the supplied in-memory inputs, displays the
structured pass / warn / fail status in a sidebar, and embeds the
existing Data Explorer (\[mod_dta_explorer2_ui()\] /
\[mod_dta_explorer2_server()\]) so the deployer can spot-check indicator
values on a leaflet map next to the validation report.

## Usage

``` r
app_validate_metadata(shp_dta, inp_dta, app_name = "Validate metadata")
```

## Arguments

- shp_dta:

  Named list of \`sf\` tibbles, one slot per admin level (same structure
  as the bundled \[ukr_shp\]). Slot names follow the
  \`admin\<N\>\_HumanName\` convention.

- inp_dta:

  Named list of metadata tibbles as returned by
  \[fct_template_reader()\] (same structure as the bundled
  \[ukr_mtdt_full\]). Must include \`general\` and \`metadata\` slots
  plus per-admin tibbles.

- app_name:

  Character. Title shown in the browser tab and the page header.
  Defaults to \`"Validate metadata"\`.

## Value

A \[shiny::shinyApp()\] object wrapped in
\[golem::with_golem_options()\] so the embedded Data Explorer can read
its \`explorer\_\*\` golem options. Called primarily for its side effect
of starting an interactive Shiny app.

## Details

Use this in tutorial Step 3 (\`build-pti-3-metadata.qmd\`) after editing
the metadata Excel: a single launcher catches schema / pipeline /
cross-reference issues that \`validate_metadata()\` alone surfaces only
as console output, and lets the deployer visually confirm indicator
distributions before compiling the deployment bundle in Step 5.

The app renders even when the validators report \`status = "fail"\` so
the deployer can see \*what\* is wrong instead of getting an R error and
no UI. The Data Explorer is wrapped in \`tryCatch()\` so a single broken
indicator does not take the whole launcher down – if the explorer fails
to construct, only its tab area shows an error message and the
validation summary is still readable.

## See also

\[validate_geometries()\], \[validate_metadata()\],
\[app_validate_shp()\] for the geometry-only sibling, and
\[launch_pti()\] for the full PTI app.

Other validation:
[`app_validate_shp()`](https://worldbank.github.io/devPTIpack/reference/app_validate_shp.md),
[`drop_inval_adm()`](https://worldbank.github.io/devPTIpack/reference/drop_inval_adm.md),
[`mod_drop_inval_adm()`](https://worldbank.github.io/devPTIpack/reference/mod_drop_inval_adm.md),
[`validate_geometries()`](https://worldbank.github.io/devPTIpack/reference/validate_geometries.md),
[`validate_metadata()`](https://worldbank.github.io/devPTIpack/reference/validate_metadata.md),
[`validate_read_metadata()`](https://worldbank.github.io/devPTIpack/reference/validate_read_metadata.md),
[`validate_read_shp()`](https://worldbank.github.io/devPTIpack/reference/validate_read_shp.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Inspect the bundled Rwanda shapefile + synthetic metadata.
app_validate_metadata(rwa_shp, rwa_mtdt_full)

# Renders even when the geometry validator fails -- useful for
# visualising *what* is broken rather than only *that* something is.
broken_shp <- rwa_shp
broken_shp$admin1_Province$admin1Pcod <- NULL
app_validate_metadata(broken_shp, rwa_mtdt_full)
} # }
```
