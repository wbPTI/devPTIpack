# Visual shapefile inspector for PTI deployers

Launches a standalone Shiny app that renders the supplied shapefile list
on a leaflet map so the deployer can visually spot holes, wrong
boundaries, or geometry artefacts before running the calculation
pipeline. Internally calls \[validate_geometries()\] with
\`error_on_fail = FALSE\` and surfaces the structured diagnostic summary
alongside the map. The app is read-only – it never mutates the inputs.

## Usage

``` r
app_validate_shp(shp, app_name = "Validate shapefiles")
```

## Arguments

- shp:

  Named list of \`sf\` tibbles, one slot per admin level (same structure
  as the bundled \[ukr_shp\]). Slot names follow the
  \`admin\<N\>\_HumanName\` convention (e.g. \`admin1_Oblast\`). Each
  layer must carry an \`admin\<N\>Pcod\` and \`admin\<N\>Name\` column.
  The app also renders shapes that fail \[validate_geometries()\] so the
  deployer can see what is wrong instead of getting a console error.

- app_name:

  Character. Title shown in the browser tab and the page header.
  Defaults to \`"Validate shapefiles"\`.

## Value

A \[shiny::shinyApp()\] object. Called primarily for its side effect of
starting an interactive Shiny app.

## Details

Use this in tutorial Step 1 (\`build-pti-1-shapefiles.qmd\`) or any time
you receive a new country's boundary data: a 30-second visual pass
catches misaligned admin levels, dropped polygons, and projection
mistakes that a structural validator alone misses.

## See also

\[validate_geometries()\] for the underlying structural validator;
\[launch_pti()\] for the full PTI app.

Other validation:
[`app_validate_metadata()`](https://worldbank.github.io/devPTIpack/reference/app_validate_metadata.md),
[`drop_inval_adm()`](https://worldbank.github.io/devPTIpack/reference/drop_inval_adm.md),
[`mod_drop_inval_adm()`](https://worldbank.github.io/devPTIpack/reference/mod_drop_inval_adm.md),
[`validate_geometries()`](https://worldbank.github.io/devPTIpack/reference/validate_geometries.md),
[`validate_metadata()`](https://worldbank.github.io/devPTIpack/reference/validate_metadata.md),
[`validate_read_metadata()`](https://worldbank.github.io/devPTIpack/reference/validate_read_metadata.md),
[`validate_read_shp()`](https://worldbank.github.io/devPTIpack/reference/validate_read_shp.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Inspect the bundled Rwanda shapefile.
app_validate_shp(rwa_shp)

# The app still renders if a layer is structurally broken -- useful
# for spotting *what* is wrong rather than only *that* something is.
broken <- rwa_shp
broken$admin1_Province$admin1Pcod <- NULL
app_validate_shp(broken)
} # }
```
