# Read a PTI metadata Excel template into the package's list-of-tibbles format

Reads every sheet of the metadata \`.xlsx\` template and returns a named
list of tibbles in the shape downstream calculation and visualisation
code expects. Per-admin sheets are reduced to the columns referenced by
\`metadata\$var_code\` (plus the \`agg\_\*\`, \`admin\<N\>\*\`,
\`area\`, \`year\` helpers); admin sheets that have no remaining
indicator columns after that reduction are dropped. If a
\`weights_table\` sheet is present and non-empty, it is converted to the
package-internal \`weights_clean\` shape via
\[fct_convert_weight_to_clean()\]. Logical filter columns in
\`metadata\` (\`fltr_exclude_pti\`, \`fltr_exclude_explorer\`,
\`fltr_overlay_pti\`, \`fltr_overlay_explorer\`,
\`legend_revert_colours\`) are coerced to logical and \`NA\`s defaulted
to \`FALSE\`.

## Usage

``` r
fct_template_reader(...)
```

## Arguments

- ...:

  Character path components forming the path to the metadata \`.xlsx\`
  file. Passed verbatim to \[file.path()\], so either a single path
  string or several path segments are accepted.

## Value

A named list of tibbles. The shape mirrors \[ukr_mtdt_full\]: one tibble
per metadata sheet (\`general\`, per-admin tibbles named
\`admin\<N\>\_\*\`, and \`metadata\`), plus a derived \`weights_clean\`
slot when the input contains a \`weights_table\` sheet.

## See also

Other data-input:
[`aggregate_hex_to_shapes()`](https://worldbank.github.io/devPTIpack/reference/aggregate_hex_to_shapes.md),
[`build_hex_metadata()`](https://worldbank.github.io/devPTIpack/reference/build_hex_metadata.md),
[`fetch_hex_data()`](https://worldbank.github.io/devPTIpack/reference/fetch_hex_data.md),
[`get_available_years()`](https://worldbank.github.io/devPTIpack/reference/get_available_years.md),
[`get_shape()`](https://worldbank.github.io/devPTIpack/reference/get_shape.md),
[`list_hex_vars()`](https://worldbank.github.io/devPTIpack/reference/list_hex_vars.md),
[`make_admin_lookup()`](https://worldbank.github.io/devPTIpack/reference/make_admin_lookup.md),
[`make_hex_grid()`](https://worldbank.github.io/devPTIpack/reference/make_hex_grid.md),
[`use_hex_vars()`](https://worldbank.github.io/devPTIpack/reference/use_hex_vars.md)

## Examples

``` r
template_path <- system.file(
  "sample_pti/app-data/sample-metadata.xlsx",
  package = "devPTIpack"
)
if (nzchar(template_path)) {
  tmplt <- fct_template_reader(template_path)
  names(tmplt)
}
#> [1] "general"        "admin1_Oblast"  "admin2_Rayon"   "admin4_Hexagon"
#> [5] "metadata"      
```
