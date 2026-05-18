# List the bundled hex variables available for the PTI pipeline

Returns a tibble summarising every variable declared in the bundled
\`inst/hex_vars_registry.yaml\`. Call this at the top of Step 4 to
browse what indicators a deployer can pull from the registry without
leaving R. The output is the entry point that decides what to pass to
\[use_hex_vars()\].

## Usage

``` r
list_hex_vars()
```

## Value

A tibble with one row per (source, canonical name). Columns:
\`source_id\`, \`source_label\`, \`canonical_name\`, \`var_name\`,
\`var_units\`, \`time_col\`, \`available_years\` (a list-column),
\`weight\`, \`fun\`, \`is_population\` (logical, \`TRUE\` for the
registry-declared population variable).

## Details

When the same canonical name appears in multiple sources, the tibble
shows one row per (source, canonical name) pair so the deployer can pick
the source they want; \[use_hex_vars()\] will then disambiguate via a
\`\_\_\<source-label\>\` suffix on the \`canonical_name\` column.

## See also

\[use_hex_vars()\] to resolve specific variables;
\[get_available_years()\] to query the live parquet for ground truth on
temporal coverage.

Other data-input:
[`aggregate_hex_to_shapes()`](https://worldbank.github.io/devPTIpack/reference/aggregate_hex_to_shapes.md),
[`build_hex_metadata()`](https://worldbank.github.io/devPTIpack/reference/build_hex_metadata.md),
[`fct_template_reader()`](https://worldbank.github.io/devPTIpack/reference/fct_template_reader.md),
[`fetch_hex_data()`](https://worldbank.github.io/devPTIpack/reference/fetch_hex_data.md),
[`get_available_years()`](https://worldbank.github.io/devPTIpack/reference/get_available_years.md),
[`get_shape()`](https://worldbank.github.io/devPTIpack/reference/get_shape.md),
[`make_admin_lookup()`](https://worldbank.github.io/devPTIpack/reference/make_admin_lookup.md),
[`make_hex_grid()`](https://worldbank.github.io/devPTIpack/reference/make_hex_grid.md),
[`use_hex_vars()`](https://worldbank.github.io/devPTIpack/reference/use_hex_vars.md)

## Examples

``` r
list_hex_vars()
#> # A tibble: 2 × 10
#>   source_id         source_label      canonical_name var_name var_units time_col
#>   <chr>             <chr>             <chr>          <chr>    <chr>     <chr>   
#> 1 wb_flood_exposure WB Space2Stats F… population     Populat… count     NA      
#> 2 wb_flood_exposure WB Space2Stats F… flood_exposur… Flood E… count     NA      
#> # ℹ 4 more variables: available_years <list>, weight <chr>, fun <chr>,
#> #   is_population <lgl>
```
