# Query a parquet endpoint for the actual available years of a variable

Reads the registry to find the parquet path and \`time_col\` for the
supplied variable, opens the dataset via \[arrow::open_dataset()\], and
returns the distinct values found in \`time_col\`. Useful when the
\`available_years\` hint in the YAML is stale (or absent) and you want
ground truth before passing \`years = ...\` to \[use_hex_vars()\].

## Usage

``` r
get_available_years(var)
```

## Arguments

- var:

  A single canonical variable name, or a \`pti_hex_var\` already
  resolved via \[use_hex_vars()\]. Required.

## Value

Integer vector of years available in the source parquet, sorted
ascending. \`integer(0)\` for non-temporal variables.

## Details

Non-temporal variables (\`time_col\` is \`NA\`) return \`integer(0)\`.

## See also

Other data-input:
[`aggregate_hex_to_shapes()`](https://worldbank.github.io/devPTIpack/reference/aggregate_hex_to_shapes.md),
[`build_hex_metadata()`](https://worldbank.github.io/devPTIpack/reference/build_hex_metadata.md),
[`fct_template_reader()`](https://worldbank.github.io/devPTIpack/reference/fct_template_reader.md),
[`fetch_hex_data()`](https://worldbank.github.io/devPTIpack/reference/fetch_hex_data.md),
[`get_shape()`](https://worldbank.github.io/devPTIpack/reference/get_shape.md),
[`list_hex_vars()`](https://worldbank.github.io/devPTIpack/reference/list_hex_vars.md),
[`make_admin_lookup()`](https://worldbank.github.io/devPTIpack/reference/make_admin_lookup.md),
[`make_hex_grid()`](https://worldbank.github.io/devPTIpack/reference/make_hex_grid.md),
[`use_hex_vars()`](https://worldbank.github.io/devPTIpack/reference/use_hex_vars.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Non-temporal variable -> integer(0) without hitting the network.
get_available_years("flood_exposure_15cm_1in100")
} # }
```
