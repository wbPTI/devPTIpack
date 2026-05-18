# Fetch hex-level indicator data from registry parquet endpoints

Reads every variable in \`vars\` from its parquet source using
\`arrow::open_dataset()\` with predicate pushdown so only the hexagons
matching \`hex_ids\` are transferred. Year resolution (arch-11 §"Year
resolution") is applied automatically before fetching.

## Usage

``` r
fetch_hex_data(
  hex_ids,
  vars,
  dataset_loader = NULL,
  available_years_lookup = NULL
)
```

## Arguments

- hex_ids:

  Character vector of H3 cell IDs. Comes from \`hex_layer\$admin9Pcod\`
  (output of \[make_hex_grid()\]).

- vars:

  Named list of \`pti_hex_var\` descriptors as returned by
  \[use_hex_vars()\]. Year resolution is applied before fetching.

- dataset_loader:

  Optional. A function \`f(path)\` that opens a data source and returns
  an object supporting \[dplyr::filter()\], \[dplyr::select()\], and
  \[dplyr::collect()\]. Defaults to \[arrow::open_dataset()\]. Supply a
  stub in unit tests to keep them network-free.

- available_years_lookup:

  Optional named list of integer vectors, keyed by (unsuffixed)
  canonical variable name. Passed to \[resolve_years_for_vars()\] as its
  \`available_years_lookup\` seam. When \`NULL\` (default),
  \`get_available_years()\` is called per temporal variable. Supply a
  stub in unit tests to stay network-free.

## Value

A tibble with columns: \`hex_id\`, \`population\` (always the first
indicator column), then one column per non-temporal variable and one
\`\<canonical_name\>\_\<year\>\` column per (temporal variable ×
resolved year).

## Details

\`fetch_hex_data()\` has no registry knowledge of its own: all parquet
paths and column names are already embedded in the \`pti_hex_var\`
descriptors by \[use_hex_vars()\].

\*\*Resolution bridge (H5 grids):\*\* when \`hex_ids\` are at H3
resolution 5 and the source data is at H6, each H5 cell is transparently
expanded to its seven H6 children for the fetch, then the H6 values are
aggregated back to H5 using each variable's \`weight\`/\`fun\` strategy.
Finer-than-source grids (H7 or higher) produce an actionable error.

\*\*Temporal variables\*\* are fetched in long format and pivoted wide
before returning. Column names follow the convention
\`\<canonical_name\>\_\<resolved_year\>\`.

## See also

Other data-input:
[`aggregate_hex_to_shapes()`](https://worldbank.github.io/devPTIpack/reference/aggregate_hex_to_shapes.md),
[`build_hex_metadata()`](https://worldbank.github.io/devPTIpack/reference/build_hex_metadata.md),
[`fct_template_reader()`](https://worldbank.github.io/devPTIpack/reference/fct_template_reader.md),
[`get_available_years()`](https://worldbank.github.io/devPTIpack/reference/get_available_years.md),
[`get_shape()`](https://worldbank.github.io/devPTIpack/reference/get_shape.md),
[`list_hex_vars()`](https://worldbank.github.io/devPTIpack/reference/list_hex_vars.md),
[`make_admin_lookup()`](https://worldbank.github.io/devPTIpack/reference/make_admin_lookup.md),
[`make_hex_grid()`](https://worldbank.github.io/devPTIpack/reference/make_hex_grid.md),
[`use_hex_vars()`](https://worldbank.github.io/devPTIpack/reference/use_hex_vars.md)

## Examples

``` r
if (FALSE) { # \dontrun{
my_shp  <- readRDS("app-data/shapes.rds")
hex_ids <- my_shp$admin9_Hexagon$admin9Pcod
vars    <- use_hex_vars("flood_exposure_15cm_1in100")
hex_data <- fetch_hex_data(hex_ids, vars)
} # }
```
