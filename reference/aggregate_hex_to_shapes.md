# Aggregate hex-level indicator data to administrative shapes

Takes the tibble from \[fetch_hex_data()\] and aggregates each indicator
column to every admin level present in \`shp_dta\`, using the
per-variable weight × function strategy supplied by the deployer.
Aggregation is always \*\*hex → admin level directly\*\* (never chained
through intermediate levels).

## Usage

``` r
aggregate_hex_to_shapes(hex_data, hex_layer, shp_dta, strategy)
```

## Arguments

- hex_data:

  Tibble returned by \[fetch_hex_data()\]: must contain a \`hex_id\`
  column, a \`population\` column (unless no indicator requests \`weight
  = "pop"\`), and one column per indicator.

- hex_layer:

  The \`admin9_Hexagon\` sf layer (or plain tibble) from \`shapes.rds\`,
  as built by \[make_hex_grid()\] and enriched by
  \[make_admin_lookup()\]. Must contain \`admin\<N\>Pcod\` (own level)
  and parent Pcod columns for every admin level in \`shp_dta\`.

- shp_dta:

  Named list of sf tibbles (or plain tibbles), one per admin level
  including the hex level. Names must follow the
  \`admin\<N\>\_\<HumanName\>\` convention.

- strategy:

  Named list specifying the aggregation weight and function for each
  indicator. \*\*Must\*\* contain a \`.default\` entry. Each entry is a
  named character vector with elements: - \`weight\`: \`"pop"\`
  (population-weighted), \`"area"\` (area-weighted), or \`"none"\`
  (unweighted). - \`fun\`: \`"mean"\`, \`"sum"\`, \`"median"\`,
  \`"min"\`, or \`"max"\`. Ignored when \`weight\` is \`"pop"\` or
  \`"area"\` (always uses weighted mean). Temporal variable columns
  (e.g. \`nightlights_2020\`) are matched by stripping the
  \`\_\<year\>\` suffix to get the stem, then looking up the stem in
  \`strategy\` before falling back to \`.default\`.

## Value

Named list of tibbles, one per slot in \`shp_dta\`. Each tibble contains
\`admin\<N\>Pcod\`, \`admin\<N\>Name\`, parent Pcod columns,
\`population\` (aggregated), and one column per indicator.

## Details

Population always aggregates as an unweighted sum regardless of any
\`population\` entry in \`strategy\` (a warning is emitted and the entry
is ignored). The population column is preserved in every output tibble
for downstream use by \[build_hex_metadata()\].

## See also

Other data-input:
[`build_hex_metadata()`](https://worldbank.github.io/devPTIpack/reference/build_hex_metadata.md),
[`fct_template_reader()`](https://worldbank.github.io/devPTIpack/reference/fct_template_reader.md),
[`fetch_hex_data()`](https://worldbank.github.io/devPTIpack/reference/fetch_hex_data.md),
[`get_available_years()`](https://worldbank.github.io/devPTIpack/reference/get_available_years.md),
[`get_shape()`](https://worldbank.github.io/devPTIpack/reference/get_shape.md),
[`list_hex_vars()`](https://worldbank.github.io/devPTIpack/reference/list_hex_vars.md),
[`make_admin_lookup()`](https://worldbank.github.io/devPTIpack/reference/make_admin_lookup.md),
[`make_hex_grid()`](https://worldbank.github.io/devPTIpack/reference/make_hex_grid.md),
[`use_hex_vars()`](https://worldbank.github.io/devPTIpack/reference/use_hex_vars.md)

## Examples

``` r
if (FALSE) { # \dontrun{
my_shp    <- readRDS("app-data/shapes.rds")
hex_layer <- my_shp$admin9_Hexagon
hex_ids   <- hex_layer$admin9Pcod
vars      <- use_hex_vars("flood_exposure_15cm_1in100")
hex_data  <- fetch_hex_data(hex_ids, vars)
aggregated <- aggregate_hex_to_shapes(
  hex_data  = hex_data,
  hex_layer = hex_layer,
  shp_dta   = my_shp,
  strategy  = list(
    .default                   = c(weight = "pop",  fun = "mean"),
    flood_exposure_15cm_1in100 = c(weight = "none", fun = "sum")
  )
)
names(aggregated)
} # }
```
