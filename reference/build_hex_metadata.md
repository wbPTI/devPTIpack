# Build the hex-pipeline metadata Excel workbook

Takes the tibble list from \[aggregate_hex_to_shapes()\] and writes a
\`metadata-hex.xlsx\` workbook in the same format as the Step-3 user
metadata template. Metadata for registry variables is auto-populated
from \`inst/hex_vars_registry.yaml\`; non-registry indicators must be
declared in \`indicator_config\`.

## Usage

``` r
build_hex_metadata(
  aggregated,
  shp_dta,
  indicator_config = NULL,
  country_name,
  output_path,
  include_hex,
  include_population = FALSE
)
```

## Arguments

- aggregated:

  Named list of tibbles as returned by \[aggregate_hex_to_shapes()\].
  Each tibble is one admin level (including \`admin9_Hexagon\`).

- shp_dta:

  Named list of sf tibbles (or plain tibbles), the same list that was
  passed to \[aggregate_hex_to_shapes()\]. Used only to identify the
  hex-level slot name when it is absent from \`aggregated\`.

- indicator_config:

  Optional tibble. Must contain a \`canonical_name\` column matching the
  stem of each non-registry indicator (e.g. \`"conflict"\` for
  \`conflict_2020\`). Additional columns (\`var_name\`,
  \`var_description\`, \`var_units\`, \`pillar_group\`, \`pillar_name\`,
  \`legend_revert_colours\`, \`fltr_exclude_pti\`,
  \`pillar_description\`) override registry defaults when the indicator
  is also in the registry; a warning is emitted per override.

- country_name:

  Character. Written to the \`general\` sheet.

- output_path:

  Character. Filesystem path for the output \`.xlsx\` file. Directories
  must already exist.

- include_hex:

  Logical. \`TRUE\` includes the \`admin9_Hexagon\` sheet; \`FALSE\`
  omits it. When not supplied and the hex grid has more than 5,000
  cells: interactive sessions prompt (default N); non-interactive
  sessions warn and default to \`FALSE\`.

- include_population:

  Logical (default \`FALSE\`). Set to \`TRUE\` to write \`population\`
  as a visible indicator row in \`metadata\` and as a column in all
  admin sheets.

## Value

Invisibly, \`output_path\`. The xlsx is validated with
\[validate_read_metadata()\] before returning; an error is thrown on
validation failure.

## Details

Population is excluded from the output by default (\`include_population
= FALSE\`); set it to \`TRUE\` to include population as a visible map
layer. When the hex grid exceeds 5,000 cells and \`include_hex\` is not
supplied, the function prompts interactively (default N) or silently
excludes the hex sheet in non-interactive sessions.

## See also

Other data-input:
[`aggregate_hex_to_shapes()`](https://worldbank.github.io/devPTIpack/reference/aggregate_hex_to_shapes.md),
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
vars      <- use_hex_vars("flood_exposure_15cm_1in100")
hex_data  <- fetch_hex_data(hex_layer$admin9Pcod, vars)
aggregated <- aggregate_hex_to_shapes(
  hex_data  = hex_data,
  hex_layer = hex_layer,
  shp_dta   = my_shp,
  strategy  = list(.default = c(weight = "pop", fun = "mean"),
                   flood_exposure_15cm_1in100 = c(weight = "none", fun = "sum"))
)
build_hex_metadata(
  aggregated   = aggregated,
  shp_dta      = my_shp,
  country_name = "Rwanda",
  output_path  = "app-data/metadata-hex.xlsx",
  include_hex  = FALSE
)
} # }
```
