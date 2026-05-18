# Resolve hex variable names against the registry

Takes canonical variable names (unquoted, quoted, or both via \`...\`)
and returns a list of \`pti_hex_var\` descriptors ready to pass to
\`fetch_hex_data()\`. \*\*Always injects the population variable\*\*
(tagged \`internal = TRUE\`) regardless of what the deployer requests –
downstream aggregation needs it.

## Usage

``` r
use_hex_vars(..., years = NULL)
```

## Arguments

- ...:

  Canonical variable names. Either bare symbols, quoted strings, or
  a mix. Duplicates are removed silently. Pass no names to get just the
  auto-injected population variable.

- years:

  Optional integer vector of requested years for temporal variables.
  Stored on each \`pti_hex_var\` for the year resolver in
  \[fetch_hex_data()\] (arch-11 §"Year resolution" / issue
  [\#110](https://github.com/worldbank/devPTIpack/issues/110)) to
  consult; this function does \*\*not\*\* resolve nearest-year
  substitutions itself.

## Value

A list of \`pti_hex_var\` objects, named by their (possibly suffixed)
canonical name. Population always appears in the list, tagged \`internal
= TRUE\`.

## Details

When the same canonical name appears in multiple sources, the returned
descriptor's \`canonical_name\` is suffixed with
\`\_\_\<source-label\>\` (e.g. \`poverty_rate\_\_DEC\`,
\`poverty_rate\_\_local\`) so each becomes a distinct entry in the
returned list and a distinct row downstream in \`metadata-hex.xlsx\`.

## See also

\[list_hex_vars()\] to browse what is available;
\[get_available_years()\] to confirm temporal coverage.

Other data-input:
[`aggregate_hex_to_shapes()`](https://worldbank.github.io/devPTIpack/reference/aggregate_hex_to_shapes.md),
[`build_hex_metadata()`](https://worldbank.github.io/devPTIpack/reference/build_hex_metadata.md),
[`fct_template_reader()`](https://worldbank.github.io/devPTIpack/reference/fct_template_reader.md),
[`fetch_hex_data()`](https://worldbank.github.io/devPTIpack/reference/fetch_hex_data.md),
[`get_available_years()`](https://worldbank.github.io/devPTIpack/reference/get_available_years.md),
[`get_shape()`](https://worldbank.github.io/devPTIpack/reference/get_shape.md),
[`list_hex_vars()`](https://worldbank.github.io/devPTIpack/reference/list_hex_vars.md),
[`make_admin_lookup()`](https://worldbank.github.io/devPTIpack/reference/make_admin_lookup.md),
[`make_hex_grid()`](https://worldbank.github.io/devPTIpack/reference/make_hex_grid.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Resolve a single variable; population is auto-injected.
vars <- use_hex_vars("flood_exposure_15cm_1in100")
names(vars)
# Pass a year hint for temporal variables (year resolver runs in
# fetch_hex_data() per arch-11 #110).
vars <- use_hex_vars("flood_exposure_15cm_1in100", years = 2020L)
} # }
```
