# Strip unplottable admin levels out of a pre-plot data structure

Walks the pre-plot list and drops any element whose \`admin_level\` name
is listed in \`adm_to_drom\` for that element's \`pti_codes\`.

## Usage

``` r
drop_inval_adm(dta, adm_to_drom)
```

## Arguments

- dta:

  A pre-plot list as produced upstream of the plotting modules – each
  element has at least a \`pti_codes\` slot and a named \`admin_level\`.

- adm_to_drom:

  Named list keyed by \`pti_codes\` whose values are the admin-level
  names to remove for that PTI – typically the output of
  \[get_min_admin_wght()\].

## Value

The input list with offending elements removed.

## See also

Other validation:
[`app_validate_metadata()`](https://worldbank.github.io/devPTIpack/reference/app_validate_metadata.md),
[`app_validate_shp()`](https://worldbank.github.io/devPTIpack/reference/app_validate_shp.md),
[`mod_drop_inval_adm()`](https://worldbank.github.io/devPTIpack/reference/mod_drop_inval_adm.md),
[`validate_geometries()`](https://worldbank.github.io/devPTIpack/reference/validate_geometries.md),
[`validate_metadata()`](https://worldbank.github.io/devPTIpack/reference/validate_metadata.md),
[`validate_read_metadata()`](https://worldbank.github.io/devPTIpack/reference/validate_read_metadata.md),
[`validate_read_shp()`](https://worldbank.github.io/devPTIpack/reference/validate_read_shp.md)

## Examples

``` r
if (FALSE) { # \dontrun{
drop_inval_adm(preplot_dta, get_min_admin_wght(unavail, weights_clean))
} # }
```
