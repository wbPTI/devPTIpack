# Drop-invalid-admin module server

Filters administrative levels that cannot be plotted because the
currently-weighted indicators are unavailable there. Watches
\`wt_dta()\$indicators_list\` to derive the unavailable admin levels per
weighting scheme via \[get_vars_un_avbil()\] / \[get_min_admin_wght()\];
fires a \`shiny::showNotification()\` listing the dropped levels when
any are dropped; returns a reactive that yields \`dta()\` filtered via
\[drop_inval_adm()\].

## Usage

``` r
mod_drop_inval_adm(id, dta, wt_dta)
```

## Arguments

- id:

  Character. Shiny module namespace ID.

- dta:

  Reactive. The pre-plot data structure that downstream modules consume
  – typically the output of \[preplot_reshape_wghtd_dta()\].

- wt_dta:

  Reactive. A list-like value carrying at least \`indicators_list\` (a
  tibble with availability metadata) and \`weights_clean\` (the named
  list of per-scheme weights tibbles).

## Value

A reactive expression that yields \`dta()\` with admin levels excluded
where the active weighting cannot produce scores.

## See also

Other validation:
[`app_validate_metadata()`](https://worldbank.github.io/devPTIpack/reference/app_validate_metadata.md),
[`app_validate_shp()`](https://worldbank.github.io/devPTIpack/reference/app_validate_shp.md),
[`drop_inval_adm()`](https://worldbank.github.io/devPTIpack/reference/drop_inval_adm.md),
[`validate_geometries()`](https://worldbank.github.io/devPTIpack/reference/validate_geometries.md),
[`validate_metadata()`](https://worldbank.github.io/devPTIpack/reference/validate_metadata.md),
[`validate_read_metadata()`](https://worldbank.github.io/devPTIpack/reference/validate_read_metadata.md),
[`validate_read_shp()`](https://worldbank.github.io/devPTIpack/reference/validate_read_shp.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mod_drop_inval_adm("drop-inval", dta = preplot_dta, wt_dta = weights_dta)
} # }
```
