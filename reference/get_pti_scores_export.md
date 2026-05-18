# Reshape PTI scores into per-admin-level export tibbles

Walks the structured PTI list and, for each admin level, joins every
scheme's score tibble into a single wide tibble with one \`\<scheme\> -
PTI Score\` and \`\<scheme\> - PTI Priority\` column pair per weighting
scheme. Drops geometry, label, and area columns. The priority column is
computed via the per-scheme \`recode_function()\` attached by
\`add_legend_paras()\`.

## Usage

``` r
get_pti_scores_export(plotted_dta)
```

## Arguments

- plotted_dta:

  The post-filter / post-legend structured PTI list (one element per
  admin level / scheme; each element a list with \`pti_dta\`,
  \`pti_codes\`, \`admin_level\`, and a \`leg\` block).

## Value

A named list of tibbles – one per admin level. Each name is
\`"\<admin_level\> PTI Scores"\` (e.g. \`"admin1 PTI Scores"\`).

## See also

Other data-export:
[`get_pti_weights_export()`](https://worldbank.github.io/devPTIpack/reference/get_pti_weights_export.md),
[`get_vars_un_avbil()`](https://worldbank.github.io/devPTIpack/reference/get_vars_un_avbil.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# plotted_dta is the post-legend reactive returned by mod_plot_pti2_srv()
get_pti_scores_export(plotted_dta())
} # }
```
