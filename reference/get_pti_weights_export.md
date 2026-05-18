# Reshape weight schemes into a single export tibble

Joins each weight scheme's \`(var_code, weight)\` tibble onto the
indicator dictionary (\`var_code\`, \`var_name\`, \`pillar_name\`),
relabelling the weight column to \`"Weights - \<scheme\>"\`. Reduces all
schemes to a single wide tibble keyed by indicator, then drops
\`var_code\` and renames the dictionary columns to user-facing headers.

## Usage

``` r
get_pti_weights_export(wghts_dta, indic_dta)
```

## Arguments

- wghts_dta:

  Named list of weight tibbles, one per scheme. Each element a tibble
  with \`var_code\` and \`weight\`.

- indic_dta:

  The indicator dictionary tibble (with \`var_code\`, \`var_name\`,
  \`pillar_name\`).

## Value

A tibble with \`Variable name\`, \`Pillar\`, and one \`Weights -
\<scheme\>\` column per scheme.

## See also

Other data-export:
[`get_pti_scores_export()`](https://worldbank.github.io/devPTIpack/reference/get_pti_scores_export.md),
[`get_vars_un_avbil()`](https://worldbank.github.io/devPTIpack/reference/get_vars_un_avbil.md)

## Examples

``` r
data(rwa_mtdt_full)
weights <- get_rand_weights(rwa_mtdt_full$metadata)
get_pti_weights_export(weights, rwa_mtdt_full$metadata)
#> # A tibble: 3 × 4
#>   `Variable name`     Pillar            `Weights - wlefo 1` `Weights - fiexo 2`
#>   <chr>               <chr>                           <dbl>               <dbl>
#> 1 Poverty rate        Poverty & welfare                  -1                  -2
#> 2 Adult literacy rate Poverty & welfare                   1                   0
#> 3 Road density        Connectivity                        1                   2
```
