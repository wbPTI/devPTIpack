# Enumerate all weighting-scheme combinations of a given size

For each \`n_combo\` in \`n_items\`, enumerates every \`combn(var_codes,
n_combo)\` and returns one weighting scheme per combination, with weight
\`1\` on the variables in the combination and the others omitted
entirely. Useful for exhaustive batch analyses (e.g. all 3-of-N PTIs).

## Usage

``` r
get_all_weights_combs(var_codes, n_items = 3)
```

## Arguments

- var_codes:

  Character vector of variable codes to draw combinations from.

- n_items:

  Integer or integer vector. Combination sizes to enumerate. Defaults to
  \`3\` (all 3-of-N combinations).

## Value

A named list of tibbles. Each element is a tibble with columns
\`var_code\` and \`weight\`; element names follow the pattern \`"Wght of
\<n_combo\> comb no. \<index\>"\`. Total length is
\`sum(choose(length(var_codes), n_items))\`.

## See also

Other weights:
[`get_min_admin_wght()`](https://worldbank.github.io/devPTIpack/reference/get_min_admin_wght.md),
[`get_rand_weights()`](https://worldbank.github.io/devPTIpack/reference/get_rand_weights.md)

## Examples

``` r
data(rwa_mtdt_full)
codes <- rwa_mtdt_full$metadata$var_code
combos <- get_all_weights_combs(codes, n_items = 2)
length(combos)
#> [1] 3
choose(length(codes), 2)
#> [1] 3
```
