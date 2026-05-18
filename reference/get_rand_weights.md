# Generate a random list of weighting schemes

Builds between 1 and 5 weighting schemes (uniform random count), each
named with a random 5-letter prefix and a sequential index, and each
weight drawn from \`runif(-2, 2)\` then rounded to the nearest integer.
Useful for smoke-testing custom PTI configurations with realistic-shaped
(but meaningless) weights.

## Usage

``` r
get_rand_weights(indicators_list)
```

## Arguments

- indicators_list:

  Tibble with at least a \`var_code\` column, typically the output of
  \`get_indicators_list()\`.

## Value

A named list of tibbles. Each element is a tibble with columns
\`var_code\` and \`weight\`; element names follow the pattern
\`"\<5-letter-prefix\> \<index\>"\`.

## See also

Other weights:
[`get_all_weights_combs()`](https://worldbank.github.io/devPTIpack/reference/get_all_weights_combs.md),
[`get_min_admin_wght()`](https://worldbank.github.io/devPTIpack/reference/get_min_admin_wght.md)

## Examples

``` r
data(rwa_mtdt_full)
set.seed(1)
wts <- get_rand_weights(rwa_mtdt_full$metadata)
length(wts) >= 1 && length(wts) <= 5
#> [1] TRUE
all(vapply(wts, function(x) all(c("var_code", "weight") %in% names(x)),
           logical(1)))
#> [1] TRUE
```
