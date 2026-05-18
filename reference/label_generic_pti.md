# Apply a glue-template \`pti_label\` column to per-admin PTI tibbles

Iterates over each admin-level tibble in \`dta\` and creates a
\`pti_label\` column by interpolating \`glue_expr\` against the row's
columns (typically \`spatial_name\`, \`pti_name\`, \`pti_score\`).
Override \`glue_expr\` to customise hover popups in the leaflet map.

## Usage

``` r
label_generic_pti(dta, glue_expr = generic_pti_glue())
```

## Arguments

- dta:

  Named list of per-admin PTI tibbles, as returned by
  \[agg_pti_scores()\] (must contain the columns referenced by
  \`glue_expr\`).

- glue_expr:

  Character. A glue-compatible template; defaults to
  \[generic_pti_glue()\]. Each row's columns are available as
  replacement variables.

## Value

A named list of the same shape as \`dta\`, each tibble with an added
\`pti_label\` character column.

## See also

Other pti-pipeline:
[`compile_pti_data()`](https://worldbank.github.io/devPTIpack/reference/compile_pti_data.md),
[`generic_pti_glue()`](https://worldbank.github.io/devPTIpack/reference/generic_pti_glue.md),
[`run_pti_pipeline()`](https://worldbank.github.io/devPTIpack/reference/run_pti_pipeline.md)

## Examples

``` r
scores <- list(
  admin1 = tibble::tibble(
    spatial_name = c("Cherkasy", "Kyiv"),
    pti_name = "Equal weights",
    pti_score = c(0.42, -0.31)
  )
)

labelled <- label_generic_pti(scores)
labelled$admin1$pti_label[1]
#> <strong>Cherkasy</strong><br/>Weighting scheme: <strong>Equal weights</strong><br/>PTI score: <strong>0.42000</strong><br/>

custom <- "{spatial_name}: {round(pti_score, 2)}"
label_generic_pti(scores, glue_expr = custom)$admin1$pti_label
#> Cherkasy: 0.42
#> Kyiv: -0.31
```
