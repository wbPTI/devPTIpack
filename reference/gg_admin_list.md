# Build a list of ggplot maps from PTI admin data

For each indicator column in \`dta\`, joins the matching admin
geometries from \`mt\` and produces a \`ggplot\` choropleth. Used by the
metadata-PDF rmarkdown templates (\`inst/metadata.Rmd\`,
\`inst/sample_pti/app-data/pti-metadata-pdf.Rmd\`) to render one map per
indicator alongside its description.

## Usage

``` r
gg_admin_list(dta, multiply = 1, mt = NULL, metadata = NULL)
```

## Arguments

- dta:

  A tibble with one or more \`adminN\` identifier columns and additional
  indicator columns to plot. Long format produced by \`pivot_pti_dta()\`
  is not required – the function pivots internally.

- multiply:

  Numeric. Currently unused; preserved for backward compatibility with
  caller scripts.

- mt:

  Named list of admin geometry tibbles (e.g. \`ukr_shp\`). The element
  whose name matches the \`adminN\` column in \`dta\` is joined onto the
  long-pivoted data via \`dplyr::right_join\`. Required (the function
  errors if \`NULL\`).

- metadata:

  Optional tibble with \`var_code\` and \`var_name\` columns used to
  relabel indicators in the plot title. When \`NULL\`, indicator codes
  are used verbatim.

## Value

A list of \`ggplot\` objects, one per indicator, with \`geom_sf\`
polygons filled by the indicator value.

## Details

Variable codes are relabelled to human-readable \`var_name\` values when
\`metadata\` supplies them; otherwise the raw code is kept. Rows whose
value is \`NA\` are dropped.

## Examples

``` r
data(rwa_shp)
data(rwa_mtdt_full)
maps <- gg_admin_list(
  dta      = rwa_mtdt_full$admin1_Province,
  mt       = rwa_shp,
  metadata = rwa_mtdt_full$metadata
)
#> Joining with `by = join_by(admin0Pcod)`
length(maps)
#> [1] 4
inherits(maps[[1]], "ggplot")
#> [1] TRUE
```
