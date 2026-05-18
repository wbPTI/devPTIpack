# Ukraine sample PTI metadata input

A named list mirroring the structure that \[fct_template_reader()\]
produces from a PTI metadata Excel template. Bundled as the canonical
sample metadata input for \[launch_pti()\], \[run_pti_pipeline()\],
examples, and the test suite. Pairs with \[ukr_shp\] – the admin slot
names line up with the geometry slot names so the two can be passed
directly to the pipeline.

## Usage

``` r
ukr_mtdt_full
```

## Format

A named list of length 5:

- general:

  \`tbl_df\` with 1 row and column \`country\` – country-level metadata
  (display name, etc.).

- admin1_Oblast:

  \`tbl_df\` with 27 rows and 8 columns. Keys \`admin0Pcod\`,
  \`admin1Pcod\`, \`admin1Name\`, \`year\`; remaining 4 columns are
  indicator values (\`var\_\*\`) measured at the oblast level.

- admin2_Rayon:

  \`tbl_df\` with 629 rows and 11 columns. Keys \`admin0Pcod\`,
  \`admin1Pcod\`, \`admin2Pcod\`, \`admin2Name\`, \`year\`; remaining 6
  columns are indicator values (\`var\_\*\`) measured at the rayon
  level.

- admin4_Hexagon:

  \`tbl_df\` with 1,939 rows and 11 columns. Keys \`admin0Pcod\`,
  \`admin1Pcod\`, \`admin2Pcod\`, \`admin4Pcod\`, \`admin4Name\`,
  \`year\`; remaining 5 columns are indicator values (\`var\_\*\`)
  measured at the hex-cell level.

- metadata:

  \`tbl_df\` with 9 rows (one per indicator) and 14 columns describing
  each indicator: \`var_code\`, \`var_name\`, \`var_description\`,
  \`var_order\`, \`var_units\`, \`spatial_level\`, \`pillar_group\`,
  \`pillar_name\`, \`pillar_description\`, \`fltr_exclude_pti\`,
  \`fltr_exclude_explorer\`, \`fltr_overlay_pti\`,
  \`fltr_overlay_explorer\`, \`legend_revert_colours\`.

Indicator column names embed their semantics – for example
\`var_nval6_na_adm12\` denotes a 6-valued indicator with NAs available
at admin1 and admin2. Synthetic by design, so the suite can exercise the
calculation pipeline without exposing real administrative data.

## Source

Internal sample data bundled with the package for examples and tests.
Indicator values are synthetic.

## See also

Other sample-data:
[`rwa_mtdt_full`](https://worldbank.github.io/devPTIpack/reference/rwa_mtdt_full.md),
[`rwa_shp`](https://worldbank.github.io/devPTIpack/reference/rwa_shp.md),
[`ukr_shp`](https://worldbank.github.io/devPTIpack/reference/ukr_shp.md)

## Examples

``` r
data(ukr_mtdt_full)
names(ukr_mtdt_full)
#> [1] "general"        "admin1_Oblast"  "admin2_Rayon"   "admin4_Hexagon"
#> [5] "metadata"      
ukr_mtdt_full$general
#> # A tibble: 1 × 1
#>   country
#>   <chr>  
#> 1 Ukraine
head(ukr_mtdt_full$metadata)
#> # A tibble: 6 × 14
#>   var_code            var_name var_description var_order var_units spatial_level
#>   <chr>               <chr>    <chr>               <int> <chr>     <chr>        
#> 1 var_nval3_skewd_ad… var_nva… var_nval3_skew…         1 ""        admin1_Oblast
#> 2 var_nval6_na_adm12  var_nva… var_nval6_na_a…         2 ""        admin2_Rayon 
#> 3 var_nval15_small_s… var_nva… var_nval15_sma…         3 ""        admin2_Rayon 
#> 4 var_nvalinf_skewd_… var_nva… var_nvalinf_sk…         4 ""        admin2_Rayon 
#> 5 var_nval60_na_adm4  var_nva… var_nval60_na_…         5 ""        admin4_Hexag…
#> 6 var_nval4_small_sk… var_nva… var_nval4_smal…         6 ""        admin4_Hexag…
#> # ℹ 8 more variables: pillar_group <chr>, pillar_name <chr>,
#> #   pillar_description <chr>, fltr_exclude_pti <lgl>,
#> #   fltr_exclude_explorer <lgl>, fltr_overlay_pti <lgl>,
#> #   fltr_overlay_explorer <lgl>, legend_revert_colours <lgl>
```
