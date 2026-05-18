# Ukraine sample administrative boundaries

A named list of \`sf\` tibbles representing Ukraine's administrative
boundaries at four hierarchical levels. Bundled with the package as the
canonical sample geometry input for \[launch_pti()\],
\[run_pti_pipeline()\], examples, and the test suite.

## Usage

``` r
ukr_shp
```

## Format

A named list of length 4. Each element is an \`sf\` / \`tbl_df\` with
one row per polygon at that admin level:

- admin0_Country:

  1 row – country polygon (Ukraine).

- admin1_Oblast:

  27 rows – first-level admin regions (oblasts).

- admin2_Rayon:

  629 rows – second-level admin regions (rayons).

- admin4_Hexagon:

  1,939 rows – H3 hexagonal grid cells used as a synthetic admin4 level.

Each tibble contains, at its admin depth \`N\`, the columns:

- adminNPcod:

  Character. Unique polygon identifier (P-code) – e.g. \`admin0Pcod\` at
  admin0, \`admin1Pcod\` at admin1.

- adminNName:

  Character. Human-readable polygon name.

- area:

  Numeric. Polygon area in the layer's native units.

- geometry:

  \`sfc_MULTIPOLYGON\`. Spatial geometry column.

Sub-admin tibbles also carry the parent-level P-code columns (e.g.
\`admin1_Oblast\` has both \`admin0Pcod\` and \`admin1Pcod\`) so joins
to higher levels work without re-deriving keys.

## Source

Internal sample data bundled with the package for examples and tests.
Derived from public Ukraine administrative boundaries.

## Details

Slot names follow the package convention \`adminN_HumanName\` –
\`adminN\` is \`admin0\` for the country polygon, then
\`admin1\`..\`admin9\` for nested sub-divisions; \`HumanName\` is a
single word naming the level (e.g. \`Oblast\`, \`Rayon\`, \`Hexagon\`).
The bundled sample uses levels 0, 1, 2, and 4 (admin3 is intentionally
skipped; \`admin4\` is a synthetic H3 hexagon grid). This naming is what
downstream functions like \[get_adm_levels()\] and
\[expand_adm_levels()\] parse to build the admin hierarchy.

## See also

Other sample-data:
[`rwa_mtdt_full`](https://worldbank.github.io/devPTIpack/reference/rwa_mtdt_full.md),
[`rwa_shp`](https://worldbank.github.io/devPTIpack/reference/rwa_shp.md),
[`ukr_mtdt_full`](https://worldbank.github.io/devPTIpack/reference/ukr_mtdt_full.md)

## Examples

``` r
data(ukr_shp)
names(ukr_shp)
#> [1] "admin0_Country" "admin1_Oblast"  "admin2_Rayon"   "admin4_Hexagon"
head(ukr_shp[["admin1_Oblast"]])
#> Simple feature collection with 6 features and 4 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: 24.89846 ymin: 44.39125 xmax: 39.07302 ymax: 52.36816
#> Geodetic CRS:  WGS 84
#> # A tibble: 6 × 5
#>   admin0Pcod admin1Pcod admin1Name        area                          geometry
#>   <chr>      <chr>      <chr>            <dbl>                    <GEOMETRY [°]>
#> 1 UK         UK01       Cherkasy        20918. MULTIPOLYGON (((30.41548 48.5839…
#> 2 UK         UK02       Chernihiv       32483. POLYGON ((31.54029 50.52451, 31.…
#> 3 UK         UK03       Chernivtsi       8185. POLYGON ((25.00833 47.84307, 24.…
#> 4 UK         UK04       Crimea          26456. POLYGON ((34.12903 44.42986, 34.…
#> 5 UK         UK05       Dnipropetrovs'k 31871. POLYGON ((33.64359 47.48654, 33.…
#> 6 UK         UK06       Donets'k        26833. POLYGON ((38.23455 47.11986, 38.…
```
