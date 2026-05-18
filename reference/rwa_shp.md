# Rwanda sample administrative boundaries

A named list of \`sf\` tibbles representing Rwanda's administrative
boundaries at four hierarchical levels, including an H3-6 hex grid.
Bundled with the package as the canonical sample geometry input for
user-facing \`@examples\` across exported functions, and the
worked-example country in the Build-a-PTI website tutorial.

## Usage

``` r
rwa_shp
```

## Format

A named list of length 4. Each element is an \`sf\` / \`tbl_df\`:

- admin0_Country:

  1 row – country polygon (Rwanda).

- admin1_Province:

  5 rows – provinces.

- admin2_District:

  30 rows – districts.

- admin9_Hexagon:

  507 rows – H3-6 hex cells covering the country envelope, retained by
  centroid-in-polygon filter.

Each tibble carries the standard \`adminNPcod\` / \`adminNName\` /
\`area\` / \`geometry\` columns plus every ancestor \`admin\<k\>Pcod\`
on sub-admin layers. \*\*\`area\` is in km^2\*\* (EPSG:4326 + s2
geodesic computation). On \`admin9_Hexagon\`, \`admin9Pcod\` is the H3
index string at resolution 6 (globally unique) and \`admin9Name\` is the
same value – hexagons have no human name.

Built by \`data-raw/generate-rwa-package-data.R\` via
\[make_hex_grid()\] (for the hex layer) and \[make_admin_lookup()\] (for
the parent Pcod cascade).

## Source

Boundary geometries: geoBoundaries (<https://www.geoboundaries.org>)
\`gbOpen\` release for Rwanda (\`shapeISO = "RWA"\`), licensed CC-BY
4.0. See the raw GeoJSONs under \`inst/template_pti/sample-data/\`
(\`rwa_adm0.geojson\`, \`rwa_adm1.geojson\`, \`rwa_adm2.geojson\`) and
the compilation script at \`data-raw/generate-rwa-package-data.R\`. The
hex layer is computed deterministically from \`admin0_Country\` at H3-6
via \[make_hex_grid()\].

## Details

Slot names follow the package convention \`adminN_HumanName\`. The
bundled sample uses levels 0, 1, 2, and 9: country (1 polygon), 5
provinces, 30 districts, and 507 H3-6 hexagons (~36 km^2 each) covering
the country envelope. Pair with \[rwa_mtdt_full\] for a working PTI
calculation.

Compared with the test-suite-oriented \[ukr_shp\]: smaller (~543 vs
2,596 polygons), built from a public CC-BY 4.0 source, and safe to
render in tutorials and embed in screenshots.

## See also

Other sample-data:
[`rwa_mtdt_full`](https://worldbank.github.io/devPTIpack/reference/rwa_mtdt_full.md),
[`ukr_mtdt_full`](https://worldbank.github.io/devPTIpack/reference/ukr_mtdt_full.md),
[`ukr_shp`](https://worldbank.github.io/devPTIpack/reference/ukr_shp.md)

## Examples

``` r
data(rwa_shp)
names(rwa_shp)
#> [1] "admin0_Country"  "admin1_Province" "admin2_District" "admin9_Hexagon" 
head(rwa_shp[["admin1_Province"]])
#> Simple feature collection with 5 features and 4 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 28.86171 ymin: -2.839973 xmax: 30.89908 ymax: -1.04745
#> Geodetic CRS:  WGS 84
#> # A tibble: 5 × 5
#>   admin1Pcod              admin1Name   area                  geometry admin0Pcod
#>   <chr>                   <chr>       <dbl>             <POLYGON [°]> <chr>     
#> 1 91480417B22690809677968 City of Ki…  733. ((30.24777 -1.845749, 30… RWA       
#> 2 91480417B18399648061901 Southern P… 5984. ((29.98206 -1.91296, 29.… RWA       
#> 3 91480417B74917756285357 Northern P… 3287. ((30.24777 -1.845749, 30… RWA       
#> 4 91480417B20985019767017 Eastern Pr… 9509. ((30.01937 -2.074118, 30… RWA       
#> 5 91480417B44016762519517 Western Pr… 5905. ((29.65887 -1.734995, 29… RWA       
head(rwa_shp[["admin9_Hexagon"]])
#> Simple feature collection with 6 features and 6 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 30.36778 ymin: -1.565653 xmax: 30.54621 ymax: -1.379127
#> Geodetic CRS:  WGS 84
#>        admin9Pcod      admin9Name     area              admin2Pcod
#> 1 866add407ffffff 866add407ffffff 40.86591 39286606B53863210323147
#> 2 866add40fffffff 866add40fffffff 40.87297 39286606B53863210323147
#> 3 866add417ffffff 866add417ffffff 40.88521 39286606B31387731734731
#> 4 866add41fffffff 866add41fffffff 40.89235 39286606B53863210323147
#> 5 866add427ffffff 866add427ffffff 40.83935 39286606B31387731734731
#> 6 866add42fffffff 866add42fffffff 40.84647 39286606B31387731734731
#>                admin1Pcod admin0Pcod                       geometry
#> 1 91480417B20985019767017        RWA POLYGON ((30.47485 -1.50018...
#> 2 91480417B20985019767017        RWA POLYGON ((30.53286 -1.52312...
#> 3 91480417B20985019767017        RWA POLYGON ((30.4258 -1.537811...
#> 4 91480417B20985019767017        RWA POLYGON ((30.48382 -1.56075...
#> 5 91480417B20985019767017        RWA POLYGON ((30.46587 -1.43964...
#> 6 91480417B20985019767017        RWA POLYGON ((30.52388 -1.46257...
```
