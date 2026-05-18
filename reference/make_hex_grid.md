# Build an H3 hexagonal grid covering a country boundary

Constructs a uniform H3 hexagonal grid as a standard \`sf\` tibble
conforming to the basic \`admin9_Hexagon\` column contract
(\`admin0Pcod\`, \`admin9Pcod\`, \`admin9Name\`, \`area\`,
\`geometry\`).

## Usage

``` r
make_hex_grid(country_polygon, resolution = 6)
```

## Arguments

- country_polygon:

  \`sf\` object containing the country boundary. If multi-row, all rows
  are unioned via \[sf::st_union()\] before proceeding – the user may
  pass any admin layer (e.g. \`admin1_Province\`) and the function will
  derive the outer envelope. If the object carries an \`admin0Pcod\`
  column, the first row's value is inherited onto every hex cell;
  otherwise \`admin0Pcod\` is set to \`NA\` and a warning is emitted.
  The input is automatically reprojected to EPSG:4326 if not already so.

- resolution:

  Integer scalar. H3 resolution for the returned grid. Acceptable
  values: \`5\`, \`6\`, \`7\`. Defaults to \`6\`. The internal
  coarse-filter resolution is derived as \`resolution - 2\`.

## Value

An \`sf\` tibble in EPSG:4326 with one row per retained hex cell.
Columns: \`admin0Pcod\` (character, inherited from input or \`NA\`),
\`admin9Pcod\` (character, H3 index at \`resolution\`, globally unique),
\`admin9Name\` (character, same as \`admin9Pcod\` – hexagons have no
human name), \`area\` (numeric, km²), \`geometry\` (\`sfc_POLYGON\`).

## Details

This is the \*\*first\*\* of the two-step Step-1 hex workflow: this
function produces the partial layer; \`make_admin_lookup()\` (arch-10
§3, issue [\#109](https://github.com/worldbank/devPTIpack/issues/109))
then populates the parent Pcod columns (\`admin1Pcod\`, \`admin2Pcod\`,
...) via centroid-in-polygon joins so the layer satisfies the full
cascade rule. Calling \[validate_geometries()\] on a \`my_shp\` list
that contains \`make_hex_grid()\`'s raw output but no parent Pcods on
the hex layer will fail the cascade check – this is expected. Use
\`make_admin_lookup()\` to complete the cascade before validation.

The algorithm has three steps, only two of which involve spatial
operations:

1\. \*\*Coarse spatial filter\*\* (spatial): find every H3 cell at
\`resolution - 2\` that intersects the (unioned) country polygon.
Operates on a handful of large cells. 2. \*\*H3 child expansion\*\*
(pure H3 math): expand each coarse cell to all of its \`resolution\`
children deterministically via the H3 hierarchy. No spatial computation.
3. \*\*Centroid filter\*\* (spatial): compute centroids of every fine H3
cell from step 2; retain only those whose centroid lies within the
country polygon. Point-in-polygon is fast even at tens of thousands of
points.

Both spatial steps (1 and 3) are wrapped in an \`s2\` fallback: on error
with \`s2\` enabled, the call is retried with \`s2\` disabled.
\`on.exit()\` restores the caller's \`sf::sf_use_s2()\` state regardless
of outcome.

All hex cells at a given resolution have near-identical area (H3-5 ~252
km², H3-6 ~36 km², H3-7 ~5.16 km²) – this is expected, not a data error.

## See also

\[make_admin_lookup()\] (forthcoming, arch-10 §3) for the cascade
builder that wires hex cells into the parent admin layers;
\[validate_geometries()\] for the structural validator the
\`make_admin_lookup()\`-enriched layer satisfies.

Other data-input:
[`aggregate_hex_to_shapes()`](https://worldbank.github.io/devPTIpack/reference/aggregate_hex_to_shapes.md),
[`build_hex_metadata()`](https://worldbank.github.io/devPTIpack/reference/build_hex_metadata.md),
[`fct_template_reader()`](https://worldbank.github.io/devPTIpack/reference/fct_template_reader.md),
[`fetch_hex_data()`](https://worldbank.github.io/devPTIpack/reference/fetch_hex_data.md),
[`get_available_years()`](https://worldbank.github.io/devPTIpack/reference/get_available_years.md),
[`get_shape()`](https://worldbank.github.io/devPTIpack/reference/get_shape.md),
[`list_hex_vars()`](https://worldbank.github.io/devPTIpack/reference/list_hex_vars.md),
[`make_admin_lookup()`](https://worldbank.github.io/devPTIpack/reference/make_admin_lookup.md),
[`use_hex_vars()`](https://worldbank.github.io/devPTIpack/reference/use_hex_vars.md)

## Examples

``` r
data(rwa_shp)

# Build a coarse H3-5 hex grid over Rwanda (~100 cells; fast for
# examples). The default is resolution = 6 (~500 cells, ~40 km^2);
# use 7 for ~5 km^2 cells on small countries or high-detail apps.
hex <- make_hex_grid(rwa_shp$admin0_Country, resolution = 5)
nrow(hex)
#> [1] 37
head(hex)
#> Simple feature collection with 6 features and 4 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 29.9206 ymin: -2.399834 xmax: 30.34563 ymax: -1.922914
#> Geodetic CRS:  WGS 84
#>   admin0Pcod      admin9Pcod      admin9Name     area
#> 1        RWA 856ad803fffffff 856ad803fffffff 288.2476
#> 2        RWA 856ad80bfffffff 856ad80bfffffff 288.6746
#> 3        RWA 856ad80ffffffff 856ad80ffffffff 288.6492
#> 4        RWA 856ad813fffffff 856ad813fffffff 287.8398
#> 5        RWA 856ad817fffffff 856ad817fffffff 287.8146
#> 6        RWA 856ad81bfffffff 856ad81bfffffff 288.2693
#>                         geometry
#> 1 POLYGON ((30.25174 -2.21219...
#> 2 POLYGON ((30.16219 -2.34876...
#> 3 POLYGON ((30.32785 -2.35703...
#> 4 POLYGON ((30.17572 -2.06748...
#> 5 POLYGON ((30.34119 -2.07576...
#> 6 POLYGON ((30.08617 -2.20391...

# The function tolerates multi-row input: provinces are unioned to
# the country envelope before tiling.
hex_from_provinces <- make_hex_grid(
  rwa_shp$admin1_Province,
  resolution = 5
)
```
