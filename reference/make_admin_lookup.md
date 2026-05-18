# Build the parent-child P-code cascade for a shapefile list

Given a named list of \`sf\` admin layers (any number, any levels
including \`admin9_Hexagon\`), populates every parent \`admin\<k\>Pcod\`
column on every layer via centroid-in-polygon joins, so the resulting
list satisfies the cascade rule used by the calculation pipeline and
\[validate_geometries()\].

## Usage

``` r
make_admin_lookup(shp_list)
```

## Arguments

- shp_list:

  Named list of \`sf\` objects. Slot names must follow the
  \`admin\<N\>\_\<HumanName\>\` convention (e.g. \`admin1_Province\`).
  The list may contain any subset of levels 0-9 and need not be in any
  particular order. Each layer must already be in \*\*EPSG:4326\*\*; no
  reprojection is performed.

## Value

A named list with the same slot names as the input but every sub-admin
layer enriched with all ancestor \`admin\<k\>Pcod\` columns required by
the cascade rule. The coarsest layer is unchanged. The internal lookup
table is \*\*not\*\* returned – it is an implementation detail.

## Details

This is the \*\*second\*\* of the two-step Step-1 hex workflow:
\[make_hex_grid()\] (arch-10 §2, issue
[\#108](https://github.com/worldbank/devPTIpack/issues/108)) produces
the partial \`admin9_Hexagon\` layer; \`make_admin_lookup()\` wires it
(and every other sub-admin layer) into the parent chain so the list can
be saved to \`app-data/shapes.rds\` and consumed by the app.

Algorithm (arch-10 §3.3):

1\. \*\*Parse order\*\* – extract the \`\<N\>\` digit from each
\`admin\<N\>\_\<HumanName\>\` slot name and sort layers numerically
ascending (coarsest -\> finest). Level numbers need not be contiguous
(e.g. \`admin0\`, \`admin1\`, \`admin2\`, \`admin9\` is a valid
sequence). The order of the input list does not matter. 2.
\*\*Pre-flight validation\*\* per layer – checks the layer is an \`sf\`
object, that \`admin\<N\>Pcod\` and \`admin\<N\>Name\` exist + are
unique + have no \`NA\`, and that an \`area\` column is present. If
\`area\` is missing, the function warns and computes it in place as
\`as.numeric(units::set_units(sf::st_area(geometry), "km^2"))\` in the
layer's existing CRS (assumed EPSG:4326 – no reprojection is performed).
3. \*\*Centroid-in-polygon join\*\* per adjacent level pair (parent -\>
child). The child's centroids are spatially joined to the parent's
polygons via \`sf::st_within()\`. Both the centroid step and the join
step are wrapped in an \`s2\` fallback (see \[make_hex_grid()\] for the
rationale). When a centroid lies exactly on a boundary and matches two
or more parents, one parent is picked at random and a warning lists the
number of affected polygons. 4. \*\*Many-to-one validation\*\* – every
child Pcod must match exactly one parent. Zero matches (orphan child) is
an error. 5. \*\*Cascade propagation\*\* – after the immediate parent
Pcod is populated, every ancestor Pcod (already present on the parent
layer from previous iterations) is also copied onto the child by joining
on the immediate-parent Pcod.

Hexagons (\`admin9_Hexagon\`) are processed identically to any other
admin layer; there are no exceptions to the cascade rule.

## See also

\[make_hex_grid()\] for the upstream hex grid builder;
\[validate_geometries()\] for the structural validator that the
\`make_admin_lookup()\`-enriched list satisfies.

Other data-input:
[`aggregate_hex_to_shapes()`](https://worldbank.github.io/devPTIpack/reference/aggregate_hex_to_shapes.md),
[`build_hex_metadata()`](https://worldbank.github.io/devPTIpack/reference/build_hex_metadata.md),
[`fct_template_reader()`](https://worldbank.github.io/devPTIpack/reference/fct_template_reader.md),
[`fetch_hex_data()`](https://worldbank.github.io/devPTIpack/reference/fetch_hex_data.md),
[`get_available_years()`](https://worldbank.github.io/devPTIpack/reference/get_available_years.md),
[`get_shape()`](https://worldbank.github.io/devPTIpack/reference/get_shape.md),
[`list_hex_vars()`](https://worldbank.github.io/devPTIpack/reference/list_hex_vars.md),
[`make_hex_grid()`](https://worldbank.github.io/devPTIpack/reference/make_hex_grid.md),
[`use_hex_vars()`](https://worldbank.github.io/devPTIpack/reference/use_hex_vars.md)

## Examples

``` r
data(rwa_shp)

# Strip the cascade columns to simulate raw admin layers.
raw <- list(
  admin0_Country  = rwa_shp$admin0_Country,
  admin1_Province = rwa_shp$admin1_Province[, c(
    "admin1Pcod", "admin1Name", "area", "geometry"
  )],
  admin2_District = rwa_shp$admin2_District[, c(
    "admin2Pcod", "admin2Name", "area", "geometry"
  )]
)

enriched <- make_admin_lookup(raw)
names(enriched$admin2_District)
#> [1] "admin2Pcod" "admin2Name" "area"       "geometry"   "admin1Pcod"
#> [6] "admin0Pcod"
# admin0Pcod, admin1Pcod, admin2Pcod, admin2Name, area, geometry
```
