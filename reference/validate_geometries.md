# Validate every geometry layer in a shapes list

Iterates over \`existing_shapes\` and runs the per-layer checks on each,
then performs a top-level mapping-table consistency check: the row count
of \[get_mt()\] applied to the full shapes list must equal the row count
of the most-disaggregated layer (admin levels are required to form a
strict hierarchy). Per-layer findings are emitted as cli alerts as they
are discovered; on completion, prints a single summary and returns the
structured \`list(status, summary, issues)\` result.

## Usage

``` r
validate_geometries(existing_shapes, error_on_fail = TRUE)
```

## Arguments

- existing_shapes:

  Named list of \`sf\` tibbles, one per admin level. Element names must
  follow \`admin\<N\>\_\<Name\>\` (e.g. \`admin1_Oblast\`); see
  \[ukr_shp\] for the canonical shape.

- error_on_fail:

  Logical. If \`TRUE\` (default), throws on any \`fail\`-level issue at
  the end of the run. Pass \`FALSE\` to inspect the structured result
  instead.

## Value

Invisibly, a list with components \`status\` (\`"pass"\`, \`"warn"\`, or
\`"fail"\`), \`summary\` (one-line string), and \`issues\` (list of
issue records). Each issue carries \`level\`, \`check\`, \`message\`,
and optional \`details\`. The \`check\` field is a stable short
identifier (e.g. \`"pcod-unique"\`, \`"parent-pcod-cascade"\`,
\`"hierarchy-row-count"\`); programmatic callers should branch on it
rather than on \`message\` text.

## See also

Other validation:
[`app_validate_metadata()`](https://worldbank.github.io/devPTIpack/reference/app_validate_metadata.md),
[`app_validate_shp()`](https://worldbank.github.io/devPTIpack/reference/app_validate_shp.md),
[`drop_inval_adm()`](https://worldbank.github.io/devPTIpack/reference/drop_inval_adm.md),
[`mod_drop_inval_adm()`](https://worldbank.github.io/devPTIpack/reference/mod_drop_inval_adm.md),
[`validate_metadata()`](https://worldbank.github.io/devPTIpack/reference/validate_metadata.md),
[`validate_read_metadata()`](https://worldbank.github.io/devPTIpack/reference/validate_read_metadata.md),
[`validate_read_shp()`](https://worldbank.github.io/devPTIpack/reference/validate_read_shp.md)

## Examples

``` r
result <- validate_geometries(rwa_shp)
#> 
#> ── Layer: admin0_Country ──
#> 
#> ✔ validate_single_geom: all checks passed.
#> 
#> ── Layer: admin1_Province ──
#> 
#> ✔ validate_single_geom: all checks passed.
#> 
#> ── Layer: admin2_District ──
#> 
#> ✔ validate_single_geom: all checks passed.
#> 
#> ── Layer: admin9_Hexagon ──
#> 
#> ✔ validate_single_geom: all checks passed.
#> 
#> ── Cross-layer hierarchy ──
#> 
#> ✔ Hierarchy row count matches most-disaggregated layer.
#> ✔ validate_geometries: all checks passed.
result$status
#> [1] "pass"
length(result$issues)
#> [1] 0
```
