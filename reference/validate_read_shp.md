# Validate a shapes \`.rds\` file in isolation

Reads the \`.rds\` at \`shp_path\` and checks two invariants: (1) the
object is a non-empty named list, and (2) every \`admin\<N\>Pcod\`
column referenced inside any layer has a corresponding top-level
\`admin\<N\>\_\*\` element. Emits one cli alert per check; returns the
structured \`list(status, summary, issues)\` result invisibly.

## Usage

``` r
validate_read_shp(shp_path, error_on_fail = TRUE)
```

## Arguments

- shp_path:

  Character. Path to an \`.rds\` file containing the shapes list (the
  on-disk form of objects shaped like \[ukr_shp\]).

- error_on_fail:

  Logical. If \`TRUE\` (default), throws on any \`fail\`-level issue at
  the end of the run. Pass \`FALSE\` to inspect the structured result
  instead.

## Value

Invisibly, a list with components \`status\` (\`"pass"\`, \`"warn"\`, or
\`"fail"\`), \`summary\` (one-line string), and \`issues\` (list of
issue records).

## See also

Other validation:
[`app_validate_metadata()`](https://worldbank.github.io/devPTIpack/reference/app_validate_metadata.md),
[`app_validate_shp()`](https://worldbank.github.io/devPTIpack/reference/app_validate_shp.md),
[`drop_inval_adm()`](https://worldbank.github.io/devPTIpack/reference/drop_inval_adm.md),
[`mod_drop_inval_adm()`](https://worldbank.github.io/devPTIpack/reference/mod_drop_inval_adm.md),
[`validate_geometries()`](https://worldbank.github.io/devPTIpack/reference/validate_geometries.md),
[`validate_metadata()`](https://worldbank.github.io/devPTIpack/reference/validate_metadata.md),
[`validate_read_metadata()`](https://worldbank.github.io/devPTIpack/reference/validate_read_metadata.md)

## Examples

``` r
if (FALSE) { # \dontrun{
validate_read_shp("path/to/shapes.rds")
} # }
```
