# Compile PTI deployment artefacts from intermediate files

Final stage of the PTI build pipeline. Reads the compiled shapes RDS
plus one or more intermediate metadata Excel files (typically
\`metadata-user.xlsx\` from Step 3 and / or \`metadata-hex.xlsx\` from
Step 4), merges them into a single canonical metadata workbook,
validates the combined inputs, renders the printable indicator atlas
(\`pti-metadata.pdf\`), and bundles the boundary GeoJSONs into
\`shapefiles.zip\` so the deployment package is a single self- contained
directory.

## Usage

``` r
compile_pti_data(shp_path, metadata_paths, output_dir, error_on_fail = TRUE)
```

## Arguments

- shp_path:

  Character. Path to the compiled shapes \`.rds\` produced by Step 1
  (\`01-shapes.qmd\`). Must be readable by \[base::readRDS()\] and
  validate via \[validate_geometries()\].

- metadata_paths:

  Character vector of length 1 or more – filesystem paths to
  intermediate metadata Excel files (typically the Step 3 output
  \`metadata-user.xlsx\` and / or the Step 4 output
  \`metadata-hex.xlsx\`). Each must be readable by
  \[fct_template_reader()\].

- output_dir:

  Character. Directory where the three output artefacts
  (\`metadata.xlsx\`, \`shapefiles.zip\`, \`pti-metadata.pdf\`) are
  written. Created if it does not exist.

- error_on_fail:

  Logical. When \`TRUE\` (the default), throws if either
  \[validate_geometries()\] or \[validate_metadata()\] reports \`status
  = "fail"\` on the combined inputs. When \`FALSE\`, returns the
  structured result and lets the caller decide (matches the existing
  validator convention).

## Value

Invisibly, a list with the same shape as the existing validators: -
\`status\` – \`"pass"\` / \`"warn"\` / \`"fail"\`, the worse of the two
underlying validators' statuses. - \`summary\` – character vector of
free-text summary lines. - \`issues\` – list of all validation issues
(concatenation of the two validators' \`issues\`). Plus three extra
fields: - \`metadata_path\` – path to the canonical merged xlsx. -
\`shapefiles_path\` – path to the shapefiles zip. - \`pdf_path\` – path
to the rendered PDF, or \`NA_character\_\` if PDF rendering was skipped.

## Details

Used by Step 5 of the deployer template
(\`inst/template_pti/05-compile.qmd\`). The CLI summary mirrors the
style of the existing validators: a \`cli_h1\` per phase, alerts as work
proceeds, and a final \`cli_alert_success\` / \`cli_alert_danger\`
depending on the validators' verdict.

\## Merge contract

For 1+ inputs:

\- The \`general\` sheet is taken from the \*\*first\*\* input file
(assumption: same country across all inputs). - The \`metadata\` sheet
is row-bound across all inputs and deduplicated by \`var_code\` (last
writer wins on conflict; a warning is emitted naming the duplicates). -
Each \`admin\<N\>\_\*\` sheet is \`dplyr::full_join\`ed across all input
files on the admin-code / area / year keys, so an indicator present at a
level in only one input is preserved alongside indicators from the other
inputs. - The \`weights_table\` sheet, if any input has it with non-zero
rows, is taken from the first such input (others ignored; warning
emitted if multiple).

\## Output files (in \`output_dir\`)

\- \`metadata.xlsx\` – canonical merged metadata workbook, readable by
\[fct_template_reader()\]. - \`shapefiles.zip\` – one GeoJSON per admin
level (filename = slot name, e.g. \`admin1_Province.geojson\`). -
\`pti-metadata.pdf\` – rendered from the bundled \`inst/metadata.Rmd\`
template; one choropleth map per indicator. Skipped (with a warning)
when LaTeX is not available – the rest of the artefacts still produced.

## See also

\[validate_geometries()\], \[validate_metadata()\],
\[fct_template_reader()\], \[launch_pti()\].

Other pti-pipeline:
[`generic_pti_glue()`](https://worldbank.github.io/devPTIpack/reference/generic_pti_glue.md),
[`label_generic_pti()`](https://worldbank.github.io/devPTIpack/reference/label_generic_pti.md),
[`run_pti_pipeline()`](https://worldbank.github.io/devPTIpack/reference/run_pti_pipeline.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# After running Steps 1 + 3 of the template:
compile_pti_data(
  shp_path        = "app-data/shapes.rds",
  metadata_paths  = "app-data/metadata-user.xlsx",
  output_dir      = "app-data"
)

# Combine user metadata + HEX metadata into a single deployment bundle:
compile_pti_data(
  shp_path        = "app-data/shapes.rds",
  metadata_paths  = c("app-data/metadata-user.xlsx",
                      "app-data/metadata-hex.xlsx"),
  output_dir      = "app-data"
)
} # }
```
