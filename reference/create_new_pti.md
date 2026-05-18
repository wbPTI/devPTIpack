# Scaffold a new PTI app project

Copies the bundled \`template_pti\` skeleton (located inside the
installed package via \[app_sys()\]) into a new project directory,
creating an \`app.R\` and an RStudio project file. When called from an
RStudio session, the project is opened automatically. Use this to
bootstrap a fresh PTI deployment that you then point at your own shapes
and metadata.

## Usage

``` r
create_new_pti(path, open = TRUE, app_name = basename(path))
```

## Arguments

- path:

  Character. Full path to the new project directory. The directory is
  created automatically; if it already exists the user is prompted (via
  \[yesno::yesno()\]) before any files are overwritten. \`path = "."\`
  writes into the current working directory.

- open:

  Logical. If \`TRUE\` (default) and an RStudio session is active, opens
  the new project in a new RStudio window after scaffolding. Ignored
  outside RStudio.

- app_name:

  Character. Display name for the project, used for the RStudio project
  file. Defaults to the basename of \`path\`.

## Value

Invisibly, the absolute path to the scaffolded project (as returned by
\[fs::path_abs()\]). Returns \`invisible(NULL)\` if the user declines
the overwrite prompt.

## See also

Other pti-launch:
[`launch_pti()`](https://worldbank.github.io/devPTIpack/reference/launch_pti.md),
[`launch_pti_onepage()`](https://worldbank.github.io/devPTIpack/reference/launch_pti_onepage.md)

## Examples

``` r
# Scaffold into a temporary directory; works headlessly because
# rstudioapi::isAvailable() is FALSE outside RStudio.
new_app <- file.path(tempdir(), "demo_pti")
create_new_pti(new_app, open = FALSE)
#> ── Creating dir ────────────────────────────────────────────────────────────────
#> • Created package directory
#> ── Copying package skeleton ────────────────────────────────────────────────────
#> • Copied app skeleton
#> ── Setting the default config ──────────────────────────────────────────────────
#> • Configured app
list.files(new_app)
#>  [1] "00-master.R"              "01-shapes.qmd"           
#>  [3] "02a-user-zonal-stats.qmd" "03-metadata.qmd"         
#>  [5] "04-hex-data.qmd"          "05-compile.qmd"          
#>  [7] "06-deploy.R"              "R"                       
#>  [9] "README.md"                "app.R"                   
#> [11] "data-raw"                 "landing-page.md"         
#> [13] "sample-data"             
unlink(new_app, recursive = TRUE)
```
