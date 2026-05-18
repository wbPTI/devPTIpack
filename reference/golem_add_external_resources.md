# Bundle external resources for a PTI Shiny app

Builds the \`\<head\>\` block that every PTI app needs: registers the
bundled \`app/www\` directory as a resource path, attaches the favicon,
and bundles the CSS and JavaScript shipped with the package. Developers
building a custom UI on top of the PTI modules must include the result
inside their UI's tag list (typically via \[shiny::tagList()\]); the
bundled \[launch_pti()\] and \[launch_pti_onepage()\] entry points do
this automatically.

## Usage

``` r
golem_add_external_resources()
```

## Value

A \[shiny::tags\$head()\] block ready to be included in a Shiny UI.

## Details

Calls \[golem::get_golem_options()\] for \`pti.name\` to derive the app
title, so it must run inside a \[golem::with_golem_options()\] context.

## See also

Other package-utilities:
[`app_sys()`](https://worldbank.github.io/devPTIpack/reference/app_sys.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Inside a custom shiny UI, wrap your top-level UI with the resources:
ui <- shiny::tagList(
  shiny::bootstrapPage(
    mod_ptipage_twocol_ui("pagepti")
  ),
  golem_add_external_resources()
)
} # }
```
