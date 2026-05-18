# Resolve a path inside the installed devPTIpack package

Thin wrapper around \[system.file()\] that always sets \`package =
"devPTIpack"\`. Use it from app code or tests to locate bundled
resources (CSS, images, sample template) without having to repeat the
package name at every call site.

## Usage

``` r
app_sys(...)
```

## Arguments

- ...:

  Character path components passed through to \[system.file()\]. Joined
  with the system file separator.

## Value

A single character string. The absolute path to the requested resource
if it exists, or \`""\` if it does not (matching \[system.file()\]'s
convention for missing files).

## See also

Other package-utilities:
[`golem_add_external_resources()`](https://worldbank.github.io/devPTIpack/reference/golem_add_external_resources.md)

## Examples

``` r
app_sys("app", "www")
#> [1] "/home/runner/work/_temp/Library/devPTIpack/app/www"
app_sys("template_pti")
#> [1] "/home/runner/work/_temp/Library/devPTIpack/template_pti"
```
