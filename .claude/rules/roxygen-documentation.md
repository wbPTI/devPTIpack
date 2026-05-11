# Roxygen2 Documentation Rules

When documenting R functions in this package, follow these standards.

---

## General Rules

- Description = one sentence on *what* the function does, plus optional paragraph(s) on *how*.
- All `@param` entries must describe type and purpose.
- Examples must use only package built-in data. **Prefer `rwa_shp` / `rwa_mtdt_full`** (user-facing tutorial datasets, smaller and CC-BY 4.0 sourced). `ukr_shp` / `ukr_mtdt_full` are also bundled and remain the canonical inputs for the test suite.
- Examples must pass `devtools::check()`.
- Use `\dontrun{}` only for Shiny modules or side-effect-heavy code.
- Never combine `@noRd` with `@export` — this creates exported functions with no help page.
- Run `devtools::document()` after changes to verify `.Rd` generation.

---

## Required Fields per Function Type

| Field                               | Exported | Internal |     Module     |     Data      |
| ----------------------------------- | :------: | :------: | :------------: | :-----------: |
| `#' Title` (one sentence *what*)    |    ✓     |    ✓     |       ✓        |       ✓       |
| `#' Description` (*how*, if needed) |    ✓     |    ✓     |       ✓        |       ✓       |
| `@param`                            |    ✓     |    ✓     |       ✓        |       —       |
| `@return`                           |    ✓     |    ✓     |       ✓        |       —       |
| `@format`                           |    —     |    —     |       —        |       ✓       |
| `@importFrom`                       |    ✓     |    ✓     |       ✓        |       —       |
| `@export`                           |    ✓     |    —     |       ✓        |       ✓       |
| `@noRd`                             |    —     |    ✓     |       —        |       —       |
| `@examples`                         |    ✓     |    —     | ✓ (`\dontrun`) |       ✓       |
| `@source`                           |    —     |    —     |       —        | if applicable |

---

## Exported Functions

```r
#' Compute z-score standardised indicator values
#'
#' Standardises each indicator column to zero mean and unit variance
#' within the supplied data. Missing values are ignored during
#' standardisation and remain NA in output.
#'
#' @param pti_dta A tibble of pivoted PTI data as returned by
#'   [pivot_pti_dta()].
#' @param indicators Character vector of column names to standardise.
#'
#' @return A tibble with the same structure as `pti_dta` where indicator
#'   columns contain z-scores instead of raw values.
#'
#' @importFrom dplyr mutate across
#' @export
#'
#' @examples
#' data(ukr_mtdt_full)
#' data(ukr_shp)
#'
#' mt <- get_mt(ukr_mtdt_full)
#' pti_dta <- pivot_pti_dta(mt)
#' scores <- get_scores_data(pti_dta, indicators = names(pti_dta)[-1])
#' head(scores)
```

---

## Internal Functions (`@noRd`)

```r
#' Build HTML label for a single PTI polygon
#'
#' Constructs the popup content shown on hover using glue templates.
#'
#' @param row_dta A one-row tibble with columns for admin name,
#'   score, and indicator values.
#' @param template A glue-compatible string template.
#'
#' @return A single HTML character string.
#'
#' @importFrom glue glue
#' @noRd
```

---

## Exported Shiny Module Functions

- Document the actual meaningful parameters (reactive inputs, data arguments), not just `id, input, output, session`.
- `@return` should state "Called for side effects" when the module has no explicit return.

```r
#' Page-level PTI module server
#'
#' Orchestrates the weights input, PTI calculation, and map
#' visualisation for a single PTI page tab.
#'
#' @param id Character. Shiny module namespace ID.
#' @param inp_dta Reactive list of input data as returned by
#'   [fct_template_reader()].
#' @param shp_dta Reactive named list of sf tibbles (one per admin level).
#' @param active_tab Reactive character indicating the currently
#'   selected tab, used to defer rendering.
#'
#' @return No explicit return value. Called for side effects
#'   (renders map outputs within the Shiny session).
#'
#' @importFrom shiny moduleServer reactive observeEvent
#' @export
#'
#' @examples
#' \dontrun{
#' # Typically called inside a Shiny server function:
#' mod_ptipage_newsrv("pagepti", inp_dta = inp_dta, shp_dta = shp_dta)
#' }
```

---

## Package Built-in Data

```r
#' Ukraine sample administrative boundaries
#'
#' A named list of sf tibbles representing administrative boundaries
#' at multiple hierarchical levels. Used as sample data throughout
#' the package for examples and tests.
#'
#' @format A named list with elements:
#' \describe{
#'   \item{admin1_Oblast}{sf tibble with 25 rows — first-level admin regions.}
#'   \item{admin2_Raion}{sf tibble with 136 rows — second-level admin regions.}
#'   \item{admin3_Hromada}{sf tibble with 1,469 rows — third-level admin regions.}
#' }
#'
#' Each tibble contains at minimum:
#' \describe{
#'   \item{admin_code}{Character. Unique polygon identifier.}
#'   \item{admin_name}{Character. Human-readable name.}
#'   \item{geometry}{sfc_MULTIPOLYGON. Spatial geometry column.}
#' }
#'
#' @source World Bank internal spatial data.
#'
#' @examples
#' data(ukr_shp)
#' names(ukr_shp)
#' head(ukr_shp[[1]])
"ukr_shp"
```
