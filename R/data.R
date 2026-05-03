#' Ukraine sample administrative boundaries
#'
#' A named list of `sf` tibbles representing Ukraine's administrative
#' boundaries at four hierarchical levels. Bundled with the package as
#' the canonical sample geometry input for [launch_pti()],
#' [run_pti_pipeline()], examples, and the test suite.
#'
#' Slot names follow the package convention `adminN_HumanName` —
#' `adminN` is `admin0` for the country polygon, then `admin1`..`admin9`
#' for nested sub-divisions; `HumanName` is a single word naming the
#' level (e.g. `Oblast`, `Rayon`, `Hexagon`). The bundled sample uses
#' levels 0, 1, 2, and 4 (admin3 is intentionally skipped; `admin4` is
#' a synthetic H3 hexagon grid). This naming is what downstream
#' functions like [get_adm_levels()] and [expand_adm_levels()] parse
#' to build the admin hierarchy.
#'
#' @format A named list of length 4. Each element is an `sf` / `tbl_df`
#'   with one row per polygon at that admin level:
#' \describe{
#'   \item{admin0_Country}{1 row — country polygon (Ukraine).}
#'   \item{admin1_Oblast}{27 rows — first-level admin regions (oblasts).}
#'   \item{admin2_Rayon}{629 rows — second-level admin regions (rayons).}
#'   \item{admin4_Hexagon}{1,939 rows — H3 hexagonal grid cells used as a
#'     synthetic admin4 level.}
#' }
#'
#' Each tibble contains, at its admin depth `N`, the columns:
#' \describe{
#'   \item{adminNPcod}{Character. Unique polygon identifier (P-code) —
#'     e.g. `admin0Pcod` at admin0, `admin1Pcod` at admin1.}
#'   \item{adminNName}{Character. Human-readable polygon name.}
#'   \item{area}{Numeric. Polygon area in the layer's native units.}
#'   \item{geometry}{`sfc_MULTIPOLYGON`. Spatial geometry column.}
#' }
#'
#' Sub-admin tibbles also carry the parent-level P-code columns
#' (e.g. `admin1_Oblast` has both `admin0Pcod` and `admin1Pcod`) so
#' joins to higher levels work without re-deriving keys.
#'
#' @source Internal sample data bundled with the package for examples
#'   and tests. Derived from public Ukraine administrative boundaries.
#'
#' @examples
#' data(ukr_shp)
#' names(ukr_shp)
#' head(ukr_shp[["admin1_Oblast"]])
"ukr_shp"

#' Ukraine sample PTI metadata input
#'
#' A named list mirroring the structure that [fct_template_reader()]
#' produces from a PTI metadata Excel template. Bundled as the canonical
#' sample metadata input for [launch_pti()], [run_pti_pipeline()],
#' examples, and the test suite. Pairs with [ukr_shp] — the admin slot
#' names line up with the geometry slot names so the two can be passed
#' directly to the pipeline.
#'
#' @format A named list of length 5:
#' \describe{
#'   \item{general}{`tbl_df` with 1 row and column `country` —
#'     country-level metadata (display name, etc.).}
#'   \item{admin1_Oblast}{`tbl_df` with 27 rows and 8 columns. Keys
#'     `admin0Pcod`, `admin1Pcod`, `admin1Name`, `year`; remaining 4
#'     columns are indicator values (`var_*`) measured at the oblast
#'     level.}
#'   \item{admin2_Rayon}{`tbl_df` with 629 rows and 11 columns. Keys
#'     `admin0Pcod`, `admin1Pcod`, `admin2Pcod`, `admin2Name`, `year`;
#'     remaining 6 columns are indicator values (`var_*`) measured at
#'     the rayon level.}
#'   \item{admin4_Hexagon}{`tbl_df` with 1,939 rows and 11 columns. Keys
#'     `admin0Pcod`, `admin1Pcod`, `admin2Pcod`, `admin4Pcod`,
#'     `admin4Name`, `year`; remaining 5 columns are indicator values
#'     (`var_*`) measured at the hex-cell level.}
#'   \item{metadata}{`tbl_df` with 9 rows (one per indicator) and 14
#'     columns describing each indicator: `var_code`, `var_name`,
#'     `var_description`, `var_order`, `var_units`, `spatial_level`,
#'     `pillar_group`, `pillar_name`, `pillar_description`,
#'     `fltr_exclude_pti`, `fltr_exclude_explorer`, `fltr_overlay_pti`,
#'     `fltr_overlay_explorer`, `legend_revert_colours`.}
#' }
#'
#' Indicator column names embed their semantics — for example
#' `var_nval6_na_adm12` denotes a 6-valued indicator with NAs available
#' at admin1 and admin2. Synthetic by design, so the suite can exercise
#' the calculation pipeline without exposing real administrative data.
#'
#' @source Internal sample data bundled with the package for examples
#'   and tests. Indicator values are synthetic.
#'
#' @examples
#' data(ukr_mtdt_full)
#' names(ukr_mtdt_full)
#' ukr_mtdt_full$general
#' head(ukr_mtdt_full$metadata)
"ukr_mtdt_full"
