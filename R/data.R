#' Ukraine sample administrative boundaries
#'
#' A named list of `sf` tibbles representing Ukraine's administrative
#' boundaries at four hierarchical levels. Bundled with the package as
#' the canonical sample geometry input for [launch_pti()],
#' [run_pti_pipeline()], examples, and the test suite.
#'
#' Slot names follow the package convention `adminN_HumanName` --
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
#'   \item{admin0_Country}{1 row -- country polygon (Ukraine).}
#'   \item{admin1_Oblast}{27 rows -- first-level admin regions (oblasts).}
#'   \item{admin2_Rayon}{629 rows -- second-level admin regions (rayons).}
#'   \item{admin4_Hexagon}{1,939 rows -- H3 hexagonal grid cells used as a
#'     synthetic admin4 level.}
#' }
#'
#' Each tibble contains, at its admin depth `N`, the columns:
#' \describe{
#'   \item{adminNPcod}{Character. Unique polygon identifier (P-code) --
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
#' examples, and the test suite. Pairs with [ukr_shp] -- the admin slot
#' names line up with the geometry slot names so the two can be passed
#' directly to the pipeline.
#'
#' @format A named list of length 5:
#' \describe{
#'   \item{general}{`tbl_df` with 1 row and column `country` --
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
#' Indicator column names embed their semantics -- for example
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

#' Rwanda sample administrative boundaries
#'
#' A named list of `sf` tibbles representing Rwanda's administrative
#' boundaries at three hierarchical levels. Bundled with the package
#' as the canonical sample geometry input for user-facing `@examples`
#' across exported functions, and the worked-example country in the
#' Build-a-PTI website tutorial.
#'
#' Slot names follow the package convention `adminN_HumanName`. The
#' bundled sample uses levels 0, 1, and 2: country (1 polygon), 5
#' provinces, and 30 districts. Pair with [rwa_mtdt_full] for a
#' working PTI calculation.
#'
#' Compared with the test-suite-oriented [ukr_shp]: smaller (66 vs
#' 2,596 polygons), simpler (no synthetic admin4 hex grid), and built
#' from a public CC-BY 4.0 source -- safe to render in tutorials and
#' embed in screenshots.
#'
#' @format A named list of length 3. Each element is an `sf` / `tbl_df`:
#' \describe{
#'   \item{admin0_Country}{1 row -- country polygon (Rwanda).}
#'   \item{admin1_Province}{5 rows -- provinces.}
#'   \item{admin2_District}{30 rows -- districts.}
#' }
#'
#' Each tibble carries the standard `adminNPcod` / `adminNName` / `area`
#' / `geometry` columns plus parent-level P-codes on sub-admin layers
#' (`admin1_Province` carries `admin0Pcod`; `admin2_District` carries
#' both `admin0Pcod` and `admin1Pcod`). The admin1-parent-of-admin2
#' relationship is derived in `data-raw/generate-rwa-package-data.R`
#' via a centroid-in-polygon spatial join.
#'
#' @source Boundary geometries: geoBoundaries
#'   (\url{https://www.geoboundaries.org}) `gbOpen` release for Rwanda
#'   (`shapeISO = "RWA"`), licensed CC-BY 4.0. See the raw GeoJSONs
#'   under `inst/template_pti/sample-data/` (`rwa_adm0.geojson`,
#'   `rwa_adm1.geojson`, `rwa_adm2.geojson`) and the compilation
#'   script at `data-raw/generate-rwa-package-data.R`.
#'
#' @examples
#' data(rwa_shp)
#' names(rwa_shp)
#' head(rwa_shp[["admin1_Province"]])
"rwa_shp"

#' Rwanda sample PTI metadata input
#'
#' A named list mirroring the structure that [fct_template_reader()]
#' produces from a PTI metadata Excel template. Bundled as the
#' user-facing sample metadata input -- pair with [rwa_shp] for a
#' worked PTI calculation in `@examples` blocks across exported
#' functions.
#'
#' Compiled from `inst/template_pti/sample-data/sample-metadata-adm1-adm2.xlsx`
#' (the multi-level synthetic Rwanda workbook): 3 indicators measured
#' at admin2_District (with admin1_Province also populated for the
#' Data Explorer tab).
#'
#' Compared with [ukr_mtdt_full]: smaller (3 indicators vs 9), simpler
#' (2 admin levels vs 4), and synthetic-by-design (no real social or
#' economic data) -- safe to publish in tutorials and screenshots.
#'
#' @format A named list of length 4:
#' \describe{
#'   \item{general}{`tbl_df` 1x1 -- `country` ("Rwanda").}
#'   \item{admin1_Province}{`tbl_df` 5x7. Keys `admin0Pcod`,
#'     `admin1Pcod`, `admin1Name`, `year`; remaining 3 columns are the
#'     indicator values at province level (`poverty_rate`,
#'     `literacy_rate`, `road_density`).}
#'   \item{admin2_District}{`tbl_df` 30x8. Keys `admin0Pcod`,
#'     `admin1Pcod`, `admin2Pcod`, `admin2Name`, `year`; remaining 3
#'     columns are the same indicators measured at district level.}
#'   \item{metadata}{`tbl_df` 3x14 -- one row per indicator. Same
#'     14-column schema as [ukr_mtdt_full]'s metadata sheet.}
#' }
#'
#' Note: this bundle does not include a `weights_clean` slot --
#' [fct_template_reader()] only adds it when the source xlsx has a
#' `weights_table` sheet, and the Rwanda template ships without one
#' (deployers build weights interactively in the Shiny UI).
#'
#' @source Boundary keys (`adminNPcod`, `adminNName`) inherited from
#'   the geoBoundaries Rwanda data underpinning [rwa_shp] (CC-BY 4.0).
#'   Indicator values are synthetic, generated by the seeded script at
#'   `inst/template_pti/data-raw/generate-synthetic-metadata.R`.
#'
#' @examples
#' data(rwa_mtdt_full)
#' names(rwa_mtdt_full)
#' rwa_mtdt_full$general
#' head(rwa_mtdt_full$metadata)
"rwa_mtdt_full"
