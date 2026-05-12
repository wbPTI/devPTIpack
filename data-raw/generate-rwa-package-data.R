# data-raw/generate-rwa-package-data.R
#
# Compile the Rwanda tutorial inputs (GeoJSONs + synthetic metadata
# workbook) shipped under inst/template_pti/sample-data/ into
# package-level datasets:
#
#   data/rwa_shp.rda          -- named list of 4 sf tibbles
#                                (admin0_Country, admin1_Province,
#                                admin2_District, admin9_Hexagon).
#                                Mirrors ukr_shp's 4-level shape.
#   data/rwa_mtdt_full.rda    -- parsed metadata list as produced by
#                                fct_template_reader(). Mirrors
#                                ukr_mtdt_full.
#
# Re-runnable end-to-end: source("data-raw/generate-rwa-package-data.R")
# from the package root after a fresh checkout. Output is deterministic
# given the GeoJSON + xlsx inputs (no RNG).
#
# This script is .Rbuildignore-d via the existing ^data-raw$ rule, so
# it doesn't ship in the source tarball.

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

devtools::load_all(".", quiet = TRUE)

sample_dir <- "inst/template_pti/sample-data"
stopifnot(dir.exists(sample_dir))

# Hex grid resolution -- mirrors HEX_RESOLUTION default in
# inst/template_pti/00-master.R. H3-6 ~36 km^2 per cell; ~500 cells
# over Rwanda after the centroid-in-polygon retention.
HEX_RESOLUTION <- 6L

# ---------------------------------------------------------------------------
# 1. rwa_shp -- 4 layers: admin0 / admin1 / admin2 / admin9_Hexagon
#
# Pipeline (matches Step 1 vignette §E + §F):
#   - Load + rename each admin layer (admin<N>Pcod / admin<N>Name /
#     area in km^2).
#   - Build admin9_Hexagon via make_hex_grid() at HEX_RESOLUTION.
#   - Assemble named list; call make_admin_lookup() to populate every
#     parent admin<k>Pcod via centroid-in-polygon joins (replaces the
#     previous manual centroid-join code in this script).

adm0_raw <- read_sf(file.path(sample_dir, "rwa_adm0.geojson"))
adm1_raw <- read_sf(file.path(sample_dir, "rwa_adm1.geojson"))
adm2_raw <- read_sf(file.path(sample_dir, "rwa_adm2.geojson"))

adm0 <- adm0_raw |>
  st_transform(4326) |>
  mutate(
    admin0Pcod = "RWA",
    admin0Name = shapeName,
    area       = as.numeric(units::set_units(st_area(geometry), "km^2"))
  ) |>
  select(admin0Pcod, admin0Name, area, geometry)

adm1 <- adm1_raw |>
  st_transform(4326) |>
  mutate(
    admin1Pcod = shapeID,
    admin1Name = shapeName,
    area       = as.numeric(units::set_units(st_area(geometry), "km^2"))
  ) |>
  select(admin1Pcod, admin1Name, area, geometry)

adm2 <- adm2_raw |>
  st_transform(4326) |>
  mutate(
    admin2Pcod = shapeID,
    admin2Name = shapeName,
    area       = as.numeric(units::set_units(st_area(geometry), "km^2"))
  ) |>
  select(admin2Pcod, admin2Name, area, geometry)

# Hex layer covering the country envelope.
adm9 <- make_hex_grid(adm0, resolution = HEX_RESOLUTION)

# Assemble + cascade. make_admin_lookup populates admin0Pcod /
# admin1Pcod on every sub-admin layer (and admin0Pcod / admin1Pcod /
# admin2Pcod on the hex layer) via centroid-in-polygon joins.
rwa_shp <- list(
  admin0_Country  = adm0,
  admin1_Province = adm1,
  admin2_District = adm2,
  admin9_Hexagon  = adm9
)
rwa_shp <- make_admin_lookup(rwa_shp)

# Sanity: validate_geometries should pass on the assembled list.
chk <- validate_geometries(rwa_shp, error_on_fail = FALSE)
stopifnot(chk$status %in% c("pass", "warn"))

usethis::use_data(rwa_shp, overwrite = TRUE)

# ---------------------------------------------------------------------------
# 2. rwa_mtdt_full -- parsed multi-level metadata workbook
#
# Use the multi-level synthetic xlsx (3 indicators at admin2_District,
# also present at admin1_Province) so the dataset exercises the
# cascade and is representative of a real PTI deployment.

mtdt_xlsx <- file.path(sample_dir, "sample-metadata-adm1-adm2.xlsx")
stopifnot(file.exists(mtdt_xlsx))

rwa_mtdt_full <- fct_template_reader(mtdt_xlsx)

# Sanity: structure should match what launch_pti / run_pti_pipeline
# expect.
stopifnot(
  is.list(rwa_mtdt_full),
  all(c("general", "metadata", "admin1_Province", "admin2_District") %in%
        names(rwa_mtdt_full)),
  nrow(rwa_mtdt_full$metadata) == 3L
)

usethis::use_data(rwa_mtdt_full, overwrite = TRUE)

# ---------------------------------------------------------------------------
# 3. End-to-end smoke -- validate_metadata exercises the full calc
#    pipeline (with all-equal weights) and is the canonical pair check.
#    Note: the Rwanda xlsx has no weights_table (deployers build weights
#    in the UI), so calling run_pti_pipeline directly would need an
#    explicit weights_clean argument; validate_metadata wraps that for us.

shp_path  <- tempfile(fileext = ".rds")
saveRDS(rwa_shp, shp_path)
chk_full <- validate_metadata(
  shp_path  = shp_path,
  mtdt_path = mtdt_xlsx,
  error_on_fail = FALSE
)
stopifnot(chk_full$status %in% c("pass", "warn"))
unlink(shp_path)

cli::cat_rule("Done")
cli::cat_bullet("Wrote data/rwa_shp.rda")
cli::cat_bullet("Wrote data/rwa_mtdt_full.rda")
cli::cat_bullet(sprintf(
  "rwa_shp:        %d layers, %d polygons total",
  length(rwa_shp),
  sum(vapply(rwa_shp, nrow, integer(1L)))
))
cli::cat_bullet(sprintf(
  "rwa_mtdt_full:  %d slots, %d indicators",
  length(rwa_mtdt_full),
  nrow(rwa_mtdt_full$metadata)
))
