# data-raw/generate-rwa-package-data.R
#
# Compile the Rwanda tutorial inputs (GeoJSONs + synthetic metadata
# workbook) shipped under inst/template_pti/sample-data/ into
# package-level datasets:
#
#   data/rwa_shp.rda          -- named list of 3 sf tibbles
#                                (admin0_Country, admin1_Province,
#                                admin2_District). Mirrors ukr_shp.
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

# ---------------------------------------------------------------------------
# 1. rwa_shp -- mirrors ukr_shp structure
#
# Lifted from inst/template_pti/01-shapes.qmd: read the geoBoundaries
# GeoJSONs, attach admin<N>Pcod / admin<N>Name / area, derive admin1
# parent of each admin2 polygon via a centroid-in-polygon spatial join,
# assemble the named list.

adm0_raw <- read_sf(file.path(sample_dir, "rwa_adm0.geojson"))
adm1_raw <- read_sf(file.path(sample_dir, "rwa_adm1.geojson"))
adm2_raw <- read_sf(file.path(sample_dir, "rwa_adm2.geojson"))

adm0 <- adm0_raw |>
  mutate(
    admin0Pcod = "RWA",
    admin0Name = shapeName,
    area       = as.numeric(st_area(geometry))
  ) |>
  select(admin0Pcod, admin0Name, area, geometry)

adm1 <- adm1_raw |>
  mutate(
    admin0Pcod = "RWA",
    admin1Pcod = shapeID,
    admin1Name = shapeName,
    area       = as.numeric(st_area(geometry))
  ) |>
  select(admin0Pcod, admin1Pcod, admin1Name, area, geometry)

adm2_centroids <- suppressWarnings(st_centroid(adm2_raw))
parent_lookup  <- suppressWarnings(
  st_join(adm2_centroids, adm1_raw, join = st_within, suffix = c("", "_p"))
) |>
  st_drop_geometry() |>
  transmute(child_id = shapeID, admin1Pcod = shapeID_p)

adm2 <- adm2_raw |>
  left_join(parent_lookup, by = c("shapeID" = "child_id")) |>
  mutate(
    admin0Pcod = "RWA",
    admin2Pcod = shapeID,
    admin2Name = shapeName,
    area       = as.numeric(st_area(geometry))
  ) |>
  select(admin0Pcod, admin1Pcod, admin2Pcod, admin2Name, area, geometry)

rwa_shp <- list(
  admin0_Country  = adm0,
  admin1_Province = adm1,
  admin2_District = adm2
)

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
