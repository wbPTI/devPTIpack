# Generate seeded synthetic Rwanda metadata Excel files for the template.
#
# Inputs:
#   sample-data/rwa_adm0.geojson, rwa_adm1.geojson, rwa_adm2.geojson
# Outputs:
#   sample-data/sample-metadata-adm1.xlsx          (Adm1 only)
#   sample-data/sample-metadata-adm1-adm2.xlsx     (Adm1 + Adm2)
#
# Indicators (3, plausible Rwanda-shaped ranges):
#   poverty_rate    -- 0-100 %
#   literacy_rate   -- 0-100 %
#   road_density    -- 0-10 km/km^2
#
# Determinism: set.seed(42). Re-running produces byte-identical sheet
# contents (verified via identical() of read-back tibbles).

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tibble)
  library(writexl)
  library(stringr)
})

set.seed(42)

# Resolve script-relative paths so this works whether sourced from the
# template root or from inst/template_pti/data-raw/.
this_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, mustWork = FALSE),
  error = function(e) NA_character_
)
if (is.na(this_file) || !nzchar(this_file)) {
  # Running interactively; assume cwd is template root.
  template_root <- getwd()
} else {
  template_root <- normalizePath(file.path(dirname(this_file), ".."))
}

shp_dir <- file.path(template_root, "sample-data")

adm0 <- read_sf(file.path(shp_dir, "rwa_adm0.geojson"))
adm1 <- read_sf(file.path(shp_dir, "rwa_adm1.geojson"))
adm2 <- read_sf(file.path(shp_dir, "rwa_adm2.geojson"))

stopifnot(nrow(adm0) == 1L, nrow(adm1) == 5L, nrow(adm2) == 30L)

# Build adm1 keys (admin1Pcod / admin1Name) using shapeID / shapeName.
adm1_meta <- adm1 |>
  st_drop_geometry() |>
  transmute(
    admin0Pcod = "RWA",
    admin1Pcod = shapeID,
    admin1Name = shapeName
  ) |>
  arrange(admin1Name)

# Spatial-join adm2 -> adm1 to derive the parent admin1Pcod for each
# admin2 polygon. Use centroids on the same CRS to avoid invalid-geom
# warnings.
adm2_centroids <- suppressWarnings(st_centroid(adm2))
adm2_join <- suppressWarnings(
  st_join(adm2_centroids, adm1, join = st_within, suffix = c("", "_adm1"))
)

adm2_meta <- adm2_join |>
  st_drop_geometry() |>
  transmute(
    admin0Pcod = "RWA",
    admin1Pcod = shapeID_adm1,
    admin2Pcod = shapeID,
    admin2Name = shapeName
  ) |>
  arrange(admin2Name)

stopifnot(!any(is.na(adm2_meta$admin1Pcod)))

# Indicator definitions.
indicators <- tibble::tribble(
  ~var_code,        ~var_name,            ~var_description,                                       ~var_units,        ~min,  ~max,
  "poverty_rate",   "Poverty rate",       "Share of population below the national poverty line.", "%",                  0,   100,
  "literacy_rate",  "Adult literacy rate", "Share of adults (15+) who can read and write.",        "%",                  0,   100,
  "road_density",   "Road density",       "Length of paved roads per square kilometre.",          "km/km^2",            0,    10
)

# Synthetic indicator values per polygon.
gen_values <- function(n, lo, hi) {
  round(runif(n, lo, hi), 2)
}

# Adm1-level indicator panel.
adm1_panel <- adm1_meta |>
  mutate(year = NA)
for (i in seq_len(nrow(indicators))) {
  v <- indicators$var_code[i]
  adm1_panel[[v]] <- gen_values(nrow(adm1_panel), indicators$min[i], indicators$max[i])
}

# Adm2-level indicator panel (re-uses the same indicator codes).
adm2_panel <- adm2_meta |>
  mutate(year = NA)
for (i in seq_len(nrow(indicators))) {
  v <- indicators$var_code[i]
  adm2_panel[[v]] <- gen_values(nrow(adm2_panel), indicators$min[i], indicators$max[i])
}

# Metadata sheet (indicator dictionary).
build_metadata_sheet <- function(spatial_level) {
  tibble(
    var_code = indicators$var_code,
    var_name = indicators$var_name,
    var_description = indicators$var_description,
    var_order = seq_len(nrow(indicators)),
    var_units = indicators$var_units,
    spatial_level = spatial_level,
    pillar_group = c("Welfare", "Welfare", "Connectivity"),
    pillar_name = c("Poverty & welfare", "Poverty & welfare", "Connectivity"),
    pillar_description = c(
      "Indicators capturing material welfare and human-capital outcomes.",
      "Indicators capturing material welfare and human-capital outcomes.",
      "Indicators capturing transport and access infrastructure."
    ),
    fltr_exclude_pti = FALSE,
    fltr_exclude_explorer = FALSE,
    fltr_overlay_pti = FALSE,
    fltr_overlay_explorer = FALSE,
    legend_revert_colours = c(TRUE, FALSE, FALSE) # higher poverty = worse outcome
  )
}

general_sheet <- tibble(country = "Rwanda")

# --- File 1: Adm1 only ----------------------------------------------------

mtdt_adm1 <- list(
  general = general_sheet,
  admin1_Province = adm1_panel,
  metadata = build_metadata_sheet("admin1_Province")
)

out1 <- file.path(shp_dir, "sample-metadata-adm1.xlsx")
write_xlsx(mtdt_adm1, path = out1)
cat("Wrote", out1, "(", file.size(out1), "bytes)\n")

# --- File 2: Adm1 + Adm2 --------------------------------------------------

# For the multi-level file, indicators are observed at admin2_District
# (the most-disaggregated level). The metadata.spatial_level field
# follows the same convention used by ukr_mtdt_full -- one row per
# (var_code, spatial_level) where the indicator is natively observed.
mtdt_adm12 <- list(
  general = general_sheet,
  admin1_Province = adm1_panel,
  admin2_District = adm2_panel,
  metadata = build_metadata_sheet("admin2_District")
)

out2 <- file.path(shp_dir, "sample-metadata-adm1-adm2.xlsx")
write_xlsx(mtdt_adm12, path = out2)
cat("Wrote", out2, "(", file.size(out2), "bytes)\n")

invisible(list(adm1 = out1, adm12 = out2))
