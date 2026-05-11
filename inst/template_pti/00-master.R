# 00-master.R -- pipeline orchestrator.
#
# Renders the step `.qmd` files top-to-bottom in their canonical order.
# Each step writes its output into `app-data/`; downstream steps read
# from there. Run this script with the project root as the working
# directory:
#
#   source("00-master.R")
#
# Comment / uncomment the lines below to skip optional or future steps.
#
# Step state:
#   01  Shapes              -- working (Rwanda data + validate_geometries)
#   02a Zonal stats         -- optional stub; user runs manually if needed
#   03  User metadata Excel -- working (Rwanda synthetic data + validate_metadata)
#   04  HEX data            -- stub (blocked by HEX API; not rendered here)
#   05  Compile             -- working (compile_pti_data merges + validates + renders)
#   06  Deploy              -- manual; see 06-deploy.R

# ── Hex grid configuration ───────────────────────────────────────────────────
# H3 resolution for the hex grid built in Step 1 and used throughout
# the pipeline. This is the single place that controls what resolution
# goes into the app.
#
#   5  ~252 km² per cell  -- very large countries, coarse view
#   6  ~36 km²  per cell  -- default; most country-level PTI apps
#   7  ~5 km²   per cell  -- small countries or high-detail apps
#
# Changing this after running Step 1 requires re-running Steps 1, 4,
# and 5 in sequence.
HEX_RESOLUTION <- 6L

# Set to TRUE to include hex-level polygons in the deployed app.
# When FALSE (default), hex-sourced indicators are still available at
# all admin levels -- only the hex polygons themselves are excluded.
# Recommended FALSE when the hex grid exceeds ~5,000 cells.
INCLUDE_HEX_IN_APP <- FALSE

quarto::quarto_render("01-shapes.qmd")
# quarto::quarto_render("02a-user-zonal-stats.qmd")  # optional
quarto::quarto_render("03-metadata.qmd")
# quarto::quarto_render("04-hex-data.qmd")           # stub, blocked by HEX API
quarto::quarto_render("05-compile.qmd")

# Deployment runs manually:
# source("06-deploy.R")
