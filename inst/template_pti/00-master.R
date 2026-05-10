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

quarto::quarto_render("01-shapes.qmd")
# quarto::quarto_render("02a-user-zonal-stats.qmd")  # optional
quarto::quarto_render("03-metadata.qmd")
# quarto::quarto_render("04-hex-data.qmd")           # stub, blocked by HEX API
quarto::quarto_render("05-compile.qmd")

# Deployment runs manually:
# source("06-deploy.R")
