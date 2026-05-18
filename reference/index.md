# Package index

## Launch a PTI app

Entry points that scaffold a project and serve the Shiny dashboard.

- [`create_new_pti()`](https://worldbank.github.io/devPTIpack/reference/create_new_pti.md)
  : Scaffold a new PTI app project
- [`launch_pti()`](https://worldbank.github.io/devPTIpack/reference/launch_pti.md)
  : Launch a multi-tab PTI Shiny app
- [`launch_pti_onepage()`](https://worldbank.github.io/devPTIpack/reference/launch_pti_onepage.md)
  : Launch a single-page PTI Shiny app

## Calculation pipeline

Compute scores, label outputs, and compile deployment artefacts.

- [`compile_pti_data()`](https://worldbank.github.io/devPTIpack/reference/compile_pti_data.md)
  : Compile PTI deployment artefacts from intermediate files
- [`generic_pti_glue()`](https://worldbank.github.io/devPTIpack/reference/generic_pti_glue.md)
  : Default glue template for PTI map popups
- [`label_generic_pti()`](https://worldbank.github.io/devPTIpack/reference/label_generic_pti.md)
  : Apply a glue-template \`pti_label\` column to per-admin PTI tibbles
- [`run_pti_pipeline()`](https://worldbank.github.io/devPTIpack/reference/run_pti_pipeline.md)
  : Run the full PTI calculation pipeline

## Data input

Read shapes and metadata workbooks from disk.

- [`aggregate_hex_to_shapes()`](https://worldbank.github.io/devPTIpack/reference/aggregate_hex_to_shapes.md)
  : Aggregate hex-level indicator data to administrative shapes
- [`build_hex_metadata()`](https://worldbank.github.io/devPTIpack/reference/build_hex_metadata.md)
  : Build the hex-pipeline metadata Excel workbook
- [`fct_template_reader()`](https://worldbank.github.io/devPTIpack/reference/fct_template_reader.md)
  : Read a PTI metadata Excel template into the package's
  list-of-tibbles format
- [`fetch_hex_data()`](https://worldbank.github.io/devPTIpack/reference/fetch_hex_data.md)
  : Fetch hex-level indicator data from registry parquet endpoints
- [`get_available_years()`](https://worldbank.github.io/devPTIpack/reference/get_available_years.md)
  : Query a parquet endpoint for the actual available years of a
  variable
- [`get_shape()`](https://worldbank.github.io/devPTIpack/reference/get_shape.md)
  : Load admin-level shape data for a PTI app
- [`list_hex_vars()`](https://worldbank.github.io/devPTIpack/reference/list_hex_vars.md)
  : List the bundled hex variables available for the PTI pipeline
- [`make_admin_lookup()`](https://worldbank.github.io/devPTIpack/reference/make_admin_lookup.md)
  : Build the parent-child P-code cascade for a shapefile list
- [`make_hex_grid()`](https://worldbank.github.io/devPTIpack/reference/make_hex_grid.md)
  : Build an H3 hexagonal grid covering a country boundary
- [`use_hex_vars()`](https://worldbank.github.io/devPTIpack/reference/use_hex_vars.md)
  : Resolve hex variable names against the registry

## Weighting schemes

Build weight sets passed to the calculation pipeline.

- [`get_all_weights_combs()`](https://worldbank.github.io/devPTIpack/reference/get_all_weights_combs.md)
  : Enumerate all weighting-scheme combinations of a given size
- [`get_min_admin_wght()`](https://worldbank.github.io/devPTIpack/reference/get_min_admin_wght.md)
  : Reduce per-scheme weights to the admin levels that must be hidden
- [`get_rand_weights()`](https://worldbank.github.io/devPTIpack/reference/get_rand_weights.md)
  : Generate a random list of weighting schemes

## Data export

Bundle scores and weight tables for the dashboard’s download buttons.

- [`get_pti_scores_export()`](https://worldbank.github.io/devPTIpack/reference/get_pti_scores_export.md)
  : Reshape PTI scores into per-admin-level export tibbles
- [`get_pti_weights_export()`](https://worldbank.github.io/devPTIpack/reference/get_pti_weights_export.md)
  : Reshape weight schemes into a single export tibble
- [`get_vars_un_avbil()`](https://worldbank.github.io/devPTIpack/reference/get_vars_un_avbil.md)
  : Identify (variable, admin-level) pairs with no native data

## Input validation

Structural and visual checks on shapes and metadata inputs.

- [`app_validate_metadata()`](https://worldbank.github.io/devPTIpack/reference/app_validate_metadata.md)
  : Standalone validation app – geometry + metadata + Data Explorer
- [`app_validate_shp()`](https://worldbank.github.io/devPTIpack/reference/app_validate_shp.md)
  : Visual shapefile inspector for PTI deployers
- [`drop_inval_adm()`](https://worldbank.github.io/devPTIpack/reference/drop_inval_adm.md)
  : Strip unplottable admin levels out of a pre-plot data structure
- [`mod_drop_inval_adm()`](https://worldbank.github.io/devPTIpack/reference/mod_drop_inval_adm.md)
  : Drop-invalid-admin module server
- [`validate_geometries()`](https://worldbank.github.io/devPTIpack/reference/validate_geometries.md)
  : Validate every geometry layer in a shapes list
- [`validate_metadata()`](https://worldbank.github.io/devPTIpack/reference/validate_metadata.md)
  : Verify that a shapes file and metadata file together produce valid
  PTI scores
- [`validate_read_metadata()`](https://worldbank.github.io/devPTIpack/reference/validate_read_metadata.md)
  : Validate a metadata \`.xlsx\` file in isolation
- [`validate_read_shp()`](https://worldbank.github.io/devPTIpack/reference/validate_read_shp.md)
  : Validate a shapes \`.rds\` file in isolation

## Visualisation helpers

ggplot2-based map builders shared by the dashboard and the PDF render.

- [`gg_admin_list()`](https://worldbank.github.io/devPTIpack/reference/gg_admin_list.md)
  : Build a list of ggplot maps from PTI admin data

## Shiny modules

The reactive building blocks behind
[`launch_pti()`](https://worldbank.github.io/devPTIpack/reference/launch_pti.md).

- [`mod_dta_explorer2_server()`](https://worldbank.github.io/devPTIpack/reference/mod_dta_explorer2_server.md)
  : Data-explorer page server module
- [`mod_dta_explorer2_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_dta_explorer2_ui.md)
  : Data-explorer page UI
- [`mod_leaf_side_panel_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_leaf_side_panel_ui.md)
  : Map side-panel UI for a PTI page
- [`mod_map_pti_leaf_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_map_pti_leaf_ui.md)
  : Map container UI for PTI page modules
- [`mod_plot_pti2_srv()`](https://worldbank.github.io/devPTIpack/reference/mod_plot_pti2_srv.md)
  : Main PTI map orchestration module
- [`mod_pti_comparepage_newsrv()`](https://worldbank.github.io/devPTIpack/reference/mod_pti_comparepage_newsrv.md)
  : Comparison-page server module
- [`mod_pti_comparepage_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_pti_comparepage_ui.md)
  : Comparison-page UI for two side-by-side PTI maps
- [`mod_ptipage_newsrv()`](https://worldbank.github.io/devPTIpack/reference/mod_ptipage_newsrv.md)
  : Page-level PTI server module
- [`mod_ptipage_twocol_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_ptipage_twocol_ui.md)
  [`mod_ptipage_box_ui()`](https://worldbank.github.io/devPTIpack/reference/mod_ptipage_twocol_ui.md)
  : Two-column UI for a PTI page
- [`mod_tab_open_first_newserv()`](https://worldbank.github.io/devPTIpack/reference/mod_tab_open_first_newserv.md)
  : Tab-opening invalidator for custom PTI page layouts

## Package utilities

golem boilerplate and resource resolvers.

- [`app_sys()`](https://worldbank.github.io/devPTIpack/reference/app_sys.md)
  : Resolve a path inside the installed devPTIpack package
- [`golem_add_external_resources()`](https://worldbank.github.io/devPTIpack/reference/golem_add_external_resources.md)
  : Bundle external resources for a PTI Shiny app

## Bundled sample data

Datasets ready to feed the pipeline. Rwanda for user-facing examples;
Ukraine for the test suite.

- [`rwa_mtdt_full`](https://worldbank.github.io/devPTIpack/reference/rwa_mtdt_full.md)
  : Rwanda sample PTI metadata input
- [`rwa_shp`](https://worldbank.github.io/devPTIpack/reference/rwa_shp.md)
  : Rwanda sample administrative boundaries
- [`ukr_mtdt_full`](https://worldbank.github.io/devPTIpack/reference/ukr_mtdt_full.md)
  : Ukraine sample PTI metadata input
- [`ukr_shp`](https://worldbank.github.io/devPTIpack/reference/ukr_shp.md)
  : Ukraine sample administrative boundaries
