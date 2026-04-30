---
name: "Architecture cleanup: remove legacy dead code"
about: Remove verified dead code across 6 batches
labels: tech-debt, refactoring
---

# Architecture Cleanup: Remove Legacy Dead Code

**Reference:** [`.github/docs/arch-01-cleanup.md`](../.github/docs/arch-01-cleanup.md)

## Summary

Code-verified audit (2026-04-29) found **~40 functions** that are dead code and
**4 entire files** that can be deleted. Legacy modules (`mod_calc_pti`,
`mod_explorer`, `mod_info_page`, `mod_map_pti_leaf` server, `mod_weights`,
`fct_export_data`) coexist with their modern replacements and add ~2000+ lines
of unused code.

## Batches

### Batch 1 — Immediate (no refactoring needed)
- [ ] Delete `R/fct_export_data.R` (4 orphaned functions)
- [ ] Delete `R/mod_explorer.R` (4 functions, replaced by `mod_dta_explorer2`)
- [ ] Delete `R/mod_info_page.R` (3 functions, replaced by `mod_infotab`)
- [ ] Delete `R/mod_calc_pti.R` (3 functions, replaced by `mod_calc_pti2`)
- [ ] Remove dead functions from 10+ other files (see cleanup doc for full list)
- [ ] Delete `data/ukr_mtdt_nodata_level.rda` (undocumented, zero references)
- [ ] Run `devtools::document()` to regenerate NAMESPACE

### Batch 2 — Remove legacy runners and backends
- [ ] Delete `run_pti`, `run_onepage_pti`, `run_onepage_pti_sample`, `run_dev_map_pti`, `run_dev_pti_plot`
- [ ] Delete `app_server`, `app_server_input_simple`, `app_server_sample_pti_vis`
- [ ] Delete `app_ui`, `app_server_sample_pti_vis_ui`
- [ ] Delete `mod_pti_onepage.R` entirely

### Batch 3 — Extract `mod_map_pti_leaf_ui`, remove legacy map server
- [ ] Extract `mod_map_pti_leaf_ui()` to own file (used by active UI code)
- [ ] Delete 12 legacy server functions from `mod_map_pti_leaf.R` (~1200 lines)
- [ ] Delete `make_shapes()` and `make_labels()` from `fct_legend_map_satelites.R`

### Batch 4 — Migrate sample app to `launch_pti()`
- [ ] Rewrite `inst/sample_pti/app.R` to use `launch_pti()`
- [ ] Delete `run_new_pti`, `app_new_pti_server`, `app_new_pti_ui`
- [ ] Delete `mod_plot_pti_comparison_srv`

### Batch 5 — Extract shared helpers, remove `mod_weights.R`
- [ ] Extract `mod_wt_btns_srv` + `mod_collect_wt_srv` to own file (needed by `mod_DT_inputs`)
- [ ] Delete remaining 11 functions from `mod_weights.R`
- [ ] Delete `mod_new_weights.R` entirely

### Batch 6 (Optional) — Deprecate convenience wrappers
- [ ] Deprecate `mod_explrr_onepage_ui/server`
- [ ] Deprecate `render_metadata()`

## Architecture Doc Corrections (already applied)

1. `get_pti_weights_export()` was incorrectly listed as "Legacy: defined but unused" → it IS called by `mod_export_pti_data_server()`
2. `get_indicators_list()` was claimed to be duplicated in `dta_cleaners.R` and `supporting-goe-prep.R` → it only exists in `dta_cleaners.R`

## Acceptance Criteria

- [ ] All batch 1 items removed, `R CMD check` passes
- [ ] No regressions in `testthat` test suite
- [ ] NAMESPACE regenerated after each batch
- [ ] `architecture.md` updated to remove deleted entries
