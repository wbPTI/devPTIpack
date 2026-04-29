# Architecture Cleanup Plan — `devPTIpack`

> Verified against source code on 2026-04-29. Each function was traced through the
> entire `R/`, `inst/`, `dev/`, and `tests/` directories.

---

## Removal Batches

### Batch 1 — Immediate (zero callers, pure dead code)

No refactoring required. Delete files/functions and run `devtools::document()`.

| Item                                                       | Action                                                                                                                                                     |
| ---------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Entire file** `R/fct_export_data.R`                      | Delete — all 4 functions (`rename_one`, `prepare_tibble_to_export`, `prepare_DT_export`, `dt_wrap_exported`) are an orphaned chain with zero callers       |
| **Entire file** `R/mod_explorer.R`                         | Delete — superseded by `mod_dta_explorer2.R`. All 4 functions only called from legacy `app_server()` / `app_ui()`                                          |
| **Entire file** `R/mod_info_page.R`                        | Delete — superseded by `mod_infotab.R`. All 3 functions only called from legacy `app_server()`                                                             |
| **Entire file** `R/mod_calc_pti.R`                         | Delete — superseded by `mod_calc_pti2.R`. `mod_calc_pti_server` only called from legacy `app_server()`. Remove `export(extrapo_one_weight)` from NAMESPACE |
| `get_golem_config()` in `app_config.R`                     | Delete function — zero callers anywhere                                                                                                                    |
| `fct_convert_clean_to_weight()` in `fct_template_reader.R` | Delete function — only referenced in commented-out code in `mod_weights.R`                                                                                 |
| `fct_exp_wt_to_internal()` in `fct_inp_for_exp.R`          | Delete stub — zero callers. Remove `export(fct_exp_wt_to_internal)` from NAMESPACE                                                                         |
| `get_pal_lab_fn()` in `fct_legend_map_satelites.R`         | Delete — never called; duplicates inline logic in `legend_map_satelite()`                                                                                  |
| `zeros_after_period()` in `fct_legend_map_satelites.R`     | Delete — never called anywhere                                                                                                                             |
| `mod_plot_poly_leaf_server_ui()` in `mod_plot_poly_leaf.R` | Delete — empty `tagList()`, never called                                                                                                                   |
| `make_ggmap_2()` in `mod_plot_poly_leaf.R`                 | Delete — only referenced in commented-out code                                                                                                             |
| `make_gg_line_map_2()` in `mod_plot_poly_leaf.R`           | Delete — only referenced in commented-out code                                                                                                             |
| `make_spplot()` in `mod_plot_poly_leaf.R`                  | Delete — body is commented out, zero active callers                                                                                                        |
| `make_sp_line_map()` in `mod_plot_poly_leaf.R`             | Delete — body is empty, only dev/commented references                                                                                                      |
| `mod_new_weights_ui()` in `mod_new_weights.R`              | Delete — zero callers                                                                                                                                      |
| `mod_weights_html_ui()` in `mod_new_weights.R`             | Delete — only called from dead `mod_new_weights_ui`. Remove NAMESPACE export                                                                               |
| `mod_load_shapes_server()` in `mod_load_shapes.R`          | Delete — legacy `callModule` wrapper, zero callers                                                                                                         |
| `mod_dwnld_dta_btn_ui()` in `mod_dwnld_dta.R`              | Delete — zero callers                                                                                                                                      |
| `mod_dwnld_local_file_ui()` in `mod_dwnld_local_file.R`    | Delete UI function only (keep `mod_dwnld_local_file_server` — it is active)                                                                                |
| `mod_export_recoded_srv()` in `mod_weights.R`              | Delete — only called from legacy `app_server` + `app_server_input_simple`                                                                                  |
| `mod_weights_rand_server()` in `mod_weights_rand.R`        | Delete — only called from legacy `app_server_input_simple`                                                                                                 |
| `plot_admin_level_ggmap()` in `supporting-goe-prep.R`      | Delete — only self-referential, no external callers                                                                                                        |
| `plot_pti_plotslist()` in `supporting-goe-prep.R`          | Delete — zero callers                                                                                                                                      |
| `get_intercestions()` in `supporting-goe-prep.R`           | Delete — zero callers (also has a typo in name)                                                                                                            |
| `plot_single_ggmap()` in `supporting-goe-prep.R`           | Delete — only called from other dead code above                                                                                                            |
| `data/ukr_mtdt_nodata_level.rda`                           | Delete — undocumented, zero references anywhere                                                                                                            |

After this batch: clean up corresponding commented-out `do.call(...)` lines in
`mod_pti_map_side_pan.R` (L338–346, L432–440) and run `devtools::document()` to
regenerate NAMESPACE.

---

### Batch 2 — Remove legacy runners and their backends

Remove legacy `run_*` entry points and the server/UI functions they depend on.

| Item                                              | Action                                                           |
| ------------------------------------------------- | ---------------------------------------------------------------- |
| `run_pti()` in `run_app.R`                        | Delete — **zero** callers anywhere                               |
| `run_onepage_pti_sample()` in `run_app.R`         | Delete — zero callers                                            |
| `run_onepage_pti()` in `run_app.R`                | Delete — only called by `run_onepage_pti_sample` (removed above) |
| `run_dev_map_pti()` in `run_app.R`                | Delete — dev-only, only referenced in commented-out dev/ scripts |
| `run_dev_pti_plot()` in `run_app.R`               | Delete — dev-only                                                |
| `app_server()` in `app_server.R`                  | Delete — only caller was `run_pti()`                             |
| `app_server_input_simple()` in `app_server.R`     | Delete — only caller was `run_dev_map_pti()`                     |
| `app_server_sample_pti_vis()` in `app_server.R`   | Delete — only caller was `run_dev_pti_plot()`                    |
| `app_ui()` in `app_ui.R`                          | Delete — only callers were `run_pti()` and `run_dev_map_pti()`   |
| `app_server_sample_pti_vis_ui()` in `app_ui.R`    | Delete — only caller was `run_dev_pti_plot()`                    |
| `mod_pti_onepage_ui()` in `mod_pti_onepage.R`     | Delete — only called from `run_onepage_pti()` + dev/             |
| `mod_pti_onepage_server()` in `mod_pti_onepage.R` | Delete — same                                                    |

After this batch: `mod_pti_onepage.R` can be deleted entirely.
Remove all corresponding NAMESPACE exports.

---

### Batch 3 — Extract `mod_map_pti_leaf_ui`, remove legacy map server (~1200 lines)

**Prerequisite:** Extract `mod_map_pti_leaf_ui()` to its own small file (or
inline into `mod_ptipage_core.R`) — it is actively used by `mod_ptipage_twocol_ui`,
`mod_ptipage_box_ui`, and `mod_pti_comparepage_ui`.

Then delete these 12 legacy server functions from `mod_map_pti_leaf.R`:

| Function                    | Reason                                 |
| --------------------------- | -------------------------------------- |
| `mod_map_pti_leaf_srv`      | Only called from legacy `app_server()` |
| `mod_map_pti_leaf_page_ui`  | Only called from legacy `app_ui()`     |
| `mod_map_pti_leaf_page_srv` | Only called from legacy `app_server()` |
| `mod_init_leaf_pti_srv`     | Internal to `mod_map_pti_leaf_srv`     |
| `mod_flyto_leaf_srv`        | Internal to `mod_map_pti_leaf_srv`     |
| `mod_clean_leaf_srv`        | Internal to `mod_map_pti_leaf_srv`     |
| `mod_map_pal_srv`           | Internal to `mod_map_pti_leaf_srv`     |
| `mod_map_dta_srv`           | Internal to `mod_map_pti_leaf_srv`     |
| `mod_plot_map_srv`          | Internal to `mod_map_pti_leaf_srv`     |
| `mod_plot_controls_srv`     | Internal to `mod_map_pti_leaf_srv`     |
| `mod_plot_legend_srv`       | Internal to `mod_map_pti_leaf_srv`     |
| `mod_clean_buble_data_srv`  | Internal to `mod_map_pti_leaf_srv`     |

After extraction, `make_shapes()` and `make_labels()` in `fct_legend_map_satelites.R`
become dead code and can also be removed.

---

### Batch 4 — Migrate `inst/sample_pti/app.R` to `launch_pti()`

**Prerequisite:** Rewrite `inst/sample_pti/app.R` to call `launch_pti()` instead
of `run_new_pti()`.

Then remove:

| Item                                                 | Action                                                               |
| ---------------------------------------------------- | -------------------------------------------------------------------- |
| `run_new_pti()` in `run_app.R`                       | Delete                                                               |
| `app_new_pti_server()` in `app_server.R`             | Delete                                                               |
| `app_new_pti_ui()` in `app_ui.R`                     | Delete                                                               |
| `mod_plot_pti_comparison_srv()` in `mod_plot_pti2.R` | Delete — thin wrapper, modern code uses `mod_pti_comparepage_newsrv` |

After this batch: `app_server.R` likely becomes empty and can be deleted.
`app_ui.R` retains only `golem_add_external_resources()`.

---

### Batch 5 — Migrate `mod_weights.R` dependencies

**Prerequisite:** Migrate `app_server_sample_pti_vis` and `app_new_pti_server`
(already removed in Batch 4) away from `mod_weights_server`. Extract
`mod_wt_btns_srv` and `mod_collect_wt_srv` to their own file — `mod_DT_inputs_server`
depends on them.

Then remove from `mod_weights.R`:

| Function                | Reason                           |
| ----------------------- | -------------------------------- |
| `mod_weights_ui`        | No remaining callers             |
| `mod_weights_server`    | No remaining callers             |
| `mod_indicarots_srv`    | Internal to `mod_weights_server` |
| `mod_gen_wt_inputs_srv` | Internal to `mod_weights_server` |
| `mod_wt_name_srv`       | Internal to `mod_weights_server` |
| `mod_wt_select_srv`     | Internal to `mod_weights_server` |
| `mod_wt_uplod_srv`      | Internal to `mod_weights_server` |
| `mod_wt_delete_srv`     | Internal to `mod_weights_server` |
| `mod_wt_fill_srv`       | Internal to `mod_weights_server` |
| `mod_wt_save_srv`       | Internal to `mod_weights_server` |
| `mod_download_wt_srv`   | Internal to `mod_weights_server` |

Keep (relocated):
- `mod_wt_btns_srv` — used by `mod_DT_inputs_server`
- `mod_collect_wt_srv` — used by `mod_DT_inputs_server`

Also remove `mod_new_demo_weights_server` and `mod_new_weights_server` from
`mod_new_weights.R` (their only caller `mod_pti_onepage_server` was removed in Batch 2).

After this batch: `mod_weights.R` can be deleted. `mod_new_weights.R` can be deleted.
`mod_weights_rand.R` retains only `mod_weights_rand_ui`, `get_rand_weights`,
`get_all_weights_combs`.

---

### Batch 6 (Optional) — Deprecate convenience wrappers

| Item                                                     | Notes                                                                                                                   |
| -------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------- |
| `mod_explrr_onepage_ui/server` in `mod_explrr_onepage.R` | Exported thin wrappers over `mod_dta_explorer2_*`. Only called from `dev/90-app-examples.R`. Deprecate before removing. |
| `render_metadata()` in `render_metadata_pdf.R`           | Exported, zero internal callers. May have external users. Deprecate first.                                              |

---

### Future Consolidation Candidates

| Item                                                                  | Notes                                                                                                                            |
| --------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------- |
| `mod_first_open_count_server()` vs `mod_tab_open_first_newserv()`     | Nearly identical deferred-render triggers. Consolidate into one.                                                                 |
| `mod_map_dwnld_srv()` duplicate `mod_dwnld_local_file_server()` calls | Lines 477–484 register the handler with both bare and `ns()`-wrapped IDs — likely a bug.                                         |
| `gg_admin_list()` in `supporting-goe-prep.R`                          | Only active function in that file (used by metadata PDFs). Relocate to `plot_pti_helpers.R`.                                     |
| Commented-out code blocks                                             | `mod_map_pti_leaf.R`, `mod_plot_poly_leaf.R`, `mod_weights.R`, `calc_pti_expander.R` — remove after corresponding cleanup batch. |
| Deprecated dplyr usage                                                | Migrate `group_by_at`, `distinct_at`, `rename_at`, `filter_at`, `summarise_all`, `vars`, `select_if` → `across()` / `.by`.       |

---

## Detailed Function-by-Function Reference

### `R/mod_calc_pti.R` — Legacy Calculation ✂️ Batch 1

| Function              | Called From                | Safe to Remove? |
| --------------------- | -------------------------- | --------------- |
| `mod_calc_pti_ui`     | None (dead)                | **Yes**         |
| `mod_calc_pti_server` | `app_server()` (legacy)    | **Yes**         |
| `extrapo_one_weight`  | `mod_calc_pti_server` only | **Yes**         |

---

### `R/mod_weights.R` — Legacy Weights ✂️ Batch 1 (partial) + Batch 5

| Function                 | Called From                                       | Safe to Remove?                |
| ------------------------ | ------------------------------------------------- | ------------------------------ |
| `mod_weights_ui`         | `mod_pti_onepage_ui`, `app_ui`                    | No → **Yes after Batch 2 + 5** |
| `mod_weights_server`     | `app_server_sample_pti_vis`, `app_new_pti_server` | No → **Yes after Batch 4 + 5** |
| `mod_indicarots_srv`     | `mod_weights_server`, `mod_new_weights.R`         | No → **Yes after Batch 5**     |
| `mod_gen_wt_inputs_srv`  | `mod_weights_server`, `mod_new_weights_server`    | No → **Yes after Batch 5**     |
| `mod_wt_btns_srv`        | **`mod_DT_inputs_server`** (active!)              | No — extract first             |
| `mod_wt_name_srv`        | `mod_weights_server`                              | No → **Yes after Batch 5**     |
| `mod_wt_select_srv`      | `mod_weights_server`                              | No → **Yes after Batch 5**     |
| `mod_wt_uplod_srv`       | `mod_weights_server`                              | No → **Yes after Batch 5**     |
| `mod_wt_delete_srv`      | `mod_weights_server`                              | No → **Yes after Batch 5**     |
| `mod_wt_fill_srv`        | `mod_weights_server`                              | No → **Yes after Batch 5**     |
| `mod_wt_save_srv`        | `mod_weights_server`                              | No → **Yes after Batch 5**     |
| `mod_collect_wt_srv`     | **`mod_DT_inputs_server`** (active!)              | No — extract first             |
| `mod_download_wt_srv`    | `mod_weights_server`                              | No → **Yes after Batch 5**     |
| `mod_export_recoded_srv` | `app_server` + `app_server_input_simple` (legacy) | **Yes** ✂️ Batch 1              |

---

### `R/mod_new_weights.R` — Dev Weights ✂️ Batch 1 (partial) + Batch 5

| Function                      | Called From                    | Safe to Remove?                |
| ----------------------------- | ------------------------------ | ------------------------------ |
| `mod_new_weights_ui`          | None (dead)                    | **Yes** ✂️ Batch 1              |
| `mod_weights_html_ui`         | Dead `mod_new_weights_ui` only | **Yes** ✂️ Batch 1              |
| `mod_new_demo_weights_server` | `mod_pti_onepage_server`       | No → **Yes after Batch 2 + 5** |
| `mod_new_weights_server`      | `mod_pti_onepage_server`       | No → **Yes after Batch 2 + 5** |

---

### `R/mod_weights_rand.R` ✂️ Batch 1 (partial)

| Function                  | Called From                        | Safe to Remove?            |
| ------------------------- | ---------------------------------- | -------------------------- |
| `mod_weights_rand_ui`     | `mod_weights_ui` (active)          | No → **Yes after Batch 5** |
| `mod_weights_rand_server` | `app_server_input_simple` (legacy) | **Yes** ✂️ Batch 1          |
| `get_rand_weights`        | Tests + dev scripts                | No                         |
| `get_all_weights_combs`   | `fct_validate_metadata` (active)   | No                         |

---

### `R/mod_map_pti_leaf.R` — Legacy Map ✂️ Batch 3

| Function                    | Called From                                        | Safe to Remove?              |
| --------------------------- | -------------------------------------------------- | ---------------------------- |
| `mod_map_pti_leaf_ui`       | **Active** (mod_ptipage_core, mod_pti_comparepage) | **No** — extract to own file |
| `mod_map_pti_leaf_srv`      | `app_server` (legacy)                              | **Yes**                      |
| `mod_map_pti_leaf_page_ui`  | `app_ui` (legacy)                                  | **Yes**                      |
| `mod_map_pti_leaf_page_srv` | `app_server` (legacy)                              | **Yes**                      |
| `mod_init_leaf_pti_srv`     | Legacy only                                        | **Yes**                      |
| `mod_flyto_leaf_srv`        | Legacy only                                        | **Yes**                      |
| `mod_clean_leaf_srv`        | Legacy only                                        | **Yes**                      |
| `mod_map_pal_srv`           | Legacy only                                        | **Yes**                      |
| `mod_map_dta_srv`           | Legacy only                                        | **Yes**                      |
| `mod_plot_map_srv`          | Legacy only                                        | **Yes**                      |
| `mod_plot_controls_srv`     | Legacy only                                        | **Yes**                      |
| `mod_plot_legend_srv`       | Legacy only                                        | **Yes**                      |
| `mod_clean_buble_data_srv`  | Legacy only                                        | **Yes**                      |

---

### `R/mod_explorer.R` — Legacy Explorer ✂️ Batch 1

| Function                     | Called From           | Safe to Remove? |
| ---------------------------- | --------------------- | --------------- |
| `mod_explorer_tab_ui`        | `app_ui` (legacy)     | **Yes**         |
| `mod_explorer_side_panel_ui` | Internal only         | **Yes**         |
| `mod_explorer_server`        | `app_server` (legacy) | **Yes**         |
| `mod_select_var_srv`         | Internal only         | **Yes**         |

---

### `R/mod_info_page.R` — Legacy Info ✂️ Batch 1

| Function               | Called From                    | Safe to Remove? |
| ---------------------- | ------------------------------ | --------------- |
| `mod_info_page_ui`     | Never called                   | **Yes**         |
| `mod_info_page_server` | `app_server` variants (legacy) | **Yes**         |
| `compile_guides`       | Internal only                  | **Yes**         |

---

### `R/mod_plot_pti2.R` ✂️ Batch 4

| Function                      | Called From                                       | Safe to Remove?       |
| ----------------------------- | ------------------------------------------------- | --------------------- |
| `mod_plot_pti2_srv`           | Core active                                       | No                    |
| `mod_plot_pti_comparison_srv` | `app_server_sample_pti_vis`, `app_new_pti_server` | **Yes after Batch 4** |

---

### `R/mod_plot_poly_leaf.R` ✂️ Batch 1

| Function                       | Called From             | Safe to Remove? |
| ------------------------------ | ----------------------- | --------------- |
| `mod_plot_poly_leaf_server_ui` | Never called            | **Yes**         |
| `mod_plot_poly_leaf_server`    | Active core             | No              |
| `mod_plot_leaf_export`         | Active internal         | No              |
| `make_ggmap`                   | Active (map export)     | No              |
| `make_ggmap_2`                 | Commented-out code only | **Yes**         |
| `make_gg_line_map`             | Active (map export)     | No              |
| `make_gg_line_map_2`           | Commented-out code only | **Yes**         |
| `make_spplot`                  | Body commented out      | **Yes**         |
| `make_sp_line_map`             | Body empty              | **Yes**         |

---

### `R/mod_plot_poly_legend.R` — All Active

| Function                      | Called From                   | Safe to Remove? |
| ----------------------------- | ----------------------------- | --------------- |
| `mod_plot_poly_legend_server` | `mod_plot_poly_leaf_server`   | No              |
| `plot_pti_legend`             | `mod_plot_poly_legend_server` | No              |
| `remove_pti_legend`           | `mod_plot_poly_legend_server` | No              |

---

### `R/mod_plot_init_leaf.R` — All Active

| Function                    | Called From                                     | Safe to Remove? |
| --------------------------- | ----------------------------------------------- | --------------- |
| `mod_plot_init_leaf_server` | `mod_plot_pti2_srv`, `mod_dta_explorer2_server` | No              |
| `plot_leaf_line_map2`       | Internal                                        | No              |

---

### `R/plot_pti_helpers.R` — All Active

| Function                    | Called From                                     | Safe to Remove? |
| --------------------------- | ----------------------------------------------- | --------------- |
| `preplot_reshape_wghtd_dta` | `mod_plot_pti2_srv`, `mod_dta_explorer2_server` | No              |
| `get_current_levels`        | Multiple active modules                         | No              |
| `filter_admin_levels`       | `mod_plot_pti2_srv`, `mod_dta_explorer2_server` | No              |
| `add_legend_paras`          | `mod_plot_pti2_srv`, `mod_dta_explorer2_server` | No              |
| `complete_pti_labels`       | `mod_plot_pti2_srv`                             | No              |
| `plot_pti_polygons`         | `mod_plot_poly_leaf_server`                     | No              |
| `clean_pti_polygons`        | `mod_plot_poly_leaf_server`                     | No              |
| `add_pti_poly_controls`     | `mod_plot_poly_leaf_server`                     | No              |
| `clean_pti_poly_controls`   | `mod_plot_poly_leaf_server`                     | No              |
| `check_existing_groups`     | `add_pti_poly_controls`                         | No              |

---

### `R/fct_legend_map_satelites.R` ✂️ Batch 1 (partial) + Batch 3

| Function              | Called From                    | Safe to Remove?            |
| --------------------- | ------------------------------ | -------------------------- |
| `legend_map_satelite` | Active core                    | No                         |
| `recode_val_base`     | Active internal                | No                         |
| `get_pal_lab_fn`      | **Never called**               | **Yes** ✂️ Batch 1          |
| `make_shapes`         | `mod_plot_legend_srv` (legacy) | No → **Yes after Batch 3** |
| `make_labels`         | `mod_plot_legend_srv` (legacy) | No → **Yes after Batch 3** |
| `zeros_after_period`  | **Never called**               | **Yes** ✂️ Batch 1          |

---

### `R/fct_export_data.R` — Entire File Dead ✂️ Batch 1

| Function                   | Called From         | Safe to Remove? |
| -------------------------- | ------------------- | --------------- |
| `rename_one`               | Internal dead chain | **Yes**         |
| `prepare_tibble_to_export` | Internal dead chain | **Yes**         |
| `prepare_DT_export`        | Internal dead chain | **Yes**         |
| `dt_wrap_exported`         | Zero callers        | **Yes**         |

---

### `R/fct_template_reader.R` ✂️ Batch 1 (partial)

| Function                      | Called From             | Safe to Remove? |
| ----------------------------- | ----------------------- | --------------- |
| `fct_template_reader`         | Active core             | No              |
| `fct_convert_weight_to_clean` | Active internal         | No              |
| `fct_convert_clean_to_weight` | Commented-out code only | **Yes**         |

---

### `R/fct_inp_for_exp.R` ✂️ Batch 1 (partial)

| Function                 | Called From         | Safe to Remove? |
| ------------------------ | ------------------- | --------------- |
| `fct_inp_for_exp`        | Active              | No              |
| `fct_internal_wt_to_exp` | Active              | No              |
| `fct_exp_wt_to_internal` | Zero callers (stub) | **Yes**         |

---

### `R/mod_export_pti_data.R` — All Active

| Function                     | Called From                                                                 | Safe to Remove? |
| ---------------------------- | --------------------------------------------------------------------------- | --------------- |
| `mod_export_pti_data_server` | `app_server_sample_pti_vis`, `app_new_pti_server`, `mod_pti_onepage_server` | No              |
| `get_pti_scores_export`      | `prepare_export_data`, `mod_export_pti_data_server`                         | No              |
| `get_pti_weights_export`     | `mod_export_pti_data_server` (**not** unused — architecture doc was wrong)  | No              |

---

### `R/supporting-goe-prep.R` ✂️ Batch 1 (partial)

| Function                 | Called From                          | Safe to Remove?                       |
| ------------------------ | ------------------------------------ | ------------------------------------- |
| `plot_admin_level_ggmap` | Only self-referential                | **Yes**                               |
| `plot_pti_plotslist`     | Zero callers                         | **Yes**                               |
| `gg_admin_list`          | **Active:** metadata PDFs in `inst/` | No — relocate to `plot_pti_helpers.R` |
| `get_intercestions`      | Zero callers                         | **Yes**                               |
| `plot_single_ggmap`      | Only dead code                       | **Yes**                               |

---

### `R/mod_load_shapes.R` ✂️ Batch 1 (partial)

| Function                 | Called From       | Safe to Remove? |
| ------------------------ | ----------------- | --------------- |
| `mod_load_shapes_server` | Dead (no callers) | **Yes**         |
| `mod_get_shape_srv`      | Active            | No              |
| `get_shape`              | Active            | No              |

---

### `R/mod_dwnld_dta.R` ✂️ Batch 1 (partial)

| Function                    | Called From  | Safe to Remove? |
| --------------------------- | ------------ | --------------- |
| `mod_dwnld_dta_btn_ui`      | Zero callers | **Yes**         |
| `mod_dwnld_dta_link_ui`     | Active       | No              |
| `mod_dwnld_dta_xlsx_server` | Active       | No              |
| `mod_dwnld_file_server`     | Active       | No              |

---

### `R/mod_dwnld_local_file.R` ✂️ Batch 1 (partial)

| Function                      | Called From                  | Safe to Remove? |
| ----------------------------- | ---------------------------- | --------------- |
| `mod_dwnld_local_file_ui`     | Zero callers                 | **Yes**         |
| `mod_dwnld_local_file_server` | Active (`mod_map_dwnld_srv`) | No              |

---

### `R/app_config.R` ✂️ Batch 1 (partial)

| Function           | Called From  | Safe to Remove? |
| ------------------ | ------------ | --------------- |
| `app_sys`          | Active       | No              |
| `get_golem_config` | Zero callers | **Yes**         |

---

### `R/app_server.R` ✂️ Batch 2 + Batch 4

| Function                    | Called From                   | Safe to Remove?   |
| --------------------------- | ----------------------------- | ----------------- |
| `app_server`                | `run_pti` only (legacy)       | **Yes** ✂️ Batch 2 |
| `app_server_input_simple`   | `run_dev_map_pti` only (dev)  | **Yes** ✂️ Batch 2 |
| `app_server_sample_pti_vis` | `run_dev_pti_plot` only (dev) | **Yes** ✂️ Batch 2 |
| `app_new_pti_server`        | `run_new_pti` (sample app)    | **Yes** ✂️ Batch 4 |

---

### `R/app_ui.R` ✂️ Batch 2 + Batch 4

| Function                       | Called From                | Safe to Remove?   |
| ------------------------------ | -------------------------- | ----------------- |
| `app_ui`                       | Legacy runners only        | **Yes** ✂️ Batch 2 |
| `app_server_sample_pti_vis_ui` | Dev runner only            | **Yes** ✂️ Batch 2 |
| `app_new_pti_ui`               | `run_new_pti` (sample app) | **Yes** ✂️ Batch 4 |
| `golem_add_external_resources` | Active everywhere          | No                |

---

### `R/run_app.R` ✂️ Batch 2 + Batch 4

| Function                 | Called From                   | Safe to Remove?   |
| ------------------------ | ----------------------------- | ----------------- |
| `run_pti`                | Zero callers                  | **Yes** ✂️ Batch 2 |
| `run_onepage_pti`        | `run_onepage_pti_sample` only | **Yes** ✂️ Batch 2 |
| `run_onepage_pti_sample` | Zero callers                  | **Yes** ✂️ Batch 2 |
| `run_dev_map_pti`        | Dev only (commented)          | **Yes** ✂️ Batch 2 |
| `run_dev_pti_plot`       | Dev only                      | **Yes** ✂️ Batch 2 |
| `run_new_pti`            | `inst/sample_pti/app.R`       | **Yes** ✂️ Batch 4 |

---

### `R/mod_pti_onepage.R` ✂️ Batch 2

| Function                 | Called From                | Safe to Remove? |
| ------------------------ | -------------------------- | --------------- |
| `mod_pti_onepage_ui`     | `run_onepage_pti` (legacy) | **Yes**         |
| `mod_pti_onepage_server` | `run_onepage_pti` (legacy) | **Yes**         |

---

### `R/mod_explrr_onepage.R` ✂️ Batch 6

| Function                    | Called From                  | Safe to Remove? |
| --------------------------- | ---------------------------- | --------------- |
| `mod_explrr_onepage_ui`     | `dev/90-app-examples.R` only | Deprecate first |
| `mod_explrr_onepage_server` | `dev/90-app-examples.R` only | Deprecate first |

---

### Active Files — Fully Verified, No Removals

- `R/launch_pti.R` — `launch_pti()`, `launch_pti_onepage()` — primary public API
- `R/mod_ptipage_core.R` — `mod_ptipage_twocol_ui`, `mod_ptipage_box_ui`, `mod_ptipage_newsrv`
- `R/mod_calc_pti2.R` — `mod_calc_pti2_ui`, `mod_calc_pti2_server`
- `R/mod_pti_comparepage.R` — `mod_pti_comparepage_ui`, `mod_pti_comparepage_newsrv`
- `R/mod_dta_explorer2.R` — all 8 functions active
- `R/mod_infotab.R` — `mod_infotab_server`
- `R/mod_wt_inp.R` — all 14 functions active
- `R/mod_DT_inputs.R` — all 6 functions active
- `R/calc_pti_helpers.R` — all 6 functions active
- `R/calc_pti_expander.R` — all 6 functions active
- `R/fct_validate_metadata.R` — all 3 functions active
- `R/validators.R` — all 2 functions active
- `R/fct_create_new_pti.R` — `create_new_pti` active
- `R/fct_helpers.R` — `add_logo` active
- `R/fct_guide.R` — `guide_launch_pti` active
- `R/mod_drop_inval_adm.R` — all 4 functions active
- `R/mod_first_open_count.R` — active
- `R/mod_tab_open.R` — active
- `R/mod_waiter.R` — all 3 functions active
- `R/mod_fetch_data.R` — active
- `R/mod_pti_map_side_pan.R` — all 7 functions active
- `R/dta_cleaners.R` — `get_indicators_list` active (NOT duplicated — see corrections)
- `R/data.R` — dataset docs active

---

## Corrections Applied to `architecture.md`

1. **`get_pti_weights_export()`**: Was listed as "Legacy: defined but unused". **Corrected** to "Active: called by `mod_export_pti_data_server`".

2. **`get_indicators_list()` duplication claim**: Was described as "defined identically in both `dta_cleaners.R` and `supporting-goe-prep.R`". **Corrected**: it only exists in `dta_cleaners.R`. No duplication.

3. **`make_shapes()` / `make_labels()`**: Listed as "Active: `mod_plot_legend_srv`". More precisely, they are called from the **legacy** `mod_plot_legend_srv` inside `mod_map_pti_leaf.R` and become dead code once that file is cleaned (Batch 3).
