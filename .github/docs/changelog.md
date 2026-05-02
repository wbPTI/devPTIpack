# Changelog — `devPTIpack` Refactoring

> Each entry logs a discrete change made during the architecture redesign.
> Format: date, scope, and one-line summary of what changed and why.

---

## 2026-04-29

| Scope | Change                                                                                                                                           |
| ----- | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| Docs  | Created `.github/docs/arch-00-overview.md` — legacy architecture map                                                                             |
| Docs  | Created `.github/docs/arch-01-cleanup.md` — function-by-function removal plan (6 batches)                                                        |
| Docs  | Created `.github/docs/arch-02-docs.md` — documentation implementation order                                                                      |
| Rules | Created `.claude/rules/roxygen-documentation.md` — roxygen2 templates and required fields                                                        |
| Rules | Created `.claude/CLAUDE.md` — project context and conventions for AI agents                                                                      |
| Docs  | Added "Permanent Functions" audit to `arch-01-cleanup.md` — visibility (39 exported / ~56 internal) and doc quality for all ~95 active functions |
| Docs  | Fixed spelling/formatting in `arch-00-overview.md` workflow steps 1–7                                                                            |

## 2026-05-01

| Scope   | Change                                                                                                                                          |
| ------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| Docs    | Rewrote `PLAN.md` as a thin tracker that defers to `.github/docs/` and the GitHub issues; replaced stale pre-redesign content                   |
| Tooling | Created `.claude/skills/tdd-permanent-fn/SKILL.md` — scaffolds Tier-1 testthat files for permanent functions per arch-03 / arch-02.01           |
| Tooling | Created `.claude/skills/cleanup-batch/SKILL.md` — executes one numbered cleanup batch from arch-01 with pre-flight, tests, check, smoke-test    |
| Tooling | Created `.claude/skills/roxygen-document/SKILL.md` — adds/upgrades roxygen2 per the rules file with template selection and example verification |
| Tooling | Created `.claude/skills/issue-progress-comment/SKILL.md` — drafts confirmation-gated progress comments for #8/#10/#11/#12/#13                   |
| Tooling | Created `.claude/agents/r-package-reviewer.md` sub-agent for read-only diff review of NAMESPACE, roxygen, debug artefacts, and R CMD check      |
| Tooling | Created `.claude/hooks/auto-changelog.sh` Stop hook that auto-drafts changelog rows from the uncommitted diff and is idempotent across reruns   |
| Tooling | Created `.claude/settings.json` to register the auto-changelog Stop hook for both collaborators                                                 |
| Tooling | Updated `.claude/CLAUDE.md` to document the new integration branch (`koichi-arch-redesign`), the skills/sub-agent table, and the hook behaviour |
| Config  | Added `.claude/` and `PLAN.md` to `.Rbuildignore` so they don't ship with the package build                                                     |
| Config  | Added `.claude/settings.local.json` to `.gitignore` so machine-local permissions stay out of the repo                                           |
| Code    | Added `R/run_pti_pipeline.R` exporting `run_pti_pipeline()` — Shiny-free orchestrator for the seven-step calc pipeline (issue #10 deliverable 1) |
| Tests   | Added `tests/testthat/helper-test-data.R` providing bundled data plus deterministic pipeline intermediates for Tier-1 tests                     |
| Tests   | Added `tests/testthat/test-calc-pipeline.R` — first batch of Tier-1 cases for `get_mt`/`get_adm_levels`/`clean_geoms`/`pivot_pti_dta`/`get_weighted_data`/`get_scores_data` plus end-to-end via `run_pti_pipeline()`, pinning the lexicographic-sort and 1-row-NA quirks per arch-03 |
| Config  | Regenerated `NAMESPACE` to export `run_pti_pipeline`; bumped `RoxygenNote` to 7.3.3 as a side-effect of `roxygen2::roxygenise()`                |
| Tests   | Extended `helper-test-data.R` with `test_expanded` and `test_merged` so downstream Tier-1 tests don't each rebuild the expand/merge pipeline    |
| Tests   | Added Level A.7 + A.8 cases to `test-calc-pipeline.R` covering `expand_adm_levels` and `merge_expandedn_adm_levels`; pins the >1-element-match short-circuit and the missing-`year` error per arch-02.01 |
| Tests   | Extended `helper-test-data.R` with `test_extrap` (multi-scheme expand+merge) and `test_aggregated` so A.10/A.11 don't each rebuild the chain  |
| Tests   | Added Level A.9 cases to `test-calc-pipeline.R` covering `agg_pti_scores` — output shape, scheme row-binding, spatial_name lookup, NA propagation under both `na_rm_pti2 = FALSE/TRUE`, all-NA-row zero-sum behaviour, and NA-Pcod row dropping |
| Tests   | Added Level A.10 cases (`label_generic_pti` + `generic_pti_glue`) covering glue-class output, HTML structure, NA -> "No data" rendering, 5-decimal score formatting, and list-shape preservation |
| Tests   | Added Level A.11 cases (`structure_pti_data`) covering output slots, `pti_codes` mapping, wide pivot to `pti_score..pti_ind_N`, row-count-preserving geometry join, the synthetic "No data" filler for missing (Pcod, scheme) combos, and admin_level extraction from shape list names |
| Tests   | Added Level B integration tests — one per adjacent-stage pair (pivot->weight->score->expand->merge->agg->label->structure) so contracts between stages are pinned in addition to the orchestrator-level checks |
| Tooling | Added "PLAN.md Sync (COMPULSORY)" section to `.claude/CLAUDE.md` — every PR that touches tracked work updates `PLAN.md` in the same commit                                                            |
| Tooling | Added a final "Sync `PLAN.md`" step to the `tdd-permanent-fn`, `cleanup-batch`, and `roxygen-document` skills so tick-off is part of the workflow, not an afterthought                                |
| Docs    | Synced `PLAN.md`: marked 1a-1c complete, expanded 1e with a per-file checklist, resolved §3.1 and §3.2 (branch + skills shipped), pruned answered §9 questions, added a §11 "Progress log" of merged PRs |
| Tests   | Added `tests/testthat/test-validators.R` (1e bullet 1) — Tier-1 tests for `validate_single_geom`, `validate_geometries`, `validate_read_shp`, `validate_read_metadata`, `validate_metadata`. Includes a `capture_validator()` helper that mocks the validators' internal `testthat::test_that` calls so their inner expectation failures don't pollute the outer suite; pins the pre-existing empty-pattern `str_detect` bug in `validate_read_shp` (refactor will land via #7) |
| Docs    | `PLAN.md` ticked the validators item under §4.1 1e and logged PR #20 in §11; suite total updated to 450 expectations |
| Tests   | Added `tests/testthat/test-template-reader.R` (1e bullet 2) — covers `fct_template_reader` (sheet structure, fltr_* logical coercion, NULL `weights_clean` when no `weights_table` sheet, var_code filtering), `fct_convert_weight_to_clean` (synthetic `wsN..*` columns), `get_shape` (all three branches), and `create_new_pti` (template scaffolding into a tempdir with `open = FALSE` and stdout silenced) |
| Docs    | `PLAN.md` ticked template-reader under §4.1 1e and logged PR #21 in §11; suite total updated to 478 expectations |
| Tests   | Added `tests/testthat/test-indicators-list.R` (1e bullet 3) — covers `get_indicators_list` happy path, column schema, var_code uniqueness, nested `admin_levels_years` shape, the `fltr_exclude_pti` filter, alternative `fltr_var` arg, and a PINNED error on a non-existent fltr_var |
| Tests   | Removed legacy placeholder `tests/testthat/test-get_indicators_list.R` (a 6-line tautology `expect_gt(2*2, 3)`) — superseded by the new `test-indicators-list.R` |
| Docs    | `PLAN.md` ticked indicators-list under §4.1 1e and logged PR #22 in §11; suite total updated to 507 expectations |
| Tests   | Added `tests/testthat/test-drop-inval-adm.R` (1e bullet 4) — covers `get_vars_un_avbil` (output shape + explicit admin_levels arg), `get_min_admin_wght` (zero-weight / fully-available / weighting-an-unavailable-var / multi-scheme), and `drop_inval_adm` (empty drops, scheme+admin match, drop-everything) |
| Docs    | `PLAN.md` ticked drop-inval-adm under §4.1 1e and logged PR #23 in §11; suite total updated to 523 expectations |
| Tests   | Added `tests/testthat/test-legend-palette.R` (1e bullet 5) — output schema, factor-vs-continuous-branch behaviour, single-unique-value, binary, NA -> "No data", `legend_revert_colours` flip, plus full `recode_val_base` coverage including the single-break patch and NA->no_data_label semantics. Pins arch-03 §1.5 spec corrections (the input `c(1,5,3,7,2)` takes the integer branch, not the continuous branch the doc assumed) |
| Docs    | `PLAN.md` ticked legend-palette under §4.1 1e and logged PR #24 in §11; suite total updated to 542 expectations |
| Tests   | Added `tests/testthat/test-plot-helpers.R` (1e bullet 6) — covers `preplot_reshape_wghtd_dta`, `get_current_levels`, `filter_admin_levels`, `add_legend_paras`, `complete_pti_labels`, `check_existing_groups`. Pinned 2 newly-discovered bugs (`complete_pti_labels` no-op missing assignment; `check_existing_groups` errors on empty `old_grps`) and 1 spec asymmetry (`filter_admin_levels` enters by name, filters by value) |
| Docs    | `PLAN.md` ticked plot-helpers under §4.1 1e, logged PR #25, added a new §12 "Discovered bugs" table consolidating bugs surfaced while writing Tier-1 tests; suite total updated to 615 expectations |
| Tests   | Added `tests/testthat/test-export.R` (1e bullet 7) — covers `get_pti_scores_export` (per-admin tibble shape, ' PTI Scores' suffix, geometry/label/area dropped, per-scheme Score+Priority columns), `get_pti_weights_export` (row count, var_code drop + Variable/Pillar rename, per-scheme Weights columns), `fct_internal_wt_to_exp` (long shape, row count = vars*schemes, empty scheme tibble -> 0 rows), `fct_inp_for_exp` (general/admin/metadata structure, Pcod columns dropped, indicators renamed by var_name, fltr_* dropped from metadata). Pinned a new bug: `fct_internal_wt_to_exp(list())` errors at the downstream `left_join` because `imap_dfr(list())` produces a 0x0 tibble |
| Docs    | `PLAN.md` ticked export under §4.1 1e, logged PR #26, added the empty-list-errors bug to §12; suite total updated to 673 expectations |
| Tests   | Added `tests/testthat/test-explorer-helpers.R` (1e bullet 8) — covers `reshaped_explorer_dta` (per-admin tibble shape, generic pti_* columns, glue-class HTML labels, var_name resolution), `get_var_choices` (nested-by-pillar list shape, var_name -> var_code mapping), `filter_var_explorer` (value match, names(vars) match, no match, empty input). Pinned a new bug: `get_var_choices(empty_tibble)` errors with 'attempt to set an attribute on NULL' because the fallback branch tries to name a NULL output |
| Docs    | `PLAN.md` ticked explorer-helpers under §4.1 1e, logged PR #27, added the new `get_var_choices` empty bug to §12; suite total updated to 700 expectations |
| Tests   | Added `tests/testthat/test-map-render.R` (1e bullet 9) — class-assertion coverage for the non-interactive renderers: `make_ggmap` (default + no-shp_dta + show_interval branches), `make_gg_line_map`, `plot_leaf_line_map2` (NULL show_adm_levels and explicit subset). These return ggplot/leaflet objects whose internal state isn't easily assertable in Tier 1; deeper coverage stays in Tier 3 |
| Docs    | `PLAN.md` ticked map-render under §4.1 1e, logged PR #28; suite total updated to 708 expectations |
| Tests   | Added `tests/testthat/test-dt-construction.R` (1e bullet 10) — covers `prep_input_data` (column schema, pillar+variable row count, numericInput HTML in variable rows), `make_vis_targets_for_dt` (columnDefs+colnames shape, var_name+ui visible vs invisible mapping), `make_input_DT` (returns dt_out + nested_dta list, dt_out is a DT htmlwidget). Pinned arch-03 §1.11 spec correction: row count is `#pillars + #indicators`, not just `#indicators` |
| Docs    | `PLAN.md` ticked dt-construction under §4.1 1e and the outer 1e box (1e complete!), logged PR #29, expanded the Phase 1 ASCII tree to surface the next sub-items (1f CI, 1g Tier 2); suite total updated to 725 expectations |

## 2026-05-02

| Scope   | Change                                                                                                                                          |
| ------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| Config  | Added `.github/workflows/tests.yaml` (1f) — runs `testthat::test_local()` on push/PR to main and koichi-arch-redesign via r-lib/actions v2. Local suite ~30s, well under arch-03's 2-min budget |
| Docs    | `PLAN.md` ticked 1f, updated the Phase 1 tree to mark 1g as next, logged PR #30                                                                  |
| Tests   | Added `tests/testthat/test-mod-calc-pti2.R` (1g first module) — Tier-2 `shiny::testServer` coverage for `mod_calc_pti2_server`: happy path (admin-keyed slots), all-zero weights -> 0 scores, single-indicator weight list, identical-input dedup, weight-change recompute. Notes the multi-admin-only constraint pinned in PLAN.md §12 (single-admin indicators trip the expand_adm_levels NULL short-circuit) |
| Docs    | `PLAN.md` expanded 1g into a 7-bullet checklist (one per module), ticked the first module, logged PR #31; suite total 725 -> 792 expectations |
| Tests   | Added `tests/testthat/test-mod-DT-inputs.R` (1g second module) — Tier-2 `shiny::testServer` coverage for `mod_DT_inputs_server`: initial-render shape (all `var_code` rows, weights `NA_real_`), direct numericInput change → `current_values()`, all-zero + all-one button paths (round-trip simulated via `setInputs` because `updateNumericInput` does not propagate back to `input[[id]]` in `MockShinySession`), `update_dta()` push, and the 500ms throttle window. 6 `test_that` blocks / 13 expectations; full local suite passes |
| Docs    | `PLAN.md` ticked `mod_DT_inputs_server` under §4.1 1g (2/7 modules covered), bumped Phase-1 ASCII tree to "in progress (2/7)", logged PR #33, and added a §11 footnote explaining the suite-total recount: prior `sum(res$nb)` totals (725, 792) over-counted internal `testthat::test_that()` calls inside `validate_geometries()` / `validate_single_geom()`; the summary-reporter `PASS` figure (now 612) is the source of truth going forward |
| Tests   | Added `tests/testthat/test-mod-drop-inval-adm.R` (1g third module) — Tier-2 `shiny::testServer` coverage for `mod_drop_inval_adm` per arch-03 §2.3: no-drops happy path (output identical to input), indicator missing at admin1 → admin1 dropped, `shiny::showNotification` fires on drop (intercepted via `testthat::local_mocked_bindings(.package = "shiny")`), notification suppressed when nothing is dropped, and weight-0 indicator does not trigger a drop. 5 `test_that` blocks / 10 expectations; uses synthetic minimal inputs (one indicator, two admin slots) so each test surgically controls whether a drop is required |
| Docs    | `PLAN.md` ticked `mod_drop_inval_adm` under §4.1 1g (3/7 modules covered), bumped Phase-1 ASCII tree to "in progress (3/7)", added §12 entry pinning the `get_vars_un_avbil` fill-direction asymmetry (admin1-only indicators are treated as available at admin2 because the lag-fill propagates DOWN from earlier-sorted admin levels — also flagged the `is.na(value & !any_larger)` paren oddity); logged this PR; suite total updated to 622 PASS |
| Config  | Added `.claude/scheduled_tasks.lock` to `.gitignore` — runtime artefact created by `ScheduleWakeup`; should never enter git or the changelog (it had auto-drafted a noisy row in PR #34's range) |
| Docs    | Replaced two `TBD` placeholders in `PLAN.md` (§4.1 1g `mod_drop_inval_adm` bullet + §11 progress-log row) with `#34` now that the PR is merged |
