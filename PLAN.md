# devPTIpack — Working Plan

> **Living document.** Tracks current phase, immediate next actions, and shared
> workflow conventions. Every PR that touches tracked work updates this file in
> the same commit (see [`.claude/CLAUDE.md`](.claude/CLAUDE.md) §"PLAN.md Sync").
> The authoritative architecture lives under [`.github/docs/`](.github/docs/) and
> the master GitHub tracker is [`#9`](https://github.com/worldbank/devPTIpack/issues/9).
> When this file disagrees with `.github/docs/`, `.github/docs/` wins.

---

## 1. Where we are

| Concern | Source of truth |
|---|---|
| Architecture overview & redesign workflow | [`.github/docs/arch-00-overview.md`](.github/docs/arch-00-overview.md) |
| Function-by-function cleanup audit | [`.github/docs/arch-01-cleanup.md`](.github/docs/arch-01-cleanup.md) |
| Roxygen2 standards | [`.claude/rules/roxygen-documentation.md`](.claude/rules/roxygen-documentation.md) |
| Documentation implementation order | [`.github/docs/arch-02-docs.md`](.github/docs/arch-02-docs.md) |
| Calculation pipeline test spec (~71 cases) | [`.github/docs/arch-02.01-testing-calc-pipeline.md`](.github/docs/arch-02.01-testing-calc-pipeline.md) |
| Three-tier testing strategy + per-fn test map | [`.github/docs/arch-03-testing.md`](.github/docs/arch-03-testing.md) |
| Workspace, vignettes & pkgdown plan | [`.github/docs/arch-04-workspace.md`](.github/docs/arch-04-workspace.md) |
| Hex (H3) ingestion design | [`.github/docs/arch-05-hex-ingestion.md`](.github/docs/arch-05-hex-ingestion.md) |
| Per-change log (compulsory) | [`.github/docs/changelog.md`](.github/docs/changelog.md) |
| Project conventions for AI agents | [`.claude/CLAUDE.md`](.claude/CLAUDE.md) |

GitHub issues map:
- [#9](https://github.com/worldbank/devPTIpack/issues/9) — master tracker
- [#10](https://github.com/worldbank/devPTIpack/issues/10) — testing framework + `run_pti_pipeline()` orchestrator
- [#8](https://github.com/worldbank/devPTIpack/issues/8) — legacy cleanup (subsumes #2/#3/#4)
- [#11](https://github.com/worldbank/devPTIpack/issues/11) — roxygen2 documentation
- [#12](https://github.com/worldbank/devPTIpack/issues/12) — workspace, vignettes, pkgdown site
- [#13](https://github.com/worldbank/devPTIpack/issues/13) — hex ingestion pipeline (independent)
- [#5](https://github.com/worldbank/devPTIpack/issues/5), [#7](https://github.com/worldbank/devPTIpack/issues/7), [#6](https://github.com/worldbank/devPTIpack/issues/6), [#1](https://github.com/worldbank/devPTIpack/issues/1) — relate to upstream/global DB and validation; partially superseded by #9 sub-issues, see arch-00 § "Relationship to Pre-Existing Issues"

---

## 2. Execution order (TDD-first)

```
Phase 0  Setup & shared workflow ──────────────────────────────┐  ✓ done
                                                               │
Phase 1  Test baseline for permanent functions     (#10)       │  ✓ done
   ├─ 1a   `run_pti_pipeline()` orchestrator             ✓ #15 │
   ├─ 1b   Tier-1 calc-pipeline tests                    ✓ #15-#18
   ├─ 1e   Tier-1 tests for the remaining files         ✓ #20-#29
   ├─ 1f   CI guard via .github/workflows/tests.yaml     ✓ #30
   └─ 1g   Tier 2 (shiny::testServer for 7 modules)            ✓ all 7 covered
            ▼                                                  │
Phase 2  Cleanup legacy code in batches            (#8)        │ Tests
   ├─ Batch 1  Dead files & functions                          │ guard
   ├─ Batch 2  Legacy runners + app_server/app_ui              │ each
   ├─ Batch 3  Legacy map server (~1200 lines)                 │ batch
   ├─ Batch 4  Migrate sample app to launch_pti()              │
   └─ Batch 5  Remove mod_weights.R legacy                     │
            ▼                                                  │
Phase 3  Roxygen2 docs for all permanent fns       (#11)       │
            ▼                                                  │
Phase 4  Vignettes + pkgdown deploy                (#12)       │
            ▼                                                  │
Phase 5  Hex ingestion pipeline                    (#13)  ← independent; can start anytime on a parallel branch
```

**Why this order.** Tests pin behaviour before we delete or refactor. Each cleanup
batch then re-runs the suite — green = no regression. Docs come once the API
stabilises (no point documenting code that's about to be deleted). Vignettes &
pkgdown depend on stable, documented exports.

**Hybrid (in effect):** when writing a Tier-1 test for a permanent function, we
also draft its roxygen at the same time. Phase 3 then sweeps only what's missed.

---

## 3. Phase 0 — Setup & shared workflow ✓

### 3.1 Branch strategy (resolved)

- Integration branch: **`koichi-arch-redesign`** off `main`.
- Sub-branches per phase / batch (e.g. `tests/calc-pipeline-baseline`,
  `cleanup/batch-1`) PR'd into `koichi-arch-redesign`.
- Once Phase 1–4 are complete, `koichi-arch-redesign` PRs into `main`.
- Each PR keeps `R CMD check` green and updates the changelog and `PLAN.md`.

### 3.2 Tooling — committed under `.claude/` ✓

| Tool | Type | Status |
|---|---|---|
| [`tdd-permanent-fn`](.claude/skills/tdd-permanent-fn/SKILL.md) | skill | ✓ committed |
| [`cleanup-batch`](.claude/skills/cleanup-batch/SKILL.md) | skill | ✓ committed |
| [`roxygen-document`](.claude/skills/roxygen-document/SKILL.md) | skill | ✓ committed |
| [`issue-progress-comment`](.claude/skills/issue-progress-comment/SKILL.md) | skill | ✓ committed |
| [`r-package-reviewer`](.claude/agents/r-package-reviewer.md) | sub-agent | ✓ committed |
| [`auto-changelog.sh`](.claude/hooks/auto-changelog.sh) | Stop hook | ✓ committed (auto-drafts changelog rows from the diff) |

### 3.3 Working agreements

- **Changelog is mandatory.** Hook drafts; humans refine.
- **PLAN.md sync is mandatory.** Skills include the step; CLAUDE.md states the rule.
- **Examples in roxygen** must use only `ukr_shp` / `ukr_mtdt_full`.
- **Don't touch legacy code** flagged for deletion in arch-01 unless executing a cleanup batch.
- **`R CMD check` must stay green** between phases.

---

## 4. Phase 1 — Tests (#10)

> **Critical scoping rule.** Write tests *only* for the permanent functions
> listed in arch-01 § "Permanent Functions". Do not test legacy functions
> scheduled for deletion in arch-01 batches.

### 4.1 Concrete next actions

- [x] **1a — Orchestrator.** `run_pti_pipeline()` exported, documented, used as
      the test seam (PR #15).
- [x] **1b — Helper + initial fixtures.** `tests/testthat/helper-test-data.R`
      with deterministic intermediates (PR #15, extended in #16, #17).
- [x] **1c — Tier 1 calc pipeline.** [`test-calc-pipeline.R`](tests/testthat/test-calc-pipeline.R)
      now covers the full arch-02.01 spec — Level A.1–A.11, Level B integration
      (7 pairs), Level C end-to-end (PRs #15→#16→#17→#18). Suite total: 398
      expectations passing. Synthetic `.rds` fixtures (`fx_shp_*`) deferred —
      added when a test cannot be expressed inline.
- [x] **1e — Tier 1 remaining files** (arch-03 §1.2–1.11): **all 10 files done**, suite total 725 expectations.
      - [x] [`test-validators.R`](tests/testthat/test-validators.R) — `validate_geometries`, `validate_single_geom`, `validate_metadata`, `validate_read_shp`, `validate_read_metadata` (PR #20; 12 blocks / 52 expectations; pinned the empty-pattern str_detect bug in `validate_read_shp`)
      - [x] [`test-template-reader.R`](tests/testthat/test-template-reader.R) — `fct_template_reader`, `fct_convert_weight_to_clean`, `get_shape`, `create_new_pti` (PR #21; 11 blocks / 28 expectations)
      - [x] [`test-indicators-list.R`](tests/testthat/test-indicators-list.R) — `get_indicators_list` (PR #22; 7 blocks / 29 expectations; replaces the placeholder `test-get_indicators_list.R`)
      - [x] [`test-legend-palette.R`](tests/testthat/test-legend-palette.R) — `legend_map_satelite`, `recode_val_base` (PR #24; 12 blocks / 19 expectations; pinned arch-03 §1.5 spec corrections — integer vs continuous branch behaviour)
      - [x] [`test-plot-helpers.R`](tests/testthat/test-plot-helpers.R) — `preplot_reshape_wghtd_dta`, `get_current_levels`, `filter_admin_levels`, `add_legend_paras`, `complete_pti_labels`, `check_existing_groups` (PR #25; 18 blocks / 73 expectations; pinned 2 newly-discovered bugs and 1 spec asymmetry — see §12 Discovered bugs). `plot_pti_polygons`/`clean_pti_polygons`/`add_pti_poly_controls`/`clean_pti_poly_controls` are leaflet-rendering helpers and stay in Tier 3 (manual)
      - [x] [`test-drop-inval-adm.R`](tests/testthat/test-drop-inval-adm.R) — `get_vars_un_avbil`, `get_min_admin_wght`, `drop_inval_adm` (PR #23; 10 blocks / 16 expectations)
      - [x] [`test-export.R`](tests/testthat/test-export.R) — `get_pti_scores_export`, `get_pti_weights_export`, `fct_inp_for_exp`, `fct_internal_wt_to_exp` (PR #26; 14 blocks / 58 expectations; pinned `fct_internal_wt_to_exp(list())` left-join error)
      - [x] [`test-explorer-helpers.R`](tests/testthat/test-explorer-helpers.R) — `reshaped_explorer_dta`, `get_var_choices`, `filter_var_explorer` (PR #27; 11 blocks / 27 expectations; pinned `get_var_choices` empty-tibble error and the actual nested-by-pillar return shape)
      - [x] [`test-map-render.R`](tests/testthat/test-map-render.R) — `make_ggmap`, `make_gg_line_map`, `plot_leaf_line_map2` (PR #28; 7 blocks / 8 expectations; class-only assertions for ggplot/leaflet renderers)
      - [x] [`test-dt-construction.R`](tests/testthat/test-dt-construction.R) — `prep_input_data`, `make_vis_targets_for_dt`, `make_input_DT` (PR #29; 8 blocks / 17 expectations; pinned arch-03 §1.11 spec correction — pillar rows interleaved with variable rows)
- [x] **1f — CI guard.** [`.github/workflows/tests.yaml`](.github/workflows/tests.yaml)
      runs `testthat::test_local()` on every push / PR to `main` /
      `koichi-arch-redesign`. Local suite finishes in ~30s, well under
      arch-03's 2-min budget. PR #30.
- [x] **1g — Tier 2 (after Tier 1 green).** Module-server tests via
      `shiny::testServer` (arch-03 §2). 7 modules — one PR each:
      - [x] `mod_calc_pti2_server` — happy path, all-zero, single-indicator,
            identical-input dedup, weight-change recompute (PR #31; 5 blocks /
            67 expectations)
      - [x] `mod_DT_inputs_server` — initial render (NA weights, all
            var_codes), direct numericInput → current_values, all-zero +
            all-one button paths (round-trip simulated via `setInputs`
            because `updateNumericInput` does not round-trip in
            `MockShinySession`), `update_dta()` push, 500ms throttle
            window (PR #33; 6 blocks / 13 expectations)
      - [x] `mod_drop_inval_adm` — no-drops happy path, indicator
            missing at admin1 → admin1 dropped, `showNotification` fires
            on drop (mocked via `local_mocked_bindings`), notification
            suppressed when no drops, weight-0 indicator does not trigger
            a drop. Pinned the asymmetric `get_vars_un_avbil` fill
            direction (PLAN.md §12 new entry). PR #34; 5 blocks / 10
            expectations
      - [x] `mod_get_admin_levels_srv` — no-filter passthrough,
            `default_adm_level` matches admin key / display value /
            "All" (case-insensitive) / nothing (→ last element),
            `show_adm_levels` filtering by name and value, single
            non-matching `show_adm_levels` (→ last element),
            `default_adm_level` precedence over `show_adm_levels`,
            update on `cur_levels` reactive change. PR #36; 11 blocks /
            12 expectations
      - [x] `mod_fltr_sel_var2_srv` — initial `updatePickerInput`
            with display-name choices, selection → debounced filter
            of `preplot_dta`, switching selection swaps surviving
            slots, `first_open(TRUE)` → auto-select first display
            name, `add_selected()` override (single-var-per-pillar
            happy path) + pinned multi-var-pillar `purrr::map_lgl` bug
            (PLAN.md §12 new entry). PR #37; 7 blocks / 18 expectations
      - [x] `mod_wt_save_newsrv` — save-new (empty store), overwrite,
            save-alongside-existing; button-UI labels and disabled
            state for "Save and plot new PTI" / "Provide a name" /
            "Modify weights" / "No changes to save" / "Save changes
            and plot PTI". Internal `current_btn_ui()` reactiveVal
            tapped directly from `expr` scope (renderUI side effect).
            Delete logic lives in `mod_wt_delete_newsrv`, out of
            scope for arch-03 §2.6. PR #38; 8 blocks / 17 expectations
      - [x] `mod_export_pti_data_server` — returned named list shape
            (`Country` + `Weighting schemes` + per-admin scores),
            `Country` slot mirrors `weights_dta()$general`, weights
            tibble has one column per scheme, per-admin score slots
            are reversed (finer admin first), `req()` halts when
            `plotted_dta()` is NULL. Synthetic minimal inputs (the
            wrapper does no calc work itself). PR #39; 6 blocks /
            13 expectations

### 4.2 Tier 3 timing

`shinytest2` automation deferred until **after** pkgdown deploys (Phase 4).
Tier 3 stays manual until then (arch-03 §3.2 checklist).

---

## 5. Phase 2 — Cleanup (#8)

Drive each batch from arch-01 § "Removal Batches" using the
[`cleanup-batch`](.claude/skills/cleanup-batch/SKILL.md) skill. Discipline:

1. Branch off integration: `cleanup/batch-N`.
2. Delete files/functions per the batch table.
3. Run `devtools::document()` → `devtools::test()` → `devtools::check()`.
4. Smoke-test: `launch_pti(shp_dta = ukr_shp, inp_dta = ukr_mtdt_full)` opens.
5. Append changelog row(s) and update `PLAN.md`.
6. Open PR; merge once green.

Batch order is fixed by dependencies (arch-01 §"Cleanup Strategy"). Don't reorder
without re-checking caller graphs.

- [x] Batch 1 — dead files & functions (PR TBD): 4 whole files
      (`fct_export_data.R`, `mod_explorer.R`, `mod_info_page.R`,
      `mod_calc_pti.R`), ~20 individual functions, 1 orphan `.rda`,
      and 3 NAMESPACE exports (`extrapo_one_weight`,
      `fct_exp_wt_to_internal`, `mod_weights_html_ui`). Plus a
      pre-existing DESCRIPTION fix (Authors@R needed comma-separated
      `c(...)` wrapping; added explicit Author/Maintainer fields)
      that was needed to make `R CMD check` produce useful output.
- [x] Batch 2 — legacy runners + app_server/app_ui (PR TBD): deleted
      whole file `R/mod_pti_onepage.R` (both UI + server) and 10
      individual functions across `R/run_app.R` (`run_pti`,
      `run_onepage_pti`, `run_onepage_pti_sample`, `run_dev_map_pti`,
      `run_dev_pti_plot`), `R/app_server.R` (`app_server`,
      `app_server_input_simple`, `app_server_sample_pti_vis`), and
      `R/app_ui.R` (`app_ui`, `app_server_sample_pti_vis_ui`). Kept
      `run_new_pti` / `app_new_pti_server` / `app_new_pti_ui` /
      `golem_add_external_resources` (Batch 4 territory). Removed 7
      NAMESPACE exports via `roxygen2::roxygenise()`. Suite stays at
      682 PASS — no regressions.
- [ ] Batch 3 — legacy map server (~1200 lines)
- [ ] Batch 4 — migrate sample app to `launch_pti()`
- [ ] Batch 5 — remove `mod_weights.R` legacy

---

## 6. Phase 3 — Documentation (#11)

Follow arch-02-docs § "Implementation Order" and use the
[`roxygen-document`](.claude/skills/roxygen-document/SKILL.md) skill.

- [ ] Phase 3.1 (data) → 3.6 (remaining modules) as listed in arch-02-docs.
- [ ] Any function whose body changed in Phase 2 cleanup needs its docs reviewed.
- [ ] After each file's docs land, run `devtools::document()` and confirm
      `R CMD check` produces no doc-related notes.

---

## 7. Phase 4 — Workspace, vignettes & pkgdown (#12)

Per arch-04. Concrete cuts:

- [ ] Delete `dev/` (git history preserves it).
- [ ] Replace `vignettes/dataprep.Rmd` stub with the grouped articles in
      arch-04 §"Vignette Groups". **Highest-value first:**
      `calculation-pipeline.Rmd`, `data-preparation.Rmd`.
- [ ] Source for `pti-overview.Rmd` / `project-history.Rmd`: the
      [WB OpenKnowledge PTI article](https://openknowledge.worldbank.org/server/api/core/bitstreams/1fa677a7-7c1c-5a39-8705-511a7038e3a2/content).
- [ ] Update `_pkgdown.yml` to add grouped article navigation per arch-04
      §"_pkgdown.yml Updates".
- [ ] Add a GitHub Action for pkgdown build & GitHub Pages deploy.
- [ ] Confirm `R CMD check` builds vignettes cleanly.
- [ ] **After this phase:** add `shinytest2` automation for Tier 3.

---

## 8. Phase 5 — Hex ingestion (#13, independent)

Independent track. Five new exported functions, all developer-facing, all
pre-deployment. Spec: arch-05.

**Decision:** deferred until Phases 1–4 are complete. The calculation pipeline
is geometry-agnostic so this is non-blocking.

---

## 9. Open questions for the team

*(All Phase 0 questions resolved — see §3. Below are the still-open ones.)*

1. **Tier 2 module-server tests.** Cover all 7 modules listed in arch-03 §2.1–2.7
   in one PR, or one per PR? Recommend one per PR for review velocity.
2. **CI runtime budget.** arch-03 sets <2 min for `devtools::test()`. Phase 1c
   currently runs in well under that locally; confirm post-cleanup once GitHub
   Actions runner timing is known.

---

## 10. Definition of done (end-state, all phases)

Lifted from arch-00 §"End-State Goals":

- [ ] Only the modern pipeline remains. Public entry points: `launch_pti()`,
      `launch_pti_onepage()`, `create_new_pti()`.
- [ ] All ~95 permanent functions have complete roxygen2 docs.
- [ ] Test coverage > 80%; Tier 1 + Tier 2 automated in CI.
- [ ] Package website deployed on GitHub Pages with grouped vignettes.
- [ ] Hex ingestion pipeline (#13) lands on its own milestone.
- [ ] `R CMD check` passes with 0 warnings, 0 errors, 0 notes.

---

## 11. Progress log

> One line per merged PR — what changed and which PLAN item it advanced.

| PR | Date | Phase item | Outcome |
|---|---|---|---|
| [#15](https://github.com/worldbank/devPTIpack/pull/15) | 2026-04-30 | 1a, 1b, 1c (start) | `run_pti_pipeline()` orchestrator + helper + Level A.1–A.6 + Level C |
| [#16](https://github.com/worldbank/devPTIpack/pull/16) | 2026-05-01 | 1c | Level A.7 (`expand_adm_levels`) + A.8 (`merge_expandedn_adm_levels`) |
| [#17](https://github.com/worldbank/devPTIpack/pull/17) | 2026-05-01 | 1c | Level A.9 (`agg_pti_scores`) |
| [#18](https://github.com/worldbank/devPTIpack/pull/18) | 2026-05-01 | 1c | Level A.10 (`label_generic_pti`) + A.11 (`structure_pti_data`) + Level B integration |
| [#19](https://github.com/worldbank/devPTIpack/pull/19) | 2026-05-01 | tooling | Operationalize PLAN.md sync (CLAUDE.md rule + skill steps); sync PLAN.md to current state |
| [#20](https://github.com/worldbank/devPTIpack/pull/20) | 2026-05-01 | 1e (validators) | Tier-1 tests for `validate_*` functions; pinned the empty-pattern bug in `validate_read_shp` |
| [#21](https://github.com/worldbank/devPTIpack/pull/21) | 2026-05-01 | 1e (template-reader) | Tier-1 tests for `fct_template_reader`, `fct_convert_weight_to_clean`, `get_shape`, `create_new_pti` |
| [#22](https://github.com/worldbank/devPTIpack/pull/22) | 2026-05-01 | 1e (indicators-list) | Tier-1 tests for `get_indicators_list`; deleted the placeholder `test-get_indicators_list.R` |
| [#23](https://github.com/worldbank/devPTIpack/pull/23) | 2026-05-01 | 1e (drop-inval-adm) | Tier-1 tests for `get_vars_un_avbil`, `get_min_admin_wght`, `drop_inval_adm` |
| [#24](https://github.com/worldbank/devPTIpack/pull/24) | 2026-05-01 | 1e (legend-palette) | Tier-1 tests for `legend_map_satelite`, `recode_val_base`; pinned the integer/continuous branch split |
| [#25](https://github.com/worldbank/devPTIpack/pull/25) | 2026-05-01 | 1e (plot-helpers) | Tier-1 tests for `preplot_reshape_wghtd_dta`/`get_current_levels`/`filter_admin_levels`/`add_legend_paras`/`complete_pti_labels`/`check_existing_groups`; pinned 2 bugs (see §12) |
| [#26](https://github.com/worldbank/devPTIpack/pull/26) | 2026-05-01 | 1e (export) | Tier-1 tests for `get_pti_scores_export`, `get_pti_weights_export`, `fct_inp_for_exp`, `fct_internal_wt_to_exp`; pinned `fct_internal_wt_to_exp(list())` failure (see §12) |
| [#27](https://github.com/worldbank/devPTIpack/pull/27) | 2026-05-01 | 1e (explorer-helpers) | Tier-1 tests for `reshaped_explorer_dta`, `get_var_choices`, `filter_var_explorer`; pinned `get_var_choices(empty)` NULL-attribute error (see §12) |
| [#28](https://github.com/worldbank/devPTIpack/pull/28) | 2026-05-01 | 1e (map-render) | Tier-1 class-assertion tests for `make_ggmap`, `make_gg_line_map`, `plot_leaf_line_map2` |
| [#29](https://github.com/worldbank/devPTIpack/pull/29) | 2026-05-01 | **1e complete** (dt-construction) | Tier-1 tests for `prep_input_data`, `make_vis_targets_for_dt`, `make_input_DT`; pinned arch-03 §1.11 spec correction — pillar rows interleaved with variable rows |
| [#30](https://github.com/worldbank/devPTIpack/pull/30) | 2026-05-02 | 1f (CI guard) | `.github/workflows/tests.yaml` runs `testthat::test_local()` on push / PR; local suite ~30s, well under the 2-min budget |
| [#31](https://github.com/worldbank/devPTIpack/pull/31) | 2026-05-02 | 1g (mod_calc_pti2) | Tier-2 `shiny::testServer` tests for `mod_calc_pti2_server` — happy path, all-zero, single-indicator, dedup, weight-change recompute |
| [#33](https://github.com/worldbank/devPTIpack/pull/33) | 2026-05-02 | 1g (mod_DT_inputs) | Tier-2 `shiny::testServer` tests for `mod_DT_inputs_server` — initial-render NA weights, direct input → current_values, all-zero + all-one button paths, `update_dta()` push, 500ms throttle window. Recounted §11 suite total via summary-reporter `PASS` |
| [#34](https://github.com/worldbank/devPTIpack/pull/34) | 2026-05-02 | 1g (mod_drop_inval_adm) | Tier-2 `shiny::testServer` tests for `mod_drop_inval_adm` — no-drops happy path, indicator missing at admin1 → admin1 dropped, `showNotification` fires on drop (mocked via `local_mocked_bindings`), suppressed when no drops, weight-0 indicator does not trigger a drop. Pinned `get_vars_un_avbil` fill-direction asymmetry as §12 entry |
| [#35](https://github.com/worldbank/devPTIpack/pull/35) | 2026-05-02 | chore (PLAN/gitignore) | Replace two PR-#34 `TBD` placeholders in PLAN.md with `#34`; gitignore `.claude/scheduled_tasks.lock` so the auto-changelog hook stops drafting noise rows for it |
| [#36](https://github.com/worldbank/devPTIpack/pull/36) | 2026-05-02 | 1g (mod_get_admin_levels_srv) | Tier-2 `shiny::testServer` tests for `mod_get_admin_levels_srv` — no-filter passthrough, `default_adm_level` (name / value / "All" case-insensitive / non-match → last), `show_adm_levels` filtering, single non-match → last, `default_adm_level` precedence over `show_adm_levels`, update on `cur_levels` change |
| [#37](https://github.com/worldbank/devPTIpack/pull/37) | 2026-05-02 | 1g (mod_fltr_sel_var2_srv) | Tier-2 `shiny::testServer` tests for `mod_fltr_sel_var2_srv` — initial `updatePickerInput` with display-name choices (mocked via `local_mocked_bindings(.package = "shinyWidgets")`), selection → debounced filter of `preplot_dta`, switching selection swaps surviving slots, `first_open(TRUE)` → auto-select first display name, `add_selected()` override (single-var-per-pillar happy path) + pinned multi-var-pillar `purrr::map_lgl` bug as new §12 entry |
| [#38](https://github.com/worldbank/devPTIpack/pull/38) | 2026-05-02 | 1g (mod_wt_save_newsrv) | Tier-2 `shiny::testServer` tests for `mod_wt_save_newsrv` — save-new (empty store), overwrite, save-alongside-existing; button-UI states for "Save and plot new PTI" / "Provide a name" / "Modify weights" / "No changes to save" / "Save changes and plot PTI" via internal `current_btn_ui()` reactiveVal. Delete logic lives in `mod_wt_delete_newsrv`, out of scope |
| [#39](https://github.com/worldbank/devPTIpack/pull/39) | 2026-05-02 | **1g complete** (mod_export_pti_data_server) | Tier-2 `shiny::testServer` tests for `mod_export_pti_data_server` — returned named list (`Country` + `Weighting schemes` + per-admin scores), `Country` slot mirrors `weights_dta()$general`, weights tibble has one column per scheme, per-admin score slots are reversed (finer admin first), `req()` halts on NULL `plotted_dta`. **Closes 1g — all 7 Tier-2 modules covered.** |
| [#40](https://github.com/worldbank/devPTIpack/pull/40)  | 2026-05-02 | **Phase 2 starts** (Batch 1) | arch-01 Batch 1 — delete 4 whole files (`fct_export_data.R`, `mod_explorer.R`, `mod_info_page.R`, `mod_calc_pti.R`), ~20 dead functions, 1 orphan `.rda`, 3 NAMESPACE exports; clean up commented-out `do.call(make_*_2/spplot/sp_line_map, ...)` lines; regenerate NAMESPACE/Rd via `roxygen2::roxygenise()`. Pre-existing DESCRIPTION fix (Authors@R `c(...)` wrap + explicit Author/Maintainer) folded in to make `R CMD check` produce useful output. Suite stays at 682 PASS — no regression. |
| TBD                                                    | 2026-05-02 | Phase 2 Batch 2 | arch-01 Batch 2 — delete whole file `R/mod_pti_onepage.R` (both `mod_pti_onepage_ui` + `mod_pti_onepage_server`), trim `R/run_app.R` to keep only `run_new_pti` (delete `run_pti`/`run_onepage_pti`/`run_onepage_pti_sample`/`run_dev_map_pti`/`run_dev_pti_plot`), trim `R/app_server.R` to keep only `app_new_pti_server` (delete `app_server`/`app_server_input_simple`/`app_server_sample_pti_vis`), trim `R/app_ui.R` to keep `app_new_pti_ui` + `golem_add_external_resources` (delete `app_ui`/`app_server_sample_pti_vis_ui`). 7 NAMESPACE exports dropped via `roxygen2::roxygenise()`. Suite stays at 682 PASS — no regression. |

Suite total after this branch: **0 failures / 1 skip / 682 PASS** (`testthat::test_local()`; cleanup-only PR, no test delta).

> **Suite totals revised on 2026-05-02:** prior counts in §11 were
> derived from `sum(res$nb)` over `as.data.frame(testthat::test_local())`,
> which over-counts internal `testthat::test_that()` calls inside
> `validate_geometries()` and `validate_single_geom()` (those use
> `test_that()` as a runtime validation mechanism — pinned in §12 as
> candidates for the #7 refactor). The summary-reporter `PASS` count is
> now the source of truth. Per-PR "expectations" deltas above were
> measured the same way and are similarly inflated; left as-is for
> historical record, with this footnote covering the methodology shift.
>
> **Phase 1e milestone reached** with PR #29: all 10 Tier-1 test files
> from arch-03 §1.2–§1.11 are now in place. 9 bugs / spec
> corrections pinned along the way (see §12). 1f (CI guard) merged in
> #30. **1g (Tier 2) complete** — all 7 modules covered:
> `mod_calc_pti2` (#31), `mod_DT_inputs` (#33), `mod_drop_inval_adm`
> (#34), `mod_get_admin_levels_srv` (#36), `mod_fltr_sel_var2_srv`
> (#37), `mod_wt_save_newsrv` (#38), `mod_export_pti_data_server`
> (this branch). **Phase 1 closes when this lands.** Next up: Phase 2
> cleanup batches (arch-01 § "Removal Batches").

---

## 12. Discovered bugs (pinned in tests)

> Bugs surfaced *while* writing Tier-1 tests. Pinned with `expect_*`
> assertions on the current (broken) behaviour so the tests fail when a
> future fix lands — at which point the assertion is updated to the new
> contract. Cleanup-phase candidates.

| Loc | Bug | Pin (test) |
|---|---|---|
| [`R/plot_pti_helpers.R::complete_pti_labels`](R/plot_pti_helpers.R#L113-L133) | The function maps over `dta` but never assigns the result; returns the original `dta` unchanged. Intent is to append `<strong>{priority_label}</strong>` to each `pti_label`. The deployed app silently misses the priority-rank suffix. One-line fix. | [test-plot-helpers.R:complete_pti_labels: returns input unchanged (PINNED BUG)](tests/testthat/test-plot-helpers.R) |
| [`R/plot_pti_helpers.R::check_existing_groups`](R/plot_pti_helpers.R#L286) | Errors with a vctrs size error when `old_grps` is `character(0)` — `str_detect(string, character(0))` is invalid. arch-03 §1.6 expects "first of current shown" in this case. | [test-plot-helpers.R:check_existing_groups: empty old errors (PINNED)](tests/testthat/test-plot-helpers.R) |
| [`R/plot_pti_helpers.R::filter_admin_levels`](R/plot_pti_helpers.R#L60) | Asymmetry: the if-branch enters when `to_fltr` matches *names* of admin levels (e.g. `"admin1"`), but the inner `keep()` predicate compares values (e.g. `"Oblast"`). Passing a name returns 0 entries. Pinned, not a bug per se but worth normalising in the cleanup phase. | [test-plot-helpers.R:filter_admin_levels: name-only filter returns 0 entries (PINNED)](tests/testthat/test-plot-helpers.R) |
| [`R/validators.R::validate_read_shp`](R/validators.R) | Empty-pattern `str_detect` when no admin codes are extra (i.e. the shape file is "perfect") — error caught by the function's internal `test_that`. Refactor target under issue #7. | [test-validators.R:validate_read_shp: round-trips through an .rds path (PINNED BUG)](tests/testthat/test-validators.R) |
| [`R/calc_pti_helpers.R::get_adm_levels`](R/calc_pti_helpers.R#L34) | Lexicographic sort produces `admin1 < admin10 < admin2`. Internally consistent with downstream code (which compares first digit only) but wrong for any deployment with ≥10 admin levels. | [test-calc-pipeline.R:get_adm_levels: sort is lexicographic, not numeric (PINNED)](tests/testthat/test-calc-pipeline.R) |
| [`R/calc_pti_helpers.R::get_scores_data`](R/calc_pti_helpers.R) | 1-row `year × var_code` group produces `NA` (not `0`) because `sd()` of length-1 returns `NA` (not `NaN`), so the `is.nan` filter misses. | [test-calc-pipeline.R:get_scores_data: 1-row groups produce NA, not 0 (PINNED)](tests/testthat/test-calc-pipeline.R) |
| [`R/calc_pti_expander.R::expand_adm_levels`](R/calc_pti_expander.R) | When >1 list element name matches an admin level (`length(...) == 1` guard fails), the entire source-loop iteration returns nested `NULL`s. Silent data loss. | [test-calc-pipeline.R:expand_adm_levels: >1 element matches (PINNED)](tests/testthat/test-calc-pipeline.R) |
| [`R/fct_inp_for_exp.R::fct_internal_wt_to_exp`](R/fct_inp_for_exp.R#L51) | Errors with "Join columns in `x` must be present" when called with an empty `weights_clean = list()`. Cause: `imap_dfr(list())` yields a 0×0 tibble that has no `var_code` column for the downstream `left_join`. Should early-return on length-0 input. | [test-export.R:fct_internal_wt_to_exp: empty list errors at left_join (PINNED)](tests/testthat/test-export.R) |
| [`R/mod_dta_explorer2.R::get_var_choices`](R/mod_dta_explorer2.R#L302) | Errors with "attempt to set an attribute on NULL" when called with an empty `indicators_list`. The fallback branch `names(out) <- "Indicators"` runs even when `out` is `NULL`. A length-0 list output would be more useful. | [test-explorer-helpers.R:get_var_choices: empty indicators tibble errors (PINNED)](tests/testthat/test-explorer-helpers.R) |
| [`R/mod_drop_inval_adm.R::get_vars_un_avbil`](R/mod_drop_inval_adm.R#L72-L101) | Asymmetric availability check. The fill logic uses `lag(value)` (after `arrange(admin_level)`), so an indicator that exists only at an earlier-sorted level (e.g. admin1 only) is treated as "available" at later-sorted levels (admin2) — admin2 is never surfaced as unavailable. Reverse direction (admin2-only → admin1 unavailable) works because admin1 is first-sorted and has no `lag`. Also: the `is.na(value & !any_larger)` expression looks like missing parens — likely meant `(is.na(value) & !any_larger)`. Caught while writing Tier-2 tests for `mod_drop_inval_adm`. | [test-mod-drop-inval-adm.R: comment block above "Notification side effect" (DOCUMENTED, not pinned — Tier-2 test avoids the bug per the skill rule)](tests/testthat/test-mod-drop-inval-adm.R) |
| [`R/mod_dta_explorer2.R::mod_fltr_sel_var2_srv`](R/mod_dta_explorer2.R#L216-L235) | The `add_selected()` observer's predicate `purrr::map_lgl(choices(), ~ { .x %in% c(selected_add, selected_now) | .x %in% names(c(selected_add, selected_now)) })` errors with "Result must be length 1, not N" whenever any pillar holds >1 variable, because `.x` is the length-N character vector for that pillar and `%in%` returns length-N. Should be `map(...) %>% map_lgl(any)`, or use `any(.x %in% ...)` inside the predicate. The Tier-2 test avoids the bug for happy-path coverage by using single-var-per-pillar choices, and pins the underlying predicate failure separately. | [test-mod-var-selector.R: add_selected() with multi-var pillar fails the inner predicate (PINNED BUG)](tests/testthat/test-mod-var-selector.R) |

---

*This plan is a thin tracker. For depth, follow the links in §1.*
