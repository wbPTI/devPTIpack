# devPTIpack — Working Plan

> **Living document.** Tracks current phase, immediate next actions, and shared
> workflow conventions. Every PR that touches tracked work updates this file in
> the same commit (see [`.claude/CLAUDE.md`](.claude/CLAUDE.md) §"PLAN.md Sync").
> The authoritative architecture lives under [`.github/docs/`](.github/docs/) and
> the master GitHub tracker is [`#9`](https://github.com/worldbank/devPTIpack/issues/9).
> When this file disagrees with `.github/docs/`, `.github/docs/` wins.

---

## 1. Where we are

**Status snapshot (2026-05-07):** Phases 1 (#10), 2 (#8), 3 (#11) closed.
**Phase 2.5 §12 bug-fix sprint COMPLETE — all 14 of 14 bugs fixed**
(#1 `complete_pti_labels` PR #56; #5 `get_adm_levels` PR #57; #6
`get_scores_data` PR #59; #7 `expand_adm_levels` PR #60, bundled
with the boundary-regex follow-up — the 14th §12 row; #10
`get_vars_un_avbil` symmetric availability PR #61; #2
`check_existing_groups` empty-old PR #62; #4 `validate_read_shp`
empty-pattern PR #64; #8 `fct_internal_wt_to_exp` empty list PR
#65; #9 `get_var_choices` empty indicators PR #66; #11
`mod_fltr_sel_var2_srv` multi-var pillar PR #67; #13
`mod_dwnld_file_server` broken downloads + `launch_pti()` path-
default API PR #68; #3 `filter_admin_levels` name-vs-value
asymmetry PR #69; #12 `gg_admin_list` undefined-default PR #70).
Phase 4 (#12, vignettes & pkgdown) and Phase 5 (#13, hex ingestion)
are next. R-CMD-check workflow gates merge with `error-on:
'"error"'`. Suite at **0 fail / 1 skip / 710 PASS** (PR #70 adds a
new Tier-1 test file with 2 test_that blocks / 8 expectations
covering the missing-`mt` error and the bundled-data success path).

**Update (2026-05-12):** PR #63 (`eb-docs-pkgdown` -> `koichi-arch-redesign`)
merged on 2026-05-12 (merge commit `effb832`), bringing the full arch-09
documentation chain (PRs #89-#103) and Eduard's new arch-10 + arch-11
design docs onto the integration line. `R CMD check` is at **0/0/0**.
Two new work tracks are now active off `koichi-arch-redesign`:
**arch-10** (Step 1 shapefiles enhancement, parent
[#104](https://github.com/worldbank/devPTIpack/issues/104)) opens with
[#106](https://github.com/worldbank/devPTIpack/issues/106) -- Step 1
vignette doc fixes (this PR); **arch-11** (hex data access pipeline,
parent [#107](https://github.com/worldbank/devPTIpack/issues/107))
supersedes arch-05 and provides the concrete implementation track for
[#13](https://github.com/worldbank/devPTIpack/issues/13).

| Concern | Source of truth |
|---|---|
| Architecture overview & redesign workflow | [`.github/docs/arch-00-overview.md`](.github/docs/arch-00-overview.md) |
| Function-by-function cleanup audit | [`.github/docs/arch-01-cleanup.md`](.github/docs/arch-01-cleanup.md) |
| Roxygen2 standards | [`.claude/rules/roxygen-documentation.md`](.claude/rules/roxygen-documentation.md) |
| Documentation implementation order | [`.github/docs/arch-02-docs.md`](.github/docs/arch-02-docs.md) |
| Calculation pipeline test spec (~71 cases) | [`.github/docs/arch-02.01-testing-calc-pipeline.md`](.github/docs/arch-02.01-testing-calc-pipeline.md) |
| Three-tier testing strategy + per-fn test map | [`.github/docs/arch-03-testing.md`](.github/docs/arch-03-testing.md) |
| Workspace, vignettes & pkgdown plan | [`.github/docs/arch-04-workspace.md`](.github/docs/arch-04-workspace.md) |
| Hex (H3) ingestion design (superseded by arch-11) | [`.github/docs/arch-05-hex-ingestion.md`](.github/docs/arch-05-hex-ingestion.md) |
| Step 1 shapefiles enhancement (`make_hex_grid`, `make_admin_lookup`) | [`.github/docs/arch-10-step1-shapefiles-enhancement.md`](.github/docs/arch-10-step1-shapefiles-enhancement.md) |
| Hex data access pipeline (registry, fetch, aggregate, metadata) | [`.github/docs/arch-11-hex-data-access.md`](.github/docs/arch-11-hex-data-access.md) |
| Per-change log (compulsory) | [`.github/docs/changelog.md`](.github/docs/changelog.md) |
| Project conventions for AI agents | [`.claude/CLAUDE.md`](.claude/CLAUDE.md) |

GitHub issues map:
- [#9](https://github.com/worldbank/devPTIpack/issues/9) — master tracker
- [#10](https://github.com/worldbank/devPTIpack/issues/10) — testing framework + `run_pti_pipeline()` orchestrator
- [#8](https://github.com/worldbank/devPTIpack/issues/8) — legacy cleanup (subsumes #2/#3/#4)
- [#11](https://github.com/worldbank/devPTIpack/issues/11) — roxygen2 documentation
- [#12](https://github.com/worldbank/devPTIpack/issues/12) — workspace, vignettes, pkgdown site
- [#13](https://github.com/worldbank/devPTIpack/issues/13) — hex ingestion pipeline (independent; superseded by #107)
- [#104](https://github.com/worldbank/devPTIpack/issues/104) — arch-10: Step 1 shapefiles enhancement (`make_hex_grid`, `make_admin_lookup`)
- [#107](https://github.com/worldbank/devPTIpack/issues/107) — arch-11: hex data access pipeline (supersedes arch-05/#13)
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
Phase 2  Cleanup legacy code in batches            (#8)        │  ✓ done
   ├─ Batch 1  Dead files & functions                          │
   ├─ Batch 2  Legacy runners + app_server/app_ui              │
   ├─ Batch 3  Legacy map server (~1200 lines)                 │
   ├─ Batch 4  Migrate sample app to launch_pti()              │
   ├─ Batch 5  Remove mod_weights.R legacy                     │
   └─ Batch 6  Delete convenience wrappers                     │
            ▼                                                  │
Phase 3  Roxygen2 docs for all permanent fns       (#11)       │  ✓ done
   ├─ Batch 1  Package data (R/data.R)                   ✓ #47 │
   ├─ Batch 2  Core calculation                          ✓ #48 │
   ├─ Batch 3  Data I/O & validation                     ✓ #49 │
   ├─ Batch 4  Visualisation helpers                     ✓ #50 │
   ├─ Batch 5  Entry points & app infrastructure         ✓ #51 │
   ├─ Batch 6a PTI rendering & display stack             ✓ #52 │
   └─ Batch 6b Closes Phase 3                            ✓ #53 │
            ▼                                                  │
Phase 2.5  §12 bug-fix sprint (concurrent)         (—)         │  ✓ done (14/14)
   ├─ #1 complete_pti_labels (silent data corruption)    ✓ #56 │
   ├─ #5 get_adm_levels lex sort (silent corruption)     ✓ #57 │
   ├─ #6 get_scores_data 1-row -> NA (silent corrup)     ✓ #59 │
   ├─ #7 expand_adm_levels >1-slot silent NULL +         ✓ #60 │
   │     boundary-regex follow-up (latent (d))                  │
   ├─ #10 get_vars_un_avbil asymmetric lag-fill          ✓ #61 │
   ├─ #2 check_existing_groups empty-old vctrs error     ✓ #62 │
   ├─ #4 validate_read_shp empty-pattern str_detect      ✓ #64 │
   ├─ #8 fct_internal_wt_to_exp empty list left_join     ✓ #65 │
   ├─ #9 get_var_choices empty indicators NULL attr      ✓ #66 │
   ├─ #11 mod_fltr_sel_var2_srv multi-var pillar         ✓ #67 │
   ├─ #13 mod_dwnld_file_server broken downloads +       ✓ #68 │
   │      launch_pti() path-default API (Arch-2)                │
   ├─ #3 filter_admin_levels name-vs-value asymmetry     ✓ #69 │
   └─ #12 gg_admin_list undefined-default                ✓ #70 │
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

- [x] Batch 1 — dead files & functions ([PR #40](https://github.com/worldbank/devPTIpack/pull/40)): 4 whole files
      (`fct_export_data.R`, `mod_explorer.R`, `mod_info_page.R`,
      `mod_calc_pti.R`), ~20 individual functions, 1 orphan `.rda`,
      and 3 NAMESPACE exports (`extrapo_one_weight`,
      `fct_exp_wt_to_internal`, `mod_weights_html_ui`). Plus a
      pre-existing DESCRIPTION fix (Authors@R needed comma-separated
      `c(...)` wrapping; added explicit Author/Maintainer fields)
      that was needed to make `R CMD check` produce useful output.
- [x] Batch 2 — legacy runners + app_server/app_ui ([PR #41](https://github.com/worldbank/devPTIpack/pull/41)): deleted
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
- [x] Batch 3 — legacy map server (~1200 lines) (PR #43): extracted
      `mod_map_pti_leaf_ui` to its own file `R/mod_map_pti_leaf_ui.R`
      (the kept UI used by `mod_pti_comparepage_ui`, `mod_weights_ui`,
      and `mod_ptipage_core.R`). Deleted whole file `R/mod_map_pti_leaf.R`
      (1258 lines, all 12 legacy server functions go) and `make_shapes()` +
      `make_labels()` from `R/fct_legend_map_satelites.R`. Folded in a
      one-line patch to `app_new_pti_ui` to use the modern
      `mod_pti_comparepage_ui` instead of the deleted
      `mod_map_pti_leaf_page_ui`. Relocated 2 `@importFrom` tags
      (`leaflet::colorFactor` → `legend_map_satelite`,
      `leaflet::addTiles` → `plot_leaf_line_map2`) that were
      auto-pruned from NAMESPACE because their roxygen lived in the
      deleted file. Suite stays at 682 PASS — no regressions.
- [x] Batch 4 — migrate sample app to `launch_pti()` (PR #44): rewrote
      `inst/sample_pti/app.R` to load shapes via `get_shape()` and
      metadata via `fct_template_reader()`, then call `launch_pti()`
      directly (drops `default_adm_level`, `choose_adm_levels`,
      `explorer_*`, `full_ui`, and `pti_landing_page` knobs that have no
      modern equivalent — `launch_pti` would need a signature extension
      to preserve them, out of scope for this batch). Deleted
      `run_new_pti` (whole `R/run_app.R`), `app_new_pti_server` (whole
      `R/app_server.R` — file `git rm`'d), `app_new_pti_ui` (kept only
      `golem_add_external_resources` in `R/app_ui.R`), and
      `mod_plot_pti_comparison_srv` from `R/mod_plot_pti2.R`. Relocated
      5 `@importFrom` tags auto-pruned from NAMESPACE because their
      roxygen lived in the deleted files: `shiny::shinyApp`,
      `shiny::navbarPage`, `shiny::tabPanel`, `golem::with_golem_options`
      → `launch_pti_onepage`; `shiny::reactiveValues` → `mod_wt_inp_ui`.
      Suite stays at 682 PASS — no regressions.
- [x] Batch 5 — remove `mod_weights.R` legacy (PR #45): extracted
      `mod_wt_btns_srv` and `mod_collect_wt_srv` (the only two functions
      `mod_DT_inputs_server` still uses) to a new file
      `R/mod_wt_btns_collect.R` with proper `@noRd` roxygen and explicit
      `@importFrom` tags (`shiny`, `dplyr`, `purrr`, `stringr`,
      `tibble`). Deleted whole file `R/mod_weights.R` (928 lines, all 13
      remaining legacy functions go: `mod_weights_ui`,
      `mod_weights_server`, `mod_indicarots_srv`, `mod_gen_wt_inputs_srv`,
      `mod_wt_name_srv`, `mod_wt_select_srv`, `mod_wt_uplod_srv`,
      `mod_wt_delete_srv`, `mod_wt_fill_srv`, `mod_wt_save_srv`,
      `mod_download_wt_srv`, plus the originals of the two extracted
      functions). Deleted whole file `R/mod_new_weights.R` (112 lines —
      `mod_new_demo_weights_server` + `mod_new_weights_server`; only
      caller `mod_pti_onepage_server` was removed in Batch 2). Net diff
      ~970 lines net removed (largest single batch). NAMESPACE: dropped
      1 export (`mod_weights_ui`); added 4 imports from the new file
      (`purrr::map2`, `purrr::map_dfr`, `shiny::updateNumericInput`,
      `tibble::tibble`); no auto-prune fallout. `mod_weights_rand.R`
      retains only `mod_weights_rand_ui`, `get_rand_weights`,
      `get_all_weights_combs` as planned. Suite stays at 682 PASS — no
      regressions.
- [x] Batch 6 — delete convenience wrappers ([PR #46](https://github.com/worldbank/devPTIpack/pull/46)): deleted whole files
      `R/mod_explrr_onepage.R` (`mod_explrr_onepage_ui`, `mod_explrr_onepage_server`,
      both exported thin wrappers over `mod_dta_explorer2_*`) and
      `R/render_metadata_pdf.R` (`render_metadata`, exported wrapper over
      `rmarkdown::render`). **Departure from arch-01's "deprecate first"
      framing** — the audit showed both targets had no live external surface:
      (a) `mod_explrr_onepage_*`'s only callers are in
      `dev/90-app-examples.R`, itself broken since Batch 2 (it still
      references the removed `mod_pti_onepage_*`) and slated for arch-04
      Phase 4 deletion; (b) `render_metadata` is already broken on shipped
      installs because it calls `system.file("pti-metadata-pdf.Rmd")`
      which resolves to `""` (the Rmd actually lives at
      `inst/sample_pti/app-data/`, not the `inst/` package root). A formal
      deprecation cycle (`.Deprecated()` body for one release) is bureaucratic
      without a release cadence to deprecate against; both also violate
      the project's `@noRd + @export` rule. The arch-04 reference to
      `render_metadata()` (workspace doc plan) is a one-line `system.file`
      fix away from working — when arch-04 wants this, it can reintroduce
      it as a fixed function rather than carrying the broken one forward.
      NAMESPACE delta: dropped 3 exports (`mod_explrr_onepage_ui`,
      `mod_explrr_onepage_server`, `render_metadata`) and 2 importFroms
      (`here::here`, `rmarkdown::render` — neither used elsewhere in
      `R/`). Net diff ~50 lines removed. **Closes Phase 2.** Suite stays
      at 682 PASS — no regressions.

---

## 6. Phase 3 — Documentation (#11)

Follow arch-02-docs § "Implementation Order" and use the
[`roxygen-document`](.claude/skills/roxygen-document/SKILL.md) skill.

- [x] **Batch 1 — Package data** ([PR #47](https://github.com/worldbank/devPTIpack/pull/47)): rewrote `R/data.R` standalone
      roxygen for `ukr_shp` and `ukr_mtdt_full` per the rules-file data
      template (`@format` with `\describe{}` per slot, `@source`,
      runnable `@examples`). Replaced `@describeIn` inheritance on
      `ukr_mtdt_full` (which mis-attributed it as a geometries doc).
      Documented real bundled-data shape (`admin0/1/2/4` not the
      rules-file example's stale `admin1/2/3`; real column names
      `adminNPcod` / `adminNName` / `area` / `geometry`). Two
      `man/*.Rd` help pages now exist where there was previously one
      shared inherited page.
- [x] **Batch 2 — Core calculation** ([PR #48](https://github.com/worldbank/devPTIpack/pull/48)): rewrote roxygen for all
      12 functions across `R/calc_pti_helpers.R` (6 fns) and
      `R/calc_pti_expander.R` (6 fns). **Honored arch-01's "Permanent
      Functions" classification** — un-exported the 10 INTERNAL fns
      (`get_mt`, `get_adm_levels`, `pivot_pti_dta`, `clean_geoms`,
      `get_weighted_data`, `get_scores_data`, `expand_adm_levels`,
      `agg_pti_scores`, `structure_pti_data`; `merge_expandedn_adm_levels`
      was already non-exported) and re-typed them with the internal
      `@noRd` template (typed `@param`, explicit `@return`, full
      `@importFrom`, no `@examples`). Closes the 10× `@noRd + @export`
      rule violation. Promoted `generic_pti_glue` from `@describeIn
      label_generic_pti` to a standalone exported doc; both retained
      `@export` per arch-01 (users may customise PTI popup labels).
      Pinned 3 §12 bugs via `@note`: lex-sort in `get_adm_levels`,
      1-row → NA in `get_scores_data`, >1 element silent NULL in
      `expand_adm_levels`. Stripped pre-existing debug residue per
      arch-01:436 (~50 lines: 2 `# browser()`, 2 large commented-out
      alternate implementations in `structure_pti_data`, several
      stray comment lines). NAMESPACE delta: dropped 9 `export()`,
      added 7 `importFrom`s for explicit dependencies that lost their
      previous bulk-import coverage. Test-side: converted 7
      `devPTIpack::fn()` qualified calls in 3 pre-existing test files
      ([test-shps-converters.R](tests/testthat/test-shps-converters.R),
      [test-calc_pti.R](tests/testthat/test-calc_pti.R),
      [test-weighting-logic.R](tests/testthat/test-weighting-logic.R))
      to unqualified — needed because `::` requires export and the
      tests' `pkgload::load_all()` exposure is unaffected.
- [x] **Batch 3 -- Data I/O & validation** ([PR #49](https://github.com/worldbank/devPTIpack/pull/49)): rewrote roxygen for
      11 functions across 5 files -- `R/fct_template_reader.R` (2 fns),
      `R/fct_validate_metadata.R` (3 fns), `R/validators.R` (2 fns),
      `R/dta_cleaners.R` (1 fn), and `R/mod_drop_inval_adm.R` (4 fns --
      folded in here for thematic coherence with the validation theme;
      arch-02-docs §3 listed them implicitly in §6, but `drop_inval_adm`
      / `get_vars_un_avbil` / `get_min_admin_wght` are tightly coupled
      with the validators). Honored arch-01's "Permanent Functions"
      classification: un-exported `validate_single_geom` and
      `get_indicators_list` (closes the `@noRd + @export` rule
      violations on both); promoted `validate_read_shp` /
      `validate_read_metadata` / `get_vars_un_avbil` /
      `get_min_admin_wght` / `drop_inval_adm` from `@describeIn` chains
      to standalone exported docs (each gets its own `man/*.Rd` help
      page; previously they shared the umbrella's help page). Pinned 2
      §12 bugs via `@note`: `validate_read_shp` empty-pattern
      `str_detect` (issue #7), `get_vars_un_avbil` asymmetric `lag()`
      fill (PR #34). Stripped 4 `# browser()` debug residue lines per
      arch-01:436 (fct_template_reader.R x2, validators.R, dta_cleaners.R)
      plus an 8-line commented-out runtime `test_that` block in
      validators.R replaced by the live `if (is.na(adm_order)...)`
      block. NAMESPACE delta: dropped 2 `export()`
      (`validate_single_geom`, `get_indicators_list`); added explicit
      `importFrom`s on the rewritten functions. Kept the `@import dplyr
      purrr stringr readxl` bulk-import directive on
      `fct_template_reader` -- the `pkgload::load_all()` test environment
      relies on the bulk import to keep `mod_DT_inputs_server`'s 500ms
      throttle test deterministic across `testServer` calls (subtle
      shiny scheduler interaction; explicit-import-only triggers a
      cross-test reactive-state leak in the throttle Tier-2 test).
      Test-side: converted 7 `devPTIpack::get_indicators_list()`
      qualified calls in 2 pre-existing test files
      ([test-calc_pti.R](tests/testthat/test-calc_pti.R) x4,
      [test-weighting-logic.R](tests/testthat/test-weighting-logic.R)
      x3) to unqualified, and qualified 5 unqualified `tribble(`
      calls in
      [test-get_uavailab_admin.R](tests/testthat/test-get_uavailab_admin.R)
      to `tibble::tribble(` (the previous transitive availability via
      bulk imports no longer covers it). Suite stays at 682 PASS -- no
      regressions.
- [x] **Batch 4 -- Visualisation helpers** ([PR #50](https://github.com/worldbank/devPTIpack/pull/50)): rewrote roxygen for
      12 functions across `R/plot_pti_helpers.R` (10 fns) and
      `R/fct_legend_map_satelites.R` (2 fns). All 12 are INTERNAL per
      arch-01 § "Permanent Functions" / "Visualisation Helpers" -- this
      batch un-exports every one of them and converts each to the
      `@noRd` internal template (typed `@param`, explicit `@return`,
      full `@importFrom`, no `@export`, no `@examples`). Closes 4
      `@noRd + @export` rule violations (`get_current_levels`,
      `filter_admin_levels`, `add_legend_paras`, `legend_map_satelite`)
      plus 1 stray-tag typo (the original `legend_map_satelite` had a
      `@param dta` tag for a parameter the function never had).
      Promoted 4 `@describeIn
      plot_pti_polygons` chains to standalone `@noRd` docs
      (`clean_pti_polygons`, `add_pti_poly_controls`,
      `clean_pti_poly_controls`, `check_existing_groups`); the
      `@describeIn` chain was acceptable while all 5 were exported but
      becomes meaningless when nothing has a help page. Pinned 3 §12
      bugs via `@note` (all from PR #25, all in plot_pti_helpers.R):
      `filter_admin_levels` name-vs-value asymmetry,
      `complete_pti_labels` missing-assignment no-op (priority-rank
      suffix never appended in production), `check_existing_groups`
      empty-pattern `str_detect` error. Stripped 4 `# browser()`
      debug-residue lines per arch-01:436 (plot_pti_helpers.R x2,
      fct_legend_map_satelites.R x2). Left the larger commented-out
      alternate-implementation blocks in fct_legend_map_satelites.R
      alone (lines 60-83 alternate quantile fallbacks, ~190-210
      alternate `recode_values` closure, ~310-360 commented-out
      `get_shift` / `generic_pal` -- these are alternate-algorithm
      drafts, not debug residue, and stripping them is out of arch-01's
      "remove commented-out blocks" mandate which lists specific files;
      `fct_legend_map_satelites.R` is not on that list). Fixed the
      `@describeIn plot_pti_polygons plot_pti_polygons` typo on
      `add_pti_poly_controls` (function name appeared twice in the
      previous one-liner). Package-source fix: converted one
      `devPTIpack::get_current_levels()` qualified call in
      `R/mod_export_pti_data.R::get_pti_scores_export` to unqualified
      (the `::` form requires export and now errors after the
      un-export); 9 export-data tests broke and went green again after
      this one-line fix. NAMESPACE delta: dropped 11 `export()`
      (`add_legend_paras`, `add_pti_poly_controls`,
      `check_existing_groups`, `clean_pti_poly_controls`,
      `clean_pti_polygons`, `complete_pti_labels`, `filter_admin_levels`,
      `get_current_levels`, `legend_map_satelite`, `plot_pti_polygons`,
      `preplot_reshape_wghtd_dta`); added 7 `importFrom`s
      (`leaflet::addLayersControl` / `clearGroup` /
      `layersControlOptions` / `showGroup`; `purrr::map2_chr`;
      `shiny::HTML`; and a few re-localised tags from explicit
      `@importFrom` sweeps). Test-side: no qualified-call conversions
      needed -- the existing `devPTIpack:::legend_map_satelite` triple-colon
      calls in `tests/testthat/test-legend-mapping.R` keep working
      across the export status flip (`:::` resolves regardless of
      export); other test files use unqualified calls that resolve via
      `pkgload::load_all()`. Suite stays at 682 PASS -- no regressions.
- [x] **Batch 5 -- Entry points & app infrastructure** ([PR #51](https://github.com/worldbank/devPTIpack/pull/51)): rewrote roxygen for
      5 functions across 4 files -- `R/launch_pti.R`
      (`launch_pti_onepage` + `launch_pti`), `R/fct_create_new_pti.R`
      (`create_new_pti`), `R/app_config.R` (`app_sys`), and
      `R/app_ui.R` (`golem_add_external_resources` -- folded in here
      for thematic coherence with the entry-points theme; arch-01:24
      classifies it as EXPORTED but it lived in app_ui.R behind
      `@noRd`). All 5 are EXPORTED per arch-01 § "Entry Points & App
      Scaffolding" -- this batch is the inverse shape of Batches 2-4
      (no un-exports; one promote-to-exported on
      `golem_add_external_resources`). Honored the rules-file Exported
      template: typed `@param`, `@return`, `@importFrom`, `@export`,
      runnable or `\dontrun{}` `@examples`. Closes 1 `@noRd + @export`
      rule violation (`app_sys` had both tags), 1
      `@noRd`-on-an-EXPORTED-fn arch-01 violation
      (`golem_add_external_resources`). Promoted the
      `@describeIn launch_pti_onepage` chain on `launch_pti` to a
      standalone help page -- the chain was awkward because the two
      apps diverge in usage (one-page is a focused viewer, multi-tab
      pulls in compare + explorer + cicerone tour), and only
      `launch_pti` takes the `tabs` and `app_name` parameters.
      Examples: `app_sys` runs unwrapped (`app_sys("app", "www")`);
      `create_new_pti` runs unwrapped via a `tempdir()` scaffold with
      `open = FALSE` (rstudioapi gates skip cleanly outside RStudio);
      `launch_pti` / `launch_pti_onepage` /
      `golem_add_external_resources` use `\dontrun{}` per the rules-file
      "side-effect-heavy code" clause. Stripped 1 debug-residue line
      (`R/launch_pti.R:173` -- a commented-out
      `observe(cat("tab: ", active_tab(), "\n"))` instrumentation).
      No pinned §12 bugs touched this batch. NAMESPACE delta: added
      1 export (`golem_add_external_resources`); added 2 `importFrom`s
      (`cicerone::use_cicerone`, `rlang::dots_list`); dropped 1 stale
      `importFrom` (`bsplus::use_bs_tooltip` -- carried over on
      `golem_add_external_resources` from the `@noRd` era but never
      called in `R/`; reviewer-flagged should-fix).
      Test-side: no qualified-call conversions needed -- nothing was
      un-exported; the new `golem_add_external_resources` export
      strictly widens the public surface. Folded in the Batch 4 `TBD ->
      #50` swap on the §6 row above and on the §11 row below. Suite
      stays at 682 PASS -- no regressions.
- [x] **Batch 6a -- PTI rendering & display stack** ([PR #52](https://github.com/worldbank/devPTIpack/pull/52)):
      rewrote roxygen for 44 functions across 16 files in the rendering
      stack -- `R/mod_ptipage_core.R` (3), `R/mod_pti_comparepage.R` (2),
      `R/mod_calc_pti2.R` (2), `R/mod_dta_explorer2.R` (8),
      `R/mod_export_pti_data.R` (3), `R/mod_load_shapes.R` (2),
      `R/mod_plot_pti2.R` (1), `R/mod_plot_init_leaf.R` (2),
      `R/mod_plot_poly_leaf.R` (4), `R/mod_plot_poly_legend.R` (3),
      `R/mod_pti_map_side_pan.R` (7), `R/mod_map_pti_leaf_ui.R` (1),
      `R/mod_dwnld_dta.R` (3), `R/mod_dwnld_local_file.R` (1),
      `R/run_pti_pipeline.R` (1), `R/mod_fetch_data.R` (1). Honored
      arch-01: un-exported 15 INTERNAL fns (6 in `mod_dta_explorer2.R`,
      4 in `mod_plot_poly_leaf.R`, 3 in `mod_plot_poly_legend.R`, 2 in
      `mod_plot_init_leaf.R`); promoted 3 EXPORTED fns from `@noRd` to
      `@export` (closes 3 arch-01 violations: `mod_leaf_side_panel_ui`,
      `mod_map_pti_leaf_ui`, `mod_pti_comparepage_newsrv`). Promoted
      14 `@describeIn` members to standalone docs across the chains in
      `mod_dta_explorer2`, `mod_plot_init_leaf`, `mod_plot_poly_leaf`,
      `mod_plot_poly_legend`, and `mod_ptipage_core` (split
      `mod_ptipage_newsrv` out of the UI chain since server vs UI
      diverged in usage). Pinned 2 §12 bugs via `@note`:
      `get_var_choices` empty-indicators NULL-attr (PR #27),
      `mod_fltr_sel_var2_srv` multi-var-pillar predicate (PR #37).
      Stripped 6 debug-residue lines per arch-01:436: 4 `# browser()`
      in `mod_plot_poly_leaf.R`, 1 in `mod_pti_map_side_pan.R`, 1 in
      `mod_dta_explorer2.R`; plus the 14-line commented-out
      `withProgress`/`tempfile` block in `mod_plot_leaf_export`
      (alternate impl that was never re-enabled). Reviewer-iteration
      fixes: rewrote `get_pti_weights_export` and `run_pti_pipeline`
      examples to drop the un-exported `get_indicators_list()` call
      (used `ukr_mtdt_full$metadata` directly instead -- the function
      only reads `var_code`/`var_name`/`pillar_name`, all present) so
      both examples pass under `R CMD check --run-examples` where only
      exported symbols resolve through `library(devPTIpack)`; flattened
      all `[name()]` cross-refs from EXPORTED roxygen blocks to plain
      backticks where the target is `@noRd` (cosmetic; package isn't
      in markdown mode so the link syntax rendered literally).
      Suite stays at 682 PASS -- no regression.
- [x] **Batch 6b -- All remaining weights-input + UI infra utilities**
      ([PR #53](https://github.com/worldbank/devPTIpack/pull/53)):
      audited 37 functions across 12 files closing Phase 3 -- 35 fns
      across 11 files rewritten in this PR; 2 fns in
      `R/mod_wt_btns_collect.R` already at standard from Batch 5
      extraction PR #45, no edits needed. File-by-file:
      `R/mod_wt_inp.R` (14), `R/mod_DT_inputs.R` (7),
      `R/mod_wt_btns_collect.R` (2; verified-only),
      `R/mod_first_open_count.R` (1),
      `R/mod_tab_open.R` (1), `R/mod_waiter.R` (3),
      `R/mod_weights_rand.R` (3), `R/mod_infotab.R` (1),
      `R/fct_guide.R` (1), `R/fct_helpers.R` (1; `add_logo`),
      `R/fct_inp_for_exp.R` (2), `R/supporting-goe-prep.R` (1;
      `gg_admin_list`). Honored arch-01: un-exported 2 INTERNAL fns
      (`fct_inp_for_exp`, `fct_internal_wt_to_exp` -- arch-01:166-167
      classifies both INTERNAL but they were exported via the
      `@noRd + @export` rule violation; un-exporting now closes the
      violation and aligns NAMESPACE with arch-01); kept 4 EXPORTED
      fns exported (`mod_tab_open_first_newserv`, `gg_admin_list`,
      `get_rand_weights`, `get_all_weights_combs`) by dropping their
      `@noRd` tags (closes 4 more `@noRd + @export` rule violations
      without moving NAMESPACE). Net 6 rule-violations closed in this
      batch. Promoted the 14-fn `@describeIn mod_wt_inp_ui` chain in
      `R/mod_wt_inp.R` to standalone `@noRd` docs (Batch 4 precedent
      for full-chain split when umbrella + members are all INTERNAL);
      promoted the 3-fn `@describeIn mod_waiter_newsrv` chain
      similarly; split `get_rand_weights` and `get_all_weights_combs`
      out of the `@describeIn mod_weights_rand_ui` chain so their
      EXPORTED help pages stand alone. Pinned 1 §12 bug via `@note`:
      `fct_internal_wt_to_exp(list())` left-join error (PR #26).
      Stripped 8 `# browser()` debug-residue lines per arch-01:436:
      4 in `R/mod_DT_inputs.R` (lines 71/170/254/277 in the
      pre-Batch-6b file), 1 in `R/mod_wt_inp.R` (line 790, inside
      the upload-stub `mod_wt_uplod_newsrv` body), 3 in
      `R/supporting-goe-prep.R` (lines 17/22/38). Also stripped the
      8-line commented-out alternate-impl `observeEvent(update_dta())`
      block in `R/mod_DT_inputs.R` (lines 77-84 in the pre-Batch-6b
      file; contained an embedded `browser()` on line 81; the live
      `observe(...)` block above already implements the same logic
      without the debugger trap). User's prompt-time hypothesis
      counts diverged in three places that surfaced during the audit
      and were corrected at the scope-proposal gate before any roxygen
      edit: (1) `fct_inp_for_exp` + `fct_internal_wt_to_exp` are
      arch-01-INTERNAL not EXPORTED, so the close direction was
      drop-`@export` not drop-`@noRd`; (2) `R/mod_infotab.R` had no
      `# browser()` at line ~71 (and none anywhere); (3) total
      browser-strip count was 8 lines + 1 alt-impl block, not the 1
      implied. NAMESPACE delta: dropped 2 `export()`
      (`fct_inp_for_exp`, `fct_internal_wt_to_exp`); added a batch of
      explicit `importFrom`s on the rewritten functions that lost
      bulk-import coverage when their `@describeIn` chain dispersed.
      Test-side: no qualified-call conversions needed -- the existing
      unqualified `fct_inp_for_exp(...)` / `fct_internal_wt_to_exp(...)`
      calls in `tests/testthat/test-export.R` resolve via
      `pkgload::load_all()` regardless of export status. Folded in
      the Batch 6a `TBD -> #52` swap on the §6 row above and on the
      §11 row below. **Closes Phase 3 (#11).** Suite stays at 682
      PASS -- no regressions.
- [ ] Any function whose body changed in Phase 2 cleanup needs its docs reviewed.
- [ ] After each batch lands, run `devtools::document()` and confirm
      `R CMD check` produces no doc-related notes.

---

## 7. Phase 4 — Workspace, vignettes & pkgdown (#12)

Per arch-04. Concrete cuts:

- [x] Delete `dev/` (git history preserves it). Removed 16 scratch files
      and the legacy `mapDwnldApp/` after confirming zero live references
      from `R/`, `tests/`, `inst/`, `vignettes/`, or `DESCRIPTION`. R CMD
      check stays at 0/0/0; test suite unchanged.
- [x] Replace `vignettes/dataprep.qmd` stub with the data-prep
      reference (PR-B `docs/build-pti-content`, per arch-06 §4 — column-by-column
      reference for boundary shapes + metadata template, common pitfalls,
      worked example using bundled `ukr_shp` / `ukr_mtdt_full`).
- [x] Fill `vignettes/articles/build-pti.qmd` with the task-oriented
      walkthrough (PR-B, per arch-06 §3 — scaffold → prep shapes → prep
      metadata → validate → launch → deploy → troubleshoot).
- [ ] Slim `vignettes/articles/methodology.qmd` to conceptual content
      ([#73](https://github.com/worldbank/devPTIpack/issues/73), follow-up to PR-B
      per arch-06 §5).
- [ ] Fill `vignettes/articles/past-projects.qmd` (deferred — out of
      arch-06 scope per §9).
- [ ] Source for additional methodology / project-history pages: the
      [WB OpenKnowledge PTI article](https://openknowledge.worldbank.org/server/api/core/bitstreams/1fa677a7-7c1c-5a39-8705-511a7038e3a2/content).
- [x] Update `_pkgdown.yml` to add grouped article navigation per arch-04
      §"_pkgdown.yml Updates" (landed via PR #63).
- [x] Add a GitHub Action for pkgdown build & GitHub Pages deploy
      (landed via PR #63).
- [x] Validator UX pass: cli output + structured return
      (`list(status, summary, issues)`) + `error_on_fail` parameter
      (PR-B, per arch-06 §6 — minimal subset of issue
      [#7](https://github.com/worldbank/devPTIpack/issues/7); CSV /
      auto-metadata / Shiny upload module deferred).
- [x] arch-09 PR #B — `app_validate_shp(shp)` exported (issue
      [#80](https://github.com/worldbank/devPTIpack/issues/80)):
      standalone Shiny launcher for visual shapefile inspection,
      renders leaflet map with one toggleable layer per admin level
      and the `validate_geometries()` summary in a sidebar; 12-test
      Tier-1+Tier-2 file (`tests/testthat/test-app-validate-shp.R`).
- [x] arch-09 PR #C — `app_validate_metadata(shp_dta, inp_dta)`
      exported (issue
      [#81](https://github.com/worldbank/devPTIpack/issues/81)):
      standalone Shiny launcher running both validators with
      `error_on_fail = FALSE`, surfacing their statuses in a sidebar,
      and embedding the existing `mod_dta_explorer2_*` Data Explorer
      for visual indicator inspection; 13-test Tier-1+Tier-2 file
      (`tests/testthat/test-app-validate-metadata.R`).
- [x] arch-09 template smoke gate — Tier-1 integration test
      (`tests/testthat/test-template-integration.R`, 34 expectations,
      ~6 s) that gates #83 by walking the template end-to-end:
      `create_new_pti()` → render Step 01 → `validate_geometries()` →
      `validate_metadata()` (simple + advanced xlsx) → `app_validate_*()`
      smoke → `launch_pti()` smoke. Issue
      [#82](https://github.com/worldbank/devPTIpack/issues/82).
- [x] arch-09 PR #D — `compile_pti_data(shp_path, metadata_paths,
      output_dir, error_on_fail)` exported (issue
      [#83](https://github.com/worldbank/devPTIpack/issues/83)):
      merges 1+ intermediate metadata xlsx files, writes canonical
      `metadata.xlsx` + `shapefiles.zip` + `pti-metadata.pdf`, runs
      both validators on the combined inputs, prints a verbose CLI
      summary, returns the validator-style structured result.
      `inst/metadata.Rmd` rewritten — parameterised, dropped
      `pacman` / `here` / heavy plotting stack; one-map-per-indicator
      using `gg_admin_list()` + ggplot2. `inst/template_pti/05-compile.qmd`
      now a working step (no longer a stub). `DESCRIPTION` Imports
      gains `zip (>= 2.3)`. 23-test file
      (`tests/testthat/test-compile-pti-data.R`).
- [ ] Confirm `R CMD check` builds vignettes cleanly.
- [ ] **After this phase:** add `shinytest2` automation for Tier 3.

### arch-09 documentation redesign (PRD [#77](https://github.com/worldbank/devPTIpack/issues/77))

- [x] Restructure `_pkgdown.yml` navbar to "About / PTI Methodology / Build a PTI"
      with Steps 0–6 dropdown (arch-09 §2, [#78](https://github.com/worldbank/devPTIpack/issues/78)).
- [x] Drop manual `reference:` groupings from `_pkgdown.yml` (arch-09 §1, §7.1);
      `@family`-tag migration tracked in [#75](https://github.com/worldbank/devPTIpack/issues/75).
- [x] Create 8 stub vignettes in `vignettes/articles/` (Steps 0–6 + `overview-paper`)
      per arch-09 §3–§4 ([#78](https://github.com/worldbank/devPTIpack/issues/78)).
  - Issue #78 — PR draft on `docs/navbar-restructure` (commit pending stop-hook autodraft).
- [x] arch-09 PR #E — write Steps 0, 1, 2 (stub), 3 vignette prose
      (issue [#84](https://github.com/worldbank/devPTIpack/issues/84)):
      Rwanda worked example throughout, `eval=FALSE` code blocks,
      condensed column-requirement tables in Steps 1 and 3 with links
      to `dataprep.qmd`, advanced multi-level subsection in Step 1,
      simple + multi-level walkthroughs in Step 3, both validator UX
      flows (`validate_*` + `app_validate_*`) wired into Steps 1 and 3.
- [x] arch-09 PR #F — rewrite `build-pti.qmd` overview + write Steps 5–6 vignettes
      (issue [#85](https://github.com/worldbank/devPTIpack/issues/85)):
      `build-pti.qmd` collapsed from 9-section walkthrough to short journey-map
      overview (input contract table, < 5-line Rwanda quickstart, 7-step journey
      table linking out, `00-master.R` pointer, where-to-start FAQ); Step 5
      written against the merged `compile_pti_data()` (inputs / outputs / CLI
      summary / structured return / failure-recovery flow / pre-launch checklist
      / `launch_pti()` tab tour); Step 6 written end-to-end (deployment-target
      table, permissions setup, CLI + UI walkthroughs, post-deploy permissions
      + custom URL, optional GitHub Pages, help links).
- [x] arch-09 final integration audit — pkgdown links, R CMD check,
      user sign-off (issue [#86](https://github.com/worldbank/devPTIpack/issues/86)).
      Automated checks: `pkgdown::build_site()` clean; all cross-references
      resolve; 26/26 R code blocks across 8 vignettes parse; NAMESPACE consistent;
      `R CMD check` 0 errors / 0 warnings / 3 notes (down from 1/2/4 — remaining
      notes pre-existing). Real bug found and fixed: `inst/template_pti/05-compile.qmd`
      had `#| eval: false` so `00-master.R`'s Step 05 silently no-op'd despite
      PR #98's claim. End-to-end smoke now produces all 4 expected `app-data/`
      artefacts.
- [x] Companion issue §7.2 — Rwanda package datasets
      (issue [#76](https://github.com/worldbank/devPTIpack/issues/76)):
      added `data/rwa_shp.rda` (3 layers, 36 polygons) and
      `data/rwa_mtdt_full.rda` (3 indicators across 2 pillars), both
      compiled by `data-raw/generate-rwa-package-data.R` from the
      template's bundled GeoJSONs + synthetic xlsx; `R/data.R` documents
      both with `@format`, `@source`, `@examples`. Migrated 11 exported
      functions' `@examples` blocks to use Rwanda data
      (`launch_pti{,_onepage}`, `run_pti_pipeline`, `gg_admin_list`,
      `get_rand_weights`, `get_all_weights_combs`, `get_pti_weights_export`,
      `get_shape`, `validate_geometries`, `app_validate_shp`,
      `app_validate_metadata`). Updated `.claude/rules/roxygen-documentation.md`
      + `.claude/CLAUDE.md` to prefer `rwa_*` for new examples. Ukraine
      datasets unchanged — test suite continues to use them.
- [x] Companion issue §7.1 — `@family`-tag migration + reference-page
      restructure (issue [#75](https://github.com/worldbank/devPTIpack/issues/75)):
      added `@family` tags to all 37 exported functions + 4 datasets
      (41 inserts across 25 files), organised into 10 concept groups
      (`pti-launch` / `pti-pipeline` / `data-input` / `weights` /
      `data-export` / `validation` / `visualisation` / `shiny-modules` /
      `package-utilities` / `sample-data`). Added a `reference:` block
      to `_pkgdown.yml` driven by `has_concept()` selectors — the
      reference index page now auto-groups by family with descriptive
      titles. Applied `@inheritParams` to collapse the `error_on_fail`
      / `shp_path` / `mtdt_path` doc duplication across `validate_*`
      siblings (3 sites). `R CMD check` stays at 0 / 0 / 3.
- [x] R CMD check NOTES cleanup (flagged in PR #99 audit, deferred as
      follow-up): License stub fixed (LICENSE -> LICENSE.md with full
      MIT text; new LICENSE DCF stub with YEAR + COPYRIGHT HOLDER);
      removed `inst/template_pti/app-data/.gitkeep` (Step 1/3/5
      `dir.create()` calls handle the directory creation on first
      run); cleaned 7 unused Imports — removed 4 truly-unused
      (`bsplus`, `config`, `tippy`, `pkgload`) and moved 3 to Suggests
      (`testthat`, `here`, `quarto`). **`R CMD check` is now 0 errors
      / 0 warnings / 0 notes.** Suite stays at PASS 803 / FAIL 0 /
      SKIP 1 / ERROR 12 (environmental).

### arch-10 Step 1 shapefiles enhancement ([#104](https://github.com/worldbank/devPTIpack/issues/104))

Step 1 vignette + two new exported geometry helpers + Rwanda
package-data rebuild. Spec:
[`arch-10-step1-shapefiles-enhancement.md`](.github/docs/arch-10-step1-shapefiles-enhancement.md).

- [x] arch-10 §1 -- fix six documentation gaps in
      `vignettes/articles/build-pti-1-shapefiles.qmd` (issue
      [#106](https://github.com/worldbank/devPTIpack/issues/106), this PR):
      `area` km² fix in §E; `admin0_Country` mandatory callout +
      simple-example inclusion; `saveRDS(..., compress = "gz")`;
      non-contiguous level numbers note; `admin<N>Name` uniqueness +
      no-NA rules in requirements table; `<HumanName>`
      no-spaces / no-colons constraint; `validate_geometries()`
      blind-spots note (CRS mismatch, area units, topological
      validity, coverage gaps).
- [ ] arch-10 §2 -- implement `make_hex_grid()` + tests (issue
      [#108](https://github.com/worldbank/devPTIpack/issues/108)).
- [ ] arch-10 §3 -- implement `make_admin_lookup()` + tests (issue
      [#109](https://github.com/worldbank/devPTIpack/issues/109);
      blocked by #108).
- [ ] arch-10 §6 -- Step 1 vignette §E rewrite + new §F hex
      workflow (issue
      [#111](https://github.com/worldbank/devPTIpack/issues/111);
      blocked by #108 + #109).
- [ ] arch-10 §5 -- rebuild `rwa_shp` with hex layer; propagate
      `shapes.rds` through Steps 2-5 (issue
      [#114](https://github.com/worldbank/devPTIpack/issues/114);
      blocked by #109 + #111).

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
| [#41](https://github.com/worldbank/devPTIpack/pull/41)  | 2026-05-02 | Phase 2 Batch 2 | arch-01 Batch 2 — delete whole file `R/mod_pti_onepage.R` (both `mod_pti_onepage_ui` + `mod_pti_onepage_server`), trim `R/run_app.R` to keep only `run_new_pti` (delete `run_pti`/`run_onepage_pti`/`run_onepage_pti_sample`/`run_dev_map_pti`/`run_dev_pti_plot`), trim `R/app_server.R` to keep only `app_new_pti_server` (delete `app_server`/`app_server_input_simple`/`app_server_sample_pti_vis`), trim `R/app_ui.R` to keep `app_new_pti_ui` + `golem_add_external_resources` (delete `app_ui`/`app_server_sample_pti_vis_ui`). 7 NAMESPACE exports dropped via `roxygen2::roxygenise()`. Suite stays at 682 PASS — no regression. |
| [#43](https://github.com/worldbank/devPTIpack/pull/43)  | 2026-05-02 | Phase 2 Batch 3 | arch-01 Batch 3 — extract `mod_map_pti_leaf_ui` to new file `R/mod_map_pti_leaf_ui.R`, delete whole file `R/mod_map_pti_leaf.R` (1258 lines, all 12 legacy server functions), delete `make_shapes()` + `make_labels()` from `R/fct_legend_map_satelites.R`. Patched `app_new_pti_ui` to use modern `mod_pti_comparepage_ui` instead of deleted `mod_map_pti_leaf_page_ui` (Batch-4 mini-patch folded in to avoid `R CMD check` undefined-symbol note). Relocated 2 `@importFrom` tags (`leaflet::colorFactor` → `legend_map_satelite`; `leaflet::addTiles` → `plot_leaf_line_map2`) auto-pruned from NAMESPACE because their roxygen lived in the deleted file. Suite stays at 682 PASS — no regression. |
| [#44](https://github.com/worldbank/devPTIpack/pull/44)  | 2026-05-03 | Phase 2 Batch 4 | arch-01 Batch 4 — rewrote `inst/sample_pti/app.R` to call `launch_pti()` directly (loads shapes via `get_shape()` and metadata via `fct_template_reader()`; drops `default_adm_level`, `choose_adm_levels`, `explorer_*`, `full_ui`, `pti_landing_page` knobs that have no modern equivalent — `launch_pti` would need a signature extension to preserve them). Deleted `run_new_pti` (whole `R/run_app.R`), `app_new_pti_server` (whole `R/app_server.R` — file `git rm`'d), `app_new_pti_ui` (kept only `golem_add_external_resources` in `R/app_ui.R`), and `mod_plot_pti_comparison_srv` from `R/mod_plot_pti2.R`. Relocated 5 `@importFrom` tags auto-pruned from NAMESPACE because their roxygen lived in the deleted files: `shiny::shinyApp`, `shiny::navbarPage`, `shiny::tabPanel`, `golem::with_golem_options` → `launch_pti_onepage`; `shiny::reactiveValues` → `mod_wt_inp_ui`. Folded in the long-pending Batch 3 `TBD → #43` swap on the §11 row above. Suite stays at 682 PASS — no regression. |
| [#45](https://github.com/worldbank/devPTIpack/pull/45)  | 2026-05-03 | Phase 2 Batch 5 | arch-01 Batch 5 — extracted `mod_wt_btns_srv` (~67 lines) and `mod_collect_wt_srv` (~20 lines) to a new file `R/mod_wt_btns_collect.R` with `@noRd` roxygen and explicit `@importFrom` tags; both are still used by `mod_DT_inputs_server` (the only modern caller). Deleted whole file `R/mod_weights.R` (928 lines, 13 zero-caller legacy functions: `mod_weights_ui`, `mod_weights_server`, `mod_indicarots_srv`, `mod_gen_wt_inputs_srv`, `mod_wt_name_srv`, `mod_wt_select_srv`, `mod_wt_uplod_srv`, `mod_wt_delete_srv`, `mod_wt_fill_srv`, `mod_wt_save_srv`, `mod_download_wt_srv`, plus the originals of the 2 extracted functions). Deleted whole file `R/mod_new_weights.R` (112 lines, `mod_new_demo_weights_server` + `mod_new_weights_server` — only caller `mod_pti_onepage_server` was removed in Batch 2). Net diff ~970 lines net removed (largest single batch in Phase 2). NAMESPACE delta: dropped 1 export (`mod_weights_ui`); added 4 imports from the new file (`purrr::map2`, `purrr::map_dfr`, `shiny::updateNumericInput`, `tibble::tibble`); no auto-prune fallout. Folded in the long-pending Batch 4 `TBD → #44` swap on the §11 row above and on the §5 Batch-4 row. Suite stays at 682 PASS — no regression. |
| [#46](https://github.com/worldbank/devPTIpack/pull/46) | 2026-05-03 | **Phase 2 Batch 6 (closes Phase 2)** | arch-01 Batch 6 — convenience-wrapper deletion. Departure from the doc's "Optional / deprecate first" framing: caller-graph audit showed both targets had no live external surface so a deprecation cycle would be bureaucratic. Deleted whole file `R/mod_explrr_onepage.R` (39 lines, `mod_explrr_onepage_ui` + `mod_explrr_onepage_server`, exported thin wrappers over `mod_dta_explorer2_*` — only callers in `dev/90-app-examples.R`, itself broken since Batch 2 and slated for arch-04 Phase 4 deletion). Deleted whole file `R/render_metadata_pdf.R` (14 lines, `render_metadata` exported wrapper over `rmarkdown::render`) — already broken on shipped installs because `system.file("pti-metadata-pdf.Rmd", package = "devPTIpack")` resolves to `""` (the Rmd lives at `inst/sample_pti/app-data/`, not the `inst/` package root); arch-04's reference to `render_metadata()` is one `system.file` keyword away from working and can be reintroduced as a fixed function when arch-04 wants it. Both also violated the project's `@noRd + @export` rule (creates exported functions with no help page). NAMESPACE delta: dropped 3 exports (`mod_explrr_onepage_ui`, `mod_explrr_onepage_server`, `render_metadata`) and 2 importFroms (`here::here`, `rmarkdown::render` — neither used elsewhere in `R/`). Folded in the Batch 5 `TBD → #45` swap on the §11 row above and on the §5 Batch-5 row. **Closes Phase 2 (#8).** Suite stays at 682 PASS — no regression. |
| [#47](https://github.com/worldbank/devPTIpack/pull/47) | 2026-05-03 | **Phase 3 starts** (Batch 1 — package data) | arch-02-docs Phase 3.1 — rewrote `R/data.R` with standalone roxygen2 docs for `ukr_shp` and `ukr_mtdt_full` per the rules-file data template. Replaced the misleading `@describeIn ukr_shp` on `ukr_mtdt_full` (it was inheriting a *geometries* description for a *metadata* object). Documented the real bundled-data shape: `ukr_shp` has 4 admin levels (`admin0_Country` / `admin1_Oblast` (27) / `admin2_Rayon` (629) / `admin4_Hexagon` (1,939); columns `adminNPcod` / `adminNName` / `area` / `geometry`) — diverges from the rules-file example's stale `admin1/2/3` skeleton; `ukr_mtdt_full` has 5 slots (`general` country tibble, 3 per-admin indicator tibbles, `metadata` 9×14 indicator dictionary). Both gain `@source`, `@format` with `\describe{}` per slot, and runnable `@examples` using only bundled data. Two `man/*.Rd` help pages now exist where there was previously one shared inherited page. Folded in the Batch 6 `TBD → #46` swap on the §5 row above and the §11 row above. Suite stays at 682 PASS — no regression. |
| [#48](https://github.com/worldbank/devPTIpack/pull/48) | 2026-05-03 | Phase 3 Batch 2 (core calculation) | arch-02-docs Phase 3.2 — rewrote roxygen for all 12 functions in `R/calc_pti_helpers.R` (6) and `R/calc_pti_expander.R` (6). Honored arch-01 § "Permanent Functions": un-exported the 10 INTERNAL fns and re-typed them with the `@noRd` template (typed `@param`, explicit `@return`, no `@examples`); kept `label_generic_pti` and `generic_pti_glue` exported with synthetic-input `@examples` (real input is now from internal `agg_pti_scores`, so the example builds a minimal scores list directly). Promoted `generic_pti_glue` from `@describeIn label_generic_pti` to standalone. Pinned 3 §12 bugs via `@note` tags: `get_adm_levels` lex sort, `get_scores_data` 1-row → NA, `expand_adm_levels` >1 element silent NULL. Stripped pre-existing debug residue per arch-01:436 (~50 lines: 2 `# browser()`, 2 large commented-out alternate implementations in `structure_pti_data`, several stray comment lines). NAMESPACE delta: dropped 9 `export()` (`agg_pti_scores`, `clean_geoms`, `expand_adm_levels`, `get_adm_levels`, `get_mt`, `get_scores_data`, `get_weighted_data`, `pivot_pti_dta`, `structure_pti_data`); added 7 `importFrom`s (`dplyr::bind_rows`, `magrittr::extract`/`extract2`, `rlang::set_names`/`sym`, `tidyr::expand`, `golem::get_golem_options`) that lost their previous bulk-import coverage. Test-side: converted 7 `devPTIpack::fn()` qualified calls in 3 pre-existing test files (`test-shps-converters.R`, `test-calc_pti.R`, `test-weighting-logic.R`) to unqualified — needed because `::` requires export and the tests' `pkgload::load_all()` exposure is unaffected. Suite stays at 682 PASS — no regression. |
| [#49](https://github.com/worldbank/devPTIpack/pull/49) | 2026-05-03 | Phase 3 Batch 3 (data I/O & validation) | arch-02-docs Phase 3.3 -- rewrote roxygen for 11 functions across 5 files: `R/fct_template_reader.R` (`fct_template_reader` exported, `fct_convert_weight_to_clean` internal), `R/fct_validate_metadata.R` (`validate_metadata` + `validate_read_shp` + `validate_read_metadata`, all exported), `R/validators.R` (`validate_geometries` exported, `validate_single_geom` un-exported per arch-01), `R/dta_cleaners.R` (`get_indicators_list` un-exported per arch-01), `R/mod_drop_inval_adm.R` (`mod_drop_inval_adm` module + `get_vars_un_avbil` + `get_min_admin_wght` + `drop_inval_adm`, all exported -- folded in here for thematic coherence with the validation theme). Honored arch-01: closes 3x `@noRd + @export` rule violations (`validate_single_geom`, `validate_geometries`, `get_indicators_list`); promoted 5 functions from `@describeIn` chains to standalone exported help pages (`validate_read_shp`, `validate_read_metadata`, `get_vars_un_avbil`, `get_min_admin_wght`, `drop_inval_adm`). Pinned 2 §12 bugs via `@note`: `validate_read_shp` empty-pattern `str_detect` (issue #7), `get_vars_un_avbil` asymmetric `lag()` fill (PR #34); the second `@note` flipped my initial example to use the working direction (admin2-only -> admin1 unavailable) instead of the buggy direction. Stripped 4 `# browser()` debug-residue lines per arch-01:436 (`fct_template_reader.R` x2, `validators.R`, `dta_cleaners.R`) plus an 8-line commented-out runtime `test_that` block in `validators.R` replaced by the live `if (is.na(adm_order)...)` branch. **Kept the `@import dplyr purrr stringr readxl` bulk-import directive on `fct_template_reader`** -- the `pkgload::load_all()` test environment relies on the bulk import to keep `mod_DT_inputs_server`'s 500ms throttle test deterministic across `testServer` calls (subtle shiny scheduler interaction; explicit-import-only triggers a cross-test reactive-state leak in the throttle Tier-2 test). Replaced `admin\d` regex notation in roxygen prose with `admin<N>` to avoid Rd "Lost braces" warnings; replaced em-dashes and arrows with `--` and `->` per the Batch 2 reviewer ASCII fix. NAMESPACE delta: dropped 2 `export()` (`get_indicators_list`, `validate_single_geom`); kept 4 bulk `import()` directives intact. Test-side: converted 7 `devPTIpack::get_indicators_list()` qualified calls in 2 pre-existing test files ([test-calc_pti.R](tests/testthat/test-calc_pti.R) x4, [test-weighting-logic.R](tests/testthat/test-weighting-logic.R) x3) to unqualified, and qualified 5 unqualified `tribble(` calls in [test-get_uavailab_admin.R](tests/testthat/test-get_uavailab_admin.R) to `tibble::tribble(`. Folded in the Batch 2 `TBD -> #48` swap on the §6 row and the §11 row above. Suite stays at 682 PASS -- no regression. |
| [#50](https://github.com/worldbank/devPTIpack/pull/50) | 2026-05-04 | Phase 3 Batch 4 (visualisation helpers) | arch-02-docs Phase 3.4 -- rewrote roxygen for 12 functions across `R/plot_pti_helpers.R` (10) and `R/fct_legend_map_satelites.R` (2). All 12 are INTERNAL per arch-01 § "Permanent Functions" / "Visualisation Helpers"; batch un-exports every one and converts to the `@noRd` internal template. Closes 4 `@noRd + @export` rule violations (`get_current_levels`, `filter_admin_levels`, `add_legend_paras`, `legend_map_satelite`; `recode_val_base` was already correct). Promoted 4 `@describeIn plot_pti_polygons` chains to standalone `@noRd` docs (`clean_pti_polygons`, `add_pti_poly_controls`, `clean_pti_poly_controls`, `check_existing_groups`) -- the chain was acceptable while all 5 were exported but becomes meaningless once nothing has a help page. Pinned 3 §12 bugs via `@note` (all PR #25, all in `plot_pti_helpers.R`): `filter_admin_levels` name-vs-value asymmetry, `complete_pti_labels` missing-assignment no-op (priority-rank suffix never appended in production), `check_existing_groups` empty-pattern `str_detect` error. Stripped 4 `# browser()` debug-residue lines per arch-01:436 (`plot_pti_helpers.R` x2, `fct_legend_map_satelites.R` x2). Left larger commented-out alternate-implementation blocks in `fct_legend_map_satelites.R` alone -- arch-01's "remove commented-out blocks" mandate names specific files and that file is not on the list. Fixed `@describeIn plot_pti_polygons plot_pti_polygons` typo on `add_pti_poly_controls`. Package-source fix: converted one `devPTIpack::get_current_levels()` qualified call in `R/mod_export_pti_data.R::get_pti_scores_export` to unqualified (the `::` form requires export and now errors after the un-export); 9 export-data tests broke and went green again after this one-line fix. NAMESPACE delta: dropped 11 `export()`; added 7 `importFrom`s (`leaflet::addLayersControl` / `clearGroup` / `layersControlOptions` / `showGroup`; `purrr::map2_chr`; `shiny::HTML`; plus minor re-localisations from explicit-import sweeps). Test-side: no qualified-call conversions needed -- the existing `devPTIpack:::legend_map_satelite` triple-colon calls in `tests/testthat/test-legend-mapping.R` keep working (`:::` resolves regardless of export status); other test files use unqualified calls that resolve via `pkgload::load_all()`. Folded in the Batch 3 `TBD -> #49` swap on the §6 row above and the §11 row above. Suite stays at 682 PASS -- no regression. |
| [#51](https://github.com/worldbank/devPTIpack/pull/51) | 2026-05-04 | Phase 3 Batch 5 (entry points & app infrastructure) | arch-02-docs Phase 3.5 -- rewrote roxygen for 5 functions across 4 files: `R/launch_pti.R` (`launch_pti_onepage` + `launch_pti`), `R/fct_create_new_pti.R` (`create_new_pti`), `R/app_config.R` (`app_sys`), `R/app_ui.R` (`golem_add_external_resources` -- folded in here for thematic coherence with the entry-points theme; arch-01:24 classifies it as EXPORTED but it lived in app_ui.R behind `@noRd`). All 5 are EXPORTED per arch-01 § "Entry Points & App Scaffolding" -- this batch is the inverse shape of Batches 2-4 (no un-exports; one promote-to-exported on `golem_add_external_resources`). Honored the rules-file Exported template: typed `@param`, `@return`, `@importFrom`, `@export`, runnable or `\dontrun{}` `@examples`. Closes 1 `@noRd + @export` rule violation (`app_sys`) and 1 `@noRd`-on-an-EXPORTED-fn arch-01 violation (`golem_add_external_resources`). Promoted the `@describeIn launch_pti_onepage` chain on `launch_pti` to a standalone help page -- the two apps diverge in usage (one-page is a focused viewer, multi-tab pulls in compare + explorer + cicerone tour), and only `launch_pti` takes the `tabs` and `app_name` parameters. Examples: `app_sys` runs unwrapped (`app_sys("app", "www")`); `create_new_pti` runs unwrapped via a `tempdir()` scaffold with `open = FALSE` (rstudioapi gates skip cleanly outside RStudio); `launch_pti` / `launch_pti_onepage` / `golem_add_external_resources` use `\dontrun{}` per the rules-file "side-effect-heavy code" clause. Stripped 1 debug-residue line (`R/launch_pti.R:173` -- a commented-out `observe(cat("tab: ", active_tab(), "\n"))` instrumentation). No pinned §12 bugs touched this batch. Reviewer-iteration adjustments: dropped a stale `@importFrom bsplus use_bs_tooltip` directive on `golem_add_external_resources` (carried from the `@noRd` era but the body never called it); flattened a `[add_logo()]` cross-ref to plain backticks since `add_logo()` is `@noRd` (avoids an "Rd cross-reference to non-existent topic" R CMD check note); added the `tabs` default to its `@param` description. NAMESPACE delta: added 1 export (`golem_add_external_resources`); added 2 `importFrom`s (`cicerone::use_cicerone`, `rlang::dots_list`); dropped 1 stale `importFrom` (`bsplus::use_bs_tooltip`). Test-side: no qualified-call conversions needed -- nothing was un-exported; the new `golem_add_external_resources` export strictly widens the public surface. Folded in the Batch 4 `TBD -> #50` swap on the §6 row and the §11 row above. Suite stays at 682 PASS -- no regression. |
| [#52](https://github.com/worldbank/devPTIpack/pull/52) | 2026-05-04 | Phase 3 Batch 6a (PTI rendering & display stack) | arch-02-docs Phase 3.6a -- rewrote roxygen for 44 functions across 16 files in the rendering stack: `R/mod_ptipage_core.R` (3), `R/mod_pti_comparepage.R` (2), `R/mod_calc_pti2.R` (2), `R/mod_dta_explorer2.R` (8), `R/mod_export_pti_data.R` (3), `R/mod_load_shapes.R` (2), `R/mod_plot_pti2.R` (1), `R/mod_plot_init_leaf.R` (2), `R/mod_plot_poly_leaf.R` (4), `R/mod_plot_poly_legend.R` (3), `R/mod_pti_map_side_pan.R` (7), `R/mod_map_pti_leaf_ui.R` (1), `R/mod_dwnld_dta.R` (3), `R/mod_dwnld_local_file.R` (1), `R/run_pti_pipeline.R` (1; cross-ref cleanup only), `R/mod_fetch_data.R` (1). Honored arch-01 § "Permanent Functions": un-exported 15 INTERNAL fns (`filter_var_explorer`, `get_var_choices`, `make_gg_line_map`, `make_ggmap`, `mod_dta_explorer2_side_ui`, `mod_fltr_sel_var2_srv`, `mod_plot_init_leaf_server`, `mod_plot_leaf_export`, `mod_plot_poly_leaf_server`, `mod_plot_poly_legend_server`, `mod_select_var_ui`, `plot_leaf_line_map2`, `plot_pti_legend`, `remove_pti_legend`, `reshaped_explorer_dta`); promoted 3 EXPORTED fns from `@noRd` to `@export` (`mod_leaf_side_panel_ui`, `mod_map_pti_leaf_ui`, `mod_pti_comparepage_newsrv` -- closes 3 arch-01 violations; the `mod_pti_comparepage_newsrv` one was missed by the scope inventory and surfaced when roxygen2 wrote out the new NAMESPACE). Promoted 14 `@describeIn` members to standalone docs across the chains in `mod_dta_explorer2`, `mod_plot_init_leaf`, `mod_plot_poly_leaf`, `mod_plot_poly_legend`; split `mod_ptipage_newsrv` out of the UI chain since server vs UI diverged in usage (Batch 5 `launch_pti` precedent). Pinned 2 §12 bugs via `@note` -- `get_var_choices` empty-indicators NULL-attr (PR #27), `mod_fltr_sel_var2_srv` multi-var-pillar predicate (PR #37); the second `@note` had to escape `\%in\%` and avoid raw `~ {` brace literals to keep the Rd parser happy. Stripped 6 debug-residue lines per arch-01:436: 4 `# browser()` in `mod_plot_poly_leaf.R` (one in `mod_plot_leaf_export`, three in `make_ggmap` / `make_gg_line_map`), 1 in `mod_pti_map_side_pan.R` (PNG download handler), 1 in `mod_dta_explorer2.R` (`mod_fltr_sel_var2_srv` `add_selected` observer); plus the 14-line commented-out `withProgress` / `tempfile` / `mapview` block in `mod_plot_leaf_export` (alternate impl that was never re-enabled). Left the larger commented-out alternate-implementation blocks in `mod_pti_map_side_pan.R` (chromote / mapshot / webshot2 PNG path, ggsave alternative; ~50 commented lines total) and the `radioButtons` / `adm_lvl_debounce` alternate path in `mod_get_admin_levels_srv` alone -- arch-01's "remove commented-out blocks" mandate names specific files and `mod_pti_map_side_pan.R` is not one of them. Examples: `get_shape` runs unwrapped via the `shape_dta = ukr_shp` short-circuit; `get_pti_weights_export` runs unwrapped via `get_indicators_list(ukr_mtdt_full)` + `get_rand_weights()`; UI / server modules use `\dontrun{}` per the rules-file "Shiny modules" clause; `get_pti_scores_export` uses `\dontrun{}` because reproducing its expected `plotted_dta` shape requires the full reactive chain (preplot + drop_inval_adm + filter_admin_levels + add_legend_paras + complete_pti_labels). NAMESPACE delta: dropped 15 `export()`; added 3 `export()`; added 5 `importFrom`s (`purrr::pmap_dfr`, `shiny::fillPage`, `shiny::incProgress`, `shiny::uiOutput`, `shiny::withProgress`) that lost their previous bulk-import coverage. Test-side: no qualified-call conversions needed -- the existing `devPTIpack:::*` triple-colon calls in tests resolve regardless of export status; no `devPTIpack::` calls existed for any of the 15 un-exported fns. Cross-ref cleanup: flattened `[mod_calc_pti2_server()]`, `[get_indicators_list()]`, `[agg_pti_scores()]` cross-refs in `run_pti_pipeline` to plain backticks since those targets are `@noRd` (avoids "Rd cross-reference to non-existent topic" R CMD check notes per the Batch 5 reviewer fix). No prior-batch TBD swap to fold in -- Batch 5 (#51) merged with the PR number already in place. Suite stays at 682 PASS -- no regression. |
| [#53](https://github.com/worldbank/devPTIpack/pull/53) | 2026-05-04 | **Phase 3 Batch 6b (closes Phase 3 -- #11)** | arch-02-docs Phase 3.6b -- audited 37 functions across 12 files closing the Phase 3 sweep (35 fns across 11 files rewritten in this PR; 2 fns in `mod_wt_btns_collect.R` already at standard from Batch 5 extraction PR #45, no edits): `R/mod_wt_inp.R` (14), `R/mod_DT_inputs.R` (7), `R/mod_wt_btns_collect.R` (2; verified-only), `R/mod_first_open_count.R` (1), `R/mod_tab_open.R` (1), `R/mod_waiter.R` (3), `R/mod_weights_rand.R` (3), `R/mod_infotab.R` (1), `R/fct_guide.R` (1), `R/fct_helpers.R` (1; `add_logo`), `R/fct_inp_for_exp.R` (2), `R/supporting-goe-prep.R` (1; `gg_admin_list`). Honored arch-01 § "Permanent Functions": un-exported 2 INTERNAL fns (`fct_inp_for_exp`, `fct_internal_wt_to_exp` -- arch-01:166-167 classifies both INTERNAL but they were exported via the `@noRd + @export` rule violation; un-exporting closes the violation and aligns NAMESPACE with arch-01); kept 4 EXPORTED fns exported by dropping their `@noRd` tags (`mod_tab_open_first_newserv`, `gg_admin_list`, `get_rand_weights`, `get_all_weights_combs` -- closes 4 more `@noRd + @export` rule violations without moving NAMESPACE). Net 6 rule-violations closed. Promoted the 14-fn `@describeIn mod_wt_inp_ui` chain to standalone `@noRd` docs (Batch 4 precedent for full-chain split when umbrella + members are all INTERNAL); promoted the 3-fn `@describeIn mod_waiter_newsrv` chain similarly; split `get_rand_weights` and `get_all_weights_combs` out of the `@describeIn mod_weights_rand_ui` chain so their EXPORTED help pages stand alone. Pinned 1 §12 bug via `@note`: `fct_internal_wt_to_exp(list())` left-join error (PR #26). Stripped 8 `# browser()` debug-residue lines per arch-01:436 (4 in `mod_DT_inputs.R`, 1 in `mod_wt_inp.R`'s upload-stub `mod_wt_uplod_newsrv` body, 3 in `supporting-goe-prep.R`); also stripped the 8-line commented-out `observeEvent(update_dta())` alternate-impl block in `mod_DT_inputs.R` (contained an embedded `browser()`; the live `observe(...)` block above implements the same logic without the debugger trap). Three scope-proposal-time corrections vs the user's prompt-time hypotheses, surfaced and corrected at the gate before any roxygen edit: (1) `fct_inp_for_exp` + `fct_internal_wt_to_exp` are arch-01-INTERNAL not EXPORTED, so the close direction was drop-`@export` not drop-`@noRd`; (2) `mod_infotab.R` had no `# browser()` at line ~71 (and none anywhere); (3) total browser-strip count was 8 lines + 1 alt-impl block, not the 1 implied. NAMESPACE delta: dropped 2 `export()` (`fct_inp_for_exp`, `fct_internal_wt_to_exp`); added a batch of explicit `importFrom`s on the rewritten functions that lost bulk-import coverage when their `@describeIn` chains dispersed (notable: `ggplot2::aes`/`geom_sf`/`ggplot`/`labs`/`theme`/`theme_minimal` localised onto `gg_admin_list`; `dplyr::case_when`/`if_any`/`right_join`; `purrr::flatten`/`imap_dfr`/`pmap_chr`/`pwalk`; ~15 new `shiny::*` symbols including `actionLink`/`fileInput`/`isolate`/`modalDialog`/`renderUI`/`throttle`/`updateSelectInput`/`verbatimTextOutput`). Test-side: no qualified-call conversions needed -- existing unqualified `fct_inp_for_exp(...)` / `fct_internal_wt_to_exp(...)` calls in `tests/testthat/test-export.R` resolve via `pkgload::load_all()` regardless of export status. Folded in the Batch 6a `TBD -> #52` swap on the §6 row above and the §11 row above. **Closes Phase 3 (#11).** Suite stays at 682 PASS -- no regression. |
| [#56](https://github.com/worldbank/devPTIpack/pull/56) | 2026-05-06 | **Phase 2.5 §12 (bug-fix #1)** | §12 bug #1 fixed -- `R/plot_pti_helpers.R::complete_pti_labels` now assigns its `purrr::map()` result back to `dta`, so popups receive the `<strong>{priority_rank}</strong>` suffix the function was already computing but discarding. Test pin in `tests/testthat/test-plot-helpers.R` flipped from no-op (asserting the bug) to contract assertion (each output label equals the original concatenated with `<strong>{recode_function(pti_score)}</strong>`). Stripped the `@note Pinned bug (PR #25)` block from the function's roxygen. Confirmed no caller depended on the broken behaviour: production caller `R/mod_plot_pti2.R:64` already piped through expecting the augmented labels; test setup `tests/testthat/test-map-render.R:16` only asserts `s3_class(g, "gg")` and never inspects label text. Folded in the Batch 6b `TBD -> #53` swap on the §6 row above and the §11 row above. Suite stays at 682 PASS -- no regression. |
| [#57](https://github.com/worldbank/devPTIpack/pull/57) | 2026-05-06 | **Phase 2.5 §12 (bug-fix #5)** | §12 bug #5 fixed -- `R/calc_pti_helpers.R::get_adm_levels` now sorts admin-level identifiers numerically (`admin1 < admin2 < admin10`) instead of lexicographically (`admin1 < admin10 < admin2`). Body rewritten from `dta %>% names() %>% str_extract("admin\\d{1,2}") %>% sort() %>% set_names(.)` to a 4-line form that filters NAs first, then permutes by `as.integer(str_extract(ids, "\\d{1,2}"))` via `order()`. Test pin in `tests/testthat/test-calc-pipeline.R` flipped from `c("admin1", "admin10", "admin2")` (lex) to `c("admin1", "admin2", "admin10")` (numeric); `test_that()` description renamed from `"sort is lexicographic, not numeric (PINNED)"` -> `"sort is numeric across mixed-digit levels"`. Stripped the 5-line `@note Pinned bug` block + retitled function description from "sorted" to "numerically-sorted" + clarified `@return` to mention NA dropping. Caller-graph audit confirmed no production caller depended on the lex order: `R/mod_plot_init_leaf.R:86` (`%in%`) and `R/fct_validate_metadata.R:51` are order-agnostic; `R/calc_pti_expander.R:44`'s outer `imap()` iteration order doesn't affect the inner per-pair logic. For the only data ever shipped (`ukr_shp` admin0/1/2/4, all single-digit), lex and numeric sort produce identical results -- this fix is a no-op on production data and only changes behaviour at ≥10 admin levels (the bug case). Out of scope: the separate first-digit-only quirks in `expand_adm_levels` (`str_extract(col, "\\d")` -- single digit not `\\d{1,2}`) and `agg_pti_scores` (`max(level)` as a string) -- pre-existing latent issues, not §12 row #5; could be added as new §12 rows in a follow-up. Folded in the PR-#56 `TBD -> #56` swap on the §11 row above and on the §12 row #1. Suite stays at 682 PASS -- one expect_equal swapped 1-for-1, no count delta. |
| [#58](https://github.com/worldbank/devPTIpack/pull/58) | 2026-05-06 | **chore (PLAN scaffold)** | Phase-2.5 sprint scaffold — added a "Status snapshot" prose paragraph at the top of §1 calling out current focus + 2/13 progress; expanded §2 execution-order tree with the closed Phase-3 batch list (Batches 1, 2, 3, 4, 5, 6a, 6b → ✓ done) and inserted a new "Phase 2.5 §12 bug-fix sprint (concurrent)" node between Phase 3 and Phase 4 listing fixed (#1 #56, #5 #57) and queued (#6 next, then #7 #10, then user-facing #2 #4 #8 #9 #11 #13, last #3 #12) bugs; added a "Status: 2 of 13 fixed" rollup paragraph above the §12 table summarising done + triage queue. Folded in the PR-#57 `TBD -> #57` swap on the §11 progress-log row dated 2026-05-06 and on the §12 row #5 in the same commit. Pure-docs PR (no R/ or tests/ touched); R-CMD-check workflow gates merge. Suite stays at 682 PASS -- not invoked, no test code touched. |
| [#59](https://github.com/worldbank/devPTIpack/pull/59) | 2026-05-06 | **Phase 2.5 §12 (bug-fix #6)** | §12 bug #6 fixed -- `R/calc_pti_helpers.R::get_scores_data` now treats singleton `(year, var_code)` groups as zero-variance (no scale to apply): non-`NA` values are set to the neutral score `0` instead of falling through `sd()`-of-length-1 -> `NA`. Body kept the existing standardise step but added a precomputed `.was_na` flag plus a `dplyr::if_else(dplyr::n() == 1L & !.was_na, 0, value)` patch before the `is.nan` -> `0` rewrite (input-`NA` rows are preserved as `NA`). First attempt used a single nested `dplyr::if_else` on `dplyr::n() == 1L` directly but failed strict size-checking (scalar condition rejected vector branches in `vec_check_size`); restructured to length-`n` condition. Test pin in `tests/testthat/test-calc-pipeline.R` flipped from `expect_true(is.na(out$scheme$adm$value))` to `expect_equal(out$scheme$adm$value, 0)`; `test_that()` description renamed from `"1-row groups produce NA, not 0 (PINNED)"` -> `"1-row groups produce 0 (no variance to scale)"` and the inline pinned-bug comment replaced with a one-line rationale. Stripped the 5-line `@note Pinned bug` block from the function's roxygen + tightened the description prose to enumerate both no-variance paths (singleton + `n>1` all-identical). Caller-graph audit confirmed no production caller depended on the `NA` behaviour: `R/mod_calc_pti2.R:81`, `R/run_pti_pipeline.R:64`, and `R/fct_validate_metadata.R:59` are pipe links that don't inspect cardinalities; `tests/testthat/helper-test-data.R:39` builds the `test_scored` fixture which fed the existing `mean ~= 0` test (line 172) — its `is.na(means$m)` filter now becomes a no-op rather than masking the bug. NAMESPACE delta: added `dplyr::if_else` and `dplyr::n` `importFrom` lines (qualified-call convention). Out of scope (interpretation Y at the design gate): "effectively `n=1`" groups (`n>1` with only one non-`NA`) -- `sd()` is still `NA` there, so the lone value comes out `NA`. Pinned test only constrains literal `n=1`; could be a follow-up §12 row if a consumer needs it. Folded in the PR-#58 `TBD -> #58` swap on the §11 row above as commit 1. Suite stays at 682 PASS (FAIL=0, SKIP=1) -- one expect_equal swapped 1-for-1, no count delta. |
| [#60](https://github.com/worldbank/devPTIpack/pull/60) | 2026-05-06 | **Phase 2.5 §12 (bug-fix #7 + boundary follow-up)** | §12 bug #7 fixed plus a bundled boundary-regex follow-up on the same function (new 14th §12 row) -- `R/calc_pti_expander.R::expand_adm_levels` now (a) errors loudly via `stop("more than one slot in 'wtd_scrd_dta' matches admin level '{adm_from}': {names}", call. = FALSE)` when >1 input slot resolves to a single level, instead of silently returning nested NULLs for the entire source-loop iteration; and (b) anchors slot-name matching as `^{adm_from}(_|$)` so iterating with `adm_from = "admin1"` no longer also picks up slots named `"admin10_..."` / `"admin11_..."` (latent (d) from prior sessions). The two changes share the same surface — one regex and one length check on slot names — so bundling them makes the rewrite atomic and avoids the row-7 multi-match error masking the boundary bug as a false positive. Test pin in `tests/testthat/test-calc-pipeline.R` flipped from `for (tgt in out$admin1) expect_null(tgt)` (4 expectations) to `expect_error(..., regexp = "admin1")` (1 expectation); `test_that()` description renamed from `"expand_adm_levels: >1 element matches a level -> NULLs (PINNED)"` -> `"expand_adm_levels: >1 slot matches a level -> error"`. Added a new test `"expand_adm_levels: slot regex is boundary-anchored (admin1 vs admin10)"` that builds a `list(admin1_Oblast = src1, admin10_Other = src1)` input and asserts `out$admin1` is non-null (boundary regex selects only the admin1 slot, doesn't trip the new multi-match error). Stripped the 6-line `@note When length(adm_from_dta_full) > 1 ...` block from the function's roxygen + rewrote the `@return` clause from "leaves are NULL when ... or the adm_from slot has more than one element matching its level pattern" to "leaves are NULL when no slot matches or the matched slot is empty; errors when more than one slot matches a single admin level". Caller-graph audit confirmed no production caller depended on the silent-NULL short-circuit: `R/mod_calc_pti2.R:82`, `R/run_pti_pipeline.R:65`, and `R/fct_validate_metadata.R:60` all pipe through to `merge_expandedn_adm_levels()` which would have produced empty merges from the silent NULLs (functionally already broken downstream); for the only data ever shipped (`ukr_shp` admin0/1/2/4, all single-digit), `wtd_scrd_dta` always has exactly one slot per level, so both new guards are unreachable on production data — this fix is a no-op on bundled fixtures and only changes behaviour on schema-violating or ≥10-admin-level deployments. Out of scope: the related single-`\\d` `str_extract` quirks at `R/calc_pti_expander.R:79-80` and `R/calc_pti_helpers.R::agg_pti_scores:237` -- same first-digit-only family but on different code paths (column-name parsing, not slot-name lookup); candidates for a future "first-digit-only sweep" §12 row. Folded in the PR-#59 `TBD -> #59` swap on the §11 row above (already complete in upstream). Suite drops from 682 PASS -> 680 PASS (FAIL=0, SKIP=1) -- net -2 expectations from the 4-`expect_null` -> 1-`expect_error` flip, +1 from the new boundary test = -2. |

| [#61](https://github.com/worldbank/devPTIpack/pull/61) | 2026-05-06 | **Phase 2.5 §12 (bug-fix #10)** | §12 bug #10 fixed -- `R/mod_drop_inval_adm.R::get_vars_un_avbil` now treats availability strictly: a `(var_code, admin_level)` pair surfaces as unavailable iff the indicator has no native data at that admin level. Body shrank from ~25 lines (lex `arrange()` + triple `lag()` back-fill + `is.na(value & !any_larger)` operator-precedence typo + `lead()`-based `any_larger` flag) to ~10 lines (`expand.grid` + `tibble::as_tibble` + `dplyr::anti_join` over the distinct `(var_code, admin_level)` set). Pre-fix: an indicator with data only at admin1 was silently `lag()`-filled into admin2 / admin4 rows, so admin2 and admin4 were never surfaced as unavailable; reverse direction (admin2-only -> admin1) worked because admin1 had no `lag`. Post-fix: both directions surface symmetrically. Decision at the gate: contract X (strict no extrapolation) over Y (aggregate-up only) and Z (bidirectional) -- X matches the existing `weighting an unavailable var produces drops` test (`var_nval4_small_skewd_adm4` admin4-only -> admin1 + admin2 dropped) and avoids re-encoding extrapolation that `expand_adm_levels` already handles in the calc pipeline. NAMESPACE delta: dropped `dplyr::lead`, `dplyr::lag`, `dplyr::group_by`, `dplyr::count`, `dplyr::rename`, `dplyr::mutate`, `dplyr::filter`, `dplyr::left_join`, `dplyr::arrange` from the `@importFrom` (the lag/lead machinery is gone); added `dplyr::distinct`, `dplyr::anti_join`, `tibble::as_tibble`. Dropped `"any_larger"` from `R/devPTIpack-package.R::globalVariables()` -- the column is no longer in the output, and `grep` confirmed no caller (in `R/` or `tests/`) read it. Roxygen: stripped the 8-line `@note Asymmetry pinned in PR #34` block; rewrote title from "Identify (variable, admin-level) pairs with no data and no upstream coverage" -> "Identify (variable, admin-level) pairs with no native data"; rewrote `@return` to drop the `any_larger` mention. Tier-1 test added in `test-drop-inval-adm.R`: `"get_vars_un_avbil: flags missing levels symmetrically"` builds a synthetic 3-indicator fixture (admin1-only / admin2-only / both) and asserts admin1-only surfaces at admin2 (the previously-broken direction), admin2-only surfaces at admin1, and the both-levels indicator surfaces nowhere. Tier-2 test added in `test-mod-drop-inval-adm.R`: `"mod_drop_inval_adm: indicator missing at admin2 -> admin2 removed"` -- reverse-direction sibling of the existing `missing at admin1 -> admin1 removed` test, replacing the prior 8-line "Note on symmetry. ... Not testing it here to avoid coupling this Tier-2 module test to a Tier-1 bug." comment block. Two pre-existing tests had to be updated to match the new contract: (a) `test-drop-inval-adm.R::"get_min_admin_wght: a fully-available var drops nothing"` -- swapped the picked var from `var_nval3_skewd_adm1` (admin1-only; the prior comment claimed it was "available at every admin level in the bundled fixture (cross-checked via probing)" but that probe was probing the broken function) to `var_nvalinf_unif_adm124`, the only fixture indicator with native data at every admin level (admin1 + admin2 + admin4); (b) `test-get_uavailab_admin.R::"get unavailable adming levels works"` -- bumped the asserted nrow() from 7 (the lag-fill bug under-counted the admin1-only direction) to 12 (9 indicators x 3 admin levels - 15 cells with native data) with an inline rationale comment. Caller-graph audit: `get_min_admin_wght` only `pull(admin_level)` from the result (`R/mod_drop_inval_adm.R:226`), so dropping the `any_larger` column is safe. Folded in the PR-#60 `TBD -> #60` swap on the §11 row above and on the §12 rows 7 + 14 (already complete in upstream). Suite goes from 680 PASS -> 683 PASS (FAIL=0, SKIP=1) -- net +2 test_that blocks (Tier-1 symmetric + Tier-2 reverse) plus 6 new individual expectations across the existing tests' contract updates. |

| [#62](https://github.com/worldbank/devPTIpack/pull/62) | 2026-05-06 | **Phase 2.5 §12 (bug-fix #2)** | §12 bug #2 fixed -- `R/plot_pti_helpers.R::check_existing_groups` now early-handles `length(old_grps) == 0` instead of erroring inside `stringr::str_detect(., character(0))` with a vctrs `pattern must have size 1` error. Body wraps the existing `check_this_too` / lookup-by-pattern block in an `if (length(old_grps) > 0)` guard; on empty input `grps_in <- character(0)`, falling through to the pre-existing "first of remaining" fallback at L457-459 which arch-03 §1.6 already specified as the empty-`old_grps` contract. Test pin in `tests/testthat/test-plot-helpers.R` flipped from `expect_error(..., regexp = "size")` (asserting the bug) to two `expect_equal`s asserting the contract: `out$out_show == "a (Country)"` (first of current) and `out$out_hide == "b (Oblast)"`. `test_that()` description renamed from `"empty old errors via str_detect (PINNED)"` -> `"empty old -> first of current shown"`; comment block rewritten to describe the contract instead of the past bug. Stripped the 5-line `@note Pinned bug (PR #25)` block from the function's roxygen. Caller-graph audit: only call site is `R/plot_pti_helpers.R:357` inside `add_pti_poly_controls`, gated by `isTruthy(old_grps)` at L356 -- so the empty-input path never fired in production today; this fix is a no-op on the deployed app and only changes behaviour for direct callers (tests). No NAMESPACE delta -- `str_detect` still used inside the guard. Folded in the PR-#61 `TBD -> #61` swap on the §11 row above and on the §12 row #10 (already complete in upstream). Suite goes from 683 PASS -> 684 PASS (FAIL=0, SKIP=1) -- net +1 expectation: 1 expect_error swapped for 2 expect_equal. |

| [#64](https://github.com/worldbank/devPTIpack/pull/64) | 2026-05-06 | **Phase 2.5 §12 (bug-fix #4)** | §12 bug #4 fixed -- `R/fct_validate_metadata.R::validate_read_shp` no longer hits the `str_detect(names(x), character(0))` vctrs size error when called on a "perfect" shapefile (every `admin{N}Pcod` field has a matching `admin{N}_*` slot). Body refactored: compute `extra_level_vec` first (without the `str_c collapse`), then guard the diagnostic-label construction (`str_c collapse`, `keep`, `str_detect on names`) with `if (length(extra_level_vec) > 0)`; on empty extras, set both label strings to `""` and let the (already correct) `expect_true(all(... %in% ...))` pass cleanly. Stripped the 5-line `@note Issue #7 ...` block from the function's roxygen. Test housekeeping: the existing pin in `tests/testthat/test-validators.R` (`"validate_read_shp: round-trips through an .rds path (PINNED BUG)"`) used `capture_validator()` which mocks `testthat::test_that` to a no-op, so the inner blocks never executed and the test passed trivially both pre- and post-fix; the PR renames it to `"validate_read_shp: perfect shapefile passes round-trip"` and rewrites the comment to drop the misleading PINNED BUG language and explain why the existing infrastructure can't pin the bug rigorously. Surfaced this caveat at the scope-proposal gate -- the user accepted the rename-only test change. The fix is verified by code inspection: only the previously-broken empty-extras branch changed, and the end-to-end `validate_metadata: bundled sample data passes end-to-end` test (which composes both `validate_read_shp` and `validate_read_metadata`) still passes. Caller-graph: only `validate_metadata` at `R/fct_validate_metadata.R:35`. No NAMESPACE delta. Out of scope: the broader arch-01 refactor target -- converting the runtime `testthat::test_that` machinery into ordinary validation calls -- bigger scope, would obsolete `capture_validator()` entirely; left for a future PR. Folded in the PR-#62 `TBD -> #62` swap on the §11 row above and on the §12 row #2 (already complete in upstream). Suite stays at 684 PASS (FAIL=0, SKIP=1) -- rename-only on the existing pin, no test count delta. |

| [#65](https://github.com/worldbank/devPTIpack/pull/65) | 2026-05-06 | **Phase 2.5 §12 (bug-fix #8)** | §12 bug #8 fixed -- `R/fct_inp_for_exp.R::fct_internal_wt_to_exp` now early-returns a 0-row tibble matching its documented `@return` schema (`var_code`, `var_name`, `weight`, `weight_scheme`) when `weights_clean` is `list()`. Pre-fix: `purrr::imap_dfr(list())` produced a 0x0 tibble (no columns), so the downstream `dplyr::left_join(., indicators_list %>% select(var_code, var_name), by = "var_code")` errored with "Join columns in 'x' must be present in the data: 'var_code'". Test pin in `tests/testthat/test-export.R` flipped from `expect_error(..., regexp = "var_code")` (1 expectation, asserting the bug) to three contract assertions: `expect_s3_class(out, "tbl_df")`, `expect_equal(nrow(out), 0L)`, `expect_setequal(names(out), c("var_code", "var_name", "weight", "weight_scheme"))`. `test_that()` description renamed from `"empty list errors at left_join (PINNED)"` -> `"empty list returns a 0-row tibble with the full schema"`. Stripped the 7-line `@note PINNED BUG (PLAN.md §12) ...` block from the function's roxygen. Caller-graph: single call site at `R/mod_wt_inp.R:914` inside `mod_wt_dwnload_newsrv`; production never feeds an empty list (download button only fires after at least one scheme is saved) -- this fix is a no-op on the deployed app and only changes behaviour for direct callers. NAMESPACE delta: added `tibble::tibble` to the function's `@importFrom` (used by the early-return constructor). The sibling test `"fct_internal_wt_to_exp: empty scheme tibble -> 0-row tibble"` already covered the n>0-but-each-empty case; this fix completes the empty-input contract by handling the n=0 case symmetrically. Folded in the PR-#64 `TBD -> #64` swap on the §11 row above and on the §12 row #4 (already complete in upstream). Suite goes from 684 PASS -> 686 PASS (FAIL=0, SKIP=1) -- net +2 expectations (1 expect_error swapped for 3 contract assertions). |

| [#66](https://github.com/worldbank/devPTIpack/pull/66) | 2026-05-06 | **Phase 2.5 §12 (bug-fix #9)** | §12 bug #9 fixed -- `R/mod_dta_explorer2.R::get_var_choices` now early-returns `list()` when `indicators_list` has 0 rows, instead of falling through to a NULL-`out` rescue branch that errored at `names(out) <- "Indicators"` with "attempt to set an attribute on NULL". One-paragraph guard at the top of the function: `if (nrow(indicators_list) == 0) return(list())`. Test pin in `tests/testthat/test-explorer-helpers.R` flipped from `expect_error(..., regexp = "attribute on NULL")` (1 expectation, asserting the bug) to two contract assertions: `expect_type(out, "list")` + `expect_length(out, 0L)`. `test_that()` description renamed from `"empty indicators tibble errors (PINNED)"` -> `"empty indicators tibble returns an empty list"`; comment block rewritten to describe the contract instead of the past bug. Stripped the 6-line `@note **Pinned bug (PLAN.md §12, PR #27)** ...` block from the function's roxygen. Caller-graph: single call site at `R/mod_dta_explorer2.R:76` inside `mod_dta_explorer2_server`, gated by `req(indicators_list())` -- so the empty path never fired in production today; this fix is a no-op on the deployed app and only changes behaviour for direct callers (tests). Downstream consumer `shinyWidgets::updatePickerInput(choices = list())` handles an empty list cleanly. No NAMESPACE delta. Sibling-test consistency: this fix follows the same defensive-empty-input pattern as PR #65 (`fct_internal_wt_to_exp(list())` -> 0-row tibble) -- both adopted at the scope-proposal gate as small surgical guards rather than full input-validation refactors. Folded in the PR-#65 stragglers from the wrapped-line TBD swap that the previous chore-commit's sed missed (PLAN.md §1 status-snapshot prose around the line break). Suite goes from 686 PASS -> 687 PASS (FAIL=0, SKIP=1) -- net +1 expectation (1 expect_error swapped for 2 contract assertions). |

| [#67](https://github.com/worldbank/devPTIpack/pull/67) | 2026-05-06 | **Phase 2.5 §12 (bug-fix #11)** | §12 bug #11 fixed -- `R/mod_dta_explorer2.R::mod_fltr_sel_var2_srv` now wraps each `%in%` test inside the `add_selected()` observer's `purrr::map_lgl()` predicate in `any(...)`: `any(.x %in% c(selected_add, selected_now)) | any(.x %in% names(c(selected_add, selected_now)))`. Pre-fix the predicate evaluated `.x %in% ...` directly, returning a length-N logical for multi-var pillars and tripping `map_lgl`'s length-1 contract with "Result must be length 1, not N". Two-character-pair surgical edit. Caller-graph: single call site at `R/mod_dta_explorer2.R:78` inside `mod_dta_explorer2_server`; `add_selected` defaults to `reactive(NULL)` and the observer is gated by `req(add_selected())`, so the latent path never fired in production -- this fix is a no-op on the deployed app today and only changes behaviour for direct callers (tests + future programmatic pushes). Test pin in `tests/testthat/test-mod-var-selector.R` flipped from a predicate-only `expect_error` (which evaluated a *copy* of the buggy expression, not the function -- so the assertion would have stayed true post-fix forever) to a proper Tier-2 `testServer` block: same shape as the existing single-var sibling, but with `multi_pillar = list("Pillar A" = c("Var Name 1" = "var_1", "Var Name 2" = "var_2"), "Pillar B" = c("Var Name 3" = "var_3"))`; pushes `add_selected("var_2")` and asserts `updatePickerInput` is called once with `inputId = "indicators"` and `selected` named `"Pillar A"` (the multi-var pillar that contains var_2). `test_that()` description renamed from `"add_selected() with multi-var pillar fails the inner predicate (PINNED BUG)"` -> `"add_selected() with multi-var pillar selects the matching pillar"`. Updated the explanatory comment block at L178-187 to drop the "happy-path uses single-var-per-pillar" caveat -- both directions are now exercised. Stripped the 9-line `@note **Pinned bug (PLAN.md §12, PR #37)** ...` block from the function's roxygen. No NAMESPACE delta. Folded in nothing -- prior PR #66 self-swapped its TBD before merge. Suite goes from 687 PASS -> 689 PASS (FAIL=0, SKIP=1) -- net +2 expectations: predicate-only `expect_error` (1) replaced with three `testServer` assertions (`expect_gt`, `expect_equal` on inputId, `expect_named` on selected). |

| [#68](https://github.com/worldbank/devPTIpack/pull/68) | 2026-05-07 | **Phase 2.5 §12 (bug-fix #13 -- Arch-2)** | §12 bug #13 fixed under **Arch-2** (auto-materialize). Two coupled changes: **(1)** `R/mod_dwnld_dta.R::mod_dwnld_file_server` now validates `filepath` at registration; on invalid input calls `shinyjs::disable(outputId)` and ships an explanatory `unavailable-<date>.txt` placeholder via the content callback (not a silent broken `.html`). On valid input the existing `basename(filepath)` filename builder is preserved. **(2)** New private helper `R/launch_pti.R::materialize_dwnld_paths(shp_dta, inp_dta, shapes_path, mtdtpdf_path, data_path)` writes in-memory `shp_dta` / `inp_dta` to session-scoped tempfiles when the corresponding paths are `NULL` (`pti-shapes-<date>.rds` via `saveRDS()`, `pti-data-export-<date>.xlsx` via `writexl::write_xlsx()`); PDF stays `NULL` since there's no in-memory equivalent. Defaults on `launch_pti()` and `launch_pti_onepage()` flipped from `"."` to `NULL` for `shapes_path` and `mtdtpdf_path`. Added new `data_path` parameter on `launch_pti()` and threaded it to `mod_dta_explorer2_server` -- closes a separate latent bug where `data_path` defaulted to NULL in the server but `launch_pti` never plumbed it through, making the explorer's "Download data" button unreachable. Architectural rationale (raised by the user at the scope-proposal gate): "users shouldn't need to know about disk paths to deploy an app -- if we have the data in memory, we should be able to serve it." Iteration history: an interim attempt used `shinyjs::hide()` plus `setdiff()`-ing `"metadata"` from `wt_dwnld_options` / `map_dwnld_options` to also clean up the orphaned " and ." connector text; the connector logic lives in three side-panel renderers (PTI / compare / explorer) and the conditionals didn't reach all of them cleanly, so reverted to plain `disable()` + placeholder. Cosmetic " and ." sentence quirk remains when no PDF is supplied; flagged as out-of-scope follow-up. New Tier-1 + Tier-2 test file `tests/testthat/test-mod-dwnld-file.R` (6 test_that blocks, 13 expectations): two cover `materialize_dwnld_paths()` (NULL fallbacks vs. explicit paths), four cover `mod_dwnld_file_server` (valid file leaves link enabled; NULL / directory / nonexistent paths trigger `shinyjs::disable`). NAMESPACE delta: added `importFrom(writexl, write_xlsx)`. Manually verified by the user via `launch_pti(shp_dta = ukr_shp, inp_dta = ukr_mtdt_full)` -- shapes + data downloads work, metadata-PDF link is greyed out. Folded in the PR-#67 `TBD -> #67` swap on the §11 row above and on the §12 row #11 (already complete in upstream). Suite goes from 689 PASS -> 702 PASS (FAIL=0, SKIP=1) -- net +13 expectations from the new test file. |

| [#69](https://github.com/worldbank/devPTIpack/pull/69) | 2026-05-07 | **Phase 2.5 §12 (bug-fix #3)** | §12 bug #3 fixed -- `R/plot_pti_helpers.R::filter_admin_levels` now treats admin keys and display values symmetrically inside the `keep()` predicate. Pre-fix: the gating branch (L116-117) entered when `to_fltr` matched either keys or values, but the inner `keep(function(x) {x$admin_level %in% to_fltr})` predicate compared `x$admin_level` (the display value, e.g. `"Oblast"`) against `to_fltr` only -- so `to_fltr = "admin1"` (a bare key) entered the branch but selected 0 entries. Fix: extend the predicate to `x$admin_level %in% to_fltr | names(x$admin_level) %in% to_fltr`. Mirrors how `mod_get_admin_levels_srv` already filters its own internal state at L237-238 (`names(.) %in% default_adm_level | (.) %in% default_adm_level`). Test pin in `tests/testthat/test-plot-helpers.R` flipped from `expect_equal(length(filter_admin_levels(preplot, "admin1")), 0L)` (1 expectation, asserting the bug) to a sharper symmetric-contract assertion `expect_equal(by_name, by_value)` (1 expectation) -- key-only and value-only filters must produce the *same* result, not just the same length. `test_that()` description renamed from `"name-only filter returns 0 entries (PINNED)"` -> `"name-only filter matches the same entries as value-only"`; comment block rewritten to describe the contract instead of the past bug. Reviewer caution applied: original proposal used a length-check + `for` loop; tightened on r-package-reviewer's nice-to-have to a direct equality check that more sharply expresses the symmetric contract. Stripped the 6-line `@note Pinned bug (PR #25)` block from the function's roxygen. Caller-graph: 2 production call sites (`R/mod_dta_explorer2.R:99`, `R/mod_plot_pti2.R:62`), both pass `sel_adm_levels()` (a named character vector from `mod_get_admin_levels_srv`); the values branch already matched in production so this fix is a no-op on the deployed app and only changes behaviour for direct callers (tests + future callers). No NAMESPACE delta. Folded in nothing -- prior PR #68 self-swapped its TBD before merge. Suite stays at 702 PASS (FAIL=0, SKIP=1) -- 1 `expect_equal` swapped 1-for-1, no count delta. |

| [#70](https://github.com/worldbank/devPTIpack/pull/70) | 2026-05-07 | **Phase 2.5 §12 (bug-fix #12 -- closes the sprint)** | §12 bug #12 fixed -- `R/supporting-goe-prep.R::gg_admin_list` default `mt = zam_bounds_simple` flipped to `mt = NULL` with an explicit `stop("'mt' is required: ...", call. = FALSE)` guard at the top of the function (option A from the scope-proposal gate; B = make `mt` required-no-default; C = default to `ukr_shp` -- both rejected, A gives the clearest failure mode without coupling to a bundled dataset). Pre-fix: calling `gg_admin_list(dta, metadata)` without explicit `mt` errored at runtime with `object 'zam_bounds_simple' not found` because `zam_bounds_simple` doesn't exist anywhere in `R/`, `data/`, or `inst/`. The broken default was silenced via `R/devPTIpack-package.R::globalVariables()` so R CMD check stayed clean -- this PR also drops `"zam_bounds_simple"` from `globalVariables()`. Updated `@param mt` doc to mention required-ness. New Tier-1 test file `tests/testthat/test-gg-admin-list.R` (2 test_that blocks, 8 expectations): missing `mt` -> `expect_error(regexp = "mt.*required")`; with `mt = ukr_shp` -> non-empty list of ggplot objects. Caller-graph: 2 production callers (`inst/metadata.Rmd:129`, `inst/sample_pti/app-data/pti-metadata-pdf.Rmd:108`) both pass `mt = bounds` explicitly -- pure latent bug. Two commented-out demo lines in those Rmds (omitting `mt`) almost certainly date from someone hitting this exact error and commenting them out. **This PR closes the Phase 2.5 §12 bug-fix sprint -- 14 of 14 bugs fixed across 14 PRs (#56, #57, #59, #60 [#7 + #14], #61, #62, #64, #65, #66, #67, #68, #69, plus this one).** Folded in nothing -- prior PR #69 self-swapped its TBD before merge. Suite goes from 702 PASS -> 710 PASS (FAIL=0, SKIP=1) -- net +8 expectations from the new test file. |

| [#79-draft](https://github.com/worldbank/devPTIpack/issues/79) | 2026-05-08 | **arch-09 PR #A2 — template scaffold + Rwanda data (draft)** | Issue #79 -- branch `feat/template-scaffold` off `eb-docs-pkgdown`. Built out `inst/template_pti/` per arch-09 §5: downloaded Rwanda GeoJSONs from geoBoundaries (Adm0=1 / Adm1=5 / Adm2=30 polygons, CC-BY 4.0); added `inst/template_pti/data-raw/generate-synthetic-metadata.R` (seeded `set.seed(42)`, deterministic; verified by re-run + `identical()` round-trip); generated `sample-metadata-adm1.xlsx` and `sample-metadata-adm1-adm2.xlsx` (3 indicators: poverty_rate, literacy_rate, road_density). Added template `.qmd` files: `01-shapes.qmd` (working: load GeoJSONs, attach `admin<N>Pcod`/`admin<N>Name`/`area`, centroid spatial-join to derive admin1 parent on admin2, `validate_geometries` -> `app-data/shapes.rds`); `02a-user-zonal-stats.qmd` (optional stub); `03-metadata.qmd` (working: `fct_template_reader` + `validate_metadata` against `app-data/shapes.rds`, copies workbook to `app-data/metadata-user.xlsx`); `04-hex-data.qmd` (HEX-API stub); `05-compile.qmd` (stub pending #83); `06-deploy.R` (manual `rsconnect::deployApp()`). Added `00-master.R` orchestrator (renders 01 + 03 by default; 02a / 04 / 05 / 06 commented). Updated `inst/template_pti/app.R` to load from `app-data/shapes.rds` + `app-data/metadata.xlsx` with the bundled `ukr_*` data shown as commented Option B fallback. Added `README.md` (file order, links to all 7 website tutorials, `app-data/` git-tracking warning, deferred-TODO list). Updated `inst/template_pti/.gitignore` to track `app-data/` + `data-raw/` by default. Added force-include exception in top-level `.gitignore` so `inst/template_pti/data-raw/` ships (overrides the package-wide `data-raw/` ignore). Smoke-tested: `create_new_pti(tempfile())` copies all 19 expected files; `00-master.R` Steps 01+03 render end-to-end via knitr::purl+source fallback (quarto subprocess fails on `library(devPTIpack)` outside an installed package -- documented limitation, not a defect of the template), producing `app-data/shapes.rds` (3.5 MB) + `app-data/metadata-user.xlsx`; `validate_geometries()` and `validate_metadata()` both return `status = "pass"` with 0 failures / 0 warnings on Rwanda inputs. Divergence noted in changelog: validator app calls (`app_validate_shp` in 01, `app_validate_metadata` in 03) commented pending #80 / #81; `compile_pti_data()` in 05-compile.qmd commented pending #83; 00-master.R does not render 04 (HEX API) or 05 by default. NOT pushed; commit on `feat/template-scaffold` only -- per the issue brief, no PR opened. |

Suite total after this branch: **0 failures / 1 skip / 710 PASS** (`testthat::test_local()`; bug-fix PR adds a new Tier-1 test file with 2 test_that blocks / 8 expectations covering the `mt`-required error and the bundled-data success path).

---

**Phase 2.5 §12 sprint summary** (closed 2026-05-07): 14 bug-fix
PRs landed across 12 calendar days (2026-05-06 through
2026-05-07; PR #56 to PR #70). Every pinned bug from §12 has been
resolved with a contract-asserting test in place of the original
bug-asserting pin. Suite grew from a 682 PASS baseline to 710
PASS via a mix of pin flips and new defensive-input test files
(`test-mod-dwnld-file.R` +13, `test-gg-admin-list.R` +8). Two
follow-up cosmetic items remain documented as out-of-scope:
the orphaned " and ." connector text in the side-panel sentence
when no `mtdtpdf_path` is supplied (PR #68 narrative), and the
broader arch-01 refactor target -- converting the runtime
`testthat::test_that` machinery in `validate_*` helpers into
ordinary validation calls (PR #64 narrative).

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

> Bugs surfaced during Tier-1 test writing **or** during the Phase 2.5
> R-CMD-check / manual-smoke gate. Pre-Phase-2.5 entries are pinned
> with `expect_*` assertions on the current (broken) behaviour so the
> tests fail when a future fix lands — at which point the assertion is
> updated to the new contract. Phase-2.5-discovered entries (rows 12+)
> have no test pin yet; pinning is a sub-task of the eventual fix PR.
> All entries are cleanup-phase candidates regardless of pin status.

**Status: 14 of 14 fixed -- sprint complete.** Done: #1
`complete_pti_labels` (PR #56), #5 `get_adm_levels` (PR #57), #6
`get_scores_data` (PR #59), #7 `expand_adm_levels` (PR #60) plus
the boundary-regex follow-up on the same function (PR #60; the
14th §12 row), #10 `get_vars_un_avbil` symmetric availability
(PR #61), #2 `check_existing_groups` empty-old (PR #62), #4
`validate_read_shp` empty-pattern (PR #64), #8
`fct_internal_wt_to_exp` empty list (PR #65), #9 `get_var_choices`
empty indicators (PR #66), #11 `mod_fltr_sel_var2_srv` multi-var
pillar (PR #67), #13 `mod_dwnld_file_server` broken downloads +
`launch_pti()` path-default API redesign (PR #68; Arch-2), #3
`filter_admin_levels` name-vs-value asymmetry (PR #69), and #12
`gg_admin_list` undefined-default (PR #70). Phase 2.5 closed --
next track is Phase 4 (#12, vignettes & pkgdown) or Phase 5
(#13, hex ingestion).

| Loc | Bug | Pin (test) |
|---|---|---|
| [`R/plot_pti_helpers.R::complete_pti_labels`](R/plot_pti_helpers.R#L186-L206) | The function mapped over `dta` but never assigned the result; returned the original `dta` unchanged, so the deployed app silently missed the `<strong>{priority_label}</strong>` suffix on every popup. One-line fix: assign the `purrr::map()` result back to `dta`. **FIXED in PR #56 (2026-05-06).** | [test-plot-helpers.R:complete_pti_labels: appends <strong>{priority_rank}</strong> per entry](tests/testthat/test-plot-helpers.R) |
| [`R/plot_pti_helpers.R::check_existing_groups`](R/plot_pti_helpers.R) | Errored with a vctrs size error when `old_grps = character(0)` — `str_detect(string, character(0))` is invalid. arch-03 §1.6 contract is "first of currently shown" on empty input, and the function already had that branch at the bottom; the bug was that the body errored before reaching it. Fix: guard the `check_this_too` / lookup-by-pattern block with `if (length(old_grps) > 0)`, falling through to the existing first-of-remaining branch on empty input. **FIXED in PR #62 (2026-05-06).** Caller `add_pti_poly_controls` is already gated by `isTruthy(old_grps)` (`R/plot_pti_helpers.R:356`) so production never reached the broken path; the fix only changes behaviour for direct callers (tests). | [test-plot-helpers.R:check_existing_groups: empty old -> first of current shown](tests/testthat/test-plot-helpers.R) |
| [`R/plot_pti_helpers.R::filter_admin_levels`](R/plot_pti_helpers.R) | Asymmetry: the gating branch entered when `to_fltr` matched either admin keys (e.g. `"admin1"`) or display values (e.g. `"Oblast"`), but the inner `keep()` predicate compared `x$admin_level` (the display value) against `to_fltr` only -- so passing a key alone returned 0 entries. Fix: extend the predicate to also test `names(x$admin_level) %in% to_fltr`, mirroring how `mod_get_admin_levels_srv` already filters its own state at L237-238. **FIXED in PR #69 (2026-05-07).** Caller-graph: 2 production call sites (`R/mod_dta_explorer2.R:99`, `R/mod_plot_pti2.R:62`); both pass `sel_adm_levels()` (a *named* character vector from `mod_get_admin_levels_srv`), so the values branch already matched -- the bug only fired for callers passing bare keys, which production never did. This fix is a no-op on the deployed app and only changes behaviour for direct callers (tests + future callers). | [test-plot-helpers.R:filter_admin_levels: name-only filter matches the same entries as value-only](tests/testthat/test-plot-helpers.R) |
| [`R/fct_validate_metadata.R::validate_read_shp`](R/fct_validate_metadata.R) | Empty-pattern `str_detect` when no admin codes are extra (i.e. the shape file is "perfect"): `str_c(character(0), collapse = "|")` returned `character(0)`, then `str_detect(names(x), character(0))` errored with vctrs `pattern must have size 1`. The error was swallowed by the surrounding `testthat::test_that()` (registered as an `expectation_error` against the inner reporter), so externally the call returned silently — but the inner expectation was failing. Fix: compute `extra_level_vec` first, guard the diagnostic-label construction (`str_c collapse`, `keep`, `str_detect on names`) with `if (length(extra_level_vec) > 0)`; on empty extras, set `extra_level <- ""` and `probl_ms <- ""` and let the (already correct) `expect_true(all(... %in% ...))` pass cleanly. **FIXED in PR #64 (2026-05-06).** Note: the existing test pin (`test-validators.R::validate_read_shp: round-trips through an .rds path (PINNED BUG)`) used `capture_validator()` which mocks `testthat::test_that` to a no-op, so the inner blocks never executed and the test trivially passed both pre- and post-fix; the PR renames the test to drop the misleading "PINNED BUG" tag and rewrites the comment. The fix is verified by code inspection — only the previously-broken empty-extras branch changed, and all surrounding tests (`validate_metadata: bundled sample data passes end-to-end`, etc.) still pass. Out of scope: the broader arch-01 refactor of the runtime-`testthat::test_that` machinery into ordinary validation calls (which would obsolete `capture_validator()` entirely). | [test-validators.R:validate_read_shp: perfect shapefile passes round-trip](tests/testthat/test-validators.R) |
| [`R/calc_pti_helpers.R::get_adm_levels`](R/calc_pti_helpers.R#L34) | Lexicographic `sort()` produced `admin1 < admin10 < admin2`, breaking the iteration order of any deployment with ≥10 admin levels. One-line fix: replace `sort()` with an integer-keyed `order()` permutation (`ids[order(as.integer(str_extract(ids, "\\d{1,2}")))]`), filtering NAs first to preserve existing behaviour. **FIXED in PR #57 (2026-05-06).** Out of scope: separate first-digit-only quirks remain in `expand_adm_levels` (`str_extract(col, "\\d")` — single digit) and `agg_pti_scores` (`max(level)` as a string). | [test-calc-pipeline.R:get_adm_levels: sort is numeric across mixed-digit levels](tests/testthat/test-calc-pipeline.R) |
| [`R/calc_pti_helpers.R::get_scores_data`](R/calc_pti_helpers.R#L218) | 1-row `(year, var_code)` group produced `NA` (not `0`) because `sd()` of length-1 returns `NA` (not `NaN`), so the `is.nan` filter missed. Singleton groups have no variance to scale, so non-`NA` values are now set to the neutral score `0` (matching the existing zero-variance branch); `NA` inputs remain `NA`. Implemented by precomputing a `.was_na` flag, standardising as before, then patching `dplyr::n() == 1L & !.was_na` rows to `0` before the `is.nan` -> `0` rewrite. **FIXED in PR #59 (2026-05-06).** Out of scope (interpretation Y): "effectively n=1" groups (n>1 with all but one row `NA`) — `sd()` over the single non-`NA` is also `NA`, so the lone usable value still comes out `NA`. Could be a follow-up §12 row if a downstream consumer needs it. | [test-calc-pipeline.R:get_scores_data: 1-row groups produce 0 (no variance to scale)](tests/testthat/test-calc-pipeline.R) |
| [`R/calc_pti_expander.R::expand_adm_levels`](R/calc_pti_expander.R) | When >1 list element name matched an admin level (the old `length(...) == 1` guard fell through), the entire source-loop iteration returned nested `NULL`s — silent data loss. Now the function `stop()`s loudly, naming the offending slots, since the input contract is one slot per level (`adminN_HumanName`). **FIXED in PR #60 (2026-05-06).** Caller-graph audit confirmed no production caller depended on the `NULL` short-circuit (`R/mod_calc_pti2.R:82`, `R/run_pti_pipeline.R:65`, `R/fct_validate_metadata.R:60` all pipe through to `merge_expandedn_adm_levels()` which would have produced empty merges from the silent NULLs); for the only data ever shipped (`ukr_shp`), `wtd_scrd_dta` always has exactly one slot per level so the new error path is unreachable. | [test-calc-pipeline.R:expand_adm_levels: >1 slot matches a level -> error](tests/testthat/test-calc-pipeline.R) |
| [`R/calc_pti_expander.R::expand_adm_levels`](R/calc_pti_expander.R#L48-L49) | Slot-name match was substring-based (`stringr::str_detect(names, adm_from)`), so iterating with `adm_from = "admin1"` also picked up slots named `"admin10_..."` / `"admin11_..."` / etc. — same first-digit-only family as the documented quirks at L79-80 and `agg_pti_scores` L237. No-op on bundled `ukr_shp` data (admin0/1/2/4, all single-digit) but a latent failure trigger at ≥10 admin levels: a deployment with both `admin1_*` and `admin10_*` slots would have tripped the new multi-match `stop()` introduced for row 7. Now anchored as `^{adm_from}(_|$)` so `"admin1"` matches only `"admin1_..."`. **FIXED in PR #60 (2026-05-06)** — bundled with the row-7 fix because the two issues live on the same surface (one regex on slot names) and the row-7 multi-match error would have masked the boundary bug as a false positive otherwise. Out of scope: the related single-`\\d` `str_extract` quirks at L79-80 / `agg_pti_scores` L237 remain — candidates for a future "first-digit-only sweep" §12 row. | [test-calc-pipeline.R:expand_adm_levels: slot regex is boundary-anchored (admin1 vs admin10)](tests/testthat/test-calc-pipeline.R) |
| [`R/fct_inp_for_exp.R::fct_internal_wt_to_exp`](R/fct_inp_for_exp.R) | Errored with "Join columns in `x` must be present in the data: 'var_code'" when called with an empty `weights_clean = list()`. Cause: `purrr::imap_dfr(list())` returns a 0x0 tibble (no columns), so the downstream `dplyr::left_join(., indicators_list, by = "var_code")` couldn't find its join key. Fix: early-return a 0-row tibble matching the documented `@return` schema (`var_code`, `var_name`, `weight`, `weight_scheme`) on length-0 input. **FIXED in PR #65 (2026-05-06).** Caller-graph: single call site at `R/mod_wt_inp.R:914` inside `mod_wt_dwnload_newsrv`; production unlikely to feed an empty list (download button only fires after at least one scheme is saved) -- the new behaviour is strictly more defensive. NAMESPACE delta: added `tibble::tibble` to the function's `@importFrom`. | [test-export.R:fct_internal_wt_to_exp: empty list returns a 0-row tibble with the full schema](tests/testthat/test-export.R) |
| [`R/mod_dta_explorer2.R::get_var_choices`](R/mod_dta_explorer2.R) | Errored with "attempt to set an attribute on NULL" when called with an empty `indicators_list`: the `arrange` -> `group_by` -> `nest` -> `pmap` -> `unlist(recursive = F)` chain returned `NULL` on 0-row input, and the rescue branch `names(out) <- "Indicators"` then tried to set a name on `NULL`. Fix: early-return `list()` when `nrow(indicators_list) == 0`. **FIXED in PR #66 (2026-05-06).** Caller-graph: single call site at `R/mod_dta_explorer2.R:76` inside `mod_dta_explorer2_server`, gated by `req(indicators_list())` -- so the empty path never fired in production today; this fix is a no-op on the deployed app and only changes behaviour for direct callers. The downstream consumer `shinyWidgets::updatePickerInput(choices = list())` handles an empty list cleanly (just clears the picker). | [test-explorer-helpers.R:get_var_choices: empty indicators tibble returns an empty list](tests/testthat/test-explorer-helpers.R) |
| [`R/mod_drop_inval_adm.R::get_vars_un_avbil`](R/mod_drop_inval_adm.R) | Asymmetric availability check — pre-fix the body used `arrange(admin_level)` + triple `lag(value)` to back-fill missing rows from earlier-sorted levels, so an indicator with data only at admin1 was silently treated as available at admin2 / admin4 (admin2 was never surfaced as unavailable). The reverse (admin2-only → admin1 unavailable) worked. The same code also had an operator-precedence typo `is.na(value & !any_larger)` (parsed as `is.na((value & !any_larger))`). Now the function is reduced to a strict no-extrapolation contract: a `(var, admin)` pair is unavailable iff the indicator has no native data at that level. Body shrank from ~25 lines to ~10 lines (`expand.grid` + `anti_join` over the distinct `(var_code, admin_level)` set). The `any_larger` column is no longer in the output (no caller read it; also dropped from `globalVariables()`). **FIXED in PR #61 (2026-05-06).** Out-of-scope decision: the package's two extrapolation paths (`expand_adm_levels`'s upward + downward branches) handle aggregation in the calc pipeline itself, so the availability check doesn't need to encode them — keeping it strict matches the existing `weighting an unavailable var produces drops` test (`var_nval4_small_skewd_adm4` admin4-only → admin1 + admin2 dropped) and avoids re-introducing the buggy lag/lead machinery. | [test-drop-inval-adm.R:get_vars_un_avbil: flags missing levels symmetrically](tests/testthat/test-drop-inval-adm.R) + [test-mod-drop-inval-adm.R:mod_drop_inval_adm: indicator missing at admin2 -> admin2 removed](tests/testthat/test-mod-drop-inval-adm.R) |
| [`R/mod_dta_explorer2.R::mod_fltr_sel_var2_srv`](R/mod_dta_explorer2.R) | The `add_selected()` observer's predicate `purrr::map_lgl(choices(), ~ { .x %in% c(selected_add, selected_now) | .x %in% names(c(selected_add, selected_now)) })` errored with "Result must be length 1, not N" whenever any pillar held >1 variable, because `.x` is the named character vector for that pillar and `%in%` returns length-N. Fix: wrap each `%in%` test in `any(...)` -- `any(.x %in% c(selected_add, selected_now)) | any(.x %in% names(c(selected_add, selected_now)))`. **FIXED in PR #67 (2026-05-06).** Caller-graph: single call site at `R/mod_dta_explorer2.R:78` inside `mod_dta_explorer2_server`; `add_selected` defaults to `reactive(NULL)` and the observer is gated by `req(add_selected())`, so the path only fires when the caller passes a non-NULL `add_selected` -- which production never did, so this fix is a no-op on the deployed app and only changes behaviour for direct callers (tests + future programmatic pushes). Test pin in `tests/testthat/test-mod-var-selector.R` flipped from a predicate-only `expect_error` (which exercised a copy of the buggy expression, not the function) to a Tier-2 `testServer` block mirroring the existing single-var test, asserting that `updatePickerInput` is called and `selected = list("Pillar A" = ...)` for an `add_selected("var_2")` push against a multi-var-pillar fixture. | [test-mod-var-selector.R: add_selected() with multi-var pillar selects the matching pillar](tests/testthat/test-mod-var-selector.R) |
| [`R/supporting-goe-prep.R::gg_admin_list`](R/supporting-goe-prep.R) | Default arg `mt = zam_bounds_simple` referenced a name that didn't exist anywhere in `R/` or `data/`. Calling `gg_admin_list(dta, metadata)` without explicit `mt` errored at runtime with `object 'zam_bounds_simple' not found`. Discovered by r-package-reviewer during PR #54 (Phase 2.5 R-CMD-check gate); the broken default was silenced via `R/devPTIpack-package.R::globalVariables()` so R CMD check stayed clean. Fix (option A from the scope-proposal gate): default flipped to `mt = NULL` with an explicit `stop("'mt' is required: provide a named list of sf tibbles (e.g. the bundled 'ukr_shp').", call. = FALSE)` guard at the top of the function. `"zam_bounds_simple"` removed from `globalVariables()`. Updated `@param mt` docs to call out the required-ness. **FIXED in PR #70 (2026-05-07).** Caller-graph: 2 production callers (`inst/metadata.Rmd:129`, `inst/sample_pti/app-data/pti-metadata-pdf.Rmd:108`) both pass `mt = bounds` explicitly -- the default never fired in production, pure latent bug. Two commented-out demo lines in those Rmds (`gg_admin_list(var_to_plot, metadata = metadata_current)` at L242 / L221, both omitting `mt`) almost certainly date from someone hitting this exact error and commenting them out. New Tier-1 test file `tests/testthat/test-gg-admin-list.R` (2 test_that blocks, 8 expectations): missing `mt` -> `expect_error(regexp = "mt.*required")`; with `mt = ukr_shp` -> returns a non-empty list of ggplot objects. | [test-gg-admin-list.R: 2 test_that blocks pinning the error message and the bundled-data success path](tests/testthat/test-gg-admin-list.R) |
| [`R/mod_dwnld_dta.R::mod_dwnld_file_server`](R/mod_dwnld_dta.R) + [`R/launch_pti.R`](R/launch_pti.R) | Filename builder `function() { basename(filepath) }` produced broken downloads when `filepath` was `NULL`, `"."`, or any path that didn't point to a real file. `launch_pti()`'s defaults `mtdtpdf_path = "."` and `shapes_path = "."` made `basename(".") == "."` ⇒ browser fell back to URL path + content-type sniff and saved the empty response as an `.html` file. **All "Download metadata" and "Download shapes" buttons failed identically across PTI / PTI-compare / Data-explorer tabs** in any `launch_pti()` call that didn't supply paths (the typical demo case). The data-explorer's "Download data" button was additionally *never reachable* from the public API -- `data_path` defaulted to `NULL` in `mod_dta_explorer2_server` but `launch_pti` never threaded it through. **FIXED in PR #68 (2026-05-07)** under **Arch-2** (auto-materialize). Two coupled changes: **(1) `mod_dwnld_file_server` validation guard** -- function now validates `filepath` at registration (truthy character, length-1, exists on disk, not a directory). On invalid input it calls `shinyjs::disable(outputId)` to grey the link out, and the content callback ships an explanatory `unavailable-<date>.txt` placeholder if a click sneaks through. On valid input the existing `basename(filepath)` filename builder is preserved (post-Arch-2, the basename comes from a sensible path either way). Note: an earlier iteration of this PR tried `shinyjs::hide()` plus `setdiff()`-ing `"metadata"` out of `wt_dwnld_options` / `map_dwnld_options` to drop the orphaned " and ." connector text from the surrounding sentence -- but the connector logic in `mod_map_dwnld_ui` lives in three places (PTI / compare / explorer side panels) and the conditionals didn't reach all of them cleanly. Reverted to plain `disable()` + placeholder; the cosmetic " and ." remains when no PDF is supplied but no longer ships a broken file. **(2) `launch_pti()` / `launch_pti_onepage()` API redesign** -- added a private helper `materialize_dwnld_paths(shp_dta, inp_dta, shapes_path, mtdtpdf_path, data_path)` that, when a path is `NULL`, writes the in-memory object to a session-scoped tempfile under a date-stamped name (`pti-shapes-<date>.rds` via `saveRDS()`, `pti-data-export-<date>.xlsx` via `writexl::write_xlsx()`). PDF metadata has no in-memory equivalent so `mtdtpdf_path = NULL` stays NULL and the link is disabled downstream. Path defaults flipped from `"."` to `NULL` on both launchers. New `data_path` parameter on `launch_pti()` (the explorer launcher) plumbs through to `mod_dta_explorer2_server`. The deliberately-distinct filename stems (`pti-shapes-<date>.rds`, `pti-data-export-<date>.xlsx`) signal to users that these are derivatives of the in-memory data, not the original source files; users who want to serve original files pass explicit paths and `basename()` returns those. Caller-graph: 5 `mod_dwnld_file_server` call sites untouched at the call site (paths now arrive valid post-materialization); 3 path-threading call sites in `launch_pti` updated. Tier-1 + Tier-2 test file `tests/testthat/test-mod-dwnld-file.R` added (6 test_that blocks, 13 expectations): two cover `materialize_dwnld_paths()` (NULL fallbacks vs. explicit paths), four cover `mod_dwnld_file_server` (valid file leaves link enabled; NULL/directory/nonexistent paths trigger `shinyjs::disable`). NAMESPACE delta: added `importFrom(writexl, write_xlsx)`. Manually verified by the user via `launch_pti(shp_dta = ukr_shp, inp_dta = ukr_mtdt_full)` -- shapes + data downloads work; metadata-PDF link is greyed out. Out of scope: the cosmetic connector-text sentence in `mod_map_dwnld_ui` (could be rewritten to live-render based on path validity, but the tradeoffs got messy fast -- left for a later UI cleanup PR). Also out of scope: the parallel `mod_dwnld_local_file_server` handler used by `mod_map_dwnld_srv` for map-tab metadata/shapes -- separate code path, not affected by the surgical guard. | [test-mod-dwnld-file.R: 6 test_that blocks for materialize_dwnld_paths + mod_dwnld_file_server](tests/testthat/test-mod-dwnld-file.R) |

---

*This plan is a thin tracker. For depth, follow the links in §1.*
