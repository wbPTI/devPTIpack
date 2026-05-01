# devPTIpack — Working Plan

> **Living document.** Tracks current phase, immediate next actions, and shared
> workflow conventions. The authoritative architecture lives under
> [`.github/docs/`](.github/docs/) and the master GitHub tracker is
> [`worldbank/devPTIpack#9`](https://github.com/worldbank/devPTIpack/issues/9).
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
Phase 0  Setup & shared workflow ──────────────────────────────┐
                                                               │
Phase 1  Test baseline for permanent functions     (#10)       │
   ├─ 1a   `run_pti_pipeline()` orchestrator                   │
   ├─ 1b   Tier-1 fixtures + tests for calc pipeline           │
   └─ 1c   Tier-1 tests for I/O, validation, helpers           │
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

**Hybrid suggestion (worth considering):** when writing a Tier-1 test for a
permanent function, also draft its roxygen at the same time. Reading the
function carefully to test it is the ideal moment to document it. We then run
a dedicated "docs sweep" in Phase 3 only for what was missed.

---

## 3. Phase 0 — Setup & shared workflow

Done before substantive work begins. Scoped to enable both collaborators to
use the same conventions.

### 3.1 Branch strategy
- **Question to resolve:** `.claude/CLAUDE.md` declares the working branch as
  `eb-arch-redesign`, but the local repo is on `main` with PLAN.md and
  `.claude/` untracked. **Need confirmation:** keep working on `eb-arch-redesign`
  (one long-lived integration branch with PRs from short-lived sub-branches), or
  switch to per-phase feature branches off `main`?
- Proposal: short-lived branches per phase/batch, PR'd into `eb-arch-redesign`
  (or `main`), each with a passing `R CMD check` and the changelog updated.

### 3.2 Tooling — Claude Code skills & sub-agents (proposed)

Goal: encode the workflow as committed skills under `.claude/` so both
collaborators use it consistently. Concrete proposals (each is one file):

| Skill / agent | Type | Purpose |
|---|---|---|
| `tdd-permanent-fn` | skill | Given a permanent function, drafts a Tier-1 test file using `helper-test-data.R` fixtures, mirrors the test cases listed in arch-03 / arch-02.01. |
| `cleanup-batch` | skill | Executes one batch from arch-01: deletes the listed files/functions, removes NAMESPACE exports, runs `devtools::document()` + `devtools::test()`, reports diff stats. |
| `roxygen-document` | skill | For a given file, fills in roxygen per `.claude/rules/roxygen-documentation.md`, runs `devtools::document()`, validates examples against `devtools::check()`. |
| `changelog-logger` | hook (Stop) | Reminds/auto-appends an entry to `.github/docs/changelog.md` after edits — already declared compulsory in CLAUDE.md but currently relies on memory. |
| `r-package-reviewer` | sub-agent | Reviews diffs for R-package conventions: NAMESPACE coherence, `@export` presence, examples runnable, no `library()` calls inside R/, no `browser()`, etc. |
| `issue-progress-comment` | skill | After a logical milestone (e.g. a test file lands, a cleanup batch is merged), drafts a status comment to post on the relevant GitHub issue (#8/#10/#11/#12/#13). |

Plan: introduce these incrementally — start with `tdd-permanent-fn` and
`changelog-logger`, since Phase 1 begins immediately.

### 3.3 Working agreements
- **Changelog is mandatory** (CLAUDE.md §"Change Logging"). Every commit appends
  to `.github/docs/changelog.md`. Skill/hook above will enforce.
- **Examples in roxygen** must use only `ukr_shp` / `ukr_mtdt_full`.
- **Don't touch legacy code** flagged for deletion in arch-01 unless executing
  a cleanup batch.
- **`R CMD check` must stay green** between phases.

---

## 4. Phase 1 — Tests (#10)

> **Critical scoping rule.** Write tests *only* for the permanent functions
> listed in arch-01 § "Permanent Functions". Do not test legacy functions
> scheduled for deletion in arch-01 batches.

### 4.1 Concrete next actions
- [ ] **1a — Orchestrator.** Implement `run_pti_pipeline()` as specified in
      arch-02.01 § "Orchestrator Function". Export it. Add roxygen2.
- [ ] **1b — Fixtures.** Create `tests/testthat/fixtures/` and the deterministic
      fixtures listed in arch-03 § "Test Data Fixtures" (start with
      `fx_shp_3lvl`, `fx_mtdt_full`, `fx_weights_*`). Generator script:
      `tests/testthat/fixtures/generate-fixtures.R`.
- [ ] **1c — Helper.** Create `tests/testthat/helper-test-data.R` per arch-03
      § "Shared Test Setup Helper".
- [ ] **1d — Tier 1 calc pipeline.** Author `test-calc-pipeline.R` (~71 cases,
      arch-02.01). Pin known quirks (lexicographic admin sort,
      `get_scores_data` 1-row NA, `pivot_pti_dta` `"administrative"` over-match,
      `expand_adm_levels` first-digit extraction) as explicit assertions, not
      skips.
- [ ] **1e — Tier 1 remaining files** (arch-03 §1.2–1.11):
      `test-indicators-list.R`, `test-template-reader.R`, `test-validators.R`,
      `test-legend-palette.R`, `test-plot-helpers.R`, `test-drop-inval-adm.R`,
      `test-export.R`, `test-explorer-helpers.R`, `test-map-render.R`,
      `test-dt-construction.R`.
- [ ] **1f — CI guard.** Confirm `devtools::test()` finishes < 2 min and run it
      via GitHub Actions on push.
- [ ] **1g — Tier 2 (after Tier 1 green).** Module-server tests via
      `shiny::testServer` (arch-03 §2). 7 modules.
- [ ] **1h — Validate baseline.** Run full suite against the *current
      (uncleaned) codebase*. Tier 1 should be green before Phase 2.

### 4.2 Open questions
- Is `shinytest2` (Tier 3 automation) in or out of scope for this push?
  Suggestion: keep it manual for now (arch-03 §3.2 checklist); add automation
  after pkgdown deploys.

---

## 5. Phase 2 — Cleanup (#8)

Drive each batch from arch-01 § "Removal Batches". Discipline:

1. Branch off main: `cleanup/batch-N`.
2. Delete files/functions per the batch table.
3. Run `devtools::document()` → `devtools::test()` → `devtools::check()`.
4. Smoke-test: `launch_pti(shp_dta = ukr_shp, inp_dta = ukr_mtdt_full)` opens.
5. Append changelog row(s).
6. Open PR; merge once green.

Batch order is fixed by dependencies (arch-01 §"Cleanup Strategy"). Don't reorder
without re-checking caller graphs.

---

## 6. Phase 3 — Documentation (#11)

Follow arch-02-docs § "Implementation Order". Per-file checklist there.

- [ ] Phase 1 (data) → Phase 6 (remaining modules) as listed.
- [ ] Track in this PR: any function whose body changed in Phase 2 cleanup needs
      its docs reviewed even if listed as "partial" before.
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
      [WB OpenKnowledge PTI article](https://openknowledge.worldbank.org/server/api/core/bitstreams/1fa677a7-7c1c-5a39-8705-511a7038e3a2/content)
      (mentioned by user). Quote / paraphrase / link as appropriate.
- [ ] Update `_pkgdown.yml` (currently 12 lines, no articles config) to add
      grouped article navigation per arch-04 §"_pkgdown.yml Updates".
- [ ] Add a GitHub Action for pkgdown build & GitHub Pages deploy.
- [ ] Confirm `R CMD check` builds vignettes cleanly.

---

## 8. Phase 5 — Hex ingestion (#13, independent)

Independent track. Can start in parallel on `feature/hex-ingestion`. Five new
exported functions, all developer-facing, all pre-deployment (no runtime API
calls). Spec: arch-05.

Defer-or-parallel decision to be made — if Phase 1–4 are the priority, this
waits. If a collaborator wants to drive this in parallel, the existing
calculation pipeline is geometry-agnostic and won't be affected.

---

## 9. Open questions for the team

1. **Working branch.** `eb-arch-redesign` per CLAUDE.md, or per-phase branches
   off `main`? (See §3.1.)
2. **Skills/sub-agents.** Greenlight on the six tools listed in §3.2? Any
   should be combined or dropped?
3. **Tier 3 automation.** `shinytest2` now or after pkgdown ships?
4. **Hex ingestion priority.** Defer until after Phase 4, or start on a parallel
   branch now?
5. **Changelog protocol.** Should the Stop hook auto-append entries from the
   diff, or just remind?
6. **Skill scope sharing.** Are the proposed skills committed to the repo
   (`.claude/`) so my collaborator's Claude Code session picks them up, or kept
   personal? My recommendation: commit them.

---

## 10. Definition of done (end-state, all phases)

Lifted from arch-00 §"End-State Goals", mirrored here as a single checklist:

- [ ] Only the modern pipeline remains. Public entry points: `launch_pti()`,
      `launch_pti_onepage()`, `create_new_pti()`.
- [ ] All ~95 permanent functions have complete roxygen2 docs.
- [ ] Test coverage > 80%; Tier 1 + Tier 2 automated in CI.
- [ ] Package website deployed on GitHub Pages with grouped vignettes.
- [ ] Hex ingestion pipeline (#13) lands on its own milestone.
- [ ] `R CMD check` passes with 0 warnings, 0 errors, 0 notes.

---

*This plan is a thin tracker. For depth, follow the links in §1.*
