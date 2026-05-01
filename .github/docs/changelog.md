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
