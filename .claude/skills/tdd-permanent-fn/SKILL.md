---
name: tdd-permanent-fn
description: Scaffold a Tier-1 testthat test file for a permanent (non-legacy) devPTIpack function per arch-03/arch-02.01 — uses bundled fixtures (ukr_shp, ukr_mtdt_full) and the helper-test-data.R harness, pins known behavioural quirks instead of skipping them.
---

# Skill: tdd-permanent-fn

Scaffold a Tier-1 `testthat` test file for **permanent** devPTIpack functions
(GitHub issue [#10](https://github.com/worldbank/devPTIpack/issues/10)).

## When to invoke

The user (or a parent task) names one or more functions to test. The function
must appear in the "Permanent Functions" registry in
[`.github/docs/arch-01-cleanup.md`](../../../.github/docs/arch-01-cleanup.md).
If the function is in a cleanup batch, **stop and warn** — we do not write
tests for code scheduled for deletion.

## Inputs (collect before scaffolding)

1. Target function(s) and their source file(s) under `R/`.
2. Whether the function belongs to:
   - the calc pipeline (`calc_pti_helpers.R` / `calc_pti_expander.R`) — use
     [`arch-02.01-testing-calc-pipeline.md`](../../../.github/docs/arch-02.01-testing-calc-pipeline.md)
     for the test case list (Level A / B / C / E).
   - any other Tier-1 area — use the per-function tables in
     [`arch-03-testing.md`](../../../.github/docs/arch-03-testing.md) §1.2–1.11.

## Procedure

1. **Confirm permanence.** Search arch-01 § "Permanent Functions" for the
   function name. If absent, halt with a warning.
2. **Locate the existing test file** under `tests/testthat/` matching the
   per-function map in arch-03 § "Per-Function Test Map". If a test file
   exists, read it first and *append* — never overwrite.
3. **Author cases.** Use the case tables in arch-02.01 (calc pipeline) or
   arch-03 (everything else) verbatim. For each row:
   - One `test_that()` block per row.
   - Inputs sourced from `helper-test-data.R` or `tests/testthat/fixtures/`.
   - Assertions: prefer `expect_equal`, `expect_s3_class`, `expect_named`,
     `expect_error`, `expect_warning` over generic `expect_true`.
4. **Pin known quirks.** Encode these as explicit assertions, never `skip()`:
   - Lexicographic admin sort in `get_adm_levels` (fails for `admin10+`).
   - `get_scores_data` leaves 1-row groups as `NA`, not `0`.
   - `pivot_pti_dta` over-matches names containing `"administrative"`.
   - `expand_adm_levels` extracts only the first digit for level comparison.
   See arch-03 §"Known Issues to Pin".
5. **Run.** `Rscript -e 'devtools::test(filter = "<basename>")'` from repo
   root. All tests must pass against the *current* (uncleaned) codebase
   before the test file lands.
6. **Update changelog.** Append a row to `.github/docs/changelog.md` under
   today's date — Scope `Tests`. (The Stop hook will draft this; refine.)
7. **Sync `PLAN.md`.** Tick the relevant box(es) under §4.1 (e.g. `1d`,
   `1e`), add new sub-bullets if the PR introduced work not previously
   enumerated, and update the "Progress log" with the PR number. See
   `.claude/CLAUDE.md` § "PLAN.md Sync (COMPULSORY)".

## Hard rules

- Never test legacy functions slated for deletion (arch-01 batches).
- Never use external network access in tests.
- Examples and fixtures use only `ukr_shp`, `ukr_mtdt_full`, or generated
  fixtures committed under `tests/testthat/fixtures/`.
- Tests must pass cleanly — no `skip_on_*`, no commented assertions.
- Test runtimes must keep `devtools::test()` under 2 minutes total.

## Skeleton (copy-and-adapt)

```r
# tests/testthat/test-<area>.R

test_that("<function>: <case-from-arch-table>", {
  # Arrange — pull from helper-test-data.R or fixtures/
  input <- test_pivoted

  # Act
  out <- pivot_pti_dta(ukr_mtdt_full, test_indicators)

  # Assert
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("admin_level", "var_code", "value"), ignore.order = TRUE)
})
```

## After authoring

- If tests fail against current code, **do not** weaken the assertion to
  match — investigate. Either the case in arch-03 is wrong (escalate via PR
  comment / changelog note) or the function actually misbehaves (pin the
  current behaviour and open a follow-up issue tagged `bug`).
- Cross-check the per-function map in arch-03 § "Per-Function Test Map" and
  tick off the corresponding row in the issue #10 acceptance checklist.
