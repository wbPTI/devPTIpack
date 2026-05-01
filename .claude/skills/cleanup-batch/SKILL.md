---
name: cleanup-batch
description: Execute one numbered legacy-cleanup batch from .github/docs/arch-01-cleanup.md end-to-end — delete listed files/functions, regenerate NAMESPACE, run tests + R CMD check, smoke-test launch_pti, and log to changelog. Refuses to run if the prior batch's tests are not green.
---

# Skill: cleanup-batch

Execute one of the six legacy-cleanup batches defined in
[`.github/docs/arch-01-cleanup.md`](../../../.github/docs/arch-01-cleanup.md)
(GitHub issue [#8](https://github.com/worldbank/devPTIpack/issues/8)).

## When to invoke

After Phase 1 baseline tests are green, the user requests cleanup of a
specific batch (1–6). The batch number must match arch-01 § "Removal Batches".

## Pre-flight (refuse to proceed if any fails)

1. **Tests are green.** `Rscript -e 'devtools::test()'` must pass against the
   current branch *before* the batch executes. If not, halt and report.
2. **Branch.** Operating from a sub-branch named `cleanup/batch-N` cut from
   the integration branch (`koichi-arch-redesign` or current per
   `.claude/CLAUDE.md`). If the branch isn't clean, halt.
3. **Permanent-function audit.** Spot-check that every item in the batch
   table appears in arch-01's batch list (not in "Permanent Functions").

## Procedure

For batch N:

1. **Read the batch table** in arch-01 § "Batch N — …".
2. **Apply each row** in table order:
   - Whole-file deletions: `git rm <path>`.
   - Single-function deletions: read the file, delete the function block,
     leave the rest intact.
   - Remove corresponding `@export` from NAMESPACE (or just regenerate).
   - Remove dependent commented-out call-sites listed in the table footer.
3. **Regenerate NAMESPACE & docs.**
   `Rscript -e 'devtools::document()'`
4. **Run tests.**
   `Rscript -e 'devtools::test()'` — must stay green.
5. **Run `R CMD check`.**
   `Rscript -e 'devtools::check(args = "--no-manual")'` — must report
   0 errors, 0 warnings (notes acceptable only if pre-existing).
6. **Smoke-test the modern app.**
   ```r
   library(devPTIpack); data(ukr_shp); data(ukr_mtdt_full)
   app <- launch_pti(shp_dta = ukr_shp, inp_dta = ukr_mtdt_full)
   shiny::runApp(app, launch.browser = FALSE)  # check it boots; user closes
   ```
   If the user is unavailable for visual verification, document this in the
   PR description and only assert that the app object constructs without error.
7. **Update changelog.** Append rows to `.github/docs/changelog.md` under
   today's date with Scope `Code` — one row per logical group (file or
   function). The Stop hook will draft these; refine each summary to be
   specific (which functions, which file, which downstream impact).
8. **Sync `PLAN.md`.** Tick the corresponding `Batch N` box under
   §5 / Phase 2, and update the "Progress log" with the PR number. See
   `.claude/CLAUDE.md` § "PLAN.md Sync (COMPULSORY)".
9. **Open PR** into the integration branch.

## Hard rules

- Never re-order or skip steps in a batch table.
- Never delete a function still listed under arch-01 § "Permanent Functions".
- If a deletion breaks tests: **stop**, report, and open a question in the PR
  rather than weakening the test.
- If the batch number's prerequisite (per arch-01 §"Cleanup Strategy") has
  not been merged, refuse — batches must run in order.
- Never bypass `R CMD check` failures (no `--no-tests`, no `--ignore-vignettes`
  unless previously authorised).

## Reporting

After the batch, return a short summary:

```
Batch N complete:
  - Files deleted: X
  - Functions deleted: Y (listed)
  - Lines removed: Z
  - Tests: <count> passing, 0 failing
  - R CMD check: 0E 0W <N>N
  - Smoke test: PASS / SKIPPED
  - PR: <url>
```
