# Project: devPTIpack

A golem-based Shiny R package for computing, visualizing, and exploring Project Targeting Indices (PTI).

## Architecture

- Modern path uses: `launch_pti()` → `mod_ptipage_newsrv` → `mod_calc_pti2_server` → `mod_plot_pti2_srv`.
- Legacy modules (`mod_weights`, `mod_calc_pti`, `mod_map_pti_leaf`, etc.) are scheduled for removal.
- The active redesign is tracked by GitHub issue [#9](https://github.com/worldbank/devPTIpack/issues/9). Sub-issues: #8 cleanup, #10 testing, #11 docs, #12 workspace/pkgdown, #13 hex ingest.
- Working plan is [`PLAN.md`](../PLAN.md) (thin tracker; the architecture lives under `.github/docs/`).

## Source-of-truth docs

- `.github/docs/arch-00-overview.md` — overview & redesign workflow
- `.github/docs/arch-01-cleanup.md` — function-by-function cleanup audit
- `.github/docs/arch-02-docs.md` — documentation implementation order
- `.github/docs/arch-02.01-testing-calc-pipeline.md` — calc pipeline test spec
- `.github/docs/arch-03-testing.md` — three-tier testing strategy
- `.github/docs/arch-04-workspace.md` — vignettes & pkgdown plan
- `.github/docs/arch-05-hex-ingestion.md` — hex (H3) ingestion design
- `.claude/rules/roxygen-documentation.md` — roxygen2 standards

## Key conventions

- Follow tidyverse style with `|>` pipe.
- Roxygen2 per `.claude/rules/roxygen-documentation.md`.
- Examples must use only built-in data. Prefer `rwa_shp` / `rwa_mtdt_full` for user-facing examples; `ukr_shp` / `ukr_mtdt_full` are also bundled and remain in use by the test suite.
- Tests use `testthat`. Tier 1 (pure functions) → Tier 2 (`shiny::testServer`) → Tier 3 (manual / `shinytest2`).
- Tests target only the **permanent** functions in arch-01 — do not test code scheduled for deletion.
- Do not touch legacy/dead code marked for removal in arch-01 unless executing a cleanup batch.

## Branching

- Default branch: `main`.
- Current integration branch: `koichi-arch-redesign` (off `main`).
- Sub-branches per phase/batch (e.g. `tests/calc-pipeline`, `cleanup/batch-1`, `docs/phase-2`) PR'd into the integration branch.
- Each PR must keep `R CMD check` green and update the changelog.

## Skills & sub-agents

Project-scoped tooling under `.claude/`:

| Tool                     | Type      | Purpose                                                                               |
| ------------------------ | --------- | ------------------------------------------------------------------------------------- |
| `tdd-permanent-fn`       | skill     | Scaffold Tier-1 tests for a permanent function per arch-03 / arch-02.01               |
| `cleanup-batch`          | skill     | Execute one arch-01 cleanup batch end-to-end (delete, document, test, check)          |
| `roxygen-document`       | skill     | Add/upgrade roxygen2 per `.claude/rules/roxygen-documentation.md`                     |
| `issue-progress-comment` | skill     | Draft a status comment for a GitHub issue from the recent diff/work                   |
| `close-issue-on-merge`   | skill     | After a PR lands, close the issue(s) it claims to close (GitHub auto-close does NOT fire because PRs target `eb-docs-pkgdown` or `koichi-arch-redesign`, not the default branch) |
| `pr-manual-verification` | skill     | Classify a PR's manual-verification needs as **None / Optional / REQUIRED** with item-level rationale; produces the `## Verification` markdown block + the chat-text one-liner |
| `r-package-reviewer`     | sub-agent | Review diffs for R-package conventions (NAMESPACE, exports, examples, no `browser()`) |

Invoke skills via the Skill tool by name. Spawn the sub-agent via the Agent tool with `subagent_type: r-package-reviewer`.

**Issue-close workflow (compulsory after every PR merge):** invoke
`close-issue-on-merge` immediately after the user reports a PR has
merged. GitHub's auto-close fires only on PRs that land on the repo's
default branch (`main`); our redesign PRs target `eb-docs-pkgdown` or
`koichi-arch-redesign`, so `Closes #N` lines never trigger and issues
silently rot otherwise.

**PR verification convention (compulsory before "ready to merge"):**
every PR opened on this repo must include a `## Verification` section
in its description with two sub-blocks -- "Done (locally / by CI)" and
"Manual verification (you)". The manual block is classified as one of
`None`, `Optional`, or `REQUIRED`. Invoke the `pr-manual-verification`
skill to produce this section before opening the PR and again before
telling the user a PR is ready to merge. The same classification ends
the chat-text summary as a single-line `**Manual verification:** <X>`
field so the user can decide at a glance. Reserve `REQUIRED` for items
CI provably cannot catch (deployed Shiny UX, broken image links in
vignettes, Quarto auto-anchor footgun on cross-page links, package
data rebuilds) -- defaulting to `REQUIRED` to be safe trains the user
to ignore the label.

## PLAN.md Sync (COMPULSORY)

[`PLAN.md`](../PLAN.md) is the working tracker. It must stay in step with
reality, not drift behind it. Every PR that **completes**, **starts**, or
**meaningfully advances** a tracked item updates `PLAN.md` in the same
commit.

What "in step" means:
- Tick `[x]` boxes for items finished by this PR.
- Add new sub-bullets under the active phase when a PR introduces work
  not previously enumerated (e.g. a new test file, a new vignette).
- Move resolved §"Open questions" out of that section into the relevant
  phase prose (or delete if no longer load-bearing).
- Update the "Progress log" / status block with the PR number and a
  one-line outcome.

The skills under `.claude/skills/` already include a final "update
PLAN.md" step. Don't yield back to the user without performing that step
when the PR's scope intersects a tracked item.

PRs that *don't* touch PLAN-tracked work (pure infra fixes, typo
corrections, dependency bumps) need no PLAN.md edit.

## Change Logging (COMPULSORY)

Every code/doc change must be logged to `.github/docs/changelog.md`.

A Stop hook (`.claude/hooks/auto-changelog.sh`, registered in `.claude/settings.json`)
auto-drafts entries from the diff at the end of each turn so nothing slips. **You should still
review and rewrite the auto-drafted summaries to be specific and informative** —
auto-drafts are placeholders, not finished entries.

**Format:**

```
## YYYY-MM-DD

| Scope  | Change                                  |
| ------ | --------------------------------------- |
| {area} | One-line summary: what was done and why |
```

**Rules:**
- One row per discrete change (file created, function rewritten, test added, etc.)
- Scope = short category: `Docs`, `Code`, `Tests`, `Rules`, `Data`, `Config`, `Tooling`
- Keep each summary to one sentence — a reader should grasp the change instantly.
- Group entries under the same date heading when working in the same session.
- Auto-drafted rows include the marker `<!-- AUTODRAFT -->`. Replace the placeholder summary with a real one before yielding.
