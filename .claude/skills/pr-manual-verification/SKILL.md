---
name: pr-manual-verification
description: Classify a devPTIpack PR's manual-verification needs as None / Optional / REQUIRED, with item-level rationale. Run when opening a PR (to populate the `## Verification` section) or before telling the user a PR is ready to merge. Produces markdown ready to paste into the PR description AND a one-liner for the chat-text summary.
---

# Skill: pr-manual-verification

This skill answers one question: **"Does Koichi need to manually verify
anything before merging this PR, or is automated coverage (local
checks + CI) sufficient?"**

The output is a single classification — one of three values, never
ambiguous — plus the markdown that goes into the PR description and
the chat-text summary.

## When to invoke

- **Before opening a PR** — to populate the `## Verification` section
  of the description.
- **Before telling the user "ready to merge"** — to confirm the
  classification is still accurate after CI has run.
- **On request** — when the user asks "do I need to check anything
  manually for PR #N?".

## The three classifications

Every PR receives exactly one:

| Classification | Meaning | When to use |
|---|---|---|
| **`None`** | CI + local checks cover everything; safe to merge once CI is green. | Pure code with unit tests; doc changes inside Rd / roxygen / markdown text; refactors without behavioural change. |
| **`Optional`** | Listed item is nice-to-have. A clean CI run is a strong proxy. | Visual rendering on pkgdown that CI builds cleanly; UI tweaks where the test exercises behaviour but not aesthetics. |
| **`REQUIRED`** | Listed item only the user can verify; do not merge before doing it. | Anything CI cannot catch: deployed Shiny app behaviour, image / figure rendering correctness, cross-page navigation flow, content changes where the wording is judgmental. |

## Procedure

### Step 1 — Identify what changed

Run (replacing `<pr>` with the actual number or `HEAD` for an unopened
branch):

```bash
gh pr diff <pr> --repo worldbank/devPTIpack --name-only
```

Categorise each changed file:

- **R code (`R/*.R`)** — exercised by `testthat` + `R CMD check`.
- **Tests (`tests/testthat/*.R`)** — exercised by `testthat`.
- **Roxygen / Rd source** — exercised by `R CMD check`'s
  examples / cross-ref / undocumented-objects checks.
- **Vignettes (`vignettes/articles/*.qmd`)** — exercised by the
  `pkgdown` CI build (renders cleanly = no errors), **not** for
  visual correctness.
- **Shiny modules / launchers (`R/mod_*.R`, `R/launch_*.R`,
  `R/app_*.R`)** — partially exercised by `shiny::testServer` Tier-2
  tests; deployed UI behaviour is **not** exercised.
- **Package data (`R/data.R` + `data/*.rda`)** — exercised by `R CMD
  check`'s `LazyData` + bundled-data tests.
- **Top-level files (`DESCRIPTION`, `NAMESPACE`, `_pkgdown.yml`,
  `.github/workflows/*`)** — `R CMD check` + the workflow's own
  dry-run if there is one.
- **Images / static assets (`vignettes/articles/img/*.{png,svg}`)** —
  **not** exercised by any CI check. Broken image links don't fail
  `pkgdown` build (they render as a broken-icon).

### Step 2 — Map each category to a classification

| Category in diff | Classification contribution |
|---|---|
| R code + tests added/modified | None (tests cover the contract) |
| Roxygen rewording / new `@param` / new `@return` | None (Rd build catches structural issues) |
| New exported function | None **if** an `@example` exists that runs in `R CMD check`. Optional **if** `\dontrun{}` only -- recommend the user `?fn` after merge to eyeball. |
| Vignette prose (`.qmd`) | Optional -- `pkgdown` CI build is a strong proxy for "renders without errors", but visual layout / table formatting / link colour is judgmental. |
| Vignette adding new images or figures | **REQUIRED** -- `pkgdown` doesn't fail on broken image links; the user must load the rendered page and confirm the figure shows. |
| Vignette anchor / cross-page links | **REQUIRED** -- Quarto auto-anchor footgun (`## E. Advanced: foo` -> `e--advanced-foo` double-dash) silently produces working links to wrong destinations. |
| Shiny module / launcher behavior change | **REQUIRED** -- `testServer` tests don't cover deployed-app UX. The user must `launch_pti(...)` and exercise the changed path. |
| Package data rebuild (`data/*.rda` changed) | **REQUIRED** -- the user should `data(<name>); str(<name>)` after merge to confirm shape, especially if the regeneration script changed. |
| DESCRIPTION metadata (Title / Description / Version) | None -- `R CMD check` catches structural issues; wording is up to the user but rarely needs manual gating. |
| `_pkgdown.yml` (navbar / reference index) | Optional -- `pkgdown` CI build succeeding is a strong proxy. User should glance at the deployed navbar after merge. |
| CI workflow file (`.github/workflows/*`) | **REQUIRED** -- the workflow's own success on the merging PR doesn't guarantee the *next* PR (different trigger paths). User should re-trigger one full run after merge. |
| Hooks / Claude config (`.claude/**`) | None -- not part of the package; affects future agent runs, not the package's behaviour. |

If multiple categories apply, take the **highest** classification
(REQUIRED > Optional > None). One REQUIRED item forces the whole PR
to REQUIRED.

### Step 3 — Cross-check CI coverage

For the changed files, confirm the relevant CI checks exist and
typically pass:

- `testthat` workflow -- runs `testthat::test_local()` against the
  PR branch.
- `test-coverage` workflow -- covr report; passes if coverage is
  computable, not a strict threshold.
- `pkgdown` workflow -- builds the `_pkgdown.yml` site; fails on Rd
  parse errors, broken cross-references, missing fonts, vignette
  render errors.
- `ubuntu-latest (R-release)` -- full `R CMD check --as-cran`.

If one of these is disabled or skipped on the PR's trigger, the
classification leans toward Optional / REQUIRED for the items that
workflow normally covers.

### Step 4 — Produce the output

Return **two artefacts**:

1. **The chat-text one-liner** (for the message that announces "PR
   #N is open / ready"):

   ```
   **Manual verification:** None.
   ```

   or

   ```
   **Manual verification:** REQUIRED -- <one short specific action>.
   ```

   For Optional, name the item but keep the line one sentence.

2. **The `## Verification` markdown block** for the PR description:

   ```markdown
   ## Verification

   ### Done (locally / by CI)
   - [ ] CI: <list the 4 checks>
   - [x] <each local check the agent ran>

   ### Manual verification (you)
   **<None | Optional | REQUIRED>** -- <rationale + items, or `None`>
   ```

The user pastes (or asks me to paste) the second block into the PR
description, and reads the first inline in chat.

## Rules

- **Never default to "REQUIRED" to be safe.** That trains the user to
  ignore the label. Reserve REQUIRED for items where CI provably
  cannot catch the failure mode.
- **Be specific about the action.** "Eyeball the rendered docs" is
  useless; "Load /reference/make_hex_grid.html and confirm the
  Examples section renders the head(hex) table" is actionable.
- **One classification per PR.** Don't produce "REQUIRED for X but
  None for Y" -- the overall classification is the maximum.
- **The agent doesn't merge anything.** Output only.

## Examples

### Example 1 — pure refactor

User opens PR that splits a long internal function across two files
with no behaviour change. Tests cover the function.

```
**Manual verification:** None.
```

```markdown
## Verification

### Done (locally / by CI)
- [ ] CI: testthat / test-coverage / pkgdown / ubuntu R-release
- [x] R CMD check 0/0/0 (local)
- [x] testthat: existing 803 PASS unchanged (local)

### Manual verification (you)
**None** -- pure refactor, no behaviour change. Existing tests cover
the function's external contract.
```

### Example 2 — Shiny module fix

User opens PR that fixes a download-button bug in
`mod_dwnld_dta.R`. `testServer` test covers the validation path but
not the actual browser download.

```
**Manual verification:** REQUIRED -- launch_pti(shp_dta = ukr_shp,
inp_dta = ukr_mtdt_full) and click each Download button on PTI /
compare / Data Explorer tabs; confirm files arrive with sensible
names and non-empty content. testServer can't observe browser-side
download UX.
```

### Example 3 — vignette rewrite with new figure

User adds `vignettes/articles/build-pti-4-hex.qmd` with a new
`img/hex-pipeline.svg` figure.

```
**Manual verification:** REQUIRED -- load the deployed pkgdown
build-pti-4-hex page and confirm the hex-pipeline.svg figure
renders. pkgdown does not fail on broken image links.
```

### Example 4 — new exported function with example

User adds `R/fct_make_hex_grid.R` with full roxygen + a runnable
`@example` using bundled `rwa_shp` + a 15-test testthat file.

```
**Manual verification:** None.
```

```markdown
## Verification

### Done (locally / by CI)
- [ ] CI: testthat / test-coverage / pkgdown / ubuntu R-release
- [x] R CMD check 0/0/0 (local)
- [x] testthat: 30 PASS / 1 SKIP for the new file (local; SKIP is
      `local_mocked_bindings` test on testthat 3.1.2, CI runs newer)
- [x] @example runs in 0.3s on bundled rwa_shp
- [x] Live spot-check: make_hex_grid(rwa_shp$admin0_Country,
      resolution = 6) -> 507 cells, median area 41.13 km^2

### Manual verification (you)
**None** -- the @example exercises the public surface and runs
during `R CMD check`. The pkgdown reference page is a plain
roxygen-to-Rd-to-HTML pipeline with no exotic markup; CI's pkgdown
build is a strong proxy for visual correctness.
```
