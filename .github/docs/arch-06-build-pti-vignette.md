# arch-06 — Build-a-PTI Vignette + Data-Prep Reference + Validator UX

> **Status:** Planning. Owner: Koichi (with Claude). Targets PR-B (`docs/build-pti-content`) stacked on PR-A (`chore/resolve-pr63-conflicts` → `eb-docs-pkgdown`).
>
> **Trigger:** PR #63 introduced an empty `build-pti.qmd` placeholder + a TBD `dataprep.qmd` stub. The `methodology.qmd` article already mixes conceptual content with a long developer setup walkthrough. The package's four exported validators emit `testthat` reporter output at the R prompt, which is jarring for end-users.
>
> **Scope of this plan:** four interlocking work items, written so you can redirect any of them before content is written.
>
> 1. Fill `vignettes/articles/build-pti.qmd` — the task-oriented "from raw data to live dashboard" walkthrough.
> 2. Fill `vignettes/dataprep.qmd` — the reference deep-dive on input data shape requirements.
> 3. Slim `vignettes/articles/methodology.qmd` — keep conceptual content; move all how-to material into build-pti.qmd / dataprep.qmd.
> 4. Minimal validator UX pass — replace `testthat::test_that` reporter spam with `cli::cli_alert_*` and add an invisible structured return value. Defer the rest of issue [#7](https://github.com/worldbank/devPTIpack/issues/7) (CSV upload, auto-metadata, `mod_validate_upload`).

---

## 1. Why these four items, why bundled

devPTIpack already has the scaffolding for app-developer onboarding (`create_new_pti()`, exported validators, `launch_pti()` with sensible defaults), but the *narrative* — "I have raw boundary files and a CSV of indicators; what do I do?" — has no canonical home. PR #63 created three article slots (Methodology, Build, Past Projects) and a Resources tab including a `dataprep` link, but only methodology has content. The result is a site that looks ready but answers half its own questions.

This plan treats the three articles + the validators as one user-flow surface:

```
[New user]
   │
   ├── reads Methodology  ──────► why PTI exists, what it computes (concept)
   │
   ├── reads Build a PTI  ──────► step-by-step recipe (task)
   │       │
   │       └── deep-dives into Data prep ──► canonical input-shape reference
   │
   └── runs validate_*   ───────► pleasant cli output + structured result
```

Touching them as one unit means: the build-pti walkthrough can confidently say "see [`dataprep`](#) for the column-by-column reference" without writing dead links; the methodology article can drop the data-prep section without losing content; and the validator output that the build-pti walkthrough screenshots/quotes is pleasant rather than `testthat`-chatter.

## 2. Audience model

We assume two reader profiles:

| Profile        | What they have                                       | What they want                                                                  | Lands on                          |
| -------------- | ---------------------------------------------------- | ------------------------------------------------------------------------------- | --------------------------------- |
| **Developer**  | Raw indicator files + boundary shapefiles for a country | A working dashboard at a URL                                                    | `build-pti.qmd`                   |
| **Reader**     | Domain interest                                      | What PTI is, why it's useful, examples                                          | `methodology.qmd`, `past-projects` |

`dataprep.qmd` is auxiliary — developers reference it from `build-pti.qmd` when they hit the data-shape questions, not as their first stop. README + `home` continue to point primarily at `build-pti.qmd` for action and `methodology.qmd` for context.

## 3. `vignettes/articles/build-pti.qmd` — proposed structure

Replaces the existing 5-step placeholder with a fully-fleshed walkthrough. Title stays "Build a PTI."

### 3.1 Section outline

```
0. Overview                        what you'll build, what you'll need, time estimate
1. Prerequisites                   R version, package install, sample-data smoke-test
2. Project layout                  create_new_pti() → folder structure walk-through
3. Prepare boundary shapes         admin_bounds.rds expected shape + concrete recipe
4. Prepare the metadata template   mtdt.xlsx sheets + concrete recipe
5. Validate inputs                 validate_geometries() + validate_metadata()
                                     → interpreting cli output
6. Launch locally                  launch_pti() walk-through, key parameters
7. Deploy                          rsconnect / shinyapps.io / Posit Connect
8. Troubleshooting                 5–8 common failure modes with the fix
9. Next steps                      link to methodology, reference index
```

### 3.2 What goes into each section (so you can redirect)

- **§0 Overview** — one paragraph framing, screenshot of a finished dashboard (link to `past-projects`), bulleted "you'll need: R 4.1+, raw boundaries, indicator data joined to those boundaries, ~1 hour."
- **§1 Prerequisites** — install snippet (`remotes::install_github("worldbank/devPTIpack")`), one-liner sample-data smoke test using `ukr_shp` + `ukr_mtdt_full` so the reader confirms install before doing real work. Pin R version to `>= 4.1.0` matching DESCRIPTION.
- **§2 Project layout** — `create_new_pti("path/to/myPTI")`, then a tree-diagram of what gets created and what each file does (1 line per file).
- **§3 Prepare boundary shapes** — *brief* description of the required shape (named list-of-sf, `admin<N>_<Name>` keys, `admin<N>Pcod`/`admin<N>Name`/`area`/`geometry` columns), then a runnable code example reading two GADM-style shapefiles and assembling them into the list. Cross-link: "see [`dataprep#shapes`](dataprep.qmd#shapes) for the full column-by-column reference and parent-child rules." Include a `saveRDS()` line to produce `admin_bounds.rds`.
- **§4 Prepare the metadata template** — explain the hub-and-spoke pattern: each indicator becomes a column in one or more `admin<N>_*` sheets; `metadata` sheet describes those columns. Show a small code snippet using `readxl` + `writexl` to assemble a metadata file from two indicator tibbles (the kgzPTIdata `100_compilation_modified.Rmd` pattern, simplified to no more than ~30 lines). Cross-link: "see [`dataprep#metadata`](dataprep.qmd#metadata) for the full per-column reference."
- **§5 Validate inputs** — show running `validate_geometries(my_shp)` and `validate_metadata("path/to/shapes.rds", "path/to/mtdt.xlsx")`. Quote the *new* cli output (post-§5 of this plan: see §6 below). Explain pass/warn/fail semantics. Show one example warning + how to fix it.
- **§6 Launch locally** — minimal `launch_pti()` call with bundled data, then add `app_name`, `show_adm_levels`, `shapes_path`, `mtdtpdf_path`, `pti_landing_page` one at a time with a sentence each.
- **§7 Deploy** — three sub-bullets: (a) shinyapps.io with `rsconnect::deployApp()`, (b) Posit Connect, (c) self-hosted Shiny Server. Don't write a full deploy tutorial — link out to those products' own docs. Note the `app-data/` paths must be relative when deploying.
- **§8 Troubleshooting** — table of "Symptom → Likely cause → Fix":
  - App launches but shows no indicators → metadata `var_code` doesn't match column names in admin sheets
  - "filter columns are not read as logical columns" → fltr_* values stored as TRUE/FALSE strings instead of booleans in the .xlsx
  - "Some values in `admin1Pcod` of admin2_X are not present in admin1_Y" → parent-child mapping broken
  - PTI map blank for some regions → see "unavailable admins" in `get_vars_un_avbil()`; usually data missing for that polygon
  - 5–8 entries total. Mostly cribbed from the existing validator messages.
- **§9 Next steps** — links to methodology, reference index, past projects.

### 3.3 Length target

~250–350 lines of QMD. Each section short — code snippets do the explaining. We resist the temptation to repeat dataprep.qmd's column reference inline.

### 3.4 What gets cut from the existing `build-pti.qmd`

Everything currently in there is placeholder text — the entire file gets rewritten. The five `## Step …` headings are reused; everything else is new.

## 4. `vignettes/dataprep.qmd` — proposed structure

This is the **reference deep-dive**. Replaces the TBD stub. Stays under "Additional Resources" in the navbar (per `_pkgdown.yml` line ~85). Title becomes: "Data preparation reference."

### 4.1 Section outline

```
0. Why this page exists           who should read this; how it relates to build-pti.qmd
1. Boundary shapes (admin_bounds.rds)
   1.1 Top-level structure        named list-of-sf
   1.2 Naming convention          admin<N>_<Name>; ADM0 mandatory
   1.3 Required columns per layer admin<N>Pcod, admin<N>Name, area, geometry
   1.4 Parent-child mapping       admin2 must contain admin0Pcod, admin1Pcod, …
   1.5 Geometry constraints       POLYGON / MULTIPOLYGON only; CRS; area in km²
   1.6 Validation                 what validate_geometries() catches and what it doesn't
2. Metadata Excel template (mtdt.xlsx)
   2.1 Sheet inventory            general, metadata, point_data, weights_table, admin<N>_*
   2.2 The `metadata` sheet       column-by-column reference (14-column table)
   2.3 Per-admin sheets           one column per indicator; admin Pcod/Name/area required
   2.4 The `weights_table` sheet  optional preset weight schemes (ws#..)
   2.5 Validation                 what validate_metadata() catches
3. Common pitfalls
4. Worked example                 mini end-to-end script using the bundled ukr data
```

### 4.2 What goes into each section

- **§0 Why this page exists** — frame as "you only need this if you're hitting validation errors or building from raw data; the [Build a PTI](articles/build-pti.html) walkthrough is the friendlier entry."
- **§1 Boundary shapes** — every fact spelled out, every column documented with type + purpose + example. Mirror the `validate_single_geom` / `validate_read_shp` invariants (those *are* the spec). Inline use of `data(ukr_shp); str(ukr_shp[[1]])`. Include a small diagram (ascii or mermaid) of admin0 ⊃ admin1 ⊃ admin2 with the Pcod-cascading rule.
- **§2 Metadata Excel template** — column-by-column table for the `metadata` sheet. Each column: name, type, required?, description, example value. Source-of-truth here is `fct_template_reader.R` and the existing methodology.qmd's "Create a metadata" section, which we lift wholesale and edit for clarity. Include the rule "**no `:` in `var_name` or `pillar_name`**" since the existing methodology.qmd flags it.
- **§3 Common pitfalls** — bullet list, ~10 entries:
  - `fltr_*` columns must be `TRUE`/`FALSE` (boolean), not strings
  - `pillar_group` must be numeric, not text
  - Each indicator's `var_code` must match a column in at least one `admin<N>_*` sheet
  - `admin0_*` layer is mandatory even if you only display higher levels
  - `area` must be in km² (reproject to UTM before computing)
  - Geometries must be `sf::st_make_valid()`-clean
  - etc.
- **§4 Worked example** — assemble `ukr_shp` + `ukr_mtdt_full` from scratch in <40 lines, save them to RDS / xlsx, validate, launch. Demonstrates the whole pipeline using package-bundled data so the example is self-contained.

### 4.3 Length target

~300–500 lines QMD (it's a reference; longer is OK). Heavy on tables and short paragraphs.

### 4.4 Cross-references

- From build-pti.qmd §3 → dataprep.qmd §1
- From build-pti.qmd §4 → dataprep.qmd §2
- From dataprep.qmd back to build-pti.qmd at §0 ("if you just want a recipe, go here instead")

## 5. `vignettes/articles/methodology.qmd` — what to slim

The current methodology.qmd is ~600 lines. Sections (per the file as it currently stands on `eb-docs-pkgdown`):

| § | Heading                                  | Keep? | Reason                                                                |
| - | ---------------------------------------- | ----- | --------------------------------------------------------------------- |
| 1 | Introduction                             | yes   | Conceptual framing — what PTI is, why it exists                       |
| 2 | Step 1: Identification                   | yes   | Conceptual — choosing thematic areas, indicators                      |
| 3 | Step 2: PTI preparation > Setup: Data cleaning | **drop** | Pure how-to. Redirected from build-pti.qmd §3–§4 + dataprep.qmd     |
| 3 | Step 2: PTI preparation > Setup: Launching a dashboard | **drop** | Pure how-to. Redirected from build-pti.qmd §6 + §7                  |
| 3 | Step 2: PTI preparation > Common indicators (HC, accessibility, disasters) | **keep** | Conceptual — what indicators tend to go in. Lift to its own §       |
| 4 | Step 3: Application and methodology      | yes   | Conceptual — z-scores, weighting formula. The math.                   |
| 5 | Step 4: Monitoring                       | yes   | Conceptual — long-term workflow                                       |

Net effect: methodology.qmd shrinks from ~600 lines to ~300, becomes purely conceptual. New §3 is "Common indicator categories" which keeps the references to `devaccess`, `devdisaster`, DHS / WorldPop / etc.

Section ordering in the slimmed file:

```
1. Introduction (what PTI is)
2. Step 1: Identification          (thematic areas, indicators)
3. Common indicator categories     (HC, accessibility, disaster — kept content)
4. Step 2: PTI calculation         (z-scores, weighted sum, formula)   ← was §4 "Application and methodology"
5. Step 3: Building a dashboard    (one paragraph + link to build-pti.qmd)
6. Step 4: Monitoring              (kept content)
```

The "Step 2: PTI preparation" → "Setup: Data cleaning" → footnote linking to `zamPTIdata` survives in the cross-reference: methodology.qmd §5 says "to actually build one, see [Build a PTI](build-pti.html); for a worked recipe end-to-end see the [zamPTIdata](https://github.com/wbPTI/zamPTIdata) example repo."

### 5.1 Decision: defer the slim

Methodology.qmd is wholesale EBukin's writing in PR #63. Slimming it = editing his unmerged work. **Resolved: option (b)** — leave methodology.qmd untouched in PR-B, ship the duplication, open follow-up issue [#73](https://github.com/worldbank/devPTIpack/issues/73) tracking the slim. Lets PR #63 merge intact and gives EBukin first crack at his own restructure.

## 6. Validator UX — minimal pass

Goal: when a user runs `validate_geometries(my_shp)` from the R console or inside Shiny, they get pleasant cli output and a programmatic return value, without changing the validator semantics.

### 6.1 Functions touched

`R/validators.R`:
- `validate_single_geom()` (internal)
- `validate_geometries()` (exported)

`R/fct_validate_metadata.R`:
- `validate_metadata()` (exported)
- `validate_read_shp()` (exported)
- `validate_read_metadata()` (exported)

### 6.2 What changes per function

For each: replace `testthat::test_that` blocks with `cli::cli_alert_*`. Return `invisible(list(status, summary, issues))`.

```r
list(
  status  = "pass" | "warn" | "fail",        # worst level seen
  summary = "12 checks run, 0 failed, 1 warning",
  issues  = list(                            # 0+ entries
    list(level = "warn", check = "...", message = "...", details = list(...)),
    ...
  )
)
```

`status` semantics:
- `pass`: every check OK
- `warn`: at least one warning, no failures
- `fail`: at least one failure (something downstream will break)

### 6.3 `cli` mapping

| Old call                          | New call                                               |
| --------------------------------- | ------------------------------------------------------ |
| `testthat::expect_true(.)` passing | `cli::cli_alert_success(check_label)`                  |
| `rlang::inform(msg)`              | `cli::cli_alert_info(msg)` (info-level note)           |
| `rlang::warn(msg)`                | `cli::cli_alert_warning(msg)`                          |
| `rlang::abort(msg)`               | record as `fail`-level issue, return early; no abort   |
| `cat('test_that(...)')`           | delete (debug spam from current code)                  |

The shift from `abort()` → returning a `fail` status is the **biggest semantic change**. It means a user calling `validate_metadata(bad_path)` will no longer get an error thrown — they get an invisible result with `status = "fail"`. **Backwards compatibility risk:** any code that expected validators to abort on hard errors will break. We add an `error_on_fail = FALSE` parameter (default `FALSE`) and document that callers wanting old behaviour pass `error_on_fail = TRUE`.

Actually — we can keep this simpler. Default `error_on_fail = TRUE` so calls inside running pipelines (which is what the current validators are used in) still abort. Interactive callers and Shiny modules pass `error_on_fail = FALSE` and inspect the structured result. That preserves the existing validate-as-precondition idiom.

### 6.4 What we deliberately don't do (deferred to issue #7)

- CSV upload path on `fct_template_reader()` — issue #7 acceptance criterion
- `generate_metadata_from_csv()` auto-metadata helper — issue #7 acceptance criterion
- `R/mod_validate_upload.R` Shiny module — issue #7 acceptance criterion
- The "6 validation steps" decomposition — issue #7 conceptual restructure
- Unit tests for the new structured return — feasible but adds scope; I'd rather ship the UX now and add tests in a fast-follow

The validator changes here are framed as "UX polish on existing surfaces," **not** "begin implementing #7." Issue #7 stays open.

### 6.5 Imports

Add `cli (>= 3.0)` to `Imports:` in DESCRIPTION. (It's already a transitive dep through `rlang`/`testthat`, but should be explicit.)

### 6.6 Tests

Add `tests/testthat/test-validators-structured-return.R` with ~6 expectations:
- `validate_geometries(ukr_shp)` returns a list with names `c("status", "summary", "issues")`
- `status == "pass"` on the bundled good data
- `length(issues) == 0` on the bundled good data
- `validate_geometries(ukr_shp[1])` (admin0 only — fails parent-child check) → `status %in% c("warn", "fail")`
- Calling with `error_on_fail = TRUE` (default) on bad data throws
- Calling with `error_on_fail = FALSE` on bad data returns the failure list

Plus existing validator tests stay green (the side-effect surface still emits messages, just via cli now).

## 7. Branch & PR plan

```
main
  └─ koichi-arch-redesign
        └─ eb-docs-pkgdown                           ◄── PR #63 base
              └─ chore/resolve-pr63-conflicts        ◄── PR #72 (merged in CI)
                    └─ docs/build-pti-content        ◄── PR-B (this work)
```

PR-B contents (commit-by-commit so review is digestible):

1. `docs(plan): arch-06 plan for build-pti vignette + validator UX` — this file.
2. `docs(dataprep): replace stub with full data-prep reference` — fills `vignettes/dataprep.qmd`.
3. `docs(build-pti): full walkthrough from raw data to live dashboard` — fills `vignettes/articles/build-pti.qmd`.
4. `docs(methodology): slim to conceptual content; cross-link build-pti` — slims `vignettes/articles/methodology.qmd` (gated on user's call in §5.1).
5. `feat(validators): cli output + structured return; backwards-compatible` — refactors the four validators.
6. `test(validators): structured-return invariants` — adds the test file.
7. `chore(plan): tick PLAN.md Phase 4 build-pti + dataprep boxes` — PLAN.md update.
8. `docs(changelog): add 2026-05-08 entries for arch-06` — changelog rows.

**Target:** `eb-docs-pkgdown`. Once PR-A (#72) merges, GitHub auto-rebases PR-B's diff to be cleaner.

### 7.1 Methodology.qmd slim deferred

Per §5.1, commit 4 is dropped. Follow-up issue opened: "Slim methodology.qmd to conceptual content per arch-06 §5."

## 8. Decisions recorded (2026-05-08)

User confirmed the following before content writing:

1. **Methodology.qmd slim** — option (b). Leave methodology.qmd untouched in PR-B; open a follow-up issue tracking the slim. (See §5.1.)
2. **Worked example data** — runnable code in dataprep.qmd §4 uses only the bundled `ukr_shp` + `ukr_mtdt_full` (so `R CMD check` examples pass). `kgzPTIdata` and `zamPTIdata` are private repos; reference them only as "see also" footnotes with the same "request access" framing the existing methodology.qmd uses.
3. **Validator `error_on_fail` default** — `TRUE`. Default behaviour is unchanged (validators abort on `fail`-level issues). Shiny modules and interactive callers explicitly pass `error_on_fail = FALSE` to inspect the structured result instead.
4. **Past-projects.qmd** — out of scope for PR-B. Different audience and content sources; will be filled in a separate effort.

## 9. Out of scope for this plan

- `vignettes/articles/past-projects.qmd` content — separate effort
- README.Rmd changes — README already has a Usage section; we don't duplicate
- Reference index reorganization in `_pkgdown.yml` — already handled by PR #63
- Translation of any content — not on the project's roadmap
- Issue #7 in full — explicitly deferred per user (only the §6 minimal pass is in scope)

---

**Next action after this plan is approved:** start commit 2 (`docs/dataprep.qmd`) since it's the leaf the rest cross-link into.
