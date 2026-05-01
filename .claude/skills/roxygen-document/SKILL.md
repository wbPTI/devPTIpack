---
name: roxygen-document
description: Add or upgrade roxygen2 documentation for one or more devPTIpack functions per .claude/rules/roxygen-documentation.md and .github/docs/arch-02-docs.md — picks correct template (exported / internal / module / data), uses ukr_shp & ukr_mtdt_full in @examples, runs devtools::document() and verifies devtools::check() examples pass.
---

# Skill: roxygen-document

Document permanent devPTIpack functions to the standard defined by
[`.claude/rules/roxygen-documentation.md`](../../rules/roxygen-documentation.md)
(GitHub issue [#11](https://github.com/worldbank/devPTIpack/issues/11)).

## When to invoke

- Opportunistically while writing tests for a function (hybrid TDD+docs flow).
- During Phase 3 dedicated documentation sweeps.
- Whenever a function's signature changes during cleanup or refactor.

## Inputs

1. Target function name(s) and source file(s).
2. Visibility classification per arch-01 § "Permanent Functions"
   (EXPORTED / INTERNAL).
3. Function type: regular / Shiny module / package data.

## Procedure

1. **Read** the function and its current docs (if any).
2. **Pick the template** from `.claude/rules/roxygen-documentation.md`:
   - Exported regular function
   - Internal function (`@noRd`, no examples)
   - Exported Shiny module (document reactive args, `\dontrun{}` examples)
   - Package data (`@format`, `@source`, runnable examples)
3. **Fill in fields** in this order:
   - Title — one sentence, *what* the function does.
   - Description — paragraph(s) on *how* / when to use, only if non-obvious.
   - `@param` — every parameter, with type + purpose.
   - `@return` — explicit type and structure (e.g. "A `tbl_df` with columns
     `admin_level` (chr), `value` (dbl). Empty when … ").
   - `@importFrom` — the specific symbols used.
   - `@export` for EXPORTED, `@noRd` for INTERNAL — never both.
   - `@examples` — only `ukr_shp` / `ukr_mtdt_full` data; module examples in
     `\dontrun{}`.
4. **If the function has no clear contract**, document the *current* behaviour
   verbatim (including known quirks pinned in tests) — do not invent
   semantics. Add a `@note` referencing the test that pins the quirk.
5. **Regenerate**: `Rscript -e 'devtools::document()'`.
6. **Verify**: `Rscript -e 'devtools::check_man()'` plus
   `Rscript -e 'devtools::run_examples(run_dontrun = FALSE)'`. All must pass
   before yielding.
7. **Changelog row** under Scope `Docs`.
8. **Sync `PLAN.md`.** Tick the relevant phase under §6 / Phase 3 and
   note the PR number in the "Progress log". See `.claude/CLAUDE.md`
   § "PLAN.md Sync (COMPULSORY)".

## Hard rules

- Never combine `@noRd` with `@export`.
- Examples must run in under 2s and use only bundled data.
- Internal functions have **no** `@examples`.
- Don't soften descriptions to hide bugs — pin the current behaviour and
  note where the test is.
- Don't write multi-paragraph prose where a list will do.

## Order (for batch documentation)

Follow `.github/docs/arch-02-docs.md` § "Implementation Order":

1. `R/data.R` (so `@examples` references work)
2. `R/calc_pti_helpers.R`, `R/calc_pti_expander.R`
3. `R/fct_template_reader.R`, `R/fct_validate_metadata.R`, `R/validators.R`,
   `R/dta_cleaners.R`
4. `R/plot_pti_helpers.R`, `R/fct_legend_map_satelites.R`
5. `R/launch_pti.R`, `R/fct_create_new_pti.R`, `R/app_config.R`
6. Remaining modules and helpers.

## Reporting

After a batch, summarise:
- Functions documented: N
- New help pages: list
- Examples runtime: total seconds
- Doc-related notes from `R CMD check`: should be 0
