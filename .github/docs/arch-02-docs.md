# Documentation Standards — `devPTIpack`

> Guidelines for roxygen2 documentation of all active functions in the package.

---

## Documentation Rules

All roxygen2 rules, required fields, and examples live in a single location:

**[`/.claude/rules/roxygen-documentation.md`](../../.claude/rules/roxygen-documentation.md)**

That file contains:
- Required fields per function type (exported, internal, module, data)
- Full template examples for each type
- General rules (e.g., never combine `@noRd` + `@export`)

---

## Implementation Order

1. **Package data** — `R/data.R` (examples in all other phases depend on these)
2. **Core calculation functions** — `calc_pti_helpers.R`, `calc_pti_expander.R`
3. **Data I/O & validation** — `fct_template_reader.R`, `fct_validate_metadata.R`, `validators.R`, `dta_cleaners.R`
4. **Visualisation helpers** — `plot_pti_helpers.R`, `fct_legend_map_satelites.R`
5. **Entry points & app infrastructure** — `launch_pti.R`, `fct_create_new_pti.R`, `app_config.R`
6. **All remaining persistent modules & utilities**

---

## Notes

- Remove `@noRd` from any function that should have a help page (i.e., all `@export` functions).
- Run `devtools::document()` after each batch to verify `.Rd` generation.
- All examples must pass `devtools::check()`.
