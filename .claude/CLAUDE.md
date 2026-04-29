# Project: devPTIpack

A golem-based Shiny R package for computing, visualizing, and exploring Priority Targeting Indices (PTI).

## Architecture

- Modern path uses: `launch_pti()` → `mod_ptipage_newsrv` → `mod_calc_pti2_server` → `mod_plot_pti2_srv`
- Legacy modules (`mod_weights`, `mod_calc_pti`, `mod_map_pti_leaf`) are being removed.
- See `.github/docs/arch-00-overview.md` for full architecture.
- See `.github/docs/arch-01-cleanup.md` for removal plan.
- See `.github/docs/arch-02-docs.md` for documentation standards.

## Key Conventions

- Follow tidyverse style with `|>` pipe.
- Use roxygen2 for all documentation (see `rules/roxygen-documentation.md`).
- Examples must only use built-in data: `ukr_shp`, `ukr_mtdt_full`.
- Tests use testthat framework.
- Do not touch legacy/dead code marked for removal unless explicitly asked.

## Change Logging (COMPULSORY)

After every code or documentation change, append an entry to `.github/docs/changelog.md`.

**Format:**

```
## YYYY-MM-DD

| Scope  | Change                                  |
| ------ | --------------------------------------- |
| {area} | One-line summary: what was done and why |
```

**Rules:**
- One row per discrete change (file created, function rewritten, test added, etc.)
- Scope = short category: `Docs`, `Code`, `Tests`, `Rules`, `Data`, `Config`
- Keep each summary to one sentence — a reader should grasp the change instantly.
- Group entries under the same date heading when working in the same session.
- Never skip this step. Log before yielding back to the user.

## Branch

- Working branch: `eb-arch-redesign`
- Default branch: `main`
