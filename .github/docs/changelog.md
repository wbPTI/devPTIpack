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
