---
name: issue-progress-comment
description: Draft a status comment for a devPTIpack GitHub issue (#8/#10/#11/#12/#13) summarising the most recent commits/PRs, ticking acceptance-criteria checkboxes that are now satisfied, and listing the next concrete actions — does not post unless the user confirms.
---

# Skill: issue-progress-comment

Draft a progress comment for an issue under
[`worldbank/devPTIpack#9`](https://github.com/worldbank/devPTIpack/issues/9).
Posting requires explicit user confirmation — the skill **drafts and shows**;
the user pastes or asks for `gh issue comment` to be run.

## Inputs

1. Issue number (must be one of #8, #10, #11, #12, #13, or another active
   issue under #9).
2. Optional: a date range (default = "since the last comment by current user
   on the issue").

## Procedure

1. **Pull the issue.** `gh issue view <N> --json title,body,comments`.
   Identify the acceptance-criteria checklist in the body.
2. **Pull recent activity.**
   - `git log --oneline --since="<since>"`.
   - `gh pr list --state all --search "in:title <area>" --json number,title,state,mergedAt`.
   - Recent rows in `.github/docs/changelog.md`.
3. **Map work to acceptance criteria.** For each `[ ]` in the issue body,
   check whether the recent activity completes it. Be conservative — only
   flag a box if there is concrete evidence (a merged PR, a test file
   committed, a doc generated).
4. **Draft a comment** with this structure:

   ```
   ### Progress update — <YYYY-MM-DD>

   **What landed**
   - <PR #N> — one-line summary
   - <commit sha> — one-line summary

   **Acceptance criteria now satisfied** (please verify)
   - [x] <criterion>
   - [x] <criterion>

   **Still open**
   - <criterion>
   - <criterion>

   **Next**
   - <concrete next action> — owner: <@user or unassigned>
   - <concrete next action>

   **Notes / decisions**
   - Optional: any deviation from arch-XX docs and why.
   ```

5. **Show the draft to the user.** Do not post.
6. **On confirmation only**, run:
   `gh issue comment <N> --body-file <tempfile>`.

## Hard rules

- Never post without explicit user confirmation.
- Never tick a checkbox unless evidence exists in commits/PRs/tests.
- Never include private credentials, machine paths, or unredacted error
  output in the comment.
- Keep the comment under ~250 words. If the work is broader, link to a PR
  or PLAN.md section instead of inlining detail.
- Match the project tone — concise, technical, no emojis.

## Reporting

Return the drafted comment to the user with a one-line summary of which
acceptance criteria it claims to satisfy.
