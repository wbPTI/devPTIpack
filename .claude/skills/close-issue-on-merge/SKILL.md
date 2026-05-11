---
name: close-issue-on-merge
description: After a PR on this repo lands, manually close the issue(s) it claims to close. GitHub will NOT auto-close them because the PRs merge into the non-default `eb-docs-pkgdown` (or `koichi-arch-redesign`) branch instead of `main`. Use after every PR merge that has a "Closes #NNN" body line.
---

# Skill: close-issue-on-merge

When a PR with `Closes #NNN` (or `Fixes` / `Resolves`) merges into the
**default branch**, GitHub auto-closes the linked issue. **devPTIpack
PRs do not merge into the default branch** — the integration branch is
`eb-docs-pkgdown` (under PR #63 → `koichi-arch-redesign` → `main`). So
the closing keyword is recorded but never fires. Issues stay open and
silently rot until someone (you) closes them.

This skill closes them deterministically.

## When to invoke

After **every** PR merge on `worldbank/devPTIpack`, before yielding
back to the user. The user will say something like:

- "PR #N merged"
- "I just merged the PR"
- "Move on to the next PR" (implying the previous one merged)

If the PR landed on `main` directly, skip this skill — GitHub already
closed the issue.

## Inputs (collect before running)

1. The merged PR number.
2. Optionally the linked issue number — if absent, parse the PR body
   for `Closes? #(\d+)` / `Fix(es)? #(\d+)` / `Resolves? #(\d+)`.

## Procedure

1. **Confirm the PR is merged** and grab the merge commit SHA + date:

   ```bash
   gh pr view <PR> --repo worldbank/devPTIpack \
     --json mergedAt,mergeCommit \
     --jq '{mergedAt, sha: .mergeCommit.oid[:7]}'
   ```

   If `mergedAt` is null, halt — the PR isn't actually merged yet.

2. **Resolve the linked issue(s)** from the PR body if not supplied:

   ```bash
   gh pr view <PR> --repo worldbank/devPTIpack --json body \
     --jq '.body' | grep -oiE '(close|closes|closed|fix|fixes|fixed|resolve|resolves|resolved) #[0-9]+' \
     | grep -oE '#[0-9]+' | sort -u
   ```

3. **For each linked issue, confirm it's still open** before closing:

   ```bash
   gh issue view <N> --repo worldbank/devPTIpack --json state \
     --jq '.state'
   ```

   If `OPEN`, proceed. If `CLOSED`, skip (no-op).

4. **Close the issue with a linkback comment** that names the PR + the
   merge commit + the date + the reason auto-close didn't fire:

   ```bash
   gh issue close <N> --repo worldbank/devPTIpack --comment \
     "Closed by PR #<PR> (merged YYYY-MM-DD as commit <sha7>). \
     Note: GitHub does not auto-close issues when PRs merge into a \
     non-default branch (this PR landed on \`eb-docs-pkgdown\`, not \
     \`main\`); closing manually."
   ```

5. **Confirm** the issue is now `CLOSED`:

   ```bash
   gh issue view <N> --repo worldbank/devPTIpack --json state,closedAt
   ```

## Hard rules

- Never close an issue whose linked PR is **not yet merged**.
- Never close an issue **silently** — always include the linkback
  comment naming the PR.
- Never close an issue not linked from the PR body. If the PR body has
  no `Closes #N`, don't infer — ask the user instead.
- Never re-open an already-closed issue. If the previous commenter
  closed it manually, leave it alone.

## Why this skill exists (one-paragraph rationale)

The arch-09 PR sequence (#78–#86, #75, #76, #73) all target
`eb-docs-pkgdown`. PR #63 in turn merges `eb-docs-pkgdown` →
`koichi-arch-redesign`. PR-to-be merges `koichi-arch-redesign` → `main`
much later. GitHub's auto-close fires only when the closing keyword
sits on a PR merging into the **repo's default branch**. So every
sub-PR's `Closes #NNN` is a dead reference until manually closed.
Forgetting means the GitHub Issues page stays cluttered with
tracker-issues that are functionally complete. This skill prevents
that drift.

## See also

- [issue-progress-comment](../issue-progress-comment/SKILL.md) — for
  drafting status comments **during** PR work.
- [`.claude/CLAUDE.md`](../../CLAUDE.md) §"Branching" — explains why
  `main` is not the merge target.
