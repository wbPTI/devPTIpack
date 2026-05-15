---
name: close-issue-on-merge
description: Fallback for closing issues a merged PR was supposed to close but GitHub's auto-close did not fire. PRs targeting `main` with a `Closes #N` body line close on their own; invoke this skill only when the keyword was forgotten, when a parent tracker issue needs closing alongside its directly-referenced sub-issue, or when the user explicitly asks to close an issue tied to a merged PR.
---

# Skill: close-issue-on-merge

When a PR with `Closes #NNN` (or `Fixes` / `Resolves`) merges into the
**default branch**, GitHub auto-closes the linked issue. Since
devPTIpack PRs now target `main` (the default branch), auto-close
fires for the common case and this skill is **rarely needed**.

Invoke this skill only in the residual cases:

- The merged PR body forgot the closing keyword.
- A parent tracker issue (e.g. [#9](https://github.com/worldbank/devPTIpack/issues/9))
  isn't directly referenced by `Closes #N` but the user wants it
  closed once all its sub-issues are done.
- GitHub somehow missed the auto-close (rare but possible if the PR
  description was edited after merge).
- The user explicitly asks to close an issue tied to a merged PR.

## When NOT to invoke

If the PR's `Closes #N` line is intact and the issue is already
`CLOSED`, GitHub's auto-close already fired — do nothing. Closing it
again is a no-op but spends a tool call and adds a noisy comment.

## Inputs (collect before running)

1. The merged PR number.
2. The issue number(s) to close — either supplied by the user or
   parsed from the PR body via the `Closes? #(\d+)` /
   `Fix(es)? #(\d+)` / `Resolves? #(\d+)` patterns.

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

3. **For each issue, confirm it's still open** before closing:

   ```bash
   gh issue view <N> --repo worldbank/devPTIpack --json state \
     --jq '.state'
   ```

   If `OPEN`, proceed. If `CLOSED`, skip — auto-close (or the user)
   already handled it.

4. **Close the issue with a linkback comment** that names the PR + the
   merge commit + the date:

   ```bash
   gh issue close <N> --repo worldbank/devPTIpack --comment \
     "Closed by PR #<PR> (merged YYYY-MM-DD as commit <sha7>)."
   ```

   If the issue is a parent tracker being closed because all its
   sub-issues are now done, expand the comment to name them
   explicitly so the rationale is recoverable later.

5. **Confirm** the issue is now `CLOSED`:

   ```bash
   gh issue view <N> --repo worldbank/devPTIpack --json state,closedAt
   ```

## Hard rules

- Never close an issue whose linked PR is **not yet merged**.
- Never close an issue **silently** — always include the linkback
  comment naming the PR.
- Never close an issue not linked from the PR body unless the user
  explicitly asks. If the PR body has no `Closes #N` and the user
  hasn't named an issue, ask first.
- Never re-open an already-closed issue.

## See also

- [issue-progress-comment](../issue-progress-comment/SKILL.md) — for
  drafting status comments **during** PR work.
- [`.claude/CLAUDE.md`](../../CLAUDE.md) §"Branching" — current
  branching convention (PRs target `main`).
