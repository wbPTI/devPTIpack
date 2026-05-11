#!/usr/bin/env bash
# Stop hook: auto-draft changelog entries for the current uncommitted diff.
#
# Behaviour:
#   - If the changelog already shows a row for today covering each changed
#     relevant file, do nothing.
#   - Otherwise, append draft rows (one per file) under today's date heading,
#     each tagged with an <!-- AUTODRAFT:<path> --> marker for idempotency.
#   - Exit 0 either way — this hook is a safety net, not a blocker.
#
# Refining the draft:
#   AUTODRAFT rows are placeholders. Replace each summary with a real one
#   before yielding to the user. The marker can be left in place or removed.
#
# Reference: .claude/CLAUDE.md § "Change Logging (COMPULSORY)".

set -uo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null)" || exit 0
CHANGELOG="$REPO_ROOT/.github/docs/changelog.md"
[ -f "$REPO_ROOT/DESCRIPTION" ] || exit 0   # not in the devPTIpack repo
[ -f "$CHANGELOG" ] || exit 0               # missing — don't crash

cd "$REPO_ROOT" || exit 0

# Collect modified-tracked + untracked files (no renames-as-arrows confusion).
CHANGED=()
while IFS= read -r line; do
  [ -n "$line" ] && CHANGED+=("$line")
done < <(
  {
    git diff --name-only HEAD 2>/dev/null
    git ls-files --others --exclude-standard 2>/dev/null
  } | sort -u
)

[ "${#CHANGED[@]}" -gt 0 ] || exit 0

# Filter to relevant paths.
RELEVANT=()
for f in "${CHANGED[@]}"; do
  case "$f" in
    .claude/settings.local.json) ;;        # local-only, never logged
    *.Rproj|.Rhistory|.RData|.Rprofile) ;; # R session detritus
    *) RELEVANT+=("$f") ;;
  esac
done
[ "${#RELEVANT[@]}" -gt 0 ] || exit 0

# If the changelog itself is in the diff, assume Claude touched it this turn
# and short-circuit. The reviewer agent / human review still catches missing
# entries.
for f in "${RELEVANT[@]}"; do
  if [ "$f" = ".github/docs/changelog.md" ]; then
    exit 0
  fi
done

scope_for() {
  case "$1" in
    R/*|inst/*) echo Code ;;
    tests/*) echo Tests ;;
    vignettes/*|_pkgdown.yml|README*|PLAN.md|.github/docs/*) echo Docs ;;
    .claude/rules/*) echo Rules ;;
    .claude/skills/*|.claude/agents/*|.claude/hooks/*|.claude/CLAUDE.md|.claude/settings.json) echo Tooling ;;
    DESCRIPTION|NAMESPACE|.Rbuildignore|.gitignore) echo Config ;;
    data/*) echo Data ;;
    *) echo Other ;;
  esac
}

today="$(date +%Y-%m-%d)"
header="## $today"

# Ensure today's section exists.
if ! grep -qF "$header" "$CHANGELOG"; then
  {
    printf '\n'
    printf '%s\n' "$header"
    printf '\n'
    printf '| Scope  | Change                                  |\n'
    printf '| ------ | --------------------------------------- |\n'
  } >> "$CHANGELOG"
fi

# Extract today's section to test for prior mentions of each path.
today_section="$(
  awk -v hdr="$header" '
    $0 == hdr { in_section = 1; next }
    /^## / && in_section { exit }
    in_section { print }
  ' "$CHANGELOG"
)"

rows_added=0
for f in "${RELEVANT[@]}"; do
  marker="<!-- AUTODRAFT:$f -->"

  # Skip if today's section already references this file (drafted or
  # human-refined).
  if printf '%s\n' "$today_section" | grep -qF -- "$f"; then
    continue
  fi

  scope="$(scope_for "$f")"
  printf '| %-6s | Updated `%s` (auto-drafted — please refine). %s |\n' \
    "$scope" "$f" "$marker" >> "$CHANGELOG"
  rows_added=$((rows_added + 1))
done

if [ "$rows_added" -gt 0 ]; then
  printf '[auto-changelog] Drafted %d row(s) in .github/docs/changelog.md.\n' "$rows_added" >&2
  printf '[auto-changelog] Replace each AUTODRAFT placeholder with a specific summary.\n' >&2
fi

exit 0
