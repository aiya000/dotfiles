#!/usr/bin/env bash
# PostToolUse hook (matcher: "Skill")
# Fires for every Skill invocation; only marks when tool_input.skill == "save-memory".
# Marks that /save-memory just ran for this session, so the PreCompact gate
# knows it's safe to let the next auto-compact through.
set -euo pipefail

input="$(cat)"

if command -v jq >/dev/null 2>&1; then
  session_id="$(printf '%s' "$input" | jq -r '.session_id // empty')"
  skill_name="$(printf '%s' "$input" | jq -r '.tool_input.skill // empty')"
else
  session_id="$(printf '%s' "$input" | grep -o '"session_id"[[:space:]]*:[[:space:]]*"[^"]*"' | head -1 | sed -E 's/.*"session_id"[[:space:]]*:[[:space:]]*"([^"]*)".*/\1/')"
  skill_name="$(printf '%s' "$input" | grep -o '"skill"[[:space:]]*:[[:space:]]*"[^"]*"' | head -1 | sed -E 's/.*"skill"[[:space:]]*:[[:space:]]*"([^"]*)".*/\1/')"
fi

[ "$skill_name" = "save-memory" ] || exit 0
[ -n "$session_id" ] || exit 0

marker_dir="$HOME/.claude/state/save-memory-markers"
mkdir -p "$marker_dir"
touch "$marker_dir/${session_id}.marker"

exit 0
