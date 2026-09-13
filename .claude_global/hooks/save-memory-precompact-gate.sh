#!/usr/bin/env bash
# PreCompact hook (matcher: "auto|manual")
# Requires a fresh /save-memory run (see save-memory-mark.sh) before every
# single compaction, whether auto-triggered or a manual /compact. If
# save-memory ran since the last compaction, the marker is consumed and
# compaction proceeds. Otherwise compaction is blocked (exit 2) with a
# reason telling Claude to run /save-memory now.
set -euo pipefail

input="$(cat)"

if command -v jq >/dev/null 2>&1; then
  session_id="$(printf '%s' "$input" | jq -r '.session_id // empty')"
else
  session_id="$(printf '%s' "$input" | grep -o '"session_id"[[:space:]]*:[[:space:]]*"[^"]*"' | head -1 | sed -E 's/.*"session_id"[[:space:]]*:[[:space:]]*"([^"]*)".*/\1/')"
fi

marker_dir="$HOME/.claude/state/save-memory-markers"
marker_file="$marker_dir/${session_id}.marker"

if [ -n "$session_id" ] && [ -f "$marker_file" ]; then
  rm -f "$marker_file"
  exit 0
fi

echo "Compaction (auto or manual /compact) is about to run and will discard fine-grained conversation detail. Run /save-memory RIGHT NOW, before doing anything else, to preserve this session's important context. Compaction was deferred this turn and will be retried automatically on the next check." >&2
exit 2
