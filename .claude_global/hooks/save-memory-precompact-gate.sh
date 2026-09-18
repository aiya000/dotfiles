#!/usr/bin/env bash
# PreCompact hook (matcher: "auto|manual")
#
# Requires a fresh /save-memory run (see save-memory-mark.sh) before a MANUAL
# /compact. If save-memory ran since the last compaction, the marker is
# consumed and compaction proceeds. Otherwise compaction is blocked (exit 2)
# and the user is told to run /save-memory first.
#
# Auto-compact is never blocked. Per the Claude Code hooks reference, blocking
# an automatic compaction either (a) silently skips a proactive compaction and
# leaves the conversation uncompacted, or (b) makes the current request fail
# outright when the compaction was recovering from a context-limit error.
# Neither is recoverable from inside this hook, so `auto` is let through.
#   https://code.claude.com/docs/en/hooks#precompact
#
# Note on where the block message lands: Claude Code surfaces it as
# <local-command-stderr>, i.e. to the USER, not to Claude. The text below is
# therefore addressed to the user, in neutral Japanese.
set -euo pipefail

input="$(cat)"

if command -v jq >/dev/null 2>&1; then
  session_id="$(printf '%s' "$input" | jq -r '.session_id // empty')"
  trigger="$(printf '%s' "$input" | jq -r '.trigger // empty')"
else
  session_id="$(printf '%s' "$input" | grep -o '"session_id"[[:space:]]*:[[:space:]]*"[^"]*"' | head -1 | sed -E 's/.*"session_id"[[:space:]]*:[[:space:]]*"([^"]*)".*/\1/')"
  trigger="$(printf '%s' "$input" | grep -o '"trigger"[[:space:]]*:[[:space:]]*"[^"]*"' | head -1 | sed -E 's/.*"trigger"[[:space:]]*:[[:space:]]*"([^"]*)".*/\1/')"
fi

marker_dir="$HOME/.claude/state/save-memory-markers"
marker_file="$marker_dir/${session_id}.marker"

# Housekeeping: markers are only consumed by a compaction, so sessions that
# never compact leave one behind forever. Drop anything older than 30 days.
if [ -d "$marker_dir" ]; then
  find "$marker_dir" -maxdepth 1 -type f -name '*.marker' -mtime +30 -delete 2>/dev/null || true
fi

# Never block an automatic compaction (see the header).
[ "$trigger" = "auto" ] && exit 0

# Without a session id the marker can neither be found nor consumed, so a block
# here would never clear. Fail open.
[ -n "$session_id" ] || exit 0

if [ -f "$marker_file" ]; then
  rm -f "$marker_file"
  exit 0
fi

echo "このセッションではまだ save-memory が実行されていません。compact すると細かい経緯が失われます。先に /save-memory を実行してから、もう一度 /compact してください。" >&2
exit 2
