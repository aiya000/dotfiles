#!/usr/bin/env bash
# Counts Claude tokens for each file given, using the real tokenizer through
# `claude -p` (subscription; no API key needed). Each file costs one short
# request whose system prompt is cached, so the per-file cost is the file
# itself plus a few tokens.
#
# Usage: count-tokens.sh FILE...
#
# Method: the file is piped into a tiny prompt; the total input tokens of that
# request minus the total of an empty prompt is the file's token count. The
# three usage counters are summed because the CLI caches the prompt prefix,
# which moves the file's tokens into cache_creation_input_tokens.
set -euo pipefail

total() {
  env -u CLAUDECODE timeout 180 claude -p --model opus --output-format json \
    --no-session-persistence 2>/dev/null |
    jq -r '.usage | .input_tokens + .cache_creation_input_tokens + .cache_read_input_tokens'
}

base=$(printf 'Reply with exactly: ok' | total)
printf '%-60s %8s %8s\n' file bytes tokens
for f in "$@"; do
  t=$( (cat "$f"; printf '\n\nReply with exactly: ok') | total )
  printf '%-60s %8d %8d\n' "$f" "$(wc -c < "$f")" "$((t - base))"
done
