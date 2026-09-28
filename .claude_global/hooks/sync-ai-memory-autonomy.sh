#!/usr/bin/env bash
# SessionStart hook (matcher: "startup|resume|clear|fork")
#
# Decides whether Claude may run the sync-ai-memory skill on its own judgement
# in this session -- once at the start, and again whenever memory or a handoff
# is about to be read or has just been written -- and injects that as context
# when it may.
#
# This replaced an earlier hook that let Claude run save-memory on its own.
# Writing memory files is now left to an explicit /save-memory; what runs
# unasked is only keeping ~/.ai-memory in step with the cloud sessions.
#
# Why a hook and not CLAUDE.md: CLAUDE.md is static text. It expands @path
# imports and nothing else, so it cannot read an environment variable. The
# `!`cmd`` form works in slash commands, not in CLAUDE.md.
#
# Opt-in, so the safe mode is the default:
#   DOTFILES_CLAUDE_SYNC_AI_MEMORY_AUTO=1  -> autonomous sync
#   DOTFILES_CLAUDE_SYNC_AI_MEMORY_AUTO=0  -> explicit /sync-ai-memory only
#   unset (or any other value)             -> explicit /sync-ai-memory only
# Only the exact string "1" turns it on. Work sessions therefore need no
# special launch, and a typo fails closed rather than starting to push.
#
# The injected text is written as factual statements rather than imperatives on
# purpose. Imperative, out-of-band-sounding instructions trip Claude's
# prompt-injection defenses, which makes it surface the text to the user
# instead of acting on it.
#   https://code.claude.com/docs/en/hooks#add-context-for-claude
set -euo pipefail

cat >/dev/null # drain the hook input; nothing here needs it

[ "${DOTFILES_CLAUDE_SYNC_AI_MEMORY_AUTO:-}" = "1" ] || exit 0

context='このセッションでは、~/.ai-memory の同期は自己判断で行われる。セッションの始めに、最初の用件に取りかかる前に sync-ai-memory スキル（prepare）が一度実行される。その後も、記憶ファイルや引継ぎを読む前、他のセッション（クラウドを含む）で書かれたものが要りそうなとき、~/.ai-memory に書いたものがまだ push されていないときには、頼まれるのを待たずに sync-ai-memory が実行される。記憶ファイルそのものを新しく書くこと（save-memory）は、頼まれたときだけ行われる。実行したことは一言だけ報告される。'

if command -v jq >/dev/null 2>&1; then
  jq -nc --arg ctx "$context" \
    '{hookSpecificOutput: {hookEventName: "SessionStart", additionalContext: $ctx}}'
else
  # SessionStart also accepts plain-text stdout as context.
  printf '%s\n' "$context"
fi
