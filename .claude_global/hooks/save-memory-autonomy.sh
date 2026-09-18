#!/usr/bin/env bash
# SessionStart hook (matcher: "startup|resume|clear|fork")
#
# Decides whether Claude may run the save-memory skill on its own judgement in
# this session, and injects that as context when it may.
#
# Why a hook and not CLAUDE.md: CLAUDE.md is static text. It expands @path
# imports and nothing else, so it cannot read an environment variable. The
# `!`cmd`` form works in slash commands, not in CLAUDE.md.
#
# Opt-in, so the safe mode is the default:
#   DOTFILES_CLAUDE_SAVE_MEMORY_AUTO=1  -> autonomous recording
#   DOTFILES_CLAUDE_SAVE_MEMORY_AUTO=0  -> explicit /save-memory only
#   unset (or any other value)          -> explicit /save-memory only
# Only the exact string "1" turns it on. Work sessions therefore need no
# special launch, and a typo fails closed rather than starting to record.
#
# The injected text is written as factual statements rather than imperatives on
# purpose. Imperative, out-of-band-sounding instructions trip Claude's
# prompt-injection defenses, which makes it surface the text to the user
# instead of acting on it.
#   https://code.claude.com/docs/en/hooks#add-context-for-claude
set -euo pipefail

cat >/dev/null # drain the hook input; nothing here needs it

[ "${DOTFILES_CLAUDE_SAVE_MEMORY_AUTO:-}" = "1" ] || exit 0

context='このセッションでは、記憶の保存は自己判断で行われる。後のセッションでも価値が残るもの（決定とその理由、ユーザーの好み、時間を溶かした落とし穴、未決の課題）が会話の中で固まった時点で、頼まれるのを待たずに save-memory スキルが実行される。作業ログや、リポジトリを読めば分かることは対象外。実行は1セッションに数回までで、実行したことは一言だけ報告される。'

if command -v jq >/dev/null 2>&1; then
  jq -nc --arg ctx "$context" \
    '{hookSpecificOutput: {hookEventName: "SessionStart", additionalContext: $ctx}}'
else
  # SessionStart also accepts plain-text stdout as context.
  printf '%s\n' "$context"
fi
